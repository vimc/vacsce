#' saninty check for input date
#' @param input
#' input is a list of objects. It consists of
#'  user - vimc (requires db connection) or external (matches montagu convention)
#'  coverage_source - touchstone for vimc, any narrative for external
#'  year_current - current year
#'  country - ISO3 code
#'  disease - disease name
#'  r_int - routine introduction year
#'  rout_hist - historical routine data, nullable if not applicable or future intro
#'  sia_hist - historical campaign data, nullable if not applicable or not of interest,
#'  proj_rules - projection rule as a list (keep_levels, incremental, catch_up_with_x, non_linear_scale_up), the combination of them make full projection trajectory
#'  keep_levels - year_from, year_to, level
#'  incremental - year_from, year_to, step
#'  catch_up_with_x - year_from, year_to, vaccine_x # usually for routine introduction
#'  non_linear_scale_up - year_from, year_to, endpoint
#'  each rule can be used multiple of times, BUT, rules have to be set-up according to timeline
#' sainty check for input data
#' input - input parameters
#' con - montagu database connection
input_check <- function(input, con){
  ## input is a list of objects containing
  stopifnot(user == "vimc" | user == "external")
  stopifnot(length(country) == 1L) # work for one country only
  stopifnot(length(disease) == 1L) # work for one disease only

  if (user == "vimc"){
    ## log-on to montagu and extract historical coverage
    ## over-write params r_int, rout_hist and sia_hist
    touch <- vimpact::get_touchstone(con, input$coverage_source)
    dat <- DBI::dbGetQuery(con,
                           paste("SELECT country, vaccine, activity_type, year, age_from, age_to, gender, target, coverage, proportion_risk",
                                 "FROM coverage_set JOIN coverage ON coverage.coverage_set = coverage_set.id",
                                 "WHERE country = $1",
                                 sprintf("AND name LIKE %s", paste0("'%", input$disease, "%'")),
                                 "AND touchstone = $2",
                                 "AND year <= $3",
                                 "AND coverage > 0"),
                           list(input$country,
                                touch,
                                input$year_current))
    input$rout_hist <- dat[dat$activity_type == "routine", ]
    input$sia_hist <- dat[dat$activity_type == "campaign", ]
    input$r_int <- min(input$rout_hist$year)
    message("Further expand if activity_types other than routine and campaign, e.g. routine_intensified")
  }

  ## satiny check for rules
  x <- length(input$proj_rules)
  sprintf("%s projection rules identified", length(input$proj_rules))
  for (i in seq_len(x)){
    print(paste("rule", i, "-",names(input$proj_rules[i])))
    print(as.data.frame(input$proj_rules[[i]]))
    stopifnot(input$proj_rules[[i]]$year_from <= input$proj_rules[[i]]$year_to)
    if(i < x){
      if(input$proj_rules[[i]]$year_to+1 == input$proj_rules[[i+1]]$year_from){
        stop("Please check continuity of time periods for projection")
      }
    }
  }

}

vac_sce <- function(input){

}

## projection rules
## keep at certain coverage level
keep_levels <- function(d, year_from, year_to, level){
 t <- d[1, ] %>%
   select(-year) %>%
   right_join(data.frame(year = year_from:year_to)) %>%
   mutate(coverage = level) %>%
   bind_rows(d) %>%
   arrange(year)
 return(t)
}

## equal-step increase
incremental <- function(d, year_from, year_to, step){
  y <- max(d$year)
  t <- d[d$year == y, ] %>%
    select(-year) %>%
    right_join(data.frame(year = year_from:year_to)) %>%
    mutate(coverage = coverage + step*(year - y)) %>%
    bind_rows(d) %>%
    arrange(year)
  stop("add threshold")
  return(t)
}

## this is specifically for routine intro, but can be used flexibly
catch_up_with_x <- function(d, year_from, year_to, vaccine_x){
  if (nrow(d) == 0){
    ## routine introduction
    c0 <- disease_x/3 # intro at 33% of target vaccine
  }
}

## ia2030 non-linear scale-up function
non_linear_scale_up <- function(d, year_from, year_to, endpoint){

}

## sanity checks
## what to check?
## continuity, range, trend, pop, age, etc
