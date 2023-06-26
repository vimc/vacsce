#' saninty check for input date
#' @param input
#' input is a list of objects. It consists of
#'  user - vimc (requires db connection) or external (matches montagu convention)
#'  coverage_src - touchstone for vimc, any narrative for external
#'  year_cur - current year
#'  country - ISO3 code
#'  disease - disease name
#'  r_int - routine introduction year
#'  rout_his - historical routine data, nullable if not applicable or future intro
#'  sia_his - historical campaign data, nullable if not applicable or not of interest,
#'  rout_fut - historical routine data, nullable if not applicable or future intro
#'  sia_fut - historical campaign data, nullable if not applicable or not of interest,
#'  proj_rul - projection rule as a list (keep_levels, incremental, catch_up_with_x, non_linear_scale_up), the combination of them make full projection trajectory
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
  stopifnot(input$user == "vimc" | input$user == "external")
  stopifnot(length(input$country) == 1L) # work for one country only
  stopifnot(length(input$disease) == 1L) # work for one disease only

  if (input$user == "vimc"){
    ## log-on to montagu and extract historical coverage
    ## over-write params r_int, rout_hist and sia_hist
    touch_his <- vimpact::get_touchstone(con, input$coverage_src_his)
    touch_fut <- vimpact::get_touchstone(con, input$coverage_src_fut)

    his <- DBI::dbGetQuery(con,
                           paste("SELECT country, vaccine, activity_type, year, age_from, age_to, gender, target, coverage, proportion_risk",
                                 "FROM coverage_set JOIN coverage ON coverage.coverage_set = coverage_set.id",
                                 "WHERE country = $1",
                                 sprintf("AND name LIKE %s", paste0("'%", input$disease, "%'")),
                                 "AND touchstone = $2",
                                 "AND coverage > 0",
                                 "AND year <= $3"),
                           list(input$country,
                                touch_his,
                                input$year_cur))
    fut <- DBI::dbGetQuery(con,
                           paste("SELECT country, vaccine, activity_type, year, age_from, age_to, gender, target, coverage, proportion_risk",
                                 "FROM coverage_set JOIN coverage ON coverage.coverage_set = coverage_set.id",
                                 "WHERE country = $1",
                                 sprintf("AND name LIKE %s", paste0("'%", input$disease, "%'")),
                                 "AND touchstone = $2",
                                 "AND coverage > 0",
                                 "AND year > $3"),
                           list(input$country,
                                touch_fut,
                                input$year_cur))

    input$rout_his <- his[his$activity_type == "routine", ]
    input$sia_his <- his[his$activity_type == "campaign", ]
    input$rout_fut <- fut[fut$activity_type == "routine", ]
    input$sia_fut <- fut[fut$activity_type == "campaign", ]
    x <-bind_rows(input$rout_his, input$rout$fut)
    input$rout_int <- x %>% group_by(vaccine, activity_type) %>%
      summarise(year_intro = min(year), .groups = "keep") %>%
      as.data.frame()
    message("Further expand if activity_types other than routine and campaign, e.g. routine_intensified")
  }

  ## satiny check for rules
  for(j in seq_len(length(input$proj_rul))){
    r <- input$proj_rul[[j]]
    x <- length(r)
    print(sprintf("%s projection rules identified for %s", x, input$rout_int$vaccine[j]))
    for (i in seq_len(x)){
      print(paste("rule", i, "-",names(r[i])))
      print(as.data.frame(r[[i]]))
      stopifnot(r[[i]]$year_from <= r[[i]]$year_to)
      if(i < x){
        if(r[[i]]$year_to+1 != r[[i+1]]$year_from){
          stop("Please check continuity of time periods for projection")
        }
      }
    }
  }


  return(input)
}

vac_sce <- function(input){
  x <- nrow(input$rout_int)

  for(i in seq_len(x)){
    # for each routine vaccine do scenario projection
    v <- input$rout_int$vaccine[i] # vaccine name
    print(sprintf("projecting trajectory for vaccine %s", v))

    r <- input$proj_rul[[i]] # projection rules
    for(j in seq_len(length(r))){
      func <- paste0(names(r[j]), sub("list", "",paste(r[j])))
      print(func)
      #eval(parse(text = func))
    }

  }
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
## d is a dataframe  (year, coverage)
incremental <- function(d, year_from, year_to, step, cap = 0.95){
  y <- max(d$year)
  t <- d[d$year == y, ] %>%
    select(-year) %>%
    right_join(data.frame(year = year_from:year_to)) %>%
    mutate(coverage = coverage + step*(year - y)) %>%
    bind_rows(d) %>%
    arrange(year) %>%
    mutate(coverage = ifelse(coverage > cap, cap, coverage))
  return(t)
}

## this is specifically for routine intro, but can be used flexibly
## d is a dataframe  (year, coverage)
catch_up_with_x <- function(d, year_from, year_to, vaccine_x_level, intro_level = 1/3){
  years <- seq(year_from, year_to, 1)
  cov <- rep(NA, length(years))
  if (nrow(d) == 0){
    ## routine introduction
    cov[1] <- vaccine_x_level*intro_level # intro at 33% of target vaccine
    cov[-1] <- IA2030_projection(year_from, covs[1], year_to, vaccine_x_level)
  } else {
    ## non-lenear scale-up
    message("intro_level is not used as not applicable")
    cov <- IA2030_projection(year_from, d[d$year == year_from -1], year_to, vaccine_x_level)
  }
  dat <- bind_rows(d, data.frame(year = years, coverage = cov))
  return(dat)
}

## ia2030 non-linear scale-up function
non_linear_scale_up <- function(d, year_from, year_to, endpoint){
  cov <- IA2030_projection(year_from, d[d$year == year_from -1], year_to, endpoint)
  dat <- bind_rows(d, data.frame(year = years, coverage = cov))
  return(dat)
}


## ia2030 non-linear function
IA2030_projection <- function(t0, c0, T, cT){
  ## per country, per vaccine
  stopifnot(t0 < T)
  if(T-t0 == 1){
    return(c(cT))
  } else {
    v <- seq_len(T - t0 -1)
    k <- log((1-cT)/(1-c0)) / (T-t0)
    l <- 1-c0
    for(i in seq_along(v)){
      v[i] <- 1 - l*exp(i * k)
    }
    return(c(v, cT))
  }
}


## sanity checks
## what to check?
## continuity, range, trend, pop, age, etc
