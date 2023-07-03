#' saninty check for input date
#' @param input
#' input - input parameters
#' can project deliveries one-by-one if one depends on another, e.g. campaign frequency may be determined by past routine services
# for each delivery can also work on rules one-by-one if a secondary rule depends on earlier projection, e.g. add a condition, if covid disruption..., if recovered, ...
# I will provide examples
input_check <- function(input){
  ## input is a list of objects
  ## need meaningful checks to avoid errors
  message("Always specify campaign in introduction as the last entry! - todo: make this identifiable")
  country <- unique(input$params$country)
  disease <- unique(input$params$disease)
  year_cur <- unique(input$params$year_cur)
  introduction <- input$params$introduction
  stopifnot(length(country) == 1L) # work for one country only
  stopifnot(length(disease) == 1L) # work for one disease only
  stopifnot(length(year_cur) == 1L) # curent year

  ## filter data
  his <-  input$src$historic %>%
    filter(country == !!country) %>%
    right_join(introduction %>% select(-year_intro), by = c("vaccine", "activity_type"))

  fut <-  input$src$future %>%
    filter(country == !!country) %>%
    right_join(introduction %>% select(-year_intro), by = c("vaccine", "activity_type"))

  ## check introduction
  ## merge data source and user specified introduction dates
  s <- his %>% group_by(vaccine, activity_type) %>%
    filter(activity_type != "campaign") %>%
    summarise(year_intro_db = min(year), .groups = "keep") %>%
    as.data.frame() %>%
    right_join(introduction, by = c("vaccine", "activity_type")) %>%
    mutate(conflict = (!is.na(year_intro) & !is.na(year_intro_db) & year_intro != year_intro_db))
  if (any(s$conflict)){
    message("conflict in introduction assumption")
    message("you may want to act as external user making up hypothetic historical coverage other than actual?")
    stop(print(s))
  }

  input$introduction <- s %>%
    mutate(year_intro = ifelse(is.na(year_intro), year_intro_db, year_intro)) %>%
    select(vaccine, activity_type, year_intro) %>%
    mutate(future_introduction = !is.na(year_intro) & year_intro > year_cur)
  input$his <- his %>% right_join(input$introduction %>% filter(!future_introduction), by = join_by("vaccine", "activity_type")) %>%
    select(-year_intro, - future_introduction)
  input$fut <- fut # future coverage is used for each delivery if no corresponding projection rule(s) specified
  message("Further expand if activity_types other than routine and campaign, e.g. routine_intensified")

  ## satiny check for rules
  for(j in seq_len(length(input$proj_rul))){
    r <- input$proj_rul[[j]]
    x <- length(r)
    if(x == 0){
      message(sprintf("%s projection rules identified for %s %s; use any future coverage specified.", x, input$introduction$activity_type[j], input$introduction$vaccine[j]))
    } else {
      message(sprintf("%s projection rules identified for %s %s", x, input$introduction$activity_type[j], input$introduction$vaccine[j]))
    }
    for (i in seq_len(x)){
      # print(paste("rule", i, "-",names(r[i])))
      # print(as.data.frame(r[[i]]))
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
  his <- input$his
  fut <- input$fut

  x <- nrow(input$introduction)

  dat <- NULL
  for(i in seq_len(x)){
    # for each vaccine delivery do scenario projection
    print(sprintf("projecting trajectory for %s %s", input$introduction$activity_type[i], input$introduction$vaccine[i]))
    d0 <- his %>%
      right_join(input$introduction[i, c("vaccine", "activity_type")], by = c("vaccine", "activity_type") )
    d1 <- fut %>%
      right_join(input$introduction[i, c("vaccine", "activity_type")], by = c("vaccine", "activity_type") )
    d <- d0 %>%
      select(year, coverage, age_from, age_to) %>%
      filter(!is.na(year)) %>%
      arrange(year)
    r <- input$proj_rul[[i]] # projection rules

    if(!is.null(r) & input$introduction$activity_type[i] != "campaign"){
      ## this is routine projection only
      ## campaign may depend on routine projection
      ## hence need to be run separately
      for(j in seq_len(length(r))){
        func <- paste0(names(r[j]), sub("list\\(", "(d, ", paste(r[j])))
        print(func)
        d <- eval(parse(text = func))
      }
      dat <- bind_rows(dat, d %>% bind_cols(input$introduction[i, c("vaccine", "activity_type")]))
    } else if(is.null(r)){
      print("No projection. Binding data from source coverage.")
      dat <- bind_rows(dat, d0, d1)
    } else {
      ## campaign projection
      for(j in seq_len(length(r))){
        func <- paste0(names(r[j]), sub("list\\(", "(d, dat, ", paste(r[j])))
        func <- gsub('\"', "'", func, fixed = TRUE)
        print(func)
        dat <- eval(parse(text = func)) %>%
          merge(input$introduction[i, c("vaccine", "activity_type")]) %>%
          bind_rows(dat)
      }
    }
  }
  dat <- dat %>%
    mutate(country = input$params$country,
           disease = input$params$disease)

  ##
}

## projection rules
## rule a.) keep at certain coverage level
## d is a data frame containing at least (year, coverage)
keep_levels <- function(d, year_from, year_to, level){
  t <- d[1, ] %>%
    select(-year) %>%
    merge(data.frame(year = year_from:year_to)) %>%
    mutate(coverage = level) %>%
    bind_rows(d) %>%
    arrange(year)
  return(t)
}

## rule b.) equal-step increase
## d is a data frame containing at least (year, coverage)
incremental <- function(d, year_from, year_to, step, cap = 0.95){
  y <- max(d$year)
  t <- d[d$year == y, ] %>%
    select(-year) %>%
    merge(data.frame(year = year_from:year_to)) %>%
    mutate(coverage = coverage + step*(year - y)) %>%
    bind_rows(d) %>%
    arrange(year)
  cap = max(c(cap, max(t$coverage))) # cap at specified or historical highest
  t <- t %>%
    mutate(coverage = ifelse(coverage > cap, cap, coverage))
  return(t)
}

## rule c.) this is specifically for routine intro, but can be used flexibly
## d is a data frame containing at least (year, coverage)
catch_up_with_x <- function(d, year_from, year_to, vaccine_x_level, intro_level = 1/3){
  years <- seq(year_from, year_to, 1)
  cov <- rep(NA, length(years))
  if (nrow(d) == 0){
    ## routine introduction
    cov[1] <- vaccine_x_level*intro_level # intro at 33% of target vaccine
    cov[-1] <- IA2030_projection(year_from, cov[1], year_to, vaccine_x_level)
  } else {
    ## non-lenear scale-up
    message("intro_level is not used as not applicable")
    cov <- IA2030_projection(year_from-1, d[d$year == year_from -1], year_to, vaccine_x_level)
  }
  dat <- bind_rows(d, data.frame(year = years, coverage = cov))
  return(dat)
}

## rule d.) ia2030 non-linear scale-up function
## d is a data frame containing at least (year, coverage)
non_linear_scale_up <- function(d, year_from, year_to, endpoint){
  years <- seq(year_from, year_to, 1)
  cov <- IA2030_projection(year_from-1, d$coverage[d$year == year_from -1], year_to, endpoint)
  dat <- bind_rows(d, data.frame(year = years, coverage = cov))
  return(dat)
}

## rule e.) projection rule for follow-up campaigns
## depending on the most recent campaign, and routine introduction data and routine coverage level of vaccine_base,
## project future follow-up campaign
## this function is designed under WHO M/MR guidance
## if this is also to be used for other diseases, may need to adjust according to relevant guidance
## d consists of
sia_follow_up <- function(d, dat, vaccine_base, year_current, year_to, look_back = 4, sia_level = 0.9){
  ## vaccine_base is a baseline vaccine for evaluating follow-up campaign frequency
  ## evaluate vaccine_base levels from a baseline year
  ## baseline year is determined by year_intro, year_current, and last_sia_year
  ## if no historical sia, baseline_year = year_current or future_year_intro if applicable
  ## if last_sia_year < year_current-look_back, baseline_year = year_current or future_year_intro if applicable
  ## if last_sia_year > year_current-look_back & year_intro <= last_sia_year, baseline_year = last_sia_year
  ## if last_sia_year > year_current-look_back & year_intro > last_sia_year, baseline_year = year_intro

  ## otherwise baseline_year = year_current

  ## MR follow-up sia frequency
  ## every 2 years if mcv1 level is < 60%
  ## every 3 years if mcv1 level is 60 - 80%
  ## every 4 years if mcv1 level is >= 80%
  ## N.B. if projected sia didn't happen, i.e. not recorded as a historical sia, postpone to year_current+1

  ## determine baseline year
  b <- dat[dat$vaccine == vaccine_base, ]
  y_int <- min(b$year)
  y_current <- year_current

  if(nrow(d) == 0L){
    y_base <- max(y_int, y_current)
  } else {
    y_last_sia <- max(d$year)
    if(y_last_sia < y_current - look_back){
      y_base <- max(y_int, y_current)
    } else if(y_last_sia > y_current - look_back & y_last_sia >= y_int){
      y_base <- y_last_sia
    } else {
      y_base <- y_int
    }
  }

  ## project future campaigns from y_base to year_to
  i <- y_base
  t <- NULL
  while (i < year_to) {
    ## evaluate vaccine_base, and determine next sia year
    if(b$coverage[b$year == i] < 0.6){
      i <- i + 2
    } else if(b$coverage[b$year == i] >= 0.8){
      i <- i + 4
    } else {
      i <- i +3
    }
    if (i < y_current){
      i <- y_current + 1
    }
    t <- c(t, i)
  }
  t <- data.frame(year = t,
             coverage = sia_level,
             age_from = 1,
             age_to = 5)

  return(bind_rows(d, t))
}

sia_catch_up <- function(d, year_intro, year_current, age_from, age_to){
  ## this is relevant to MenA, Typhoid, and HPV mult-cohort SIAs before routine introduction
  ## we need are year_intro and age groups
  ## if previously SIAs happened, only target missed cohorts

  ## find most recent sia year
  if(year_current > year_intro) {
    skip
  } else {
    if(nrow(d) == 0L){

    }
  }

}
## rule f.) projection rule for
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

## extract source coverage for vimc user
vimc_historical_data <- function(con, year_cur, coverage_src_his, coverage_src_fut){
  touch_his <- vimpact::get_touchstone(con, coverage_src_his)
  touch_fut <- vimpact::get_touchstone(con, coverage_src_fut)

  # extract data from montagu
  his <- DBI::dbGetQuery(con,
                         paste("SELECT country, vaccine, activity_type, year, age_from, age_to, gender, target, coverage, proportion_risk",
                               "FROM coverage_set JOIN coverage ON coverage.coverage_set = coverage_set.id",
                               "WHERE touchstone = $1",
                               "AND coverage > 0",
                               "AND year <= $2"),
                         list(touch_his,
                              year_cur))
  fut <- DBI::dbGetQuery(con,
                         paste("SELECT country, vaccine, activity_type, year, age_from, age_to, gender, target, coverage, proportion_risk",
                               "FROM coverage_set JOIN coverage ON coverage.coverage_set = coverage_set.id",
                               "WHERE touchstone = $1",
                               "AND coverage > 0",
                               "AND year > $2"),
                         list(touch_fut,
                              year_cur))
  return(list(historic = his,
              future = fut))
}

## sanity checks
## what to check?
## continuity, range, trend, pop, age, etc

