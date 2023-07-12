#' saninty check for input date
#' @param input
#' input - input parameters
#' input is a list contains three objects - params, src and proj_rules
#' params is a list (example below)
#' <- list(country = "<country_code>",
#'         disease = "<disease_code>",
#'         year_cur = <current_year>, # separates historical data and future projection
#'         introduction = data.frame(vaccine = c("<vaccine_1>", "<vaccine_2>"),
#'                                  activity_type = c("<activity_vaccine_1>", "<activity_vaccine_2>"),
#'                                  year_intro = c(<vaccine_1_introduction_year>, <vaccine_2_introduction_year>)))
#' src is a list (example below)
#' <- list((historic = <data frame of historical coverage source>, future = <data frame of future coverage source>)
#' coverage source data frame must contain columns below
#' c("country", "vaccine", "activity_type", "year", "age_from", "age_to", "gender", "target", "coverage", "proportion_risk")
input_check <- function(input){
  ## saninty check for input
  ## 1.) check input$params
  country <- unique(input$params$country)
  disease <- unique(input$params$disease)
  year_cur <- as.integer(unique(input$params$year_cur))
  stopifnot(length(country) == 1L) # work for one country only
  stopifnot(length(disease) == 1L) # work for one disease only
  stopifnot(length(year_cur) == 1L) # current year

  ## arrange introduction and projection rules, so that campaign is the last one - because campaign is frequently dependent on routine levels
  introduction <- input$params$introduction %>%
    mutate(index = seq_along(vaccine)) %>%
    arrange(desc(activity_type)) %>%
    mutate(index2 = seq_along(vaccine) )

  if(nrow(introduction) != length(input$proj_rul)){
    txt <- "The number of vaccine delivery (input$params$introduction) and projection rules (input$proj_rul) do not match."
    stop(txt)
  }

  input$proj_rul <- input$proj_rul[introduction$index]

  ## 2.) check input$src
  key_cols <-  c("country", "vaccine", "activity_type", "year", "age_from", "age_to", "gender", "target", "coverage", "proportion_risk")
  if(nrow(input$src$historic) > 0){
    assert_has_columns(input$src$historic, key_cols)
  }
  if(nrow(input$src$future) > 0){
    assert_has_columns(input$src$future, key_cols)
  }

  ## filter data by params specified
  his <-  input$src$historic %>%
    filter(country == !!country) %>%
    right_join(introduction %>% select(-year_intro), by = c("vaccine", "activity_type")) %>%
    filter(!is.na(year))

  fut <-  input$src$future %>%
    filter(country == !!country) %>%
    right_join(introduction %>% select(-year_intro), by = c("vaccine", "activity_type"))%>%
    filter(!is.na(year))

  ## 3.) check introduction
  ## merge data source and user specified introduction dates
  ## NA introduction will be replaced by input$src$historical
  ## non-NA introduction will over-write input$src$historical
  if (nrow(his[his$activity_type != "campaign", ]) > 0){
    s <- his %>% group_by(vaccine, activity_type) %>%
      filter(activity_type != "campaign") %>%
      summarise(year_intro_src = min(year), .groups = "keep") %>%
      as.data.frame() %>%
      right_join(introduction, by = c("vaccine", "activity_type")) %>%
      mutate(conflict = (!is.na(year_intro) & !is.na(year_intro_src) & year_intro != year_intro_src))

    if (any(s$conflict)){
      message("conflict in introduction assumption between input$params$introduction and input$src$historical")
      message("drop historical data, assume a different introduction date specified in input$params$introduction")
      message("Hints: use NA in input$params$introcution if you believe introduction happened and you want to use introduction date in data source;
              or make sure your projection rule is set up correctly")
    }
    input$introduction <- s %>%
      mutate(year_intro = ifelse(is.na(year_intro), year_intro_src, year_intro)) %>%
      select(vaccine, activity_type, year_intro, conflict) %>%
      mutate(future_introduction = !is.na(year_intro) & year_intro > year_cur)
    input$his <- his %>% right_join(input$introduction %>% filter(!future_introduction & !conflict), by = join_by("vaccine", "activity_type")) %>%
      select(-year_intro, - future_introduction, -conflict)
  } else {
    input$his <- his
    input$introduction <- input$params$introduction
    print("No historical routine data identified.")
  }

  input$fut <- fut # future coverage is used for each delivery if no corresponding projection rule(s) specified

  ## 4.) satiny check for rules
  for(j in seq_len(length(input$proj_rul))){
    r <- input$proj_rul[[j]]
    x <- length(r)
    if(x == 0){
      message(sprintf("%s projection rules identified for %s %s; use any future coverage specified.", x, input$introduction$activity_type[j], input$introduction$vaccine[j]))
    } else {
      message(sprintf("%s projection rules identified for %s %s", x, input$introduction$activity_type[j], input$introduction$vaccine[j]))
    }
    for (i in seq_len(x)){
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

#' generate vaccination scenario according to input
#' @param input
#' for details of setting up input, see input_check()
vac_sce <- function(input){
  ## log scenario generation messages.
  input <- input_check(input)
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

  return(dat)
}

#' routine coverage projection rule - keep future coverage at certain level
#' @param d a data frame containing at least (year, coverage)
#' @param year_from year from
#' @param year_to year to
#' @param level coverage level
keep_levels <- function(d, year_from, year_to, level){
  t <- d[1, ] %>%
    select(-year) %>%
    merge(data.frame(year = year_from:year_to)) %>%
    mutate(coverage = level) %>%
    bind_rows(d) %>%
    arrange(year)
  return(t)
}

#' routine coverage projection rule - annual incremental to certain threshold
#' @param d a data frame containing at least (year, coverage)
#' @param year_from year from
#' @param year_to year to
#' @param step incremental level, e.g. 1% annual increase
#' @param cap threshold capping coverage, this will be replaced by historical peak in d if applicable
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

#' routine coverage projection rule - catch up with certain vaccine
#' this is specifically for routine intro (no historical data, i.e. nrow(d) == 0), but can be used flexibly
#' @param d a data frame containing at least (year, coverage)
#' @param year_from year from
#' @param year_to year to
#' @param vaccine_x_level coverage level of the vaccine to catch up with in year_to
#' @param intro_level determines coverage level in year_from at vaccine_x_level*intro_level
catch_up_with_x <- function(d, year_from, year_to, vaccine_x_level, intro_level = 1/3){
  years <- seq(year_from, year_to, 1)
  cov <- rep(NA, length(years))
  if (nrow(d) == 0){
    ## routine introduction
    cov[1] <- vaccine_x_level*intro_level # intro at 33% of target vaccine
    cov[-1] <- IA2030_projection(year_from, cov[1], year_to, vaccine_x_level)
  } else {
    ## non-linear scale-up
    message("intro_level is not used as not applicable")
    cov <- IA2030_projection(year_from-1, d[d$year == year_from -1], year_to, vaccine_x_level)
  }
  dat <- bind_rows(d, data.frame(year = years, coverage = cov))
  return(dat)
}

#' routine coverage projection rule -  ia2030 non-linear scale-up
#' this is specifically for routine intro (no historical data, i.e. nrow(d) == 0), but can be used flexibly
#' @param d a data frame containing at least (year, coverage)
#' @param year_from year from
#' @param year_to year to
#' @param endpoint coverage level in year_to
non_linear_scale_up <- function(d, year_from, year_to, endpoint){
  years <- seq(year_from, year_to, 1)
  cov <- IA2030_projection(year_from-1, d$coverage[d$year == year_from -1], year_to, endpoint)
  dat <- bind_rows(d, data.frame(year = years, coverage = cov))
  return(dat)
}

#' campaign coverage projection rule - future follow-up campaigns
#' this function is designed under WHO M/MR guidance
#' depending on the most recent campaign, routine introduction date and routine coverage level of vaccine_base
#' @param d a data frame containing at least (year, coverage)
#' @param dat data frame containing at least vaccine_base upto year_to
#' @param vaccine_base vaccine to be dependent on
#' @param year_current year current
#' @param year_to year to
#' @param look_back evaluate follow-up sia frequency from year_current - look_back
#' @param sia_level campaign coverage level
#' @param age_from campaign target age from
#' @param age_to campaign target age to
sia_follow_up <- function(d, dat, vaccine_base, year_current, year_to, look_back = 4, sia_level = 0.9, age_from = 1, age_to = 5){
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
                  age_from = age_from,
                  age_to = age_to)

  return(bind_rows(d, t))
}

#' campaign coverage projection rule - one-off catch-up campaigns, specifically initial catch-up and mini-catch-up that targets missing cohorts
#' @param d a data frame containing at least (year, coverage)
#' @param dat data frame containing at least vaccine_base upto year_to
#' @param vaccine_base vaccine to be dependent on
#' @param year_current year current
#' @param sia_level campaign coverage level
#' @param age_from campaign target age from
#' @param age_to campaign target age to
sia_catch_up <- function(d, dat, vaccine_base, year_current, sia_level, age_from, age_to){
  ## this is relevant to MenA, Typhoid, and HPV mult-cohort SIAs before routine introduction
  ## we need are year_intro and age groups
  ## if previously SIAs happened, only target missed cohorts

  ## find routine intro year, there will be a campaign in this year
  year_intro <- min(dat$year[dat$vaccine == vaccine_base])

  ## any campaign in the past?
  ## if there is one, catch-up missing cohorts, otherwise refine age_from, age_to to only these missing cohorts
  if (nrow(d) > 0){
    year_last_sia <- max(d$year)
  } else {
    year_last_sia <- 1e5
  }

  if(year_last_sia < year_intro) {
    ## catch-up missing cohorts only, if missing cohorts are eligible, i.e. between [age_from, age_to]
    k <- year_intro - year_last_sia
    s <- intersect(1:k, age_from:age_to)
    if(length(s) > 0){
      age_from <- min(s)
      age_to <- max(s)
      t <- data.frame(year = year_intro,
                      coverage = sia_level,
                      age_from = age_from,
                      age_to = age_to)
    } else {
      return(NULL)
    }
  } else {
    t <- data.frame(year = year_intro,
                    coverage = sia_level,
                    age_from = age_from,
                    age_to = age_to)
  }
  return(bind_rows(d, t))
}

#' campaign coverage projection rule - recurrent campaigns, e.g. cholera
#' @param d a data frame containing at least (year, coverage)
#' @param dat Not used yet. It is just here for maintaining consistent campaign projection structure
#' @param year_from year from
#' @param year_to year to
#' @param frequency sia frequency
#' @param sia_level campaign coverage level
#' @param age_from campaign target age from
#' @param age_to campaign target age to
sia_recurrent <- function(d, dat, year_from, year_to, frequency, sia_level, age_from, age_to){

  if(nrow(d[d$activity_type == "campaign", ]) > 0){
    message("historical campaign is identified, project from most recent campaign year")
    year_from <- max(d$year[d$activity_type == "campaign"])+frequency
  }
  t <- data.frame(year = seq(year_from, year_to, frequency),
                  coverage = sia_level,
                  age_from = age_from,
                  age_to = age_to)
  return(t)
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

