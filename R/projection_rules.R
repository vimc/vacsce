##' routine coverage projection rule - keep future coverage at certain level
##' @title Keep future coverage at certain level
##' @param d a data frame containing at least (year, coverage)
##' @param year_from year from
##' @param year_to year to
##' @param level coverage level
##' @param gender 1 for both, 2 for males, 3 for females
##' @param age_from age_from
##' @param age_to age_to
##' @export
keep_levels <- function(d, year_from, year_to, level, gender = NULL, age_from = NULL, age_to = NULL){
  t <- d[1, ] %>%
    dplyr::select(-year) %>%
    dplyr::cross_join(data.frame(year = year_from:year_to)) %>%
    dplyr::mutate(coverage = level, rule = "keep_levels")

  if(!is.null(gender)){
    t$gender <- gender
  }
  if(!is.null(age_from)){
    t$age_from <- age_from
  }
  if(!is.null(age_to)){
    t$age_to <- age_to
  }
  t <- t %>%
    dplyr::bind_rows(d) %>%
    dplyr::arrange(year)
  return(t)
}

##' routine coverage projection rule - annual incremental to certain threshold
##' @title Incremental with fixed step
##' @param d a data frame containing at least (year, coverage)
##' @param year_from year from
##' @param year_to year to
##' @param step incremental step, e.g. 1 percent annual increase
##' @param cap threshold capping coverage, this will be replaced by historical peak in d if applicable, 95 percent by default
##' @param gender 1 for both, 2 for males, 3 for females
##' @param age_from age_from
##' @param age_to age_to
##' @export
incremental <- function(d, year_from, year_to, step, cap = 0.95, gender = NULL, age_from = NULL, age_to = NULL){
  max_y <- max(d$year)
  t <- d[d$year == max_y, ] %>% # this is just to grab one row in source data for information like region, vaccine, activity_type, age groups, etc.
    dplyr::select(-year) %>%
    dplyr::cross_join(data.frame(year = year_from:year_to)) %>%
    dplyr::mutate(coverage = coverage + step*(year - max_y), rule = "incremental")
  if(!is.null(gender)){
    t$gender <- gender
  }
  if(!is.null(age_from)){
    t$age_from <- age_from
  }
  if(!is.null(age_to)){
    t$age_to <- age_to
  }
  t <- t %>%
    dplyr::bind_rows(d) %>%
    dplyr::arrange(year)
  cap <- max(c(cap, max(d$coverage))) # cap at specified or historical highest
  t <- t %>%
    dplyr::mutate(coverage = ifelse(coverage > cap, cap, coverage))
  return(t)
}

##' routine coverage projection rule - catch up with certain vaccine
##' @title Catch-up with focal vaccine
##' @param d a data frame containing at least (year, coverage)
##' @param year_from year from
##' @param year_to year to
##' @param vaccine_x_level coverage level of the vaccine to catch up with in year_to
##' @param intro_level determines coverage level in year_from at vaccine_x_level*intro_level, 33 percent by default
##' @param gender 1 for both, 2 for males, 3 for females
##' @param age_from age_from
##' @param age_to age_to
##' @export
catch_up_with_x <- function(d, year_from, year_to, vaccine_x_level, intro_level = 1/3, gender = NULL, age_from = NULL, age_to = NULL){
  years <- seq(year_from, year_to, 1)
  cov <- rep(NA, length(years))
  d <- d[is.numeric(d$year) & d$year > 0,]
  if (nrow(d) == 0){
    ## routine introduction
    cov[1] <- vaccine_x_level*intro_level # intro at 33% of target vaccine by default (see args)
    cov[-1] <- ia2030_projection(year_from, cov[1], year_to, vaccine_x_level)
  } else {
    ## non-linear scale-up
    message("intro_level is not used as not applicable")
    cov <- ia2030_projection(year_from-1, d$coverage[d$year == year_from -1], year_to, vaccine_x_level)
  }
  t <- data_frame(year = years, coverage = cov, rule = "catch_up_with_x")
  if(!is.null(gender)){
    t$gender <- gender
  }
  if(!is.null(age_from)){
    t$age_from <- age_from
  }
  if(!is.null(age_to)){
    t$age_to <- age_to
  }
  dat <- dplyr::bind_rows(d, t)
  return(dat)
}

##' routine coverage projection rule -  ia2030 non-linear scale-up
##' @title IA2030 non-liner scale-up
##' @param d a data frame containing at least (year, coverage)
##' @param year_from year from
##' @param year_to year to
##' @param endpoint coverage level in year_to
##' @param gender 1 for both, 2 for males, 3 for females
##' @param age_from age_from
##' @param age_to age_to
##' @export
non_linear_scale_up <- function(d, year_from, year_to, endpoint, gender = NULL, age_from = NULL, age_to = NULL){
  years <- seq(year_from, year_to, 1)
  cov <- ia2030_projection(year_from-1, d$coverage[d$year == year_from -1], year_to, endpoint)
  t <- data.frame(year = years, coverage = cov, rule = "non_linear_scale_up")
  if(!is.null(gender)){
    t$gender <- gender
  }
  if(!is.null(age_from)){
    t$age_from <- age_from
  }
  if(!is.null(age_to)){
    t$age_to <- age_to
  }
  dat <- dplyr::bind_rows(d, t)
  return(dat)
}

##' campaign coverage projection rule - future follow-up campaigns
##' this function is designed under WHO M/MR guidance
##' depending on the most recent campaign, routine introduction date and routine coverage level of vaccine_base
##' @title Project future follow-up campaigns
##' @param d a data frame containing at least (year, coverage)
##' @param dat data frame containing at least vaccine_base upto year_to
##' @param vaccine_base vaccine to be dependent on
##' @param year_current year current
##' @param year_to year to
##' @param look_back evaluate follow-up sia frequency from year_current - look_back
##' @param sia_level campaign coverage level
##' @param age_from campaign target age from
##' @param age_to campaign target age to
##' @param  gender 1 for both, 2 for males, 3 for females
##' @export
sia_follow_up <- function(d, dat, vaccine_base, year_current, year_to, look_back = 4, sia_level = 0.9, age_from = 1, age_to = 5, gender = 1){
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
  ## every 4 years if mcv1 level is >= 80% & < 95%
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
  year <- NULL
  while (i < year_to) {
    ## evaluate vaccine_base, and determine next sia year
    if(b$coverage[b$year == i] < 0.6){
      i <- i + 2
    } else if(b$coverage[b$year == i] >= 0.8 & b$coverage[b$year == i] < 0.95){
      i <- i + 4
    } else if(b$coverage[b$year == i] >= 0.6 & b$coverage[b$year == i] < 0.8){
      i <- i + 3
    } else {
      i <- year_to
    }
    if (i < y_current){
      i <- y_current + 1
    }
    year <- c(year, i)
  }
  t <- data.frame(year = year,
                  coverage = sia_level,
                  age_from = age_from,
                  age_to = age_to,
                  gender = gender,
                  rule = "sia_follow_up")

  dat <- dplyr::bind_rows(d, t)  %>%
    dplyr::filter(year <= year_to)
  return(dat)
}

##' campaign coverage projection rule - one-off catch-up campaigns, specifically initial catch-up and mini-catch-up that targets missing cohorts
##' @title Project catch-up campaign
##' @param d a data frame containing at least (year, coverage)
##' @param dat data frame containing at least vaccine_base upto year_to
##' @param vaccine_base vaccine to be dependent on
##' @param sia_level campaign coverage level
##' @param age_from campaign target age from
##' @param age_to campaign target age to
##' @param gender 1 for both, 2 for males, 3 for females
##' @export
sia_catch_up <- function(d, dat, vaccine_base, sia_level, age_from, age_to, gender){
  ## this is relevant to MenA, Typhoid, and HPV mult-cohort SIAs before routine introduction
  ## we need are year_intro and age groups
  ## if previously SIAs happened, only target missed cohorts

  ## find routine intro year, there will be a campaign in this year
  year_intro <- min(dat$year[dat$vaccine == vaccine_base])
  d <- d[is.numeric(d$year) & d$year > 0, ]
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
                      age_to = age_to,
                      gender = gender,
                      rule = "sia_catch_up")
    } else {
      return(NULL)
    }
  } else {
    t <- data.frame(year = year_intro,
                    coverage = sia_level,
                    age_from = age_from,
                    age_to = age_to,
                    gender = gender,
                    rule = "sia_catch_up")
  }
  dat <- dplyr::bind_rows(d, t)
  return(dat)
}

##' campaign coverage projection rule - recurrent campaigns, e.g. cholera
##' @title Project recurrent campaigns
##' @param d a data frame containing at least (year, coverage)
##' @param dat Not used. It is just here for maintaining consistent campaign projection structure
##' @param year_from year from
##' @param year_to year to
##' @param frequency sia frequency
##' @param sia_level campaign coverage level
##' @param age_from campaign target age from
##' @param age_to campaign target age to
##' @param  gender 1 for both, 2 for males, 3 for females
##' @export
sia_recurrent <- function(d, dat = NULL, year_from, year_to, frequency, sia_level, age_from, age_to, gender){

  if(nrow(d[d$activity_type == "campaign", ]) > 0){
    message("historical campaign is identified, project from most recent campaign year")
    year_from <- max(d$year[d$activity_type == "campaign"])+frequency
  }
  t <- data.frame(year = seq(year_from, year_to, frequency),
                  coverage = sia_level,
                  age_from = age_from,
                  age_to = age_to,
                  gender = gender,
                  rule = "sia_recurrent") %>%
    dplyr::bind_rows(d) %>%
    dplyr::arrange(year)
  return(t)
}
