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
  cap <- max(c(cap, max(d$coverage))) # cap at specified or historical highest
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
  d <- d[is.numeric(d$year) & d$year > 0,]
  if (nrow(d) == 0){
    ## routine introduction
    cov[1] <- vaccine_x_level*intro_level # intro at 33% of target vaccine
    cov[-1] <- IA2030_projection(year_from, cov[1], year_to, vaccine_x_level)
  } else {
    ## non-linear scale-up
    message("intro_level is not used as not applicable")
    cov <- IA2030_projection(year_from-1, d$coverage[d$year == year_from -1], year_to, vaccine_x_level)
  }
  dat <- bind_rows(d, data_frame(year = years, coverage = cov))
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
#' @param  gender 1 for both, 2 for males, 3 for females
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
  t <- NULL
  while (i < year_to) {
    ## evaluate vaccine_base, and determine next sia year
    if(b$coverage[b$year == i] < 0.6){
      i <- i + 2
    } else if(b$coverage[b$year == i] >= 0.8 & b$coverage[b$year == i] < 0.95){
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
#' @param sia_level campaign coverage level
#' @param age_from campaign target age from
#' @param age_to campaign target age to
#' @param gender 1 for both, 2 for males, 3 for females
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
#' @param dat Not used. It is just here for maintaining consistent campaign projection structure
#' @param year_from year from
#' @param year_to year to
#' @param frequency sia frequency
#' @param sia_level campaign coverage level
#' @param age_from campaign target age from
#' @param age_to campaign target age to
#' @param  gender 1 for both, 2 for males, 3 for females
sia_recurrent <- function(d, dat = NULL, year_from, year_to, frequency, sia_level, age_from, age_to, gender){

  if(nrow(d[d$activity_type == "campaign", ]) > 0){
    message("historical campaign is identified, project from most recent campaign year")
    year_from <- max(d$year[d$activity_type == "campaign"])+frequency
  }
  t <- data.frame(year = seq(year_from, year_to, frequency),
                  coverage = sia_level,
                  age_from = age_from,
                  age_to = age_to) %>%
    bind_rows(d) %>%
    arrange(year)
  return(t)
}
