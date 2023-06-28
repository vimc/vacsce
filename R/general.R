#' saninty check for input date
#' @param input
#' input - input parameters
#' can project deliveries one-by-one if one depends on another, e.g. campaign frequency may be determined by past routine services
# for each delivery can also work on rules one-by-one if a secondary rule depends on earlier projection, e.g. add a condition, if covid disruption..., if recovered, ...
# I will provide examples
input_check <- function(input){
  ## input is a list of objects
  ## need meaningful checks to avoid errors
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

  dat <- list(NULL)
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

    if(!is.null(r)){
      for(j in seq_len(length(r))){
        func <- paste0(names(r[j]), sub("list\\(", "(d, ",paste(r[j])))
        print(func)
        d <- eval(parse(text = func))
      }
      dat[[i]] <- d %>% bind_cols(input$introduction[i, c("vaccine", "activity_type")])
    } else{
      print("No projection. Binding data from source coverage.")
      dat[[i]] <- bind_rows(d0, d1)
    }
  }
  dat <- do.call(bind_rows, dat) %>%
    mutate(country = input$params$country,
           disease = input$params$disease)
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
    arrange(year) %>%
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

