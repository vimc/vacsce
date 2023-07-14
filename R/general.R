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

## extract example data for vignette
vimc_historical_data <- function(con, year_cur = 2021){
  touch <- vimpact::get_touchstone(con, "202210covidimpact")

  # extract data from montagu
  # examples I want to include are
  # measles: past mcv1 intro, future mcv2 intro + follow-up sias
  # hpv: catch-up + future routine intro
  # mena: catch-up + mini-catch-up + future routine intro
  # anonymous country
  t <- data.frame(country = c(324, 324, 324, 50, 50, 108, 108),
                  disease =  c("Measles", "Measles", "Measles", "HPV", "HPV", "MenA", "MenA"),
                  vaccine = c("MCV1", "MCV2", "Measles", "HPV", "HPV", "MenA", "MenA"),
                  activity_type = c("routine", "routine", "campaign", "campaign", "routine", "campaign", "routine"))
  d <- DBI::dbGetQuery(con,
                       paste("SELECT country.nid AS country, vaccine, activity_type, year, age_from, age_to, gender, target, coverage, proportion_risk",
                             "FROM coverage_set JOIN coverage ON coverage.coverage_set = coverage_set.id",
                             "JOIN country ON country.id = coverage.country",
                             "WHERE touchstone = $1",
                             "And coverage_set.name LIKE '%default_nocovid%'",
                             "AND coverage > 0",
                             "AND year <= 2030"),
                       list(touch)) %>%
    right_join(t, by = c("country", "vaccine", "activity_type")) %>%
    mutate(country = "ISO", target = NA, coverage = coverage * runif(1, 0.9,1.1)) %>%
    mutate(proportion_risk = ifelse(is.na(proportion_risk, 1, proportion_risk)))
  d <- list(historic = d %>% filter(year <= year_cur),
            future = fut %>% fitted(year > year_cur))
  saveRDS(d, "inst/example_data.rds")
}
