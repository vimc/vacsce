##' saninty check for input date
##' @title Sanity check for input
##' @param input input for vacsce
##' input is a list contains three objects - params, src and proj_rules
##' params is a list (example below)
##'     list(country = "<country_code>",
##'         disease = "<disease_code>",
##'         proportion_risk = <proportion of at risk population>,
##'         year_cur = <current_year>, # separates historical data and future projection
##'         introduction = data.frame(vaccine = c("<vaccine_1>", "<vaccine_2>"),
##'                                  activity_type = c("<activity_vaccine_1>", "<activity_vaccine_2>"),
##'                                  year_intro = c(<vaccine_1_introduction_year>, <vaccine_2_introduction_year>)))
##' src is a list (example below)
##'     list((historic = <data frame of historical coverage source>, future = <data frame of future coverage source>)
##' @param key_cols Key columns in coverage data to maintain
##' @export
input_check <- function(input, key_cols){
  ## saninty check for input
  ## 1.) check input$params
  region <- unique(input$params$region)
  disease <- unique(input$params$disease)
  year_cur <- as.integer(unique(input$params$year_cur))
  proportion_risk <- input$params$proportion_risk
  stopifnot(length(region) == 1L) # work for one region only
  stopifnot(length(disease) == 1L) # work for one disease only
  stopifnot(length(year_cur) == 1L) # current year
  stopifnot(proportion_risk <= 1 & proportion_risk > 0) # proportion of at risk population

  ## arrange introduction and projection rules, so that campaign is the last one - because campaign is frequently dependent on routine levels
  introduction <- input$params$introduction %>%
    dplyr::mutate(index = seq_along(vaccine)) %>%
    dplyr::arrange(dplyr::desc(activity_type), vaccine)

  if(nrow(introduction) != length(input$proj_rul)){
    txt <- "The number of vaccine delivery (input$params$introduction) and projection rules (input$proj_rul) do not match."
    stop(txt)
  }

  input$proj_rul <- input$proj_rul[introduction$index]

  ## 2.) check input$src
  if(nrow(input$src$historic) > 0){
    assert_has_columns(input$src$historic, key_cols)
  }
  if(!is.null(input$src$future)){
    if(nrow(input$src$future) > 0){
      assert_has_columns(input$src$future, key_cols)
    }
  }


  ## filter data by params specified
  historic <-  input$src$historic %>%
    dplyr::filter(region == !!region) %>%
    dplyr::right_join(introduction %>% dplyr::select(-year_intro), by = c("vaccine", "activity_type")) %>%
    dplyr::filter(!is.na(year))
  if(!is.null(input$src$future)){
    future <-  input$src$future %>%
      dplyr::filter(region == !!region) %>%
      dplyr::right_join(introduction %>% dplyr::select(-year_intro), by = c("vaccine", "activity_type"))%>%
      dplyr::filter(!is.na(year))
  } else {
    future <- NULL
  }

  ## 3.) check introduction
  ## merge data source and user specified introduction dates
  ## NA introduction will be replaced by input$src$historical
  ## non-NA introduction will over-write input$src$historical
  if (nrow(historic[historic$activity_type != "campaign", ]) > 0){
    s <- historic %>%
      dplyr::group_by(vaccine, activity_type) %>%
      dplyr::filter(activity_type != "campaign") %>%
      dplyr::summarise(year_intro_src = min(year), .groups = "keep") %>%
      as.data.frame() %>%
      dplyr::right_join(introduction, by = c("vaccine", "activity_type")) %>%
      dplyr::mutate(conflict = (!is.na(year_intro) & !is.na(year_intro_src) & year_intro != year_intro_src))

    if (any(s$conflict)){
      message("conflict in introduction assumption between input$params$introduction and input$src$historical")
      message("drop historical data, assume a different introduction date specified in input$params$introduction")
      message("Hints: use NA in input$params$introcution if you believe introduction happened and you want to use introduction date in data source;
              or make sure your projection rule is set up correctly")
    }
    input$introduction <- s %>%
      dplyr::mutate(year_intro =
                      dplyr::case_when(is.na(year_intro) ~ year_intro_src,
                                       TRUE ~ year_intro)) %>%
      dplyr::select(vaccine, activity_type, year_intro, conflict) %>%
      dplyr::mutate(future_introduction = !is.na(year_intro) & year_intro > year_cur) %>%
      dplyr::arrange(dplyr::desc(activity_type), vaccine)
    input$historic <- historic %>%
      dplyr::right_join(input$introduction %>%
                          dplyr::filter(!future_introduction & !conflict),
                        by = c("vaccine", "activity_type")) %>%
      dplyr::select(-year_intro, - future_introduction, -conflict)
  } else {
    input$historic <- historic
    input$introduction <- introduction
    message("No historical routine data identified.")
  }

  input$future <- future # future coverage is used for each delivery if no corresponding projection rule(s) specified

  ## 4.) sanity check for rules
  for(j in seq_len(length(input$proj_rul))){
    r <- input$proj_rul[[j]]
    x <- length(r)
    if(x == 0){
      message(sprintf("%s projection rules identified for %s %s; use any future coverage specified.", x, input$introduction$activity_type[j], input$introduction$vaccine[j]))
    } else {
      message(sprintf("%s projection rules identified for %s %s", x, input$introduction$activity_type[j], input$introduction$vaccine[j]))
    }
    for (i in seq_len(x)){
      if("year_from" %in% names(r[[i]])){
        stopifnot(r[[i]]$year_from <= r[[i]]$year_to)
      }
      if(i < x){
        if("year_to" %in% names(r[[i]]) & "year_from" %in% names(r[[i+1]])){
          if(r[[i]]$year_to+1 != r[[i+1]]$year_from){
            stop("Please check continuity of time periods for projection")
          }
        }
      }
    }
  }

  return(input)
}

##' generate vaccination scenario according to input
##' @title Generate Vaccination Scenario
##' @param input
##' for details of setting up input, see input_check()
##' @param key_cols Key columns in coverage data to maintain
##' by default key cols in coverage data include c("region", "vaccine", "activity_type", "year", "age_from", "age_to", "gender", "target", "coverage", "proportion_risk")
##' @export
vac_sce <- function(input, key_cols = c("region", "vaccine", "activity_type", "year", "age_from", "age_to", "gender", "target", "coverage", "proportion_risk")){
  ## log scenario generation messages.
  input <- input_check(input, key_cols)
  historic <- input$historic
  future <- input$future
  x <- nrow(input$introduction)

  ## scale routine coverage by proportion_risk for projection
  ## after run projection rules
  ## scale routine coverage back by proportion risk
  historic <- historic %>%
    dplyr::mutate(coverage =
                    dplyr::case_when(activity_type == "routine" ~ coverage/input$params$proportion_risk,
                                     TRUE ~ coverage)) %>%
    dplyr::mutate(coverage =
                    dplyr::case_when(coverage > 1 ~ 1-1.e-12,
                                     TRUE ~ coverage))
  if(!is.null(future)){
    future <- future %>%
      dplyr::mutate(coverage =
                      dplyr::case_when(activity_type == "routine" ~ coverage/input$params$proportion_risk,
                                       TRUE ~ coverage))%>%
      dplyr::mutate(coverage =
                      dplyr::case_when(coverage > 1 ~ 1,
                                       TRUE ~ coverage))
  }

  dat <- NULL
  for(i in seq_len(x)){
    # for each vaccine delivery do scenario projection
    message(sprintf("projecting trajectory for %s %s", input$introduction$activity_type[i], input$introduction$vaccine[i]))
    d0 <- historic %>%
      dplyr::right_join(input$introduction[i, c("vaccine", "activity_type")], by = c("vaccine", "activity_type") )
    if(!is.null(future)){
      d1 <- future %>%
        dplyr::right_join(input$introduction[i, c("vaccine", "activity_type")], by = c("vaccine", "activity_type") )
    } else {
      d1 <- NULL
    }

    d <- d0 %>%
      #dplyr::select(year, coverage, age_from, age_to, gender, target) %>%
      dplyr::select(-region, -vaccine, -activity_type) %>%
      dplyr::filter(!is.na(year)) %>%
      dplyr::arrange(year)
    r <- input$proj_rul[[i]] # projection rules

    if(!is.null(unlist(r)) & input$introduction$activity_type[i] != "campaign"){
      ## this is routine projection only
      ## campaign may depend on routine projection
      ## hence need to be run separately
      for(j in seq_len(length(r))){
        func <- paste0(names(r[j]), sub("list\\(", "(d, ", paste(r[j])))
        message(func)
        d <- eval(parse(text = func))
      }
      dat <- d %>%
        dplyr::bind_cols(input$introduction[i, c("vaccine", "activity_type")]) %>%
        dplyr::bind_rows(dat)
    } else if(is.null(unlist(r))){
      message("No projection. Binding data from source coverage.")
      dat <- dplyr::bind_rows(dat, d0, d1)
    } else {
      ## campaign projection
      for(j in seq_len(length(r))){
        func <- paste0(names(r[j]), sub("list\\(", "(d, dat, ", paste(r[j])))
        func <- gsub('\"', "'", func, fixed = TRUE)
        message(func)
        d <- eval(parse(text = func))
      }
      dat <- d %>%
        dplyr::bind_cols(input$introduction[i, c("vaccine", "activity_type")]) %>%
        dplyr::bind_rows(dat)
    }
  }
  dat <- dat %>%
    dplyr::mutate(region = input$params$region,
                  disease = input$params$disease,
                  proportion_risk =input$params$proportion_risk) %>%
    dplyr::mutate(coverage =
                    dplyr::case_when(activity_type == "routine" ~ coverage*input$params$proportion_risk,
                                     TRUE ~ coverage)
    )

  return(dat)
}
