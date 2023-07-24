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

assert_has_columns <- function(data, col_names,
                               name = deparse(substitute(data))) {
  missing_names <- setdiff(col_names, colnames(data))
  if (length(missing_names) > 0) {
    stop(sprintf("Required column names %s are missing from %s",
                 paste(missing_names, collapse = ", "),
                 name))
  }
  invisible(TRUE)
}

data_frame <- function(...){
  data.frame(..., stringsAsFactors = FALSE)
}
