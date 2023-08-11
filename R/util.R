## ia2030 non-linear function
## Carter et al. 2023 https://www.sciencedirect.com/science/article/pii/S0264410X2300854X
ia2030_projection <- function(year_base, coverage_base, year_target, coverage_target){
  ## per country, per vaccine
  stopifnot(year_base < year_target)
  if(year_target-year_base == 1L){
    return(c(coverage_target))
  } else {
    v <- seq_len(year_target - year_base -1)
    k <- log((1-coverage_target)/(1-coverage_base)) / (year_target-year_base)
    l <- 1-coverage_base
    for(i in seq_along(v)){
      v[i] <- 1 - l*exp(i * k)
    }
    return(c(v, coverage_target))
  }
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
