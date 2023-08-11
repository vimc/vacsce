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
