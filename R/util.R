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
