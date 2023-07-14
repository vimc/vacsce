context("util")

test_that("assert_col_names checks required columns are present", {
  data <- data_frame(one = c("1", "2", "3"),
                     this = c(1, 2, 3))
  expect_true(assert_has_columns(data, "one"))
  expect_true(assert_has_columns(data, c("one", "this")))
  expect_error(assert_has_columns(data, c("one", "two", "three")),
               "Required column names two, three are missing from data")
})
