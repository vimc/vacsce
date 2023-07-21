context("projection_rules")

test_that("keep future at certain levels", {
  data <- data.frame(year = 2021,
                     coverage = 0.8)

  expect_equal(data.frame(coverage = c(0.8, 0.9, 0.9),
                          year = 2021:2023),
               keep_levels(data, year_from = 2022, year_to=2023, level=0.9),
               ignore_attr = TRUE)
})

test_that("incremental coverage increase", {
  data <- data.frame(year = 2021,
                     coverage = 0.8)

  ## cap not used
  expect_equal(data.frame(coverage = c(0.8, 0.85, 0.9),
                          year = 2021:2023),
  incremental(data, year_from = 2022, year_to = 2023, step = 0.05, cap = 0.95),
  ignore_attr = TRUE)
  ## cap at 95%
  expect_equal(data.frame(coverage = c(0.8, 0.95, 0.95),
                          year = 2021:2023),
               incremental(data, year_from = 2022, year_to = 2023, step = 0.2, cap = 0.95),
               ignore_attr = TRUE)
  ## cap at historical best
  expect_equal(data.frame(coverage = c(0.8, 0.8, 0.8),
                          year = 2021:2023),
               incremental(data, year_from = 2022, year_to = 2023, step = 0.2, cap = 0.75),
               ignore_attr = TRUE)
})

test_that("routine introduction catch up with vaccine X", {

  ## as routine introduction
  data <- data.frame(year = NA,
                     coverage = NA)
  expect_equal(data.frame(year = 2022:2023,
                          coverage = c(0.45, 0.9)),
               catch_up_with_x(data, year_from = 2022, year_to=2023, vaccine_x_level=0.9, intro_level= 0.5),
               ignore_attr = TRUE)

  ## for other purposes
  data <- data.frame(year = 2021,
                     coverage = 0.5)
  expect_equal(data.frame(year = 2021:2023,
                          coverage = c(0.5, 0.7763932, 0.9)),
               catch_up_with_x(data, year_from = 2022, year_to=2023, vaccine_x_level=0.9, intro_level= 0.5),
               ignore_attr = TRUE)
})

test_that("non_linear_scale_up", {

  ## as routine introduction
  data <- data.frame(year = 2021,
                     coverage = 0.5)
  expect_equal(data.frame(year = 2021:2023,
                          coverage = c(0.5, 0.7763932, 0.9)),
               non_linear_scale_up(data, year_from = 2022, year_to=2023, endpoint=0.9),
               ignore_attr = TRUE)
})

