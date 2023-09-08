
test_that("keep future at certain levels", {
  data <- data_frame(year = 2021,
                     coverage = 0.8)

  expect_equal(data_frame(coverage = c(0.8, 0.9, 0.9),
                          year = 2021:2023),
               keep_levels(data, year_from = 2022, year_to=2023, level=0.9) %>% dplyr::select(-rule),
               ignore_attr = TRUE)
})

test_that("incremental coverage increase", {
  data <- data_frame(year = 2021,
                     coverage = 0.8)

  ## cap not used
  expect_equal(data_frame(coverage = c(0.8, 0.85, 0.9),
                          year = 2021:2023),
  incremental(data, year_from = 2022, year_to = 2023, step = 0.05, cap = 0.95) %>% dplyr::select(-rule),
  ignore_attr = TRUE)
  ## cap at 95%
  expect_equal(data_frame(coverage = c(0.8, 0.95, 0.95),
                          year = 2021:2023),
               incremental(data, year_from = 2022, year_to = 2023, step = 0.2, cap = 0.95) %>% dplyr::select(-rule),
               ignore_attr = TRUE)
  ## cap at historical best
  expect_equal(data_frame(coverage = c(0.8, 0.8, 0.8),
                          year = 2021:2023),
               incremental(data, year_from = 2022, year_to = 2023, step = 0.2, cap = 0.75) %>% dplyr::select(-rule),
               ignore_attr = TRUE)
})

test_that("routine introduction catch up with vaccine X", {

  ## as routine introduction
  data <- data_frame(year = NA,
                     coverage = NA)
  expect_equal(data_frame(year = 2022:2023,
                          coverage = c(0.45, 0.9)),
               catch_up_with_x(data, year_from = 2022, year_to=2023, vaccine_x_level=0.9, intro_level= 0.5) %>% dplyr::select(-rule),
               ignore_attr = TRUE)

  ## routine intro happened, intro_level parameter ruled out
  data <- data_frame(year = 2021,
                     coverage = 0.5)
  expect_equal(data_frame(year = 2021:2023,
                          coverage = c(0.5, 0.7763932, 0.9)),
               catch_up_with_x(data, year_from = 2022, year_to=2023, vaccine_x_level=0.9, intro_level= 0.5) %>% dplyr::select(-rule),
               ignore_attr = TRUE)
})

test_that("non_linear_scale_up", {

  data <- data_frame(year = 2021,
                     coverage = 0.5)
  expect_equal(data_frame(year = 2021:2023,
                          coverage = c(0.5, 0.7763932, 0.9)),
               non_linear_scale_up(data, year_from = 2022, year_to=2023, endpoint=0.9) %>% dplyr::select(-rule),
               ignore_attr = TRUE)
})

test_that("sia_follow_up", {

  # the focus of this test is campaign frequency
  # test 1: one campaign every 4 years
  d <- data_frame(vaccine = "A",
                  activity_type = "routine",
                  year = 2020:2021,
                  coverage = c(.8,.85))
  dat <- non_linear_scale_up(d, year_from = 2022, year_to = 2025, endpoint = 0.9) %>% dplyr::select(-rule) %>%
    dplyr::mutate(vaccine = "A", activity_type = "routine")

  dd <- data_frame(vaccine = "A",
                  activity_type = "campaign",
                  year = 2020,
                  coverage = 0.8,
                  age_from = 1,
                  age_to = 15)

  expect_equal(c(2020, 2024),
               sia_follow_up(dd, dat, vaccine_base = "A", year_current = 2021, year_to = 2025)[["year"]],
               ignore_attr = TRUE)

  # test 2: varying frequency, as baseline coverage improves, less frequent
  d <- data_frame(vaccine = "A",
                  activity_type = "routine",
                  year = 2020:2021,
                  coverage = c(.5,.65))
  dat <- non_linear_scale_up(d, year_from = 2022, year_to = 2025, endpoint = 0.9) %>% dplyr::select(-rule) %>%
    dplyr::mutate(vaccine = "A", activity_type = "routine")

  dd <- data_frame(vaccine = "A",
                   activity_type = "campaign",
                   year = 2020,
                   coverage = 0.8,
                   age_from = 1,
                   age_to = 15)

  expect_equal(c(2020, 2022, 2025),
               sia_follow_up(dd, dat, vaccine_base = "A", year_current = 2021, year_to = 2025)[["year"]],
               ignore_attr = TRUE)

})


test_that("sia_catch_up",{

  ## sia in routine intro year as initial sia
  d <- data_frame(vaccine = "A",
                  activity_type = "routine",
                  year = 2020:2021,
                  coverage = c(.8,.85))
  dat <- non_linear_scale_up(d, year_from = 2022, year_to = 2025, endpoint = 0.9) %>% dplyr::select(-rule) %>%
    dplyr::mutate(vaccine = "A", activity_type = "routine")
  dd <- data_frame(year = NA, coverage = NA)
  expect_equal(2020,
               sia_catch_up(dd, dat, vaccine_base = "A", sia_level = 0.9, age_from = 10, age_to = 15, gender = 3)[["year"]],
               ignore_attr = TRUE)

  ## sia in routine intro year as mini-catch-up targeting only missed cohorts
  d <- data_frame(vaccine = "A",
                  activity_type = "routine",
                  year = 2022,
                  coverage = .8)
  dat <- non_linear_scale_up(d, year_from = 2023, year_to = 2025, endpoint = 0.9) %>% dplyr::select(-rule) %>%
    dplyr::mutate(vaccine = "A", activity_type = "routine")
  dd <- data_frame(year = 2018, coverage = 0.8, age_from = 1, age_to = 29)
  expect_equal(c(2018, 2022),
               sia_catch_up(dd, dat, vaccine_base = "A", sia_level = 0.9, age_from = 1, age_to = 29, gender = 1)[["year"]],
               ignore_attr = TRUE)
  expect_equal(c(29, 4),
               sia_catch_up(dd, dat, vaccine_base = "A", sia_level = 0.9, age_from = 1, age_to = 29, gender = 1)[["age_to"]],
               ignore_attr = TRUE)
})
