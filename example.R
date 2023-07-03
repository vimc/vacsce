rm(list = ls())
library("vimpact")
library("dplyr")
library("jenner")
con = dettl:::db_connect("production", ".")

## The tool can work on a single delivey, or a set of deliveries
## it works on only one country-disease combination each time
## in the next, I give three examples
## example 1 - wuenic touchstone for gavi country (similar for op touchstone - simply binding data from two sources)
## example 2 - wuenic touchstone for non-gavi country (same for op touchstone - latest historical + future projection)
## example 3 - model run touchstone or anything vimc/external users might be interested
## N.B. rules will over-write any source coverage provided, e.g. if future is provided via src, but projection rules specified, use projection rules

## step 1: prepare source coverage
## VIMC user extracts source coverage from montagu
## external user prepares this using provided template
## src is a list of two objects src=lis(historic, future)
## historic and future are two dataframes sharing the same structure
##  country, vaccine, activity_type, year, age_from, age_to, gender, target, coverage, proportion_risk
src <- vimc_historical_data(con, year_cur = 2021, coverage_src_his = "202207wue", coverage_src_fut = "202303gavi")

## example 1 - mcv1 for both historical and future from source, this is like wuenic+op for gavi country
params <- list(country = "GIN",
               disease = "Measles",
               year_cur = 2021,
               introduction = data.frame(vaccine = c("MCV1"),
                                         activity_type = c("routine"),
                                         year_intro = c(NA)))
proj_rules = list(
  rule1 = NULL # null means no operation needed, simply bind historical and future from src
)

## example 2 - mcv1 for historical from source + non-linear scale-up, this is like wuenic+projection for non-gavi country
params <- list(country = "GIN",
               disease = "Measles",
               year_cur = 2021,
               introduction = data.frame(vaccine = c("MCV1"),
                                         activity_type = c("routine"),
                                         year_intro = c(NA)))
proj_rules = list(
  rule1 = list(
    non_linear_scale_up = list(year_from = 2022, year_to = 2030, endpoint = 0.99)
    ) # rules are provided as a list of lists, each component is a projection rule name and its corresponding parameters
)

## next example is a complex/flexible one
## example 3 - mcv1 historical + non-linear scale-up; manually defined future intro + various rules; db sia for both past and future
params <- list(country = "GIN",
               disease = "Measles",
               year_cur = 2021,
               introduction = data.frame(vaccine = c("MCV1", "MCV2", "Measles"),
                                         activity_type = c("routine", "routine", "campaign"),
                                         year_intro = c(NA, 2024, NA)))
proj_rules = list(
  rule1 = list(non_linear_scale_up = list(year_from = 2022, year_to = 2030, endpoint = 0.99)), # rule 1 for mcv1
  rule2 = list(catch_up_with_x = list(year_from = 2024, year_to = 2027, vaccine_x_level = 0.7), # rule 2 for mcv2
               non_linear_scale_up = list(year_from = 2028, year_to = 2030, endpoint = 0.9),
               keep_levels = list(year_from = 2031, year_to = 2035, level = 0.9),
               incremental = list(year_from = 2036, year_to = 2040, step=0.02, cap = 0.95)),
  rule3 = NULL ## currently not projecting campaigns # rule 3 for campaign - campaign projection rules under development
)

## example 4 - mcv1 historical + non-linear scale-up; manually defined future intro + various rules; db sia for past and future projection
## this has to be two-steps, as campaign projection depends on routine projections
params <- list(country = "GIN",
               disease = "Measles",
               year_cur = 2021,
               introduction = data.frame(vaccine = c("MCV1", "MCV2", "Measles"),
                                         activity_type = c("routine", "routine", "campaign"),
                                         year_intro = c(NA, 2024, NA)))
proj_rules = list(
  rule1 = list(non_linear_scale_up = list(year_from = 2022, year_to = 2030, endpoint = 0.99)), # rule 1 for mcv1
  rule2 = list(catch_up_with_x = list(year_from = 2024, year_to = 2027, vaccine_x_level = 0.7), # rule 2 for mcv2
               non_linear_scale_up = list(year_from = 2028, year_to = 2030, endpoint = 0.95)),
  rule3 = list(sia_follow_up = list(vaccine_base = "MCV1", year_current = 2021, year_to = 2030)) ## rule 3 for campaign
)

## excute the following for each example
input <- list(params = params,
              src = src,
              proj_rul = proj_rules
)
input <- input_check(input) # sanity check: input structure, input parameters (any conflicts in routine introduction or projection rules, etc.)
dat <- vac_sce(input) # coverage scenario generation
