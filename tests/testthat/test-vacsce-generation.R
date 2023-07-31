# rm(list = ls())
library("vimpact")
library("dplyr")
library("jenner")

## The tool can work on a single delivery, or a set of deliveries
## it works on only one region-disease combination each time
## in the next, I give a few examples

## A few tips before examples
## STEPS: prepare input -> run vacsce for coverage scenario
## input is a list consists of three objects - params, src and proj_rul, meaning parameters, coverage source and projection rules, respectively.

## params ##
## params is a list that contains
## ------- region: <character> e.g. a country name, ISO code, sub-national region, or anything that is meaning for the user
## ------- disease: <character> any disease name of interest
## ------- proportion_risk: <numeric> the proportion of population at risk of infection, between 0-1
## ------- year_cur: <int> current year. Though this can be inferred from historical coverage data (if any), vacsce still requires this be specified under its structural design
## ------- introduction: <data.frame> introduction years by vaccine delivery, i.e. vaccine, activity_type, and year_intro.
##                       NA yera_intro is fine for campaigns or routines already introduced, cannot be NA for future routine introduction.

## src ##
## src is a list of historic and future coverage sources. src=lis(historic, future)
## ------- historic: historical coverage source
## ------- future: future coverage source
##                  historic and future are two data frames sharing the same structure (see below)
##                  (region, vaccine, activity_type, year, age_from, age_to, gender, target, coverage)

## proj_rul ##
## proj_rul is a list consists of future projection rules.
## The number of rules are determined by how many vaccine delivery specified in params$introduction
## N.B. rules will over-write any source coverage provided
## e.g. if future is provided via src, but projection rules are also specified, use projection rules
## e.g. if params$introduction specified a routine introduction different from historical coverage source, projection rules will be used to re-create historical coverage

## EXAMPLE START
## example data and common parameter set-up
## (example coverage source are collected for all examples to show)
example_data <- read.csv("inst/example_data.csv")
src <- list(historic = example_data[example_data$year <= year_cur, ],
            future = example_data[example_data$year > year_cur, ])
year_cur <- 2021
region <- "ISO"

## example 1 - mcv1 for both historical and future from source data, no projection
input <- list(params = list(region = region,
                            disease = "Measles",
                            proportion_risk = 1,
                            year_cur = year_cur,
                            introduction = data.frame(vaccine = c("MCV1"),
                                                      activity_type = c("routine"),
                                                      year_intro = c(NA))),
              src = src,
              proj_rul = list(
                rule1 = NULL # null means no operation needed, simply bind historical and future from src
              )
)
dat <- vac_sce(input)

## example 2 - mcv1 for historical from source + non-linear scale-up
input <- list(params = list(region = region,
                            disease = "Measles",
                            proportion_risk = 1,
                            year_cur = year_cur,
                            introduction = data.frame(vaccine = c("MCV1"),
                                                      activity_type = c("routine"),
                                                      year_intro = c(NA))),
              src = src,
              proj_rul = list(
                rule1 = list(non_linear_scale_up = list(year_from = 2022, year_to = 2030, endpoint = 0.99)) # rules are provided as a list of lists, each component is a projection rule name and its corresponding parameters
              )
)
dat <- vac_sce(input)

## next example is a complex/flexible one
## example 3 - mcv1 historical + non-linear scale-up; manually defined future intro + various rules; db sia for both past and future
input <- list(params = list(region = region,
                            disease = "Measles",
                            proportion_risk = 1,
                            year_cur = 2021,
                            introduction = data.frame(vaccine = c("MCV1", "MCV2", "Measles"),
                                                      activity_type = c("routine", "routine", "campaign"),
                                                      year_intro = c(NA, 2024, NA))),
              src = src,
              proj_rul = list(
                rule1 = list(non_linear_scale_up = list(year_from = 2022, year_to = 2030, endpoint = 0.99)), # rule 1 for mcv1
                rule2 = list(catch_up_with_x = list(year_from = 2024, year_to = 2027, vaccine_x_level = 0.7), # rule 2 for mcv2
                             non_linear_scale_up = list(year_from = 2028, year_to = 2030, endpoint = 0.9),
                             keep_levels = list(year_from = 2031, year_to = 2035, level = 0.9),
                             incremental = list(year_from = 2036, year_to = 2040, step=0.02, cap = 0.95)),
                rule3 = NULL ## currently not projecting campaigns # rule 3 for campaign - campaign projection rules under development
              )
)
dat <- vac_sce(input)

## example 4 - mcv1 historical + non-linear scale-up; manually defined future intro + various rules; db sia for past and future projection
## this has to be two-steps, as campaign projection depends on routine projections

input <- list(params =  list(region = region,
                             disease = "Measles",
                             proportion_risk = 1,
                             year_cur = 2021,
                             introduction = data.frame(vaccine = c("MCV1", "MCV2", "Measles"),
                                                       activity_type = c("routine", "routine", "campaign"),
                                                       year_intro = c(NA, 2024, NA))),
              src = src,
              proj_rul = list(
                rule1 = list(non_linear_scale_up = list(year_from = 2022, year_to = 2030, endpoint = 0.99)), # rule 1 for mcv1
                rule2 = list(catch_up_with_x = list(year_from = 2024, year_to = 2027, vaccine_x_level = 0.7), # rule 2 for mcv2
                             non_linear_scale_up = list(year_from = 2028, year_to = 2030, endpoint = 0.95)),
                rule3 = list(sia_follow_up = list(vaccine_base = "MCV1", year_current = 2021, year_to = 2030, look_back = 4, sia_level = 0.9, age_from = 1, age_to = 5)) ## rule 3 for campaign
              )
)
dat <- vac_sce(input)

## example 5 - hpv historical + non-linear scale-up; manually defined future intro + various rules; db sia for past and future projection for initial mac
## this has to be two-steps, as campaign projection depends on routine projections
input <- list(params = list(region = region,
                            disease = "HPV",
                            proportion_risk = 1,
                            year_cur = 2021,
                            introduction = data.frame(vaccine = c("HPV", "HPV"),
                                                      activity_type = c("routine", "campaign"),
                                                      year_intro = c(2027, NA))),
              src = src,
              proj_rul = list(
                rule1 = list(catch_up_with_x = list(year_from = 2027, year_to = 2030, vaccine_x_level = 0.7), # rule 1 for HPV routine
                             non_linear_scale_up = list(year_from = 2031, year_to = 2035, endpoint = 0.9)),
                rule2 = list(sia_catch_up = list(vaccine_base = "HPV", sia_level = 0.9, age_from = 10, age_to = 15)) ## rule 2 for initial mac campaign if future intro
              )
)
dat <- vac_sce(input)

## example 6 - Mena historical + non-linear scale-up; manually defined future intro + various rules; db sia for past and future projection for initial mac
## this has to be two-steps, as campaign projection depends on routine projections
input <- list(params = list(region = region,
                            disease = "MenA",
                            proportion_risk = 0.9,
                            year_cur = year_cur,
                            introduction = data.frame(vaccine = c("MenA", "MenA"),
                                                      activity_type = c("routine", "campaign"),
                                                      year_intro = c(2023, NA))),
              src = src,
              proj_rul = list(
                rule1 = list(catch_up_with_x = list(year_from = 2023, year_to = 2026, vaccine_x_level = 0.7), # rule 1 for MenA routine
                             non_linear_scale_up = list(year_from = 2027, year_to = 2030, endpoint = 0.9)),
                rule2 = list(sia_catch_up = list(vaccine_base = "MenA", sia_level = 0.9, age_from = 1, age_to = 29)) ## rule 2 for initial mac campaign if future intro
              )
)
dat <- vac_sce(input)

## example 7 - Cholera
input <- list(params = list(region = region,
                            disease = "Cholera",
                            year_cur = year_cur,
                            introduction = data.frame(vaccine = c("Cholera"),
                                                      activity_type = c("campaign"),
                                                      year_intro = c(NA))),
              src = src,
              proj_rul = list(
                rule1 = list(sia_recurrent = list(year_from = 2020, year_to = 2030, frequency = 3, sia_level = 0.3, age_from = 1, age_to = 60))
              )
)
dat <- vac_sce(input) # coverage scenario generation

