# rm(list = ls())
# library("vimpact")
# library("dplyr")
# library("jenner")
# source("R/general.R")
# con = dettl:::db_connect("production", ".")
#
# ## compare the tool with most recent touchstone
# #countries <- c("PAK", "IND", "NGA", "ETH", "CHN", "COD")
# countries <- c("CHN")
#
# year_current <- 2021
# year_to <- 2030
#
# src <- vimc_historical_data(con, year_cur = year_current, coverage_src_his = "202303gavi", coverage_src_fut = "202303gavi")
#
# z <- jenner:::find_year_introduction(con, "202303gavi-2")
# z <- z %>%
#   filter(country %in% countries)
#
# ## baseline data
# d0 <- bind_rows(src$historic, src$future)
#
# ## metadata to run
# ## keep year_intro information
# meta <- d0 %>%
#   filter(country %in% countries) %>%
#   select(country, vaccine, activity_type) %>%
#   distinct() %>%
#   left_join(z, by = c("country", "vaccine", "activity_type")) %>%
#   mutate(disease = vaccine) %>%
#   mutate(disease = ifelse(vaccine %in% c("MCV1", "MCV2"), "Measles", disease))%>%
#   mutate(disease = ifelse(vaccine %in% c("HepB_BD"), "HepB", disease))%>%
#   mutate(disease = ifelse(vaccine %in% c("RCV2"), "Rubella", disease))%>%
#   mutate(disease = ifelse(vaccine %in% c("DTP3"), "DTP", disease))%>%
#   mutate(disease = ifelse(vaccine %in% c("Hib3"), "Hib", disease))%>%
#   mutate(disease = ifelse(vaccine %in% c("PCV3"), "PCV", disease))
#
# meta1 <- meta %>%
#   select(disease, country) %>%
#   distinct()
#
#
#
# d <- NULL
# ## for each disease-country do work
# for (i in seq_along(meta1$country)){
#   s <- meta1[i, ]
#   s1 <- s %>% left_join(meta, by = c("country", "disease"), multiple = "all")
#   if (s$country == "CHN"){ # non-gavi country, do projection
#     params <- list(country = s$country,
#                    disease = s$disease,
#                    year_cur = year_current,
#                    introduction = s1 %>% select(vaccine, activity_type, year_intro))
#     ## routine rules
#     ## if past intro, non-linear project to 0.95
#     ## if future intro, increase to 70% in 4 years and then non-linear to 0.9
#     n_rules <- rep(list(NA), nrow(params$introduction))
#     for(j in seq_len(length(n_rules))){
#      if(params$introduction$year_intro[j] <= year_current & params$introduction$activity_type[j] == "routine"){
#        n_rules[[j]] <- list(non_linear_scale_up = list(year_from = year_current+1, year_to = year_to, endpoint = 0.99))
#      } else if(params$introduction$year_intro[j] > year_current & params$introduction$activity_type[j] == "routine"){
#        n_rules[[j]] <- list(catch_up_with_x = list(year_from = params$introduction$year_intro[j], year_to = params$introduction$year_intro[j]+3, vaccine_x_level = 0.7),
#             non_linear_scale_up = list(year_from = params$introduction$year_intro[j]+4, year_to = year_to, endpoint = 0.9))
#      } else {
#        ## campaign rules
#        ## if measles - sia catch-ups and assume mr
#        ## if hpv, typhoid and mena, initial mac
#        if(params$disease == "Measles"){
#          n_rules[[j]] <- list(sia_follow_up = list(vaccine_base = "MCV1", year_current = year_current, year_to = year_to, look_back = 4, sia_level = 0.9, age_from = 1, age_to = 5))
#        } else if (params$disease %in% c("HPV", "Typhoid", "MenA")){
#          n_rules[[j]] <- list(sia_catch_up = list(vaccine_base = "MenA", year_current = year_current, sia_level = 0.9, age_from = 1, age_to = 29))
#        } else if (params$disease == c("Cholera")){
#          n_rules[[j]] <- list(sia_recurrent = list(year_from = year_current, year_to = year_to, frequency = 3, sia_level = 0.3, age_from = 1, age_to = 60))
#        } else if (params$disease == c("JE", "YF")){
#          n_rules[[j]] <- NULL
#        }
#      }
#     }
#
#     proj_rules = n_rules
#     input <- list(params = params,
#                   src = src,
#                   proj_rul = proj_rules
#     )
#     input <- input_check(input) # sanity check: input structure, input parameters (any conflicts in routine introduction or projection rules, etc.)
#     dat <- vac_sce(input) %>%
#       filter(!is.na(year))# coverage scenario generation
#     d <- bind_rows(d, dat)
#
#   } else { # bind data
#     params <- list(country = s$country,
#                    disease = s$disease,
#                    year_cur = year_current,
#                    introduction = s1 %>% select(vaccine, activity_type, year_intro))
#     proj_rules = NULL
#     input <- list(params = params,
#                   src = src,
#                   proj_rul = proj_rules
#     )
#     input <- input_check(input) # sanity check: input structure, input parameters (any conflicts in routine introduction or projection rules, etc.)
#     dat <- vac_sce(input) %>%
#       filter(!is.na(year))# coverage scenario generation
#     d <- bind_rows(d, dat)
#   }
# }
