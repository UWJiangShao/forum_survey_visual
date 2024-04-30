library(ggplot2)

source("C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/Program/P01_clean_data_admin.R")
source("C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/Program/Function/create_plot.R")


#################################################################################
# AAP - SP 
#################################################################################

plot1_nb <- national_level_all %>%
  filter(MeasureNameAbbreviation == 'AAP', 
         str_detect(MeasureName, "Total"))


filter_criteria_plan_a <- list(MeasureNameAbbreviation = 'AAP',
                               SubmeasureName = 'All members',
                               PopulationNamePattern = "^SPS_")

plot1_plan_a <- create_plot_test(hedis_sp_all, 
                            filter_criteria_plan_a, 
                            plot1_nb, 
                            "National 50th",
                            "Adults Access (AAP)-STAR+PLUS\nTrend from MY2018 to MY2022\nService Area",
                            "Service Area", 
                            "X50thPercentile", 60, 100)


filter_criteria_plan_b <- list(MeasureNameAbbreviation = 'AAP',
                               SubmeasureName = 'All members',
                               PopulationNamePattern = "^SPM_")

plot1_plan_b <- create_plot_test(hedis_sp_all,
                            filter_criteria_plan_b,
                            plot1_nb,
                            "HEDIS\nNational\n50th",
                            "Adults Access (AAP)-STAR+PLUS\nTrend from MY2018 to MY2022\nby MCO",
                            "MCO",
                            "X50thPercentile", 60, 100)


#################################################################################
# AAP - SA
#################################################################################

plot1_nb <- national_level_all %>%
  filter(MeasureNameAbbreviation == 'AAP', 
         str_detect(MeasureName, "Total"))  

filter_criteria_plan_a <- list(MeasureNameAbbreviation = 'AAP',
                               SubmeasureName = 'All members',
                               PopulationNamePattern = "^STS_")

plot1_plan_a <- create_plot_test(hedis_star_all, 
                            filter_criteria_plan_a, 
                            plot1_nb, 
                            "HEDIS\nNational\n50th",
                            "Adults Access (AAP)-STAR Adult\nTrend from MY2018 to MY2022\nService Area",
                            "Service Area", 
                            "X50thPercentile", 
                            50, 100)

filter_criteria_plan_b <- list(MeasureNameAbbreviation = 'AAP',
                               SubmeasureName = 'All members',
                               PopulationNamePattern = "^STM_")

plot1_plan_b <- create_plot_test(hedis_star_all,
                            filter_criteria_plan_b,
                            plot1_nb,
                            "HEDIS\nNational\n50th",
                            "Adults Access (AAP)-STAR Adult\nTrend from MY2018 to MY2022\nby MCO",
                            "MCO",
                            "X50thPercentile",
                            50, 100)


#################################################################################
# W30 - SC 
#################################################################################
# plot1_nb <- national_level_all %>%
#   filter(
#     (MeasureNameAbbreviation == 'W30' & str_detect(MeasureName, "First 15 Months")) |
#       (MeasureNameAbbreviation == 'W15' & str_detect(MeasureName, "6 or more visits"))
#   )

# Note: since W15 and W30 measure change after 2019, we probably need to choose either 3 years or 5 years combine
# combined - 5 years:
# filtered_data_w30_a <- hedis_star_all %>%
#   filter(   (MeasureNameAbbreviation == 'W30' & str_detect(SubmeasureName, "first 15 months")) |
#               (MeasureNameAbbreviation == 'W15' & str_detect(SubmeasureName, "Six or more")),
#             grepl("^STS_", PopulationName))


# plot1_plan_a <- create_plot_c(filtered_data_w30_a,
#                                  plot1_nb, 
#                                  "HEDIS\nNational\nLevel",
#                                  "W30-STAR Child\nTrend from MY2018 to MY2022\nService Area v.s National Average",
#                                  "Service Area", 
#                                  "AverageRate")
# 
# 
# filtered_data_w30_b <- hedis_star_all %>%
#   filter(   (MeasureNameAbbreviation == 'W30' & str_detect(SubmeasureName, "first 15 months")) |
#               (MeasureNameAbbreviation == 'W15' & str_detect(SubmeasureName, "Six or more")),
#             grepl("^STM_", PopulationName))
# 
# 
# plot1_plan_b <- create_plot_c(filtered_data_w30_b,
#                               plot1_nb, 
#                               "HEDIS\nNational\n50th",
#                               "W30-STAR Child\nTrend from MY2018 to MY2022\nMCO v.s National 50th Percentile",
#                               "MCO", 
#                               "X50thPercentile")

plot1_nb <- national_level_all %>%
  filter(MeasureNameAbbreviation == 'W30', 
         str_detect(MeasureName, "15 Months-30 Months"))  

filter_criteria_plan_a <- list(MeasureNameAbbreviation = 'W30',
                               SubmeasureName = 'Well child visits for age15-30 months',
                               PopulationNamePattern = "^STS_")

plot1_plan_a <- create_plot_test_w30(hedis_star_all, 
                                 filter_criteria_plan_a, 
                                 plot1_nb, 
                                 "HEDIS\nNational\n50th",
                                 "W30-STAR Child\nTrend from MY2020 to MY2022\nService Area",
                                 "Service Area", 
                                 "X50thPercentile", 
                                 50, 95)


filter_criteria_plan_b <- list(MeasureNameAbbreviation = 'W30',
                               SubmeasureName = 'Well child visits for age15-30 months',
                               PopulationNamePattern = "^STM_")

plot1_plan_b <- create_plot_test_w30(hedis_star_all, 
                                     filter_criteria_plan_b, 
                                     plot1_nb, 
                                     "HEDIS\nNational\n50th",
                                     "W30-STAR Child\nTrend from MY2020 to MY2022\nby MCO",
                                     "MCO", 
                                     "X50thPercentile", 
                                     50, 95)


#################################################################################
# W30 - SK
#################################################################################
# filtered_data_w30_a <- hedis_sk_all %>%
#   filter(   (MeasureNameAbbreviation == 'W30' & str_detect(SubmeasureName, "first 15 months")) |
#               (MeasureNameAbbreviation == 'W15' & str_detect(SubmeasureName, "Six or more")),
#             grepl("^SKS_", PopulationName))
# 
# 
# plot1_plan_a <- create_plot_c(filtered_data_w30_a,
#                               plot1_nb, 
#                               "HEDIS\nNational\nLevel",
#                               "W30-STAR Kids\nTrend from MY2018 to MY2022\nService Area v.s National Average",
#                               "Service Area", 
#                               "AverageRate")
# 
# 
# filtered_data_w30_b <- hedis_sk_all %>%
#   filter(   (MeasureNameAbbreviation == 'W30' & str_detect(SubmeasureName, "first 15 months")) |
#               (MeasureNameAbbreviation == 'W15' & str_detect(SubmeasureName, "Six or more")),
#             grepl("^SKM_", PopulationName))
# 
# 
# plot1_plan_b <- create_plot_c(filtered_data_w30_b,
#                               plot1_nb, 
#                               "HEDIS\nNational\n50th",
#                               "W30-STAR Kids\nTrend from MY2018 to MY2022\nMCO v.s National 50th Percentile",
#                               "MCO", 
#                               "X50thPercentile")
# 


plot1_nb <- national_level_all %>%
  filter(MeasureNameAbbreviation == 'W30', 
         str_detect(MeasureName, "15 Months-30 Months"))  

filter_criteria_plan_a <- list(MeasureNameAbbreviation = 'W30',
                               SubmeasureName = 'Well child visits for age15-30 months',
                               PopulationNamePattern = "^SKS_")

plot1_plan_a <- create_plot_test_w30(hedis_sk_all, 
                                     filter_criteria_plan_a, 
                                     plot1_nb, 
                                     "HEDIS\nNational\n50th",
                                     "W30-STAR Kids\nTrend from MY2020 to MY2022\nService Area",
                                     "Service Area", 
                                     "X50thPercentile", 
                                     40, 100)


plot1_nb <- national_level_all %>%
  filter(MeasureNameAbbreviation == 'W30', 
         str_detect(MeasureName, "15 Months-30 Months"))  

filter_criteria_plan_a <- list(MeasureNameAbbreviation = 'W30',
                               SubmeasureName = 'Well child visits for age15-30 months',
                               PopulationNamePattern = "^SKM_")

plot1_plan_a <- create_plot_test_w30(hedis_sk_all, 
                                     filter_criteria_plan_a, 
                                     plot1_nb, 
                                     "HEDIS\nNational\n50th",
                                     "W30-STAR Kids\nTrend from MY2020 to MY2022\nby MCO",
                                     "Service Area", 
                                     "X50thPercentile", 
                                     40, 100)

#################################################################################
# IMA - SC 
#################################################################################
plot1_nb <- national_level_all %>%
  filter(MeasureNameAbbreviation == 'IMA', 
         str_detect(MeasureName, "Combination 2"))  

filter_criteria_plan_a <- list(MeasureNameAbbreviation = 'IMA',
                               SubmeasureName = 'Combination 2 Immunizations',
                               PopulationNamePattern = "^STS_")

plot1_plan_a <- create_plot_test(hedis_star_all, 
                            filter_criteria_plan_a, 
                            plot1_nb, 
                            "HEDIS\nNational\n50th",
                            "IMA-STAR Child\nTrend from MY2018 to MY2022\nService Area",
                            "Service Area", 
                            "X50thPercentile", 
                            10, 50)


filter_criteria_plan_b <- list(MeasureNameAbbreviation = 'IMA',
                               SubmeasureName = 'Combination 2 Immunizations',
                               PopulationNamePattern = "^STM_")

plot1_plan_b <- create_plot_test(hedis_star_all,
                            filter_criteria_plan_b,
                            plot1_nb,
                            "HEDIS\nNational\n50th",
                            "IMA-STAR Child\nTrend from MY2018 to MY2022\nby MCO",
                            "MCO",
                            "X50thPercentile", 
                            10, 50)



#################################################################################
# IMA - SK
#################################################################################


plot1_nb <- national_level_all %>%
  filter(MeasureNameAbbreviation == 'IMA', 
         str_detect(MeasureName, "Combination 2"))  

filter_criteria_plan_a <- list(MeasureNameAbbreviation = 'IMA',
                               SubmeasureName = 'Combination 2 Immunizations',
                               PopulationNamePattern = "^SKS_")

plot1_plan_a <- create_plot_test(hedis_sk_all, 
                            filter_criteria_plan_a, 
                            plot1_nb, 
                            "HEDIS\nNational\n50th",
                            "IMA-STAR Kids\nTrend from MY2018 to MY2022\nService Area",
                            "Service Area", 
                            "X50thPercentile", 
                            10, 60)


filter_criteria_plan_b <- list(MeasureNameAbbreviation = 'IMA',
                               SubmeasureName = 'Combination 2 Immunizations',
                               PopulationNamePattern = "^SKM_")

plot1_plan_b <- create_plot_test(hedis_sk_all,
                              filter_criteria_plan_b,
                              plot1_nb,
                              "HEDIS\nNational\n50th",
                              "IMA-STAR Kids\nTrend from MY2018 to MY2022\nby MCO",
                              "MCO",
                              "X50thPercentile", 
                              10, 60)







#################################################################################
# AAP - SP & SA
#################################################################################

plot1_nb <- national_level_all %>%
  filter(MeasureNameAbbreviation == 'AAP', 
         str_detect(MeasureName, "Total"))


filter_criteria_plan_a <- list(MeasureNameAbbreviation = 'AAP',
                               SubmeasureName = 'All members')

plot1_plan_a <- create_plot_test_2(hedis_star_all, hedis_sp_all,
                                 filter_criteria_plan_a, 
                                 plot1_nb, 
                                 "National 50th",
                                 "Adults Access (AAP)-SA and SP\nTrend from MY2018 to MY2022\nby ServiceArea",
                                 "Service Area", 
                                 "X50thPercentile", 
                                 50, 100)



# test <- hedis_star_all %>%
#   filter(MeasureNameAbbreviation == "AAP",
#          SubmeasureName == "All members",
#          grepl("^STS_", PopulationName))
# test$program <- "SA"
# 
# test2 <- hedis_sp_all %>%
#   filter(MeasureNameAbbreviation == "AAP",
#          SubmeasureName == "All members",
#          grepl("^SPS_", PopulationName))
# test2$program <- "SP"
# 
# test3 <- rbind(test, test2)









#################################################################################
# W30 - SC & SK
#################################################################################

plot1_nb <- national_level_all %>%
  filter(MeasureNameAbbreviation == 'W30', 
         str_detect(MeasureName, "15 Months-30 Months"))


filter_criteria_plan_a <- list(MeasureNameAbbreviation = 'W30',
                               SubmeasureName = 'Well child visits for age15-30 months')

plot1_plan_a <- create_plot_test_2(hedis_star_all, hedis_sk_all,
                                   filter_criteria_plan_a, 
                                   plot1_nb, 
                                   "National 50th",
                                   "W30-SC and SK\nTrend from MY2020 to MY2022\nService Area",
                                   "Service Area", 
                                   "X50thPercentile", 
                                   50, 100)







#################################################################################
# IMA - SC & SK
#################################################################################

plot1_nb <- national_level_all %>%
  filter(MeasureNameAbbreviation == 'IMA', 
         str_detect(MeasureName, "Combination 2"))


filter_criteria_plan_a <- list(MeasureNameAbbreviation = 'IMA',
                               SubmeasureName = 'Combination 2 Immunizations')

plot1_plan_a <- create_plot_test_2(hedis_star_all, hedis_sk_all,
                                   filter_criteria_plan_a, 
                                   plot1_nb, 
                                   "National 50th",
                                   "IMA-SC and SK\nTrend from MY2018 to MY2022\nby ServiceArea",
                                   "Service Area", 
                                   "X50thPercentile", 
                                   10, 50)

