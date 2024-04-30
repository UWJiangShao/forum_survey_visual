library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(writexl)
library(haven)
library(stringr)

read_sas_datasets <- function(base_path, new_dataset_name, filename) {
  years <- 2018:2022
  
  for (year in years) {
  
    folder_path <- file.path(base_path, as.character(year))
    
    sas_file_path <- file.path(folder_path, filename)
    
    if (file.exists(sas_file_path)) {
      dataset <- read_sas(sas_file_path)
      
      # select variables we want to carry to the new datasets
      dataset <- dataset %>%
        select(Measurename, SubmeasureName, PopulationName, PopulationName1, Numer, Denom, contains("rate", ignore.case = TRUE), PlanCodes) %>%
        rename_with(~"rate", contains("rate", ignore.case = TRUE)) %>%
        mutate(MeasureNameAbbreviation = str_extract(Measurename, "(?<=\\()[A-Z0-9]{3}"))
      
      # assign year
      dataset$year <- year
      
      new_var_name <- paste0(new_dataset_name, year)
      
      assign(new_var_name, dataset, envir = .GlobalEnv)
    } else {
      cat("File not found:", sas_file_path, "\n")
    }
  }
}

base_path <- "C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/data/admin data/sas"

# read in STAR dataset from MY 2018 to MY 2022
read_sas_datasets(base_path, "hedis_star_" ,"star_hedis_all.sas7bdat")


# read in STAR Kids dataset from MY 2018 to MY 2022
read_sas_datasets(base_path, "hedis_sk_" ,"starkids_hedis_all.sas7bdat")


# read in STAR+PLUS dataset from MY 2018 to MY 2022
read_sas_datasets(base_path, "hedis_sp_" ,"starplus_hedis_all.sas7bdat")


# read in National Benchmark from MY 2018 to 2022
read_csv_datasets <- function(base_path) {
  years <- 2018:2022
  
  for (year in years) {
    
    file_name <- sprintf("MY%s HEDIS Percentiles_Medicaid.csv", year)
    
    csv_file_path <- file.path(base_path, file_name)
    
    if (file.exists(csv_file_path)) {

      dataset <- read.csv(csv_file_path)
      
      dataset$year <- year
      
      new_var_name <- paste0("national_level_", year)
      
      assign(new_var_name, dataset, envir = .GlobalEnv)
    } else {
      cat("File not found:", csv_file_path, "\n")
    }
  }
}

read_csv_datasets("C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/data/admin data/national")


# turn the dataset to long format for the convenience of plotting 
hedis_star_all <- rbind(hedis_star_2018, hedis_star_2019, hedis_star_2020, hedis_star_2021, hedis_star_2022)
hedis_sk_all <- rbind(hedis_sk_2018, hedis_sk_2019, hedis_sk_2020, hedis_sk_2021, hedis_sk_2022)
hedis_sp_all <- rbind(hedis_sp_2018, hedis_sp_2019, hedis_sp_2020, hedis_sp_2021, hedis_sp_2022)

# clean the national benchmark datasets:
national_level_2018 <- national_level_2018 %>%
  select(MeasureName, MeasureNameAbbreviation, BenchmarkID = AverageID, AverageRate, X10thPercentile, X25thPercentile, X50thPercentile, X75thPercentile, X90thPercentile, year)

national_level_2019 <- national_level_2019 %>%
  select(MeasureName, MeasureNameAbbreviation, BenchmarkID = AverageID, AverageRate, X10thPercentile, X25thPercentile, X50thPercentile, X75thPercentile, X90thPercentile, year)

national_level_2020 <- national_level_2020 %>%
  select(MeasureName, MeasureNameAbbreviation, BenchmarkID, AverageRate, X10thPercentile, X25thPercentile, X50thPercentile, X75thPercentile, X90thPercentile, year)

national_level_2021 <- national_level_2021 %>%
  select(MeasureName, MeasureNameAbbreviation, BenchmarkID, AverageRate, X10thPercentile, X25thPercentile, X50thPercentile, X75thPercentile, X90thPercentile, year)

national_level_2022 <- national_level_2022 %>%
  select(MeasureName, MeasureNameAbbreviation, BenchmarkID, AverageRate, X10thPercentile, X25thPercentile, X50thPercentile, X75thPercentile, X90thPercentile, year)


national_level_all <- rbind(national_level_2018, 
                            national_level_2019, 
                            national_level_2020, 
                            national_level_2021, 
                            national_level_2022)

national_level_all <- national_level_all %>%
  filter(BenchmarkID == 2)

