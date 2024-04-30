library(readxl)
library(dplyr)
library(tidyverse)
library(openxlsx)


# read_specific_sheets <- function() {
#   years <- 2019:2023
#   base_path <- "C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/data/deliverable dataset/"
#   #base_path <-"/Users/tigershao/Dropbox (UFL)/Forum Slides Making Project/data/deliverable dataset/"
#   
#   sheet_patterns <- c("^CHIP-", "^STAR Adult-", "^STAR Child-", "^STAR Kids-", "^STAR\\+PLUS-")
#   desired_columns <- c("MCO", "Service Area", "Plan Code", "Number of Respondents", "Score")
#   
#   for (year in years) {
#     file_name <- paste0(base_path, year, ".xlsx")
#     
#     if(file.exists(file_name)) {
#       sheets <- excel_sheets(file_name)
#       
#       for (pattern in sheet_patterns) {
#         matched_sheets <- grep(pattern, sheets, value = TRUE)
#         
#         for (sheet in matched_sheets) {
#           data <- read_excel(file_name, sheet = sheet, skip = 1) %>%
#             select(all_of(desired_columns))
#           
#           if (!all(desired_columns %in% colnames(data))) {
#             warning(paste0("Sheet '", sheet, "' in file '", file_name, 
#                            "' does not contain all of the required columns. Skipping."))
#             next
#           }
#           data <- data %>%
#             mutate(`Number of Respondents` = as.numeric(`Number of Respondents`),
#                    Score = as.numeric(Score))%>%
#             filter(MCO != "Back to all measures")
#           
#           measure <- gsub("-", "", sub(pattern, "", sheet))
#           
#           # Renaming columns
#           data <- data %>%
#             rename(service_area = `Service Area`,
#                    plancode = `Plan Code`,
#                    n = `Number of Respondents`,
#                    score = Score) %>%
#             mutate(n = paste0("n_", year),
#                    score = paste0("score_", year),
#                    year = year, 
#                    measure = measure)
#           
#           data <- mutate(data, year = year, measure = measure)
#           
#           clean_sheet_name <- gsub("\\s|\\+|-", "_", sheet)
#           dataset_name <- paste0("data_", year, "_", clean_sheet_name)
#           
#           assign(dataset_name, data, envir = .GlobalEnv)
#         }
#       }
#     } else {
#       warning(paste0("File not found: ", file_name))
#     }
#   }
# }
# 
# 
# read_specific_sheets()


# 
# library(ggplot2)
# 
# ggplot(GT, aes(x = factor(year), y = Score, fill = factor(year))) +
#   geom_boxplot() +
#   
#   stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -1) +
#   facet_wrap(~ program, scales = "free") +
#   labs(title = "Distribution of 'Getting Treated'/'Getting Care Quickly' Over Time",
#        x = "Year",
#        y = "Score") +
#   theme_minimal() +
#   scale_fill_brewer(palette = "Set1")
# 
# 
# ggplot(HWDC, aes(x = factor(year), y = Score, fill = factor(year))) +
#   geom_boxplot() +
#   
#   stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -1) +
#   facet_wrap(~ program, scales = "free") +
#   labs(title = "Distribution of 'How Well Doctor Communicate' Over Time",
#        x = "Year",
#        y = "Score") +
#   theme_minimal() +
#   scale_fill_brewer(palette = "Set2")
# 
# ggplot(RHP, aes(x = factor(year), y = Score, fill = factor(year))) +
#   geom_boxplot() +
#   
#   stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -1) +
#   facet_wrap(~ program, scales = "free") +
#   labs(title = "Distribution of 'Rate Health Plan' Over Time",
#        x = "Year",
#        y = "Score") +
#   theme_minimal() +
#   scale_fill_brewer(palette = "Set3")
# 
# ggplot(RPD, aes(x = factor(year), y = Score, fill = factor(year))) +
#   geom_boxplot() +
#   
#   stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -1) +
#   facet_wrap(~ program, scales = "free") +
#   labs(title = "Distribution of 'Rate Health Plan' Over Time",
#        x = "Year",
#        y = "Score") +
#   theme_minimal() +
#   scale_fill_brewer(palette = "Set4")
# 
# ggplot(SK_Other_combine, aes(x = factor(year), y = Score, fill = factor(year))) +
#   geom_boxplot() +
#   
#   stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -1) +
#   facet_wrap(~ program, scales = "free") +
#   labs(title = "Distribution of STAR Kids other measures Over Time",
#        x = "Year",
#        y = "Score") +
#   theme_minimal() +
#   scale_fill_brewer(palette = "Set4")
# 
# 
# 
# 
# library(RColorBrewer)
# 
# 
# base_palette <- brewer.pal(9, "Set1")
# custom_palette <- colorRampPalette(base_palette)
# 
# ggplot(GT, aes(x = `Service Area`, y = Score, fill = `Service Area`)) +
#   geom_boxplot() +
#   stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -1) +
#   facet_wrap(~ factor(year), scales = "free") +
#   labs(title = "Getting Care by Service Area from 2019 to 2023",
#        x = "Service Area",
#        y = "Score") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
#   scale_fill_manual(values = custom_palette(n = length(unique(all$`Service Area`))))
# 
# 
# ggplot(HWDC, aes(x = `Service Area`, y = Score, fill = `Service Area`)) +
#   geom_boxplot() +
#   stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -1) +
#   facet_wrap(~ factor(year), scales = "free") +
#   labs(title = "HWDC by Service Area from 2019 to 2023",
#        x = "Service Area",
#        y = "Score") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
#   scale_fill_manual(values = custom_palette(n = length(unique(all$`Service Area`))))
# 
# ggplot(RHP, aes(x = `Service Area`, y = Score, fill = `Service Area`)) +
#   geom_boxplot() +
#   stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -1) +
#   facet_wrap(~ factor(year), scales = "free") +
#   labs(title = "RHP by Service Area from 2019 to 2023",
#        x = "Service Area",
#        y = "Score") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
#   scale_fill_manual(values = custom_palette(n = length(unique(all$`Service Area`))))
# 
# ggplot(RPD, aes(x = `Service Area`, y = Score, fill = `Service Area`)) +
#   geom_boxplot() +
#   stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -1) +
#   facet_wrap(~ factor(year), scales = "free") +
#   labs(title = "RPD by Service Area from 2019 to 2023",
#        x = "Service Area",
#        y = "Score") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
#   scale_fill_manual(values = custom_palette(n = length(unique(all$`Service Area`))))
# 
# 

# library(readxl)
# library(stringr)
# 

read_specific_sheets <- function() {
  years <- 2019:2023
  base_path <- "C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/data/deliverable dataset/"
  #base_path <- "/Users/tigershao/Dropbox (UFL)/Forum Slides Making Project/data/deliverable dataset/"

  sheet_patterns <- c("^CHIP-", "^STAR Adult-", "^STAR Child-", "^STAR Kids-", "^STAR\\+PLUS-")
  desired_columns <- c("MCO", "Service Area", "Plan Code", "Number of Respondents", "Score")

  for (year in years) {
    file_name <- paste0(base_path, year, ".xlsx")

    if(file.exists(file_name)) {
      sheets <- excel_sheets(file_name)

      for (pattern in sheet_patterns) {
        matched_sheets <- grep(pattern, sheets, value = TRUE)

        for (sheet in matched_sheets) {
          data <- read_excel(file_name, sheet = sheet, skip = 1) %>%
            select(all_of(desired_columns))

          if (!all(desired_columns %in% colnames(data))) {
            warning(paste0("Sheet '", sheet, "' in file '", file_name,
                           "' does not contain all of the required columns. Skipping."))
            next
          }
          data <- data %>%
            mutate(`Number of Respondents` = as.numeric(`Number of Respondents`),
                   Score = as.numeric(Score))%>%
            filter(MCO != "Back to all measures")

          measure <- gsub("-", "", sub(pattern, "", sheet))

          # Renaming columns with year suffix for 'Number of Respondents' and 'Score'
          respondents_colname <- paste0("n_", year)
          score_colname <- paste0("score_", year)

          data <- data %>%
            rename(service_area = `Service Area`,
                   plancode = `Plan Code`,
                   !!respondents_colname := `Number of Respondents`,
                   !!score_colname := Score) %>%
            mutate(year = year,
                   measure = measure)

          clean_sheet_name <- gsub("\\s|\\+|-", "_", sheet)
          dataset_name <- paste0("data_", year, "_", clean_sheet_name)

          assign(dataset_name, data, envir = .GlobalEnv)
        }
      }
    } else {
      warning(paste0("File not found: ", file_name))
    }
  }
}

# Run the function
read_specific_sheets()







library(dplyr)


extract_program <- function(dataset_name) {
  if (grepl("CHIP", dataset_name)) {
    return("CHIP")
  } else if (grepl("STAR_Adult", dataset_name)) {
    return("STAR_Adult")
  } else if (grepl("STAR_Child", dataset_name)) {
    return("STAR_Child")
  } else if (grepl("STAR_Kids", dataset_name)) {
    return("STAR_Kids")
  } else if (grepl("STAR_PLUS", dataset_name)) {
    return("STAR_PLUS")
  } else {
    return(NA)  
  }
}


dataset_names <- ls(pattern = "data_20[1-2][0-9]_")


modified_datasets <- list()


for (dataset_name in dataset_names) {

  dataset <- get(dataset_name)
  

  name_parts <- unlist(strsplit(dataset_name, "_"))
  year <- name_parts[2]
  program <- extract_program(dataset_name)
  

  colnames(dataset)[grep("n_", colnames(dataset))] <- "n"
  colnames(dataset)[grep("score_", colnames(dataset))] <- "score"
  

  dataset$year <- year
  dataset$program <- program
  

  modified_datasets[[dataset_name]] <- dataset
}


all_dataset <- bind_rows(modified_datasets)



# 
# # add the statelevel for each measure-program-year
# 
# # Calculate the mean score for each combination of measure, program, and year
# statelevel_means <- all_dataset %>%
#   group_by(measure, program, year) %>%
#   summarise(statelevel = mean(score, na.rm = TRUE), .groups = 'drop')
# 
# # Join the means back to the original dataset
# all_dataset <- all_dataset %>%
#   left_join(statelevel_means, by = c("measure", "program", "year"))
# 
# 
# library(ggplot2)
# library(dplyr)
# 
# 
# star_adult_data <- all_dataset %>%
#   filter(program == "STAR_Kids")
# 
# avg_scores <- star_adult_data %>%
#   group_by(service_area, measure, year) %>%
#   summarise(avg_score = mean(score, na.rm = TRUE)) %>%
#   ungroup()
# 
# avg_scores <- avg_scores %>%
#   filter(!(measure %in% c("Getting Treat", "Rate Health Plan", "Getting Information", "Prescriptions")))
# 
# 
# y_range <- range(avg_scores$avg_score, na.rm = TRUE)
# 
# 
# ggplot(avg_scores, aes(x = as.numeric(year), y = avg_score, group = measure, color = measure)) +
#   geom_line(size = 1.2) +
#   facet_wrap(~ service_area, scales = "free_y", ncol = 6) + # One column of plots, each service area gets its own plot
#   labs(title = "Average Score Change Over Years by Service Area for STAR Kids - Part II",
#        x = "Year",
#        y = "Average Score",
#        color = "Measure") +
#   theme_minimal() +
#   theme(legend.position = "bottom", 
#         strip.background = element_blank(), 
#         strip.text.x = element_text(size = 10)) + 
#   scale_y_continuous(limits = y_range)+
#   geom_text(aes(label = round(avg_score, 2) * 100), vjust = -1, size = 3, check_overlap = TRUE)
# 
# 



combine_function <- function(end_year, program, measure) {
  
  clean_program <- gsub("\\s|\\+|-", "_", program)
  clean_measure <- gsub("\\s|\\+|-", "_", measure)
  
  datasets <- list()
  
  for (year in 2019:end_year) {
    dataset_name <- paste0("data_", year, "_", clean_program, "_", clean_measure)
    
    if (exists(dataset_name, envir = .GlobalEnv)) {
      dataset <- get(dataset_name, envir = .GlobalEnv)
      
      if (year == 2019) {
        cols_to_keep <- c("MCO", "plancode", "service_area", paste0("n_", year), paste0("score_", year))
      } else {
        cols_to_keep <- c("plancode", paste0("n_", year), paste0("score_", year))
      }
      
      dataset <- dataset %>%
        select(all_of(cols_to_keep))
      
      datasets[[length(datasets) + 1]] <- dataset
    } else {
      warning(paste0("Dataset not found: ", dataset_name))
    }
  }
  
  combined_data <- reduce(datasets, full_join, by = "plancode")
  
  return(combined_data)
}


GCQ_CHIP <- combine_function(2022, "CHIP", "Getting_Care_Quickly")
GCQ_SA <- combine_function(2023, "STAR_Adult", "Getting_Treat")
GCQ_SC <- combine_function(2023, "STAR_Child", "Getting_Care_Quickly")
GCQ_SK <- combine_function(2023, "STAR_Kids", "Getting_Treat")
GCQ_SP <- combine_function(2023, "STAR_PLUS", "Getting_Treat")


HWDC_CHIP <- combine_function(2022, "CHIP", "HWDC")
HWDC_SA <- combine_function(2023, "STAR_Adult", "HWDC")
HWDC_SC <- combine_function(2023, "STAR_Child", "HWDC")
HWDC_SP <- combine_function(2023, "STAR_PLUS", "HWDC")

HWDC_CHIP <- combine_function(2022, "CHIP", "HWDC")
HWDC_SA <- combine_function(2023, "STAR_Adult", "HWDC")
HWDC_SC <- combine_function(2023, "STAR_Child", "HWDC")
HWDC_SP <- combine_function(2023, "STAR_PLUS", "HWDC")

RHP_CHIP <- combine_function(2022, "CHIP", "Rate_Health_Plan")
RHP_SA <- combine_function(2023, "STAR_Adult", "Rate_Health_Plan")
RHP_SC <- combine_function(2023, "STAR_Child", "Rate_Health_Plan")
RHP_SP <- combine_function(2023, "STAR_PLUS", "Rate_Health_Plan")
RHP_SK <- combine_function(2023, "STAR_Kids", "Rate_Health_Plan")

RPD_CHIP <- combine_function(2022, "CHIP", "Rate_Personal_Doctor")
RPD_SA <- combine_function(2023, "STAR_Adult", "Rate_Personal_Doctor")
RPD_SC <- combine_function(2023, "STAR_Child", "Rate_Personal_Doctor")
RPD_SP <- combine_function(2023, "STAR_PLUS", "Rate_Personal_Doctor")



subset_mco_changes <- function(df, years) {
  # Calculate the percentage changes and store them in a vector
  change_cols <- c()
  for (i in 1:(length(years) - 1)) {
    current_year <- years[i]
    next_year <- years[i + 1]
    change_col_name <- paste0("change_", next_year, "_", current_year)
    df[[change_col_name]] <- (df[[paste0("score_", next_year)]] - df[[paste0("score_", current_year)]]) / df[[paste0("score_", current_year)]]
    change_cols <- c(change_cols, change_col_name)
  }
  
  # Define a function to check if changes are consistently positive or negative
  is_consistent <- function(change_values) {
    all(change_values > 0.08) || all(change_values < -0.08)
  }
  
  # Apply this function to each row
  consistent_change <- apply(df[, change_cols], 1, is_consistent)
  
  # Subset the dataframe to only include rows with consistent changes
  subset_df <- df[consistent_change, ]
  
  # Optionally, remove the change columns before returning the subset
  subset_df <- subset_df[, !names(subset_df) %in% change_cols]
  
  return(subset_df)
}


# Assuming your dataframe is named df and the years you're interested in are 2021, 2022, and 2023
years <- c( "2021", "2022", "2023") 
 
subsetting_result <- subset_mco_changes(GCQ_SK, years)





# analyze_diff <- function(dataset, end_year) {
#   for (year in 2019:end_year) {
#     if (year > 2019) {
#       for (compare_year in 2019:(year - 1)) {
#         score_col_current <- paste0("score_", year)
#         score_col_compare <- paste0("score_", compare_year)
#         n_col_current <- paste0("n_", year)
#         n_col_compare <- paste0("n_", compare_year)
#         
#         score_diff_col <- paste0("score_diff_", year, "_", compare_year)
#         n_diff_col <- paste0("n_diff_", year, "_", compare_year)
#         
#         score_diff_rate_col <- paste0("diff_rate_score_", year, "_", compare_year)
#         n_diff_rate_col <- paste0("diff_rate_n_", year, "_", compare_year)
#         
#         # Calculating differences and rates
#         dataset[[score_diff_col]] <- dataset[[score_col_current]] - dataset[[score_col_compare]]
#         dataset[[n_diff_col]] <- dataset[[n_col_current]] - dataset[[n_col_compare]]
#         dataset[[score_diff_rate_col]] <- dataset[[score_diff_col]] / dataset[[score_col_compare]]
#         dataset[[n_diff_rate_col]] <- dataset[[n_diff_col]] / dataset[[n_col_compare]]
#       }
#     }
#   }
#   
#   return(dataset)
# }

library(dplyr)

analyze_diff <- function(dataset, end_year) {
  for (year in 2019:end_year) {
    if (year > 2019) {
      previous_year <- year - 1
      
      
      score_col_current <- paste0("score_", year)
      score_col_previous <- paste0("score_", previous_year)
      n_col_current <- paste0("n_", year)
      n_col_previous <- paste0("n_", previous_year)
      
    
      score_diff_col <- paste0("score_diff_", year, "_", previous_year)
      n_diff_col <- paste0("n_diff_", year, "_", previous_year)
      
   
      score_diff_rate_col <- paste0("diff_rate_score_", year, "_", previous_year)
      n_diff_rate_col <- paste0("diff_rate_n_", year, "_", previous_year)
      
    
      dataset[[score_diff_col]] <- dataset[[score_col_current]] - dataset[[score_col_previous]]
      dataset[[n_diff_col]] <- dataset[[n_col_current]] - dataset[[n_col_previous]]
      dataset[[score_diff_rate_col]] <- dataset[[score_diff_col]] / dataset[[score_col_previous]]
      dataset[[n_diff_rate_col]] <- dataset[[n_diff_col]] / dataset[[n_col_previous]]
    }
  }
  
  return(dataset)
}



GCQ_CHIP_diff <- analyze_diff(GCQ_CHIP, 2022)
GCQ_SA_diff <- analyze_diff(GCQ_SA, 2023)
GCQ_SC_diff <- analyze_diff(GCQ_SC, 2023)
GCQ_SK_diff <- analyze_diff(GCQ_SK, 2023)
GCQ_SP_diff <- analyze_diff(GCQ_SP, 2023)

HWDC_SA_diff <- analyze_diff(HWDC_SA, 2023)
HWDC_SC_diff <- analyze_diff(HWDC_SC, 2023)
HWDC_SP_diff <- analyze_diff(HWDC_SP, 2023)

RHP_CHIP_diff <- analyze_diff(RHP_CHIP, 2022)
RHP_SA_diff <- analyze_diff(RHP_SA, 2023)
RHP_SC_diff <- analyze_diff(RHP_SC, 2023)
RHP_SK_diff <- analyze_diff(RHP_SK, 2023)
RHP_SP_diff <- analyze_diff(RHP_SP, 2023)

RPD_CHIP_diff <- analyze_diff(RPD_CHIP, 2022)
RPD_SA_diff <- analyze_diff(RPD_SA, 2023)
RPD_SC_diff <- analyze_diff(RPD_SC, 2023)
RPD_SP_diff <- analyze_diff(RPD_SP, 2023)




library(openxlsx)

write_dfs_to_excel <- function(dfs_list, file_path) {
  wb <- createWorkbook()
  
  for (df_name in names(dfs_list)) {
    addWorksheet(wb, df_name)
    writeData(wb, df_name, dfs_list[[df_name]])
  }
  
  saveWorkbook(wb, file_path, overwrite = TRUE)
}

dfs_list <- list(
  GCQ_CHIP_diff = GCQ_CHIP_diff,
  GCQ_SA_diff = GCQ_SA_diff,
  GCQ_SC_diff = GCQ_SC_diff,
  GCQ_SK_diff = GCQ_SK_diff,
  GCQ_SP_diff = GCQ_SP_diff,
  HWDC_SA_diff = HWDC_SA_diff,
  HWDC_SC_diff = HWDC_SC_diff,
  HWDC_SP_diff = HWDC_SP_diff,
  RHP_CHIP_diff = RHP_CHIP_diff,
  RHP_SA_diff = RHP_SA_diff,
  RHP_SC_diff = RHP_SC_diff,
  RHP_SK_diff = RHP_SK_diff,
  RHP_SP_diff = RHP_SP_diff,
  RPD_CHIP_diff = RPD_CHIP_diff,
  RPD_SA_diff = RPD_SA_diff,
  RPD_SC_diff = RPD_SC_diff,
  RPD_SP_diff = RPD_SP_diff
)


# output_file_name <- "/Users/tigershao/Dropbox (UFL)/Forum Slides Making Project/data/output_diff.xlsx"

# write_dfs_to_excel(dfs_list, output_file_name)






# read_specific_sheets <- function() {
#   years <- 2019:2023
#   base_path <- "C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/data/deliverable dataset/"
#   sheet_patterns <- c("^CHIP-", "^STAR Adult-", "^STAR Child-", "^STAR Kids-", "^STAR\\+PLUS-")
#   desired_columns <- c("MCO", "Service Area", "Plan Code", "Number of Respondents", "Score")
#   
#   for (year in years) {
#     file_name <- paste0(base_path, year, ".xlsx")
#     
#     if(file.exists(file_name)) {
#       sheets <- excel_sheets(file_name)
#       
#       for (pattern in sheet_patterns) {
#         matched_sheets <- grep(pattern, sheets, value = TRUE)
#         
#         for (sheet in matched_sheets) {
#           data <- read_excel(file_name, sheet = sheet, skip = 1) %>%
#             select(all_of(desired_columns))
#           
#           # Check if all desired columns are present
#           if (!all(desired_columns %in% colnames(data))) {
#             warning(paste0("Sheet '", sheet, "' in file '", file_name, 
#                            "' does not contain all of the required columns. Skipping."))
#             next
#           }
#           
#           clean_sheet_name <- gsub("\\s|\\+|-", "_", sheet)
#           dataset_name <- paste0("data_", year, "_", clean_sheet_name)
#           
#           assign(dataset_name, data, envir = .GlobalEnv)
#         }
#       }
#     } else {
#       warning(paste0("File not found: ", file_name))
#     }
#   }
# }
# 
# read_specific_sheets()


# SC_RHP_23 <- read_excel("C:\\Users\\jiang.shao\\Dropbox (UFL)\\Forum Slides Making Project\\data\\deliverable dataset\\2023.xlsx", sheet = "STAR Child-Rate Health Plan", skip = 1)
# # 
# # 
# # 
# # SC_RHP <- read_excel("C:\\Users\\jiang.shao\\Dropbox (UFL)\\Forum Slides Making Project\\data\\summary_table.xlsx", sheet = "SC_RHP")
# 
# 
# 
# 
# combine <- function(program, suffix) {
#   years <- 2019:2023
#   combined_data <- NULL
#   pattern <- paste0("^", program, "-", suffix, "$") # Pattern to match the exact sheet name
#   
#   for (year in years) {
#     dataset_name <- paste0("data_", year, "_", gsub(" ", "_", program), "_", gsub(" ", "_", suffix))
#     
#     if (exists(dataset_name, where = .GlobalEnv)) {
#       dataset <- get(dataset_name, envir = .GlobalEnv)
#       
#       if (is.null(combined_data)) {
#         combined_data <- dataset
#       } else {
#         combined_data <- bind_rows(combined_data, dataset)
#       }
#     } else {
#       warning(paste0("Dataset not found for year ", year, ": ", dataset_name))
#     }
#   }
#   
#   return(combined_data)
# }
# 
# 
# 
# 
# CHIP_GCQ <- combine("CHIP", "Getting_Care_Quickly")
# CHIP_HWDC <- combine("CHIP", "HWDC")
# CHIP_RHP <- combine("CHIP", "Rate_Health_Plan")
# CHIP_RPD <- combine("CHIP", "Rate_Personal_Doctor")
# 
# SA_GT <- combine("STAR_Adult", "Getting_Treat")
# SA_HWDC <- combine("STAR_Adult", "HWDC")
# SA_RHP <- combine("STAR_Adult", "Rate_Health_Plan")
# SA_RPD <- combine("STAR_Adult", "Rate_Personal_Doctor")
# 
# SC_GCQ <- combine("STAR_Child", "Getting_Care_Quickly")
# SC_HWDC <- combine("STAR_Child", "HWDC")
# SC_RHP <- combine("STAR_Child", "Rate_Health_Plan")
# SC_RPD <- combine("STAR_Child", "Rate_Personal_Doctor")
# 
# SP_GT <- combine("STAR_PLUS", "Getting_Treat")
# SP_HWDC <- combine("STAR_PLUS", "HWDC")
# SP_RHP <- combine("STAR_PLUS", "Rate_Health_Plan")
# SP_RPD <- combine("STAR_PLUS", "Rate_Personal_Doctor")
# 
# SK_GT <- combine("STAR_Kids", "Getting_Treat")
# SK_ST <- combine("STAR_Kids", "Special_Therapy")
# SK_PRE <- combine("STAR_Kids", "Prescriptions")
# SK_CC <- combine("STAR_Kids", "Care_Coordination")
# SK_GI <- combine("STAR_Kids", "Getting_Information")
# SK_TRANS <- combine("STAR_Kids", "Transition")
# SK_RHP <- combine("STAR_Kids", "Rate_Health_Plan")
# SK_COUNS <- combine("STAR_Kids", "Counseling")
# 
# 
# 
# 
# SK_Other_combine <- bind_rows(
#   SK_ST = SK_ST,  
#   SK_PRE = SK_PRE,       
#   SK_CC = SK_CC,       
#   SK_GI = SK_GI,
#   SK_TRANS = SK_TRANS,       
#   SK_COUNS = SK_COUNS,
#   .id = "program"
# )
# 
# 
# library(ggplot2)
# library(reshape2)
# 
# GT <- bind_rows(
#   CHIP_GCQ = CHIP_GCQ,
#   SA_GT = SA_GT, 
#   SP_GT = SP_GT, 
#   SC_GCQ = SC_GCQ, 
#   SK_GT = SK_GT, 
#   .id = "program"
# )
# 
# HWDC <- bind_rows(
#   CHIP_HWDC = CHIP_HWDC,  
#   SA_HWDC = SA_HWDC,       
#   SP_HWDC = SP_HWDC,       
#   SC_HWDC = SC_HWDC,      
#   .id = "program"
# )
# 
# RHP <- bind_rows(
#   CHIP_RHP = CHIP_RHP,  
#   SA_RHP = SA_RHP,       
#   SC_RHP = SC_RHP,       
#   SP_RHP = SP_RHP,  
#   SK_RHP = SK_RHP,  
#   .id = "program"
# )
# 
# RPD <- bind_rows(
#   CHIP_RPD = CHIP_RPD,  
#   SA_RPD = SA_RPD,       
#   SC_RPD = SC_RPD,       
#   SP_RPD = SP_RPD,  
#   .id = "program"
# )
# 
# 
# all <- bind_rows(
#   CHIP_GCQ = CHIP_GCQ,
#   SA_GT = SA_GT, 
#   SP_GT = SP_GT, 
#   SC_GCQ = SC_GCQ, 
#   SK_GT = SK_GT, 
#   CHIP_HWDC = CHIP_HWDC,  
#   SA_HWDC = SA_HWDC,       
#   SP_HWDC = SP_HWDC,       
#   SC_HWDC = SC_HWDC,      
#   CHIP_RHP = CHIP_RHP,  
#   SA_RHP = SA_RHP,       
#   SC_RHP = SC_RHP,       
#   SP_RHP = SP_RHP,  
#   SK_RHP = SK_RHP,  
#   CHIP_RPD = CHIP_RPD,  
#   SA_RPD = SA_RPD,       
#   SC_RPD = SC_RPD,       
#   SP_RPD = SP_RPD, 
#   SK_ST = SK_ST,  
#   SK_PRE = SK_PRE,       
#   SK_CC = SK_CC,       
#   SK_GI = SK_GI,
#   SK_TRANS = SK_TRANS,       
#   SK_COUNS = SK_COUNS,
#   .id = "program"
# )

# ggplot(GT, aes(x = year, y = Score, color = `Service Area`)) +
#   geom_jitter(aes(group = `Service Area`), width = 0.2, height = 0) +
#   facet_wrap(~ program, scales = "free") +
#   theme_minimal() +
#   labs(title = "Score Changes Over Time by Service Area and Program",
#        x = "Year",
#        y = "Score")




# This function will output year v.s year + 1 comparison

# analyze_diff <- function(dataset, end_year) {
#   for (year in 2019:end_year) {
#     if (year > 2019) {
#       previous_year <- year - 1
#       
#       # Construct column names for current and previous years
#       score_col_current <- paste0("score_", year)
#       score_col_previous <- paste0("score_", previous_year)
#       n_col_current <- paste0("n_", year)
#       n_col_previous <- paste0("n_", previous_year)
#       
#       # Difference columns
#       score_diff_col <- paste0("score_diff_", year, "_", previous_year)
#       n_diff_col <- paste0("n_diff_", year, "_", previous_year)
#       
#       # Rate columns
#       score_diff_rate_col <- paste0("diff_rate_score_", year, "_", previous_year)
#       n_diff_rate_col <- paste0("diff_rate_n_", year, "_", previous_year)
#       
#       # Calculating differences and rates
#       dataset[[score_diff_col]] <- dataset[[score_col_current]] - dataset[[score_col_previous]]
#       dataset[[n_diff_col]] <- dataset[[n_col_current]] - dataset[[n_col_previous]]
#       dataset[[score_diff_rate_col]] <- dataset[[score_diff_col]] / dataset[[score_col_previous]]
#       dataset[[n_diff_rate_col]] <- dataset[[n_diff_col]] / dataset[[n_col_previous]]
#     }
#   }
#   
#   return(dataset)
# }


consecutive_rate_change_subset <- function(dataset) {

  rate_diff_cols <- grep("diff_rate_score_", names(dataset), value = TRUE)
  

  check_consecutive_change <- function(row) {
    consecutive_increase <- 0
    consecutive_decrease <- 0
    
    for (col in rate_diff_cols) {
      rate_change <- row[col]
      
      if (!is.na(rate_change)) {
        if (rate_change > 0.10) {
          consecutive_increase <- consecutive_increase + 1
          consecutive_decrease <- 0  
        } else if (rate_change < -0.10) {
          consecutive_decrease <- consecutive_decrease + 1
          consecutive_increase <- 0 
        } else {
          consecutive_increase <- 0
          consecutive_decrease <- 0 
        }
        
        if (consecutive_increase >= 3) {
          return("Improve")
        } else if (consecutive_decrease >= 3) {
          return("Getting Worse")
        }
      }
    }
    
    return(NA)
  }
  
 
  dataset$ChangeMarker <- apply(dataset, 1, check_consecutive_change)
  
 
  selected_rows <- !is.na(dataset$ChangeMarker)
  return(dataset[selected_rows, ])
}


consecutively_better_worse <- consecutive_rate_change_subset(GCQ_SA_diff)


