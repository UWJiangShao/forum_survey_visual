# Extract data from 2019 to 2023 Survey measures
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(writexl)

# Read om data:
read_specific_sheets <- function() {
  years <- 2019:2023
  base_path <- "C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/data/deliverable dataset/"
  #base_path <- "/Users/tigershao/Dropbox (UFL)/Forum Slides Making Project/data/deliverable dataset/" # for Macbook
  
  #sheet_patterns <- c("^CHIP-", "^STAR Adult-", "^STAR Child-", "^STAR Kids-", "^STAR\\+PLUS-") # Take out CHIP Jan 30
  sheet_patterns <- c("^STAR Adult-", "^STAR Child-", "^STAR Kids-", "^STAR\\+PLUS-")
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
                   Score = as.numeric(Score)
                   # `Standard Error` = round(as.numeric(`Standard Error`), 4)
                   )%>%
            filter(MCO != "Back to all measures")
          
          measure <- gsub("-", "", sub(pattern, "", sheet))
          
          # Renaming columns with year suffix for 'Number of Respondents' and 'Score'
          respondents_colname <- paste0("n_", year)
          score_colname <- paste0("score_", year)
           # Add SE
          # se_colname <- paste0("se_", year)
          
          data <- data %>%
            rename(service_area = `Service Area`,
                   plancode = `Plan Code`,
                   !!respondents_colname := `Number of Respondents`,
                   !!score_colname := Score, 
                   # !!se_colname := `Standard Error`
                   ) %>%
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


# Combine each seperated datasets and row bind them to a long format (better for plotting)

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
  # colnames(dataset)[grep("se_", colnames(dataset))] <- "se"
  
  
  dataset$year <- year
  dataset$program <- program
  
  
  modified_datasets[[dataset_name]] <- dataset
}


all_dataset <- bind_rows(modified_datasets)

# all_dataset$var <- (all_dataset$se ^ 2) * all_dataset$n


# add the statelevel for each measure-program-year

# statelevel_means <- all_dataset %>%
#   group_by(measure, program, year) %>%
#   summarise(statelevel = sum((n * score), na.rm = T) / sum(n, na.rm = T), .groups = 'drop')

statelevel_means <- all_dataset %>%
  filter(!is.na(score)) %>%  
  group_by(measure, program, year) %>%
  summarise(statelevel = sum((n * score), na.rm = T) / sum(n, na.rm = T), .groups = 'drop')


all_dataset <- all_dataset %>%
  left_join(statelevel_means, by = c("measure", "program", "year"))




# Filter the MCO name using plancode.xlsx

excel_data <- readxl::read_excel("C:/Users/jiang.shao/Dropbox (UFL)/MCO Report Card - 2024/Program/2. Admin/Data/Raw_Data/plancode.xlsx")
excel_data <- readxl::read_excel("/Users/tigershao/Dropbox (UFL)/MCO Report Card - 2024/Program/2. Admin/Data/Raw_Data/plancode.xlsx")
excel_data <- excel_data %>% select(MCONAME, SERVICEAREA, PLANCODE)
merged_df <- merge(all_dataset, excel_data, by.x = "plancode", by.y = "PLANCODE", all.x = TRUE)
merged_df$MCO <- merged_df$MCONAME
merged_df$service_area <- merged_df$SERVICEAREA
merged_df$MCONAME <- NULL
merged_df$SERVICEAREA <- NULL


write_xlsx(merged_df, 'C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/data/temp/survey_all.xlsx')



#####################################################
#plotting 01
#####################################################

# Year * Score by MCO

star_adult_data <- merged_df %>%
  filter(program == "STAR_Adult")

avg_scores <- star_adult_data %>%
  group_by(MCO, measure, year) %>%
  summarise(avg_score = mean(score, na.rm = TRUE)) %>%
  ungroup()

# avg_scores <- avg_scores %>%
#   filter((!measure %in% c("Getting Treat", "Rate Health Plan", "Getting Information", "Prescriptions")))

avg_scores <- avg_scores %>%
  filter((!is.na(MCO)))

y_range <- range(avg_scores$avg_score, na.rm = TRUE)


ggplot(avg_scores, aes(x = as.numeric(year), y = avg_score, group = measure, color = measure)) +
  geom_line(size = 1.2) +
  facet_wrap(~ MCO, scales = "free_y", ncol = 5) + # One column of plots, each service area gets its own plot
  labs(title = "Average Score Change Over Years by MCO - STAR+Kids",
       x = "Year",
       y = "Average Score",
       color = "Measure") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 18)) +
  scale_y_continuous(limits = y_range)+
  geom_text(aes(label = round(avg_score, 2) * 100), vjust = -1, size = 3, check_overlap = TRUE)


#####################################################
#plotting 02
#####################################################

# Year * Score by Service Area

star_adult_data <- merged_df %>%
  filter(program == "STAR_Kids")

avg_scores <- star_adult_data %>%
  group_by(service_area, measure, year) %>%
  summarise(avg_score = mean(score, na.rm = TRUE)) %>%
  ungroup()

# avg_scores <- avg_scores %>%
#   filter((!measure %in% c("Getting Treat", "Rate Health Plan", "Getting Information", "Prescriptions")))

avg_scores <- avg_scores %>%
  filter((!is.na(service_area)))

y_range <- range(avg_scores$avg_score, na.rm = TRUE)


ggplot(avg_scores, aes(x = as.numeric(year), y = avg_score, group = measure, color = measure)) +
  geom_line(size = 1.2) +
  facet_wrap(~ service_area, scales = "free_y", ncol = 5) + # One column of plots, each service area gets its own plot
  labs(title = "Average Score Change Over Years by service area - STAR_Kids",
       x = "Year",
       y = "Average Score",
       color = "Measure") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 18)) +
  scale_y_continuous(limits = y_range)+
  geom_text(aes(label = round(avg_score, 2) * 100), vjust = -1, size = 3, check_overlap = TRUE)



#####################################################
#plotting
#####################################################

# Year * Score by Measures 

# # Filter the dataset for STAR Adult program and the HWDC measure
# star_adult_hwdc <- merged_df %>%
#   filter(program == "STAR_Adult", measure == "HWDC")
# 
# # Group by MCO and year, then calculate the average score
# avg_scores_mco <- star_adult_hwdc %>%
#   group_by(MCO, year) %>%
#   summarise(avg_score = mean(score, na.rm = TRUE)) %>%
#   ungroup()
# 
# 
# # Plot the data
# ggplot(avg_scores_mco, aes(x = as.numeric(year), y = avg_score, group = MCO, color = MCO)) +
#   geom_line(size = 1.1) +
#   labs(title = "HWDC Measure Trend Over Years by MCO in STAR Adult Program",
#        x = "Year",
#        y = "Average Score",
#        color = "MCO") +
#   theme_minimal() +
#   theme(legend.position = "bottom")

star_adult_hwdc <- merged_df %>%
  filter(program == "STAR_Child", measure == "HWDC", !is.na(MCO), MCO != 'Cigna-HealthSpring')

# Calculate the average score for each MCO
avg_scores_mco <- star_adult_hwdc %>%
  group_by(MCO, year) %>%
  summarise(avg_score = mean(score, na.rm = TRUE)) %>%
  ungroup()

# Calculate the state-level average score
# state_level <- merged_df %>%
#   filter(measure == "HWDC",program == "STAR_Adult") %>%
#   group_by(year) %>%
#   summarise(statelevel = mean(score, na.rm = TRUE)) %>%
#   ungroup()

state_level <- merged_df %>%
  filter(measure == "HWDC", program == "STAR_Child") %>%
  select(year, statelevel) %>%
  distinct() %>%
  arrange(year)

# Combine MCO scores with state-level scores
combined_data <- avg_scores_mco %>%
  left_join(state_level, by = "year") %>%
  mutate(MCO = as.factor(MCO))



national_level_adult_HWDC <- data.frame(
  year = 2019:2023,
  score = c(0.75, 0.77, 0.765, 0.76, 0.75)
)

national_level_child_HWDC <- data.frame(
  year = 2019:2023,
  score = c(0.79, 0.81, 0.807, 0.80, 0.77)
)

national_level_adult_GCQ <- data.frame(
  year = 2019:2023,
  score = c(0.60, 0.59, 0.59, 0.56, 0.54)
)

national_level_child_GCQ <- data.frame(
  year = 2019:2023,
  score = c(0.73, 0.73, 0.73, 0.70, 0.67)
)

national_level_adult_GNC <- data.frame(
  year = 2019:2023,
  score = c(0.56, 0.55, 0.56, 0.52, 0.50)
)

national_level_child_GNC <- data.frame(
  year = 2019:2023,
  score = c(0.61, 0.61, 0.63, 0.60, 0.56)
)


# 
# national_level_adult_GCQ <- data.frame(
#   year = 2019:2023,
#   score = c(0.75, 0.77, 0.765, 0.76, 0.75)
# )

y_range <- range(c(avg_scores_mco$avg_score, state_level$statelevel, national_level_adult_GNC$score), na.rm = TRUE)

last_point <- tail(state_level, 1)

# Plotting
ggplot(combined_data, aes(x = as.numeric(year), y = avg_score, color = MCO)) +
  
  geom_line(data = state_level, aes(x = as.numeric(year), y = statelevel), color = "black",  size = 1.7) +
  geom_line(data = national_level_child_HWDC, aes(x = year, y = score), color = "purple", linetype = "dashed", size = 1.7) +
  geom_line(aes(group = MCO, alpha = 0.1), size = 1) +
  labs(title = "How well Doctor Communicate (HWDC)\nState Level v.s AHRQ National Level in STAR Child",
       x = NULL,
       y = "Score",
       color = "MCO in Texas State") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = 1),
        strip.placement = "outside",
        panel.spacing = unit(2, "lines"),
        strip.text = element_text(size = 14),
        axis.text.x.bottom  = element_text(size = rel(1.5)),
        axis.text.x.top = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),            
        axis.title.x = element_text(size = rel(1.5)),          
        axis.title.y = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))+
  scale_y_continuous(limits = c(0.5, 1)) + 
  geom_text(aes(x = 2023, y = last_point$statelevel, label = "State\nLevel"), 
            hjust = - 0.05, vjust = -0.1, color = 'black')+ 
  geom_text(aes(x = 2023, y = 0.73, label = "AHRQ\nLevel"), 
            hjust = - 0.05, vjust = -0.1, color = 'purple')



#####################################################
#plotting
#####################################################

# Year * Score by Program 

star_adult_hwdc <- merged_df %>%
  filter(measure == "Getting Care Quickly", !is.na(MCO), MCO != 'Cigna-HealthSpring', program == 'STAR_Child' | program == 'STAR_Kids')

# Calculate the average score for each MCO
avg_scores_mco <- star_adult_hwdc %>%
  group_by(program, year) %>%
  summarise(avg_score = mean(score, na.rm = TRUE)) %>%
  ungroup()

# Calculate the state-level average score # program level dont need statelevel
# state_level <- merged_df %>%
#   filter(measure == "Getting Needed Care") %>%
#   group_by(year) %>%
#   summarise(statelevel = mean(score, na.rm = TRUE)) %>%
#   ungroup()


# # Combine MCO scores with state-level scores
# combined_data <- avg_scores_mco %>%
#   left_join(state_level, by = "year")


national_level_adult_HWDC <- data.frame(
  year = 2019:2023,
  score = c(0.75, 0.77, 0.765, 0.76, 0.75)
)

national_level_child_HWDC <- data.frame(
  year = 2019:2023,
  score = c(0.79, 0.81, 0.807, 0.80, 0.77)
)

national_level_adult_GCQ <- data.frame(
  year = 2019:2023,
  score = c(0.60, 0.59, 0.59, 0.56, 0.54)
)

national_level_child_GCQ <- data.frame(
  year = 2019:2023,
  score = c(0.73, 0.73, 0.73, 0.70, 0.67)
)

national_level_adult_GNC <- data.frame(
  year = 2019:2023,
  score = c(0.56, 0.55, 0.56, 0.52, 0.50)
)

national_level_child_GNC <- data.frame(
  year = 2019:2023,
  score = c(0.61, 0.61, 0.63, 0.60, 0.56)
)


# 
national_level_adult_GCQ <- data.frame(
  year = 2019:2023,
  score = c(0.75, 0.77, 0.765, 0.76, 0.75)
)

y_range <- range(c(avg_scores_mco$avg_score, state_level$statelevel, national_level_adult_GNC$score), na.rm = TRUE)

# Plotting
ggplot(avg_scores_mco, aes(x = as.numeric(year), y = avg_score, color = program)) +
  geom_line(aes(group = program), size = 1.5) +
  geom_line(data = national_level_child_GCQ, aes(x = year, y = score), color = "purple", linetype = "dashed", size = 1) +
  facet_wrap(~ program, scales = "free_y", ncol = 4) +
  labs(title = "Getting Care Quickly\nState Level v.s AHRQ National Level\nChild Medicaid",
       x = NULL,
       y = "Score",
       color = "Program") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = 1),
        strip.placement = "outside",
        panel.spacing = unit(2, "lines"),
        strip.text = element_text(size = 14),
        axis.text.x.bottom  = element_text(size = rel(1.5)),
        axis.text.x.top = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),            
        axis.title.x = element_text(size = rel(1.5)),          
        axis.title.y = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))+
  scale_y_continuous(limits = c(0, 1))+
  geom_text(aes(label = round(avg_score, 2)), vjust = -1, size = 5, check_overlap = TRUE)






#####################################################
#plotting
#####################################################

# Variance distribution by measures
# ggplot(merged_df, aes(x = var)) + 
#   geom_histogram(bins = 30, fill = "blue", color = "black") + # Adjust bin size as needed
#   facet_wrap(~ measure, scales = "free_x") + # Use 'scales = "free_x"' to have separate x scales
#   theme_minimal() + 
#   labs(x = "Variance", y = "Frequency", title = "Distribution of Variances by Measure") +
#   theme(axis.text.x = element_text(hjust = 1), 
#         strip.text = element_text(size = 13))


ggplot(merged_df, aes(x = var, fill = year)) + 
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) + # Adjust bin size as needed
  facet_wrap(~ measure, scales = "free_x") + # Use 'scales = "free_x"' to have separate x scales
  theme_minimal() + 
  labs(x = "Variance", y = "Frequency", title = "Distribution of Variances from 2020 to 2023") +
  theme(axis.text.x = element_text(hjust = 1), # Rotate x labels if they overlap
        legend.position = "bottom", 
        strip.text = element_text(size = 13)) + # Adjust legend position as needed
  scale_fill_viridis_d()




#####################################################
#data manipulating, select top and bottom 
#####################################################
top_mcos <- merged_df %>%
  filter(year == 2023) %>%  
  group_by(service_area) %>%         
  summarize(avg_score = mean(score, na.rm = TRUE)) %>%  
  arrange(avg_score) %>%  
  slice_head(n = 3)             

top_measures <- merged_df %>%
  filter(year == 2023) %>%
  group_by(MCO, program) %>%  
  summarize(avg_score = mean(score, na.rm = TRUE), .groups = 'drop') %>%
  ungroup() %>%
  arrange(avg_score) %>%
  slice_head(n = 4)



#####################################################
# read in complaint data for 21, 22, 23
#####################################################
complaint_21 <- read_excel("/Users/tigershao/Dropbox (UFL)/Forum Slides Making Project/data/complaint data/complaint_data_all.xlsx", sheet = "2021")
complaint_22 <- read_excel("/Users/tigershao/Dropbox (UFL)/Forum Slides Making Project/data/complaint data/complaint_data_all.xlsx", sheet = "2022")
complaint_23 <- read_excel("/Users/tigershao/Dropbox (UFL)/Forum Slides Making Project/data/complaint data/complaint_data_all.xlsx", sheet = "2023")

complaint_21 <- read_excel("C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/data/complaint data/complaint_data_all.xlsx", sheet = "2021")
complaint_22 <- read_excel("C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/data/complaint data/complaint_data_all.xlsx", sheet = "2022")
complaint_23 <- read_excel("C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/data/complaint data/complaint_data_all.xlsx", sheet = "2023")

complaint_all <- rbind(complaint_21, complaint_22, complaint_23)
complaint_all$complaint_score <- as.numeric(complaint_all$complaint_score)
complaint_all$complaints <- as.numeric(complaint_all$complaints)

ggplot(complaint_all, aes(x = complaint_score, fill = program)) + 
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) + 
  facet_wrap(~ year, scales = 'free_y') + 
  theme_minimal() + 
  labs(title = "Complaint Score Distribution by Year",
       x = "Complaint Score",
       y = "Frequency") + 
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill=NA, size=1),  
        panel.grid.major = element_blank()) 



ggplot(complaint_all, aes(x = interaction(year, program), y = complaint_score, fill = program)) + 
  geom_boxplot(width = 0.6) + 
  geom_vline(xintercept = c(3.5, 6.5, 9.5), color = "black", size = 1) +
  labs(x = "Year and Program", 
       y = "Complaint Score", 
       title = "Complaint Scores by Year and Program") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1, angle = 45))


#####################################################
# calculate the real variance
#####################################################
variance_data <- aggregate(score ~ program + measure + year, data=merged_df, FUN = var)

variance_data_test <- merged_df %>%
  group_by(program, measure, year) %>%
  summarise(variance = var(score, na.rm = TRUE))

# Heat map
ggplot(variance_data_test, aes(x = program , y = measure, fill = variance)) +
  geom_tile() + 
  theme_minimal() + 
  labs(fill = "Variance", 
       x = "Program", 
       y = "Measure", 
       title = "Variance of Scores by Program and Measure") + 
  facet_wrap(~year, ncol = 1)


# Line chart
ggplot(variance_data_test, aes(x = year, y = variance, color = program, group = program)) +
  geom_line(size = 1.3)+
  theme_minimal() + 
  labs(color = "Program", 
       x = "Year", 
       y = "Variance", 
       title = "Variance of Scores Over Time by Program and Measure") + 
  facet_wrap(~measure) + 
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 20), 
        panel.spacing = unit(2, "lines"), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ 
  scale_y_continuous(limits = c(0, 0.009)) 



#####################################################
# redo the dataset - 2023 complaint * all measures
#####################################################

merged_df_2023 <- merged_df[merged_df$year == 2023,]
complaint_all_2023 <- complaint_all[complaint_all$year == 2023,]
names(complaint_all_2023)[names(complaint_all_2023) == "plan_code"] <- "plancode"

complaint_all_2023$program <- gsub("SA", "STAR_Adult", complaint_all_2023$program)
complaint_all_2023$program <- gsub("SC", "STAR_Child", complaint_all_2023$program)
complaint_all_2023$program <- gsub("SK", "STAR_Kids", complaint_all_2023$program)
complaint_all_2023$program <- gsub("SP", "STAR_PLUS", complaint_all_2023$program)

measures <- unique(merged_df_2023$measure)

merged_df_2023_spread <- merged_df_2023 %>% 
  select(MCO, service_area, plancode, measure, score, program) %>% 
  spread(key = measure, value = score)

# colnames(merged_df_2023_spread)[5:(4+length(measures))] <- paste0("Measure", 1:length(measures), "_score")

complaint_measure_2023 <- merge(merged_df_2023_spread, complaint_all_2023[, c("MCO", "service_area", "plancode", "complaint_score", "program", "complaints")], 
                       by = c("MCO", "service_area", "plancode", "program"), all = TRUE)

final_dataset[is.na(final_dataset)] <- 0

measures <- names(final_dataset)[4:(ncol(final_dataset)-1)]  

correlation_data <- final_dataset %>%
  summarize(across(all_of(measures), ~cor(.x, complaint_score, use="complete.obs"), .groups = 'drop'))
