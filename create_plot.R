
create_plot_test <- function(data_set, filter_criteria, national_data, national_label, title, color_label, rate_column, y_low, y_high) {
  
  # data process - modify if need to change criteria
  filtered_data <- data_set %>%
    filter(MeasureNameAbbreviation == filter_criteria$MeasureNameAbbreviation,
           SubmeasureName == filter_criteria$SubmeasureName,
           grepl(filter_criteria$PopulationNamePattern, PopulationName))
  
  state_level <- data_set %>%
    filter(MeasureNameAbbreviation == filter_criteria$MeasureNameAbbreviation,
           SubmeasureName == filter_criteria$SubmeasureName,
           PopulationName == 'ALL')
  

  # plotting part
  p <- ggplot(filtered_data, aes(x = year, y = rate, color = PopulationName)) +
    
    # National line
    geom_line(data = national_data, aes(x = year, y = get(rate_column)), color = "orange", linetype = "dashed", size = 1.8) +
    
    # State Average Line
    geom_line(data = state_level, aes(x = year, y = rate), color = "black", size = 1.6) +
    
    geom_line(aes(group = PopulationName), alpha = 0.3, size = 1) +
    labs(title = NULL,
         x = NULL,
         y = "Rate",
         color = color_label) +
    
    # Legend, title, font part, adjust if need
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          plot.background = element_rect(fill = "white", color = NA),
          
          legend.position = "none",
          strip.placement = "outside",
          panel.spacing = unit(2, "lines"),
          strip.text = element_text(size = 14),
          axis.text.x.bottom = element_text(size = rel(3.5)),
          axis.text.x.top = element_text(size = rel(3.5)),
          axis.text.y = element_text(size = rel(3.5)),            
          axis.title.x = element_text(size = rel(3.5)),          
          axis.title.y = element_text(size = rel(3.5)),
          plot.title = element_text(size = rel(3.5))) +
    
    # # National level label
    # geom_text(aes(x = 2022, y = national_data[[rate_column]][national_data$year == 2022], label = national_label),
    #           hjust = 1, vjust = 1.1, color = 'orange') + 
    # 
    # # State level label
    # geom_text(aes(x = 2022, y = state_level$rate[state_level$year == 2022], label = 'State Overall'),
    #           hjust = 1, vjust = 1.1, color = 'black') + 
    # 
    # Y axis range
    scale_y_continuous(limits = c(y_low, y_high)) + 
    
    # Adjust margin to show label
    theme(plot.margin = margin(t = 1, r = 30, b = 1, l = 1, unit = "mm"))
  
  # Output PNG part
  filename <- gsub("[[:punct:] ]|\\n", "_", title)
  output_dir <- "C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/Output/Plot_Rate_Year_admin/"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  path <- paste0(output_dir, filename, ".png")
  
  ggsave(path, plot = p, width = 16, height = 8.5, dpi = 300)
  
  return(p)
}




#################################W30 ################################

create_plot_test_w30 <- function(data_set, filter_criteria, national_data, national_label, title, color_label, rate_column, y_low, y_high) {
  
  # data process - modify if need to change criteria
  filtered_data <- data_set %>%
    filter(MeasureNameAbbreviation == filter_criteria$MeasureNameAbbreviation,
           SubmeasureName == filter_criteria$SubmeasureName,
           grepl(filter_criteria$PopulationNamePattern, PopulationName))
  
  state_level <- data_set %>%
    filter(MeasureNameAbbreviation == filter_criteria$MeasureNameAbbreviation,
           SubmeasureName == filter_criteria$SubmeasureName,
           PopulationName == 'ALL')
  
  
  # plotting part
  p <- ggplot(filtered_data, aes(x = year, y = rate, color = PopulationName)) +
    
    # National line
    geom_line(data = national_data, aes(x = year, y = get(rate_column)), color = "orange", linetype = "dashed", size = 1.8) +
    
    # State Average Line
    geom_line(data = state_level, aes(x = year, y = rate), color = "black", size = 1.6) +
    
    geom_line(aes(group = PopulationName), alpha = 0.3, size = 1) +
    
    labs(title = NULL,
         x = NULL,
         y = "Rate",
         color = color_label) +
    
    # Legend, title, font part, adjust if need
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          plot.background = element_rect(fill = "white", color = NA),
          
          legend.position = "none",
          strip.placement = "outside",
          panel.spacing = unit(2, "lines"),
          strip.text = element_text(size = 14),
          axis.text.x.bottom = element_text(size = rel(3.5)),
          axis.text.x.top = element_text(size = rel(3.5)),
          axis.text.y = element_text(size = rel(3.5)),            
          axis.title.x = element_text(size = rel(3.5)),          
          axis.title.y = element_text(size = rel(3.5)),
          plot.title = element_text(size = rel(3.5))) +
    
    # # National level label
    # geom_text(aes(x = 2022, y = national_data[[rate_column]][national_data$year == 2022], label = national_label),
    #           hjust = -0.1, vjust = 0.15, color = 'orange') + 
    # 
    # # State level label
    # geom_text(aes(x = 2022, y = state_level$rate[state_level$year == 2022], label = 'State\nOverall'),
    #           hjust = -0.1, vjust = 0.15, color = 'black') + 
    # 
    # Y axis range
    scale_y_continuous(limits = c(y_low, y_high)) + 
    
    scale_x_continuous(breaks = c(2020, 2021, 2022))
  
  # Output PNG part
  filename <- gsub("[[:punct:] ]|\\n", "_", title)
  output_dir <- "C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/Output/Plot_Rate_Year_admin/"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  path <- paste0(output_dir, filename, ".png")
  
  ggsave(path, plot = p, width = 16, height = 8.5, dpi = 300)
  
  return(p)
}









######################### Another set of line - AAP ###################################

# create_plot_test_2 <- function(data_set1, data_set2, filter_criteria, national_data, national_label, title, color_label, rate_column, y_low, y_high) {
#   
#   # data process - modify if need to change criteria
#   filtered_data1 <- data_set1 %>%
#     filter(MeasureNameAbbreviation == filter_criteria$MeasureNameAbbreviation,
#            SubmeasureName == filter_criteria$SubmeasureName,
#            grepl("^STS_", PopulationName))
#   
#   filtered_data1$program <- "STAR Adult"
#   
#   state_level1 <- data_set1 %>%
#     filter(MeasureNameAbbreviation == filter_criteria$MeasureNameAbbreviation,
#            SubmeasureName == filter_criteria$SubmeasureName,
#            PopulationName == 'ALL')
#   
#   filtered_data2 <- data_set2 %>%
#     filter(MeasureNameAbbreviation == filter_criteria$MeasureNameAbbreviation,
#            SubmeasureName == filter_criteria$SubmeasureName,
#            grepl("^SPS_", PopulationName))
#   
#   filtered_data2$program <- "STAR+PLUS"
#   
#   state_level2 <- data_set2 %>%
#     filter(MeasureNameAbbreviation == filter_criteria$MeasureNameAbbreviation,
#            SubmeasureName == filter_criteria$SubmeasureName,
#            PopulationName == 'ALL')
#   
#   filtered_data <- rbind(filtered_data1, filtered_data2)
#   
# 
#   # plotting part
#   p <- ggplot(test3, aes(x = year, y = rate, color = program)) +
#     
#     # National line
#     geom_line(data = national_data, aes(x = year, y = get(rate_column)), color = "orange", linetype = "dashed", size = 1.7) +
#     
#     # State Average Line 1
#     geom_line(data = state_level1, aes(x = year, y = rate), color = "red", size = 1.4) +
#     
#     # State Average Line 2
#     geom_line(data = state_level2, aes(x = year, y = rate), color = "blue", size = 1.4) +
#     
#     
#     geom_line(aes(group = PopulationName), alpha = 0.3, size = 1) +
#     
#     scale_color_manual(values = c("STAR Adult" = "red", "STAR+PLUS" = "blue")) +
#     
#     labs(title = NULL,
#          x = NULL,
#          y = "Rate",
#          color = color_label) +
#     
#     # Legend, title, font part, adjust if need
#     theme_minimal() +
#     theme(panel.background = element_rect(fill = "white"),
#           panel.border = element_rect(color = "black", fill = NA, size = 1),
#           # plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
#           plot.background = element_rect(fill = "white", color = NA),
#   
#           legend.position = "none",
#           strip.placement = "outside",
#           panel.spacing = unit(2, "lines"),
#           strip.text = element_text(size = 14),
#           axis.text.x.bottom = element_text(size = rel(3.5)),
#           axis.text.x.top = element_text(size = rel(3.5)),
#           axis.text.y = element_text(size = rel(3.5)),            
#           axis.title.x = element_text(size = rel(3.5)),          
#           axis.title.y = element_text(size = rel(3.5)),
#           plot.title = element_text(size = rel(3.5))) + 
#     scale_y_continuous(limits = c(y_low, y_high))
# 
#   
#   # Output PNG part
#   filename <- gsub("[[:punct:] ]|\\n", "_", title)
#   output_dir <- "C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/Output/Plot_Rate_Year_admin/"
#   if (!dir.exists(output_dir)) {
#     dir.create(output_dir, recursive = TRUE)
#   }
#   path <- paste0(output_dir, filename, ".png")
#   
#   ggsave(path, plot = p, width = 16, height = 8.5, dpi = 300)
#   
#   return(p)
# }

create_plot_test_2 <- function(data_set1, data_set2, filter_criteria, national_data, national_label, title, color_label, rate_column, y_low, y_high) {
  library(ggplot2)
  library(dplyr)
  
  # Process data
  filtered_data1 <- data_set1 %>%
    filter(MeasureNameAbbreviation == filter_criteria$MeasureNameAbbreviation,
           SubmeasureName == filter_criteria$SubmeasureName,
           grepl("^STS_", PopulationName))
  filtered_data1$program <- "STAR Adult"
  
  filtered_data2 <- data_set2 %>%
    filter(MeasureNameAbbreviation == filter_criteria$MeasureNameAbbreviation,
           SubmeasureName == filter_criteria$SubmeasureName,
           grepl("^SKS_", PopulationName))
  filtered_data2$program <- "STAR+PLUS"
  
  filtered_data <- rbind(filtered_data1, filtered_data2)
  
  #state level
    state_level1 <- data_set1 %>%
      filter(MeasureNameAbbreviation == filter_criteria$MeasureNameAbbreviation,
             SubmeasureName == filter_criteria$SubmeasureName,
             PopulationName == 'ALL')
    
      state_level2 <- data_set2 %>%
        filter(MeasureNameAbbreviation == filter_criteria$MeasureNameAbbreviation,
               SubmeasureName == filter_criteria$SubmeasureName,
               PopulationName == 'ALL')
  
  # Plotting
  p <- ggplot(filtered_data, aes(x = year, y = rate, color = program)) +
    geom_line(aes(group = PopulationName), alpha = 0.2, size = 0.8) +
    
    geom_line(data = national_data, aes(x = year, y = get(rate_column)), color = "#FF8000", linetype = "dashed", size = 2.1) +
    
    scale_color_manual(values = c("STAR Adult" = "#FF3333", "STAR+PLUS" = "#3399FF")) +
    
        # State Average Line 1
        geom_line(data = state_level1, aes(x = year, y = rate), color = "#FF3333", size = 1.8) +

        # State Average Line 2
        geom_line(data = state_level2, aes(x = year, y = rate), color = "#3399FF", size = 1.8) +
    
    
    # geom_text(aes(x = 2022, y = 85, label = 'State Average\nSTAR+PLUS'), color = '#3399FF', size = 6, hjust = 0.7) +
    # 
    # geom_text(aes(x = 2022, y = 60, label = 'State Average\nSTAR Adult'), color = '#FF3333', size = 6, hjust = 0.7) +
    # 
    # geom_text(aes(x = 2022, y = 70, label = 'NCQA\n50th Percentile'), color = '#FF8000', size = 6, hjust = 0.7)+
    # 


    
    labs(title = NULL, 
         x = NULL, 
         y = "Rate", 
         color = color_label) +
    theme_minimal() +
    
      theme(panel.background = element_rect(fill = "white"),
            panel.border = element_rect(color = "black", fill = NA, size = 1),
            plot.background = element_rect(fill = "white", color = NA),
            # plot.margin = margin(t = 10, r = 50, b = 10, l = 10, unit = "mm"),
            axis.text.x = element_text(hjust = 1),

            legend.position = "none",
            strip.placement = "outside",
            panel.spacing = unit(2, "lines"),
            strip.text = element_text(size = 14),
            axis.text.x.bottom = element_text(size = rel(3.5)),
            axis.text.x.top = element_text(size = rel(3.5)),
            axis.text.y = element_text(size = rel(3.5)),
            axis.title.x = element_text(size = rel(3.5)),
            axis.title.y = element_text(size = rel(3.5)),
            plot.title = element_text(size = rel(3.5))) +
     # scale_x_continuous(breaks = c(2020, 2021, 2022)) +
    scale_y_continuous(limits = c(y_low, y_high)) 
  
  # Output PNG part
  filename <- gsub("[[:punct:] ]|\\n", "_", title)
  output_dir <- "C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/Output/Plot_Rate_Year_admin/"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  path <- paste0(output_dir, filename, ".png")
  
  ggsave(path, plot = p, width = 16, height = 9, dpi = 600)
  
  return(p)
}



