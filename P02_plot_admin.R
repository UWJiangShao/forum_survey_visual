library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(writexl)

source("C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/Program/P01_clean_data_admin.R")

# Plot - 1 - Plan A. AAP from 2018 to 2022, STAR+PLUS, lines of Overall v.s. service area

plot1_data_planA <- hedis_star_all %>%
  filter(MeasureNameAbbreviation == 'AAP' , 
         SubmeasureName == 'All members',
         grepl("^SPS_", PopulationName))  

# National Level foor Plot 1 - AAP - All members
plot1_nb <- national_level_all %>%
  filter(MeasureNameAbbreviation == 'AAP', 
         str_detect(MeasureName, "Total"))  


ggplot(plot1_data_planA, aes(x = as.numeric(year), y = rate, color = PopulationName1)) +
  
  geom_line(data = plot1_nb, aes(x = year, y = AverageRate), color = "purple", linetype = "dashed", size = 1.7) +
  geom_line(aes(group = PopulationName1), alpha = 0.3,  size = 1) +
  
  labs(title = "Adults Access (AAP) - All members\nTrend from MY2018 to MY2022 - Service Area v.s National Average",
       x = NULL,
       y = "Rate",
       color = "Service Area") +
  theme_minimal() +
  theme(legend.position = "right",
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
        geom_text(aes(x = 2022, y = plot1_nb$AverageRate[plot1_nb$year == 2022], label = "HEDIS\nNational\nLevel"),
                  hjust = - 0.05, vjust = -0.1, color = 'purple')


# Plot - 1 - Plan B. AAP from 2018 to 2022, STAR+PLUS, lines of 50 Percentile v.s. all MCO

plot1_data_planb <- hedis_sp_all %>%
  filter(MeasureNameAbbreviation == 'AAP' , 
         SubmeasureName == 'All members',
         grepl("^SPM_", PopulationName))  


ggplot(plot1_data_planb, aes(x = as.numeric(year), y = rate, color = PopulationName1)) +
  
  geom_line(data = plot1_nb, aes(x = year, y = AverageRate), color = "purple", linetype = "dashed", size = 1.7) +
  geom_line(aes(group = PopulationName1), alpha = 0.3,  size = 1) +
  
  labs(title = "Adults Access (AAP) - All members\nTrend from MY2018 to MY2022\nMCO v.s National 50th Percentile",
       x = NULL,
       y = "Rate",
       color = "MCO") +
  theme_minimal() +
  theme(legend.position = "right",
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
  geom_text(aes(x = 2022, y = plot1_nb$AverageRate[plot1_nb$year == 2022], label = "HEDIS\nNational\nLevel"),
            hjust = - 0.05, vjust = -0.1, color = 'purple')


