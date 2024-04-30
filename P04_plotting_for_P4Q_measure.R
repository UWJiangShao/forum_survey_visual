library(ggplot2)

source("C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/Program/P01_clean_data_admin.R")

# 1. STAR - CHL, EED, PPC-Pre, PPC-Post
star_CHL <- hedis_star_all %>%
  filter(MeasureNameAbbreviation == 'CHL', 
         PopulationName == 'ALL', 
         SubmeasureName == 'Total') 

star_CHL_nb <- national_level_all %>%
  filter(MeasureNameAbbreviation == 'CHL', 
         str_detect(MeasureName, "Total")) 


p <- ggplot(star_CHL, aes(x = as.numeric(year), y = rate)) +
  
  geom_line(data = star_CHL_nb, aes(x = year, y = X25thPercentile), color = "red", linetype = "dashed", size = 1.8) +
  geom_line(data = star_CHL_nb, aes(x = year, y = X50thPercentile), color = "blue", linetype = "dashed", size = 1.8) +
  geom_line(data = star_CHL, size = 1.8) +
  
  labs(title = NULL,
       x = NULL,
       y = "Rate") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.background = element_rect(fill = "white", color = NA),
    
        legend.position = "none",
        axis.text.x = element_text(hjust = 1),
        strip.placement = "outside",
        panel.spacing = unit(2, "lines"),
        strip.text = element_text(size = 14),
        axis.text.x.bottom  = element_text(size = rel(3.5)),
        axis.text.x.top = element_text(size = rel(3.5)),
        axis.text.y = element_text(size = rel(3.5)),            
        axis.title.x = element_text(size = rel(3.5)),          
        axis.title.y = element_text(size = rel(3.5)),
        plot.title = element_text(size = rel(2)))
        # geom_text(aes(x = 2022, y = star_CHL_nb$X25thPercentile[star_CHL_nb$year == 2022]),
        # hjust = -0.1, vjust = 0.15, color = 'red') + 
        # geom_text(aes(x = 2022, y = star_CHL_nb$X50thPercentile[star_CHL_nb$year == 2022]),
        # hjust = -0.1, vjust = 0.15, color = 'blue') +
        # geom_text(aes(x = 2022, y = star_CHL$rate[star_CHL$year == 2022]),
        # hjust = -0.1, vjust = 0.15, color = 'black') 

filename <- gsub("[[:punct:] ]|\\n", "_", "CHL-STAR\nTrend from MY2018 to MY2022")
output_dir <- "C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/Output/Plot_P4Q_Rate_Year_admin/"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
path <- paste0(output_dir, filename, ".png")

ggsave(path, plot = p, width = 16, height = 8.5, dpi = 300)


################################################################################


star_EED_1 <- hedis_star_all %>%
  filter(MeasureNameAbbreviation == 'CDC', 
         PopulationName == 'ALL', 
         str_detect(SubmeasureName, "Eye Exam")) 

star_EED_2 <- hedis_star_all %>%
  filter(MeasureNameAbbreviation == 'EED', 
         PopulationName == 'ALL') 

star_EED <- rbind(star_EED_1, star_EED_2)
