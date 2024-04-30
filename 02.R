star_adult_hwdc <- merged_df %>%
  filter(program == "STAR_Kids", measure == "Getting Needed Care", !is.na(MCO), MCO != 'Cigna-HealthSpring')

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
  filter(measure == "Getting Needed Care", program == "STAR_Kids") %>%
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
  geom_line(aes(group = MCO, alpha = 0.01), size = 1) +
  geom_line(data = state_level, aes(x = as.numeric(year), y = statelevel), color = "black",  size = 1.7) +
  geom_line(data = national_level_child_GNC, aes(x = year, y = score), color = "purple", linetype = "dashed", size = 1.7) +
  
  labs(title = "Getting Needed Care (GNC)\nState Level v.s AHRQ National Level in STAR Kids",
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
  geom_text(aes(x = 2023, y = 0.55, label = "AHRQ\nLevel"), 
            hjust = - 0.05, vjust = -0.1, color = 'purple')
