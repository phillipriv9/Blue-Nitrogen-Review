# Read the CSV file into a data frame
All_combined <- read.csv("data/combined/AllCombined.csv")
# Assuming that All_combined has a "Habitat" column


# Keep the faceted scatter plot without marginal histograms
plot <- ggplot(All_combined, aes(x = OC_perc, y = N_perc, color = Habitat)) +
  geom_point(shape = 1, alpha = 0.3) +
  geom_smooth(aes(group = Habitat), method = "lm", se = FALSE) +
  labs(x = "Soil organic C (%)", y = "Soil N (%)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_wrap(~ Habitat)+
  scale_y_log10() + # Log-scale the y-axis
  scale_x_log10()

# Print the plot
print(plot)
