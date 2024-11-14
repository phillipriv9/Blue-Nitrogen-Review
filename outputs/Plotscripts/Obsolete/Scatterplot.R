# Read the CSV file into a data frame
All_combined <- read.csv("combined/AllCombined.csv")

# Calculate Ndensity
Ndensity <- 1000 * All_combined$N_perc / 100 * All_combined$BD_reported_g_cm3

# Create a scatter plot of Ndensity by centerdepth, colored by Habitat
scatterplot <- ggplot(All_combined, aes(x = centerdepth, y = Ndensity, color = Habitat)) +
  geom_point(shape = 1, alpha = 0.5) +
  labs(title = NULL,
       x = "Center Depth",
       y = "Ndensity (mg N/cm^3)") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  geom_smooth(aes(group = Habitat), method = "lm", se = FALSE) +
  theme(legend.position = "top-right") +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title  = element_text(size = 14))

print(scatterplot)
