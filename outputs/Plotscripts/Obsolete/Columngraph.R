library (ggplot2)
library (dplyr)
library(stringr)

# Read the CSV file into a data frame
All_combined <- read.csv("combined/AllCombined.csv")

# Filter the data for only "marsh," "mangrove," and "mudflat" habitats
filtered_data <- All_combined %>%
  filter(Habitat %in% c("marsh", "mangrove", "mudflat"))

# Calculate error bar limits
error_limits <- filtered_data %>%
  group_by(Habitat) %>%
  summarise(y_upper = mean(CN) + sd(CN), y_lower = mean(CN) - sd(CN))

# Create a column graph with means and confidence intervals for CN colored by Habitat
ggplot(filtered_data, aes(x = str_to_title(Habitat), y = CN, fill = Habitat)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.75), width = 0.25) +
  labs(title = NULL,
       x = NULL,
       y = "Mean C:N ratio",
       fill = "Habitat") +  # Specify the legend title
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels = function(x) str_to_title(x)) +  # Capitalize x-axis labels
  scale_y_continuous(expand = c(0, 0)) +  # Make the x-axis cross at y = 0
  coord_cartesian(ylim = c(0, max(error_limits$y_upper) + 15)) +  # Set y-axis limits
  theme(axis.title.x = element_text(face = "bold", size = 12),  # Adjust x-axis title properties
        axis.title.y = element_text(face = "bold", size = 12),  # Adjust y-axis title properties
        legend.title = element_text(face = "bold", size = 12),  # Adjust legend title properties
        panel.grid.major = element_blank(),  # Eliminate major gridlines
        panel.grid.minor = element_blank())  # Eliminate minor gridlines

