# Load necessary libraries
library(tidyverse)
library(ggExtra)
library(broom)

# Read the CSV file into a data frame
combined_data <- read.csv("combined/AllCombined.csv")

# Filter data for "Marsh" and "Mangrove" habitats and CN > 5
filtered_data <- combined_data %>%
  filter(Habitat %in% c("marsh", "mangrove"),
         CN > 5, CN < 50)  #filter out some wacky low and CN data


# Create a scatter plot with open circles and color by Habitat
plot <- ggplot(filtered_data, aes(x = OC_perc, y = CN, color = Habitat)) +
  geom_point(shape = 1, alpha = 0.5) +  # Use shape = 1 for open circle and set transparency
  labs(title = NULL,
       x = "Soil [Organic C] (%)",
       y = "Soil [N] (%)") +
  theme_minimal() +  # Adjusted to remove the gray background
  theme(panel.grid = element_blank()) +
  geom_smooth(aes(group = Habitat), method = "lm", se = FALSE) +  # Add linear best fit lines
  theme(legend.position = "bottom") +  # Move the legend to the bottom
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))  # Adjust legend title position

# Define custom colors for habitats
#custom_colors <- c("marsh" = "lightgreen", "mangrove" = "coral")


# Create marginal histograms with white fill
histo <- ggMarginal(plot, type = "histogram", bins = 30, fill = "lightsteelblue", line = "darkgray")

# Fit linear models
lm_equations <- filtered_data %>%
  group_by(Habitat) %>%
  do(broom::tidy(lm(N_perc ~ OC_perc, data = .)))

# Print the equations
print(lm_equations)
print(histo)
