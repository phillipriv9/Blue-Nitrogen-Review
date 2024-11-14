# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read your data (replace "your_data.csv" with your actual file name)
df1 <- read.csv("Scenarios/Globalscenarios2024.csv")

# Assuming your data is already in a suitable format, skip this step if unnecessary
df_filtered <- df1 %>%
  filter(year >= 1975) %>%  # Filter to include only years 1975 and later
  filter(scenario != "X") %>%  # Filter out scenario "X"
  filter(area.scenario != "Area stable")  # Filter out "Area stable"

# Clean up wetland.area if needed (remove commas and convert to numeric)
df_filtered$wetland.area <- as.numeric(gsub(",", "", df_filtered$wetland.area))

# Calculate NetN as Wetland Area * SLRmmyr
df_filtered$NetN <- df_filtered$wetland.area * df_filtered$SLRmmyr

# Define a color palette for scenarios
my_colors <- c("#1f78b4", "lightblue", "#33a02c", "lightgreen", "#e31a1c", "#ff7f00", "#6a3d9a")  # Adjust colors as needed

# Create scatterplot with all scenarios on one panel
ggplot(df_filtered, aes(x = wetland.area, y = SLRmmyr)) +
  geom_point(aes(color = scenario), size = 1) +  # Scatterplot points colored by scenario
  
  labs(title = "Relationship Between Wetland Area and SLRmmyr by Scenario",
       x = "Wetland Area",
       y = "SLRmmyr") +
  scale_color_manual(values = my_colors) +  # Apply the defined color palette for scatterplot
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(r = 20)),  # Adjust y-axis title margin
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.border = element_rect(color = "lightgray", fill = NA, size = 1),  # Add panel borders
        legend.position = "right") +  # Adjust legend position
  scale_x_continuous(breaks = seq(0, max(df_filtered$wetland.area), by = 100000), labels = scales::comma) +  # Adjust x-axis breaks and labels
  
  # Add filled contour based on NetN
  geom_tile(aes(fill = NetN), alpha = .1) +  # Filled contour based on NetN
  scale_fill_viridis_c(option = "plasma")  # Adjust color palette for the filled contour
