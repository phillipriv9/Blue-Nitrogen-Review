# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read your data (replace "your_data.csv" with your actual file name)
df1 <- read.csv("Scenarios/Globalscenarios2024.csv")

# Assuming your data is already in a suitable format, skip this step if unnecessary
df_long <- df1 %>%
  pivot_longer(cols = c(totalgain, totalloss, NetN),
               names_to = "Response",
               values_to = "Value") %>%
  filter(year >= 1975)  # Filter to include only years 1975 and later

# Filter out scenario "X" and "Area stable"
df_filtered <- df_long %>%
  filter(scenario != "X") %>%
  filter(area.scenario != "Area stable")

# Group by "scenario"
df_grouped <- df_filtered %>%
  group_by(scenario, year, Response) %>%
  summarise(MeanValue = mean(Value, na.rm = TRUE))

# Define a color palette for paired colors
my_colors <- c("#33a02c", "#b2df8a",  # Green shades
               "#e31a1c", "#fb9a99",  # Red shades
               "#1f78b4", "#a6cee3",  # Blue shades
               "#ff7f00", "#fdbf6f",  # Orange shades
               "#6a3d9a", "#cab2d6",  # Purple shades
               "#b15928", "#fccde5")  # Brown/pink shades

# Create scatterplot with facets for each Response and within each Response, facets for each scenario
ggplot(df_grouped, aes(x = year, y = MeanValue, color = scenario)) +
  geom_line(aes(group = scenario), size = 0.5) +  # Connect points with lines
  geom_point(size = 1) +  # Smaller symbols
  facet_grid(Response ~ ., scales = "free_y") +  # Facet vertically by Response
  labs(title = NULL,
       x = "Year",
       y = NULL) +  # No y-axis label on left side
  
  scale_color_manual(values = my_colors) +  # Apply the defined color palette
  theme_minimal() +
  theme(axis.title.y = element_blank(),  # Remove y-axis title
        strip.placement = "outside",  # Move facet labels (headings) to the left side
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.spacing.x = unit(1, "lines"),  # Increase spacing between panels
        axis.title.y.right = element_text(margin = margin(l = 20)),  # Adjust right y-axis title margin
        axis.line.y.right = element_line(color = "gray"),  # Add a line for the right y-axis
        axis.line.y = element_blank(),  # Remove left y-axis line
        axis.ticks.y = element_line(color = "gray"),  # Add tick marks to right y-axis
        panel.border = element_rect(color = "lightgray", fill = NA, size = 1)) +  # Add panel borders
  
  # Add horizontal lines at y = 0 in each facet
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  
  # Move y-axis to the right side
  scale_y_continuous(sec.axis = dup_axis()) +
  
  # Adjust legend position
  theme(legend.position = "right")
