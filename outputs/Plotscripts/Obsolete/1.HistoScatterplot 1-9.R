# Load necessary libraries
library(tidyverse)
library(ggExtra)
library(broom)

# Read the CSV file into a data frame
combined_data <- read.csv("combined/AllCombined.csv")

# Filter data for "Marsh" and "Mangrove" habitats and CN > 5
filtered_data <- combined_data %>%
  filter(Habitat %in% c("marsh", "mangrove"),
         CN > 5, CN < 100)  #filter out some wacky low and CN data

# Create a scatter plot with open circles and color by Habitat
plot <- ggplot(filtered_data, aes(x = OC_perc, y = N_perc, color = Habitat)) +
  geom_point(shape = 1, alpha = 0.5) +  # Use shape = 1 for an open circle and set transparency
  labs(title = NULL,
       x = "Soil [Organic C] (%)",
       y = "Soil [N] (%)") +
  theme_minimal() +  # Adjusted to remove the gray background
  theme(panel.grid = element_blank()) +
  geom_smooth(aes(group = Habitat), method = "lm", se = FALSE) +  # Add linear best fit lines
  theme(legend.position = "top-right") +  # Move the legend to the bottom
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  theme(axis.text = element_text(size=12))+  # Adjust laxis font size
  theme(axis.title  = element_text(size=14))
  
  
# Add text annotations to the graph panel
annotations <- data.frame(
  x = c(18, 40),  # Specify x-coordinates of the annotations
  y = c(2.5, .4),  # Specify y-coordinates of the annotations
  label = c("Marsh", "Mangrove"),  # Specify the text labels
  color = c("#00BFC4", "#F8766D")  # Specify color
)

# Add annotations to the plot
plot_with_annotations <- plot +
  annotate("text", x = annotations$x, y = annotations$y, label = annotations$label, size = 5, color = annotations$color)
  

# Create marginal histograms with white fill
histo <- ggMarginal(plot_with_annotations, type = "histogram", bins = 40, fill = "lightgray", line = "lightgray")


# Fit linear models
lm_equations <- filtered_data %>%
  group_by(Habitat) %>%
  do(broom::tidy(lm(N_perc ~ OC_perc, data = .)))

# Print the equations
print(lm_equations)
print(histo)
