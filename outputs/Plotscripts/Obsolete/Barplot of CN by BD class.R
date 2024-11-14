library(ggplot2)
library(dplyr)
library(stringr)

# Read the CSV file into a data frame
All_combined <- read.csv("combined/AllCombined.csv")

# Filter the data for only "marsh," "mangrove," and "mudflat" habitats
filtered_data <- All_combined %>%
  filter(Habitat %in% c("marsh", "mangrove", "mudflat")) %>%
  filter(!is.na(BD_reported_g_cm3))  # Exclude rows with NA for BD_reported_g_cm3

# Create a new column for faceting based on BD values
filtered_data <- filtered_data %>%
  mutate(BD_facet = ifelse(BD_reported_g_cm3 > 0.50, "Bulk density > 0.50 g cm-3", "Bulk density <= 0.50"))

# Calculate error bar limits
error_limits <- filtered_data %>%
  group_by(Habitat, BD_facet) %>%
  summarise(y_upper = mean(CN) + sd(CN), y_lower = mean(CN) - sd(CN))

# Define custom colors for habitats
custom_colors <- c("marsh" = "sienna1", "mangrove" = "darkcyan", "mudflat" = "tan4")

# Create a column graph with means and confidence intervals for CN colored by Habitat and faceted by BD_facet
ggplot(filtered_data, aes(x = str_to_title(Habitat), y = CN, fill = factor(Habitat, levels = c("marsh", "mangrove", "mudflat")))) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 1), width = 0.5) +
  labs(title = NULL,
       x = NULL,
       y = "Mean C:N ratio",
       fill = "Habitat") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  scale_x_discrete(labels = function(x) str_to_title(x)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, max(error_limits$y_upper) + 15)) +
  theme(axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text( size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~ BD_facet, scales = "fixed", ncol = 2)
