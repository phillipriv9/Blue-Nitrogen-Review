library(ggplot2)
library(dplyr)
library(stringr)

# Read the CSV file into a data frame
All_combined <- read.csv("data/combined/AllCombined.csv")

# Filter the data for only "marsh," "mangrove," and "mudflat" habitats
filtered_data <- All_combined %>%
  filter(Habitat %in% c("marsh", "mangrove")) %>%
  filter(!is.na(BD_reported_g_cm3))
  

# Create a new column for faceting based on BD values
filtered_data <- filtered_data %>%
  mutate(BD_facet = ifelse(BD_reported_g_cm3 > 0.5, "Bulk density > 0.50 g cm-3", "Bulk density <= 0.50"))

# Calculate error bar limits
error_limits <- filtered_data %>%
  group_by(Habitat, BD_facet) %>%
  summarise(
    count = n(),
    mean = mean(CN),
    sd = sd(CN)
  )

# Filter top and bottom 1% of CN values
filtered_data <- filtered_data %>%
  group_by(Habitat, BD_facet) %>%
  filter(CN > quantile(CN, 0.001) & CN < quantile(CN, 0.999))

# Plotting
CN_byBD <- ggplot(filtered_data, aes(x = str_to_title(Habitat), y = CN, fill = Habitat)) +
  geom_violin(scale = "width", alpha = 1) +
  geom_point(stat = "summary", fun = "mean", color = "black", shape = 3, position = position_dodge(width = 0.75)) +
  #geom_errorbar(data = error_limits, aes(x = str_to_title(Habitat), ymin = mean - sd, ymax = mean + sd), 
               # width = 0.5, position = position_dodge(width = 0.75)) +
  labs(title = NULL,
       x = NULL,
       y = "C:N ratio",
       fill = "Habitat") +
  theme_minimal() +
  scale_fill_manual(values = c(marsh = "#00BFC4", mangrove = "#F8766D", mudflat = "tan")) +
  scale_x_discrete(labels = function(x) str_to_title(x)) +
  scale_y_continuous(expand = c(0, 0), limits = c(5, max(error_limits$mean) + 20)) +
  theme(axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.line.x.bottom = element_line(linewidth = 0.3), panel.grid.major.x= element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~ BD_facet, scales = "fixed", ncol = 2)

# Print the summary statistics
print(error_limits)
print(CN_byBD)

# Save the combined plots
ggsave("outputs/CN_by_BD.pdf", plot = CN_byBD, width = 6, height = 6)  # Adjust overall dimensions as needed
