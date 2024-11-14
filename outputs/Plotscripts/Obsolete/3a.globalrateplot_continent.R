library(ggplot2)
library(dplyr)
library(tidyr)

# Read the CSV files into data frames
df1 <- read.csv("data/carbon/Wang_et_al_2021_world_car.csv")
regions <- read.csv("data/carbon/all.csv")

# Perform a left join to combine the data frames based on the country codes
df2 <- left_join(df1, regions)

# Use pivot_longer to transform the data frame into long format
df_long <- df2 %>%
  pivot_longer(cols = starts_with("nar"), names_to = "category", values_to = "value")

# Combine North America and Central America into a new category
df_long <- df_long %>%
  mutate(continent = ifelse(continent %in% c("North America", "Central America"), 
                            "N. and C. America", continent))

# Filter to exclude the "nar_total" category
df_long <- df_long %>%
  filter(category != "nar_total")

# Combine marsh and mangrove into a single variable for stacking
df_long <- df_long %>%
  mutate(habitat = case_when(
    category == "nar_marsh" ~ "Marsh",
    category == "nar_mangrove" ~ "Mangrove",
    TRUE ~ NA_character_ # Exclude other categories
  )) %>%
  filter(!is.na(habitat)) # Keep only marsh and mangrove

# Change the order of the continent values
df_long$continent <- factor(df_long$continent, 
                            levels = c("Africa", "Asia", "Europe", "Oceania", "N. and C. America", "South America"))

# Create the horizontal bar plot with categories on the y-axis
scaled_NAR <- ggplot(df_long, aes(y = value, x = continent, fill = habitat)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) + # Decrease width for skinnier bars
  labs(title = NULL,
       y = expression("N accumulation rate (Gg N" ~ yr^{-1}~")"),  # Corrected: added comma
       x = NULL) +                            # Remove x-axis label
  scale_fill_manual(values = c("Marsh" = "darkturquoise", 
                               "Mangrove" = "indianred1")) + # Assign different colors
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.title.y = element_text(size = 16), axis.text.y = element_text(size=14),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 12))  # Rotate x-axis labels

# Save the net N plots
ggsave("outputs/2b.scaled_NAR.pdf", plot = scaled_NAR, width = 4, height = 6)
print(scaled_NAR
      )

