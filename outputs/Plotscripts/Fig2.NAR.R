
library(dplyr)
library(tidyr)

# Read the CSV files into data frames
df1 <- read.csv("data/carbon/Wang_et_al_2021_car.csv")

# Read the CSV file into a data frame
All_combined <- read.csv("data/combined/AllCombined.csv")


# Calculate the mean value of the CN column and handle missing and infinite values
mean_CN <- mean(All_combined$CN[is.finite(All_combined$CN)], na.rm = TRUE)

mean_CN_mangrove <- All_combined %>%
  filter(Habitat == "mangrove") %>%
  summarise(mean_CN_mangrove = mean(CN[is.finite(CN)], na.rm = TRUE))

mean_CN_marsh <- All_combined %>%
  filter(Habitat == "marsh") %>%
  summarise(mean_CN_marsh = mean(CN[is.finite(CN)], na.rm = TRUE))

df2 <- df1 %>%
  mutate(
    modeledCN = ifelse(Vegetation == "Mangrove", mean_CN_mangrove, mean_CN_marsh),
    modeledCN = as.numeric(ifelse(modeledCN > 0 & modeledCN < 100, modeledCN, NA)),  # Coerce to numeric and filter values
    NAR = CAR / modeledCN
  )


path_out = 'data/combined/'
export_file <- paste(path_out,"NAR.csv")
write.csv(df2, export_file)


library(ggplot2)


NAR_histogram <- ggplot(df2, aes(y = NAR, fill = Vegetation)) +
  geom_histogram(binwidth = .08, color = "white") +
  labs(
    title = NULL,
    y = expression("N accumulation rate per area (g N" ~ m^{-2} ~ yr^{-1}~")"),
    x = "Observations"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line.x = element_line(linewidth = 0.2),
    axis.line.y = element_line(linewidth = 0.2),
    legend.position = "none"

  ) +
  scale_y_continuous(
    limits = c(0, 55),  
    breaks = seq(0, 55, by = 10)  # Specify tick mark positions
  ) +
  scale_y_log10()  # Log-scale the y-axis


head(df2)
print(summary(df2$NAR))


########################### PanelB

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
                            "N & C America", continent))

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
                            levels = c("Africa", "Asia", "Europe", "Oceania", "N & C America", "South America"))

scaled_NAR <- ggplot(df_long, aes(y = value, x = continent, fill = habitat)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) + # Decrease width for skinnier bars
  labs(title = NULL,
       y = expression("Scaled N accumulation rate (Gg N" ~ yr^{-1}~")"),  # Corrected: added comma
       x = NULL) +                            # Remove x-axis label
  scale_fill_manual(values = c("Marsh" = "darkturquoise", 
                               "Mangrove" = "indianred1")) + # Assign different colors
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, max(df_long$value, na.rm = TRUE))) +  # Ensure y-axis starts at 0
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.title.y = element_text(size = 14), 
        axis.text.y = element_text(size = 12), 
        axis.line.x = element_line(color = "lightgray", linewidth = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 10), 
        legend.position = "none")  # Rotate x-axis labels

# Print the plot
print(scaled_NAR)

# Combine the plots in a horizontall layout
fig2 <- NAR_histogram  + scaled_NAR

print(fig2)

#save the combined file with NAR and country info
write.csv(df_long, file = "data/carbon/df_long.csv", row.names = FALSE)

# Save the net N plots
ggsave("outputs/2.NAR.pdf", plot = fig2, width = 6, height = 6)

library(patchwork)

# Combine the plots in a horizontal layout with specified widths
fig2 <- (NAR_histogram + scaled_NAR) + 
  plot_layout(ncol = 3, widths = c(2, 1))  # Adjust widths as needed

print(fig2)

# Save the combined plots
ggsave("outputs/2.NAR.pdf", plot = fig2, width = 6, height = 6)  # Adjust overall dimensions as needed

ggsave("outputs/2.NAR.eps", plot = fig2, width = 6, height = 6, dpi = 300)

