
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

print(NAR_histogram
      )





########################### PanelB
########################### PanelB
library(ggplot2)
library(dplyr)

# Assuming df_total_summary and error_values are already defined
# Summarizing the data
df_total <- df_stacked %>%
  filter(habitat %in% c("nar_total"))

df_total_summary <- df_total %>%
  group_by(Region) %>%
  summarize(total = sum(value), .groups = 'drop')

# Calculate error values (5% of the total)
error_values <- df_total_summary %>%
  mutate(error = 0.05 * total)

# Create the bar plot with error bars
bar_plot <- ggplot(error_values, aes(x = Region, y = total)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha=0.1) +  # Bar plot
  geom_errorbar(aes(ymin = total - error, ymax = total + error), 
                width = 0.2, color = "black") +  # Error bars
  labs(title = "Total N Accumulation by Region",
       y = "Total N Accumulation (units)",
       x = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(ggplot2)
library(dplyr)
library(tidyr)

# Create the total summary for nar_total
df_total <- df_stacked %>%
  filter(habitat == "nar_total")

df_total_summary <- df_total %>%
  group_by(Region) %>%
  summarize(total = sum(value), .groups = 'drop')

# Calculate error values (5% of the total)
error_values <- df_total_summary %>%
  mutate(error = 0.05 * total)

# Filter to keep only marsh and mangrove
df_marshmangrove <- df_stacked %>%
  filter(habitat %in% c("nar_marsh", "nar_mangrove"))

# Calculate totals for marsh and mangrove
df_marshmangrove_summary <- df_marshmangrove %>%
  group_by(Region, habitat) %>%
  summarize(total = sum(value), .groups = 'drop')

# Create the combined plot
combined_plot <- ggplot() +
  # Add stacked bar plot for marsh and mangrove
  geom_bar(data = df_marshmangrove_summary, aes(x = Region, y = total, fill = habitat),
           stat = "identity", position = "stack", width = 0.5, alpha = 1, show.legend = FALSE) +
  # Add total bar for nar_total (with no legend)
  geom_bar(data = df_total_summary, aes(x = Region, y = total), 
           stat = "identity", fill = "skyblue", alpha = 0.0, show.legend = FALSE) +
  # Add error bars
  geom_errorbar(data = error_values,
                aes(x = Region, ymin = total - error, ymax = total + error),
                width = 0.2, color = "black", show.legend = FALSE) +
  labs(
    y = expression("Scaled N accumulation rate (Gg N" ~ yr^{-1}~")"),
    x = NULL
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 10))

# Print the combined plot
print(combined_plot)


###########################################





library(patchwork)

# Combine the plots in a horizontal layout with specified widths
fig2 <- (NAR_histogram + combined_plot + 
  plot_layout(ncol = 3, widths = c(2, 1)))  # Adjust widths as needed

print(fig2)

# Save the combined plots
ggsave("outputs/2.NAR.pdf", plot = fig2, width = 6, height = 6)  # Adjust overall dimensions as needed

ggsave("outputs/2.NAR.eps", plot = fig2, width = 6, height = 6, dpi = 300)

