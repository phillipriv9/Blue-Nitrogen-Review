
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

############################  Panel A


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
library(tidyr)

# Read the CSV files into data frames
totalNbyregion <- read.csv("data/carbon/total nitrogen by region.csv")

# Create the total summary for marsh+mangrove
  total <- totalNbyregion %>%
  filter((Habitat == "marsh+mangrove") & 
  Region %in% c("Africa", "Asia", "Europe", "NC.America", "Oceania", "S.America"))
  

# Create the bar plot with error bars
bar_plot <- ggplot(total, aes(x = Region, y = predicted_N)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha=0.1) +  # Bar plot
  geom_errorbar(aes(ymin = predicted_N - predicted_N_se, ymax = predicted_N + predicted_N_se), 
                width = 0.2, color = "black") +  # Error bars
  labs(title = "Total N Accumulation by Region",
       y = "Total N Accumulation (units)",
       x = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter to keep only marsh and mangrove data for specific regions
marshmangrove <- totalNbyregion %>%
  filter(Habitat %in% c("marsh", "mangrove") & 
           Region %in% c("Africa", "Asia", "Europe", "NC.America", "Oceania", "S.America"))


# Create the combined stacked bar plot
stacked_plot <- ggplot() +
  # Add stacked bar plot for marsh and mangrove
  geom_bar(data = marshmangrove, aes(x = Region, y = predicted_N, fill = Habitat),
           stat = "identity", position = "stack", width = 0.5, alpha = 1.0) +
  # Add error bars
  geom_errorbar(data = total, 
                aes(x = Region, ymin = predicted_N - predicted_N_se, ymax = predicted_N + predicted_N_se),
                width = 0.2, color = "black") +
  labs(
    y = expression("Scaled N accumulation rate (Gg N" ~ yr^{-1}~")"),
    x = NULL
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 10))

# Print the combined stacked plot
print(stacked_plot)


###########################################





library(patchwork)

# Combine the plots in a horizontal layout with specified widths
fig2 <- (NAR_histogram + stacked_plot + 
  plot_layout(ncol = 3, widths = c(2, 1)))  # Adjust widths as needed

print(fig2)

# Save the combined plots
ggsave("outputs/2.NAR.pdf", plot = fig2, width = 8, height = 6)  # Adjust overall dimensions as needed

#ggsave("outputs/2.NAR.eps", plot = fig2, width = 6, height = 6, dpi = 300)

