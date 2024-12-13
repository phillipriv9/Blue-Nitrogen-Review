
library(dplyr)
library(tidyr)

# Read the CSV files into data frames
df1 <- read.csv("data/carbon/Wang_et_al_2021_car.csv")

# Read the CSV file into a data frame
All_combined <- read.csv("data/combined/AllCombined.csv")
# Assuming that All_combined has a "Habitat" column

All_combined <- All_combined %>%
 filter(CN>5,CN<100)

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


# Assuming 'CN' and 'Habitat' are the relevant column names in your data frame
library(ggplot2)


ggplot(df2, aes(y = NAR, fill = Vegetation)) +
  geom_histogram(binwidth = .05, color = "white") +
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
    axis.text = element_text(size = 14),
    axis.line.x = element_line(linewidth = 0.2),
    axis.line.y = element_line(linewidth = 0.2),
    legend.position = c(0.7, 0.4),
    legend.title = element_blank(),
    legend.text = element_text(size = 16)
  ) +
  scale_y_continuous(
    limits = c(0, 55),  # Adjusted limits based on your preferences
    breaks = seq(0, 55, by = 10)  # Specify tick mark positions
  ) +
  scale_y_log10()  # Log-scale the y-axis


head(df2)
print(summary(df2$NAR))



