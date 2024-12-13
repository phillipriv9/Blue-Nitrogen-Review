
library(dplyr)
library(tidyr)

# Read the CSV files into data frames
df1 <- read.csv("data/carbon/Wang_et_al_2021_car.csv")

# Read the CSV file into a data frame
All_combined <- read.csv("data/combined/AllCombined.csv")
# Assuming that All_combined has a "Habitat" column

All_combined <- All_combined %>%
 filter(CN>3,CN<100)

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

# Summarize CN by Vegetation
CNsummary_by_vegetation <- All_combined  %>%
  group_by(Habitat) %>%
  summarise(
    mean_CN = mean(CN, na.rm = TRUE),
    sd_CN = sd(CN, na.rm = TRUE),
    ci_low = mean_CN - qnorm(0.975) * sd_CN / sqrt(n()),  # Lower confidence interval
    ci_high = mean_CN + qnorm(0.975) * sd_CN / sqrt(n())  # Upper confidence interval
  )
print(CNsummary_by_vegetation)

path_out = 'data/combined/'
export_file <- paste(path_out,"NAR.csv")
write.csv(df2, export_file)


# Assuming 'CN' and 'Habitat' are the relevant column names in your data frame
library(ggplot2)


# Create a histogram with 1 unit per bin, facet by 'Habitat'
ggplot(df2, aes(y = NAR, fill = Vegetation)) +
  geom_histogram(binwidth = 2, color = "white") +
  #facet_wrap(~ Vegetation, scales = "free") +
  labs(title = NULL,
       y = "Nitrogen accumulation rate (g N m-2 y-1)",
       x = "Obervations") +
  theme_minimal() +  # Adjusted to remove the gray background
  theme(panel.grid = element_blank(), 
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text = element_text(size=14),  # Remove legend title
        axis.line.x = element_line(linewidth=(.2)),
        legend.position = c(0.6, 0.8),
        legend.title=element_blank(),
        legend.text = element_text(size=12))

head(df2)
print(summary(df2$NAR))



# Summarize NAR by Vegetation
summary_by_vegetation <- df2 %>%
  group_by(Vegetation) %>%
  summarise(
    mean_NAR = mean(NAR, na.rm = TRUE),
    sd_NAR = sd(NAR, na.rm = TRUE),
    median_NAR = median(NAR, na.rm = TRUE),
    min_NAR = min(NAR, na.rm = TRUE),
    max_NAR = max(NAR, na.rm = TRUE),
    ci_low = mean_NAR - qnorm(0.975) * sd_NAR / sqrt(n()),  # Lower confidence interval
    ci_high = mean_NAR + qnorm(0.975) * sd_NAR / sqrt(n())  # Upper confidence interval
  )

# Print the summary
print(summary_by_vegetation)

