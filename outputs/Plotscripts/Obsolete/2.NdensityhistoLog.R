
library(dplyr)
library(tidyr)

# Read the CSV files into data frames
df1 <- read.csv("carbon/Wang_et_al_2021_car.csv")

# Read the CSV file into a data frame
All_combined <- read.csv("combined/AllCombined.csv")
# Assuming that All_combined has a "Habitat" column

All_combined <- All_combined %>%
 filter(CN>1,CN<100, Habitat %in% c("marsh", "mangrove"))

# Calculate the mean value of the CN column and handle missing and infinite values
mean_CN <- mean(All_combined$CN[is.finite(All_combined$CN)], na.rm = TRUE)


Ndensity <- 1000* All_combined$N_perc/100*All_combined$BD_reported_g_cm3
print(Ndensity)

ggplot(All_combined, aes(y = Ndensity, fill = Habitat)) +
  geom_histogram(bins= 40, color = "white") +
  labs(
    title = NULL,
    y = expression("N density (mg N" ~ cm^{-3} ~ ")"),
    x = "Observations"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 14),
    axis.line.x = element_line(linewidth = 0.2),
    axis.line.y = element_line(linewidth = 0.2),
    legend.position = c(0.7, 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
    # Adjusted limits based on your preferences
    #breaks = seq(0, 55, by = 10)  # Specify tick mark positions
  #) 
  #scale_y_log10()+  # Log-scale the y-axis
  scale_y_continuous(
    limits = c(0.0, 6))+
  scale_fill_manual(values = c("marsh" = "#00BFC4", "mangrove" = "#F8766D"))  # Specify colors for each level

  
Summary<-df %>% 
  group_by(Habitat) %>%
  summary(Ndensity, na.rm=TRUE) 

print (Summary) # ("mg cm-3")
print(summary(Ndensity, na.rm=TRUE))



