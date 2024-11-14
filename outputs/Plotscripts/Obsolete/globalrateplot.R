library(ggplot2)
library(dplyr)
library(tidyr)

# Read the CSV files into data frames
df1 <- read.csv("carbon/Wang_et_al_2021_world_car.csv")
regions <- read.csv("carbon/all.csv")

# Perform a left join to combine the data frames based on the country codes
df2 <- left_join(df1, regions)

# Use gather to transform the data frame into long format
df_long <- df2 %>%
  gather(key = "category", value = "value", starts_with("nar"))

# Change the order of the continent values
df_long$continent <- factor(df_long$continent, 
                            levels = c("Africa", "Asia", "Europe", "Oceania", "North America", "South America", "Central America"),
                            labels = c("Africa", "Asia", "Europe", "Oceania", "N. America", "S. America", "C. America"))

# Change the names of the continent values
#df_long$continent <- factor(df_long$continent, levels = unique(df_long$continent))
 #           labels = c("Africa", "Asia", "Europe", "Oceania", "N. America", "S. America", "C. America"))



# Create the bar plot with regions on the x-axis, faceted by region
ggplot(df_long, aes(x = category, y = value, fill = category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ continent, scales = "fixed", ncol = 4) +
  labs(title = NULL,
       x = NULL,
       y = "N accumultion rate (Gg yr-1)") +
  scale_fill_manual(values = c("nar_marsh" = "#00BFC4", 
                               "nar_mangrove" = "#F8766D", 
                               "nar_total" = "darkblue")) +
  theme_minimal()+
  theme(axis.text.x = element_blank())

