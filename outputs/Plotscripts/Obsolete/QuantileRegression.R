# Install and load necessary packages if not already installed
#install.packages(c("ggplot2", "quantreg"))
library(ggplot2)
library(quantreg)

# Assuming you have a data frame named All_combined_data
All_combined_data <- read.csv("combined/All_combined_data.csv")

# Specify a vector of tau values
tau_values <- c(0.25, 0.5, 0.75)  # Add or remove tau values as needed

# Plot multiple quantile regressions
ggplot(All_combined_data, aes(x = OC_perc, y = N_perc)) +
  geom_point(alpha=0.1, color = "darkcyan") + 
  stat_quantile(quantiles = tau_values, formula = y ~ x, geom = "quantile", color = "darkcyan") +
  labs(title = "Quantile Regressions of N_perc on OC_perc",
       x = "Soil organic [C] %",
       y = "Soil [N] (%)") +
  theme_minimal() +  # Remove gridlines
  theme(axis.title.x = element_text(size = 12),  # Change x-axis title size
        axis.title.y = element_text(size = 12)) # Change y-axis title size
  