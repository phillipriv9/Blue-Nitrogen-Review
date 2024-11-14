# Install and load necessary packages if not already installed
#install.packages(c("ggplot2", "quantreg"))
library(ggplot2)
library(quantreg)

# Assuming you have a data frame named All_combined
All_combined_data <- read.csv("data/combined/AllCombined.csv")

# Specify a vector of tau values
tau_values <- c(0.5)  # Add or remove tau values as needed

# Plot multiple quantile regressions
ggplot(All_combined_data, aes(x = centerdepth, y = N_perc
                              )) +
  geom_point(alpha=0.1, color = "darkcyan") + 
  stat_quantile(quantiles = tau_values, formula = y ~ x, geom = "quantile", color = "black") +
  labs(title = "Quantile Regressions of N_perc on OC_perc",
       x = "Depth (m)",
       y = "Soil [N] (%)") +
  theme_minimal() +  # Remove gridlines
  theme(axis.title.x = element_text(size = 14),  # Change x-axis title size
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14))   # Change y-axis text size
  #scale_x_log10() +  # Log-transform x-axis
  #scale_y_log10()    # Log-transform y-axisq

  