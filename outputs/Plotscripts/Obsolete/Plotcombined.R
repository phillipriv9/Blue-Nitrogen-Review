
# load library tidyverse and ggExtra
library(tidyverse)
library(ggExtra)

# set theme
theme_set(theme_bw(12))

combined_data <- read.csv("combined/combined_data.csv")

# Create a scatter plot
plot<-ggplot(combined_data, aes(x = OC_perc, y = N_perc)) +
  geom_point() +
  labs(title = "OC_perc vs N_perc",
       x = "Soil [Organic C] (%)",
       y = "Soil [N] (%)")

# create sample data frame
#sample_data <- data.frame(x, y) 

# use ggMarginal function to create marginal histogram
histo<-ggMarginal(plot, type="histogram")



# Set the file paths for PDF and JPG
pdf_path <- "plot_output.pdf"
jpg_path <- "plot_output.jpg"

#Export to PDF
ggsave(pdf_path, histo, width = 8, height = 6, units = "in", dpi = 300)

#Export to JPG
ggsave(jpg_path, histo, width = 800, height = 600, units = "px", dpi = 300)

# Confirm the exports
cat("histo exported to PDF:", pdf_path, "\n")
cat("histo exported to JPG:", jpg_path, "\n")
