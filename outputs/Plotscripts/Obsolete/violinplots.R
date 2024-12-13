 library(ggplot2)
#Set a threshold for trimming outliers on the y-axis
y_threshold <- c(3, 50)  # Adjust the threshold as needed

 
 # Specify the path to your CSV file
 csv_file_path <- "data/combined/AllCombined.csv"
 
 # Read the CSV file into a data frame
 All_combined <- read.csv(csv_file_path)
 
 # Check the structure of the data frame
 str(All_combined)
 
 
 
# Create a graph with facets and color by Habitat using Set3 palette
ggplot(All_combined, aes(x = Habitat, y = CN, fill = Habitat)) +
  geom_violin(trim = TRUE) +  # Set trim to TRUE to trim outliers
  #facet_wrap(~ study_id, scales = "free") +
  labs(title = "Violin Plot of OC_perc by Habitat, Faceted by Source",
       x = "Habitat",
       y = "C:N ratio") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  coord_cartesian(ylim = y_threshold)  # Trim values outside the specified range
