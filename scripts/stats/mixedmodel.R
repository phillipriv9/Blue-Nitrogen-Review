# Assuming your dataframe is called "your_data"
# Install and load necessary packages
# install.packages(c("lme4", "lmerTest", "car", "Matrix"))
library(lme4)
library(lmerTest)
library(car)
library(Matrix)
library(dplyr)

# Specify the path to your CSV file
csv_file_path <- "data/combined/AllCombined.csv"

# Read the CSV file into a data frame
df1 <- read.csv(csv_file_path)

# Remove rows with infinite or NA values in the CN column
df1 <- df1[is.infinite(df1$CN) & !is.na(df1$CN), ]

# Filter values greater than 100 in the CN column to remove outliers
df1_filtered <- df1 %>% filter(!is.na(CN) & CN > 100)

# Formulate the model
model <- lmer(CN ~ Habitat + centerdepth + BD_reported_g_cm3 + (1 | Source), data = df1_filtered)

# Check assumptions
qqPlot(residuals(model))
plot(model)

# Model summary
summary(model)

# Anova
Anova(model)

# Post-hoc tests
# You can perform post-hoc tests if you find significant effects
anova(model, ddf = "Kenward-Roger")
