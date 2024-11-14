#install.packages("Matrix", dependencies = TRUE)
#install.packages("MuMIn")
#install.packages("bbmle")
library(dplyr)
library(tidyverse)
library(lme4)
library(MuMIn)

df<-read.csv("data/combined/AllCombined.csv")

df$Latitude <- as.numeric(df$Latitude)


df2 <- df %>%
  filter(Habitat %in% c("marsh", "mangrove"), CN > 5, CN < 100)

head(df2)

# Assuming 'df2' is your dataframe

library(bbmle)

# Define your candidate models
model1 <- lmer(CN ~ OC_perc + (1 | Source/Plot), data = df2)
model2 <- lmer(CN ~ OC_perc + Habitat + (1 | Source/Plot), data = df2)
model3 <- lmer(CN ~ OC_perc * Habitat + (1 | Source/Plot), data = df2)

# Fit the models
fit_model1 <- AIC(model1)
fit_model2 <- AIC(model2)
fit_model3 <- AIC(model3)

# Compare AIC values
AIC_values <- c(Model1 = fit_model1, Model2 = fit_model2, Model3 = fit_model3)
print(AIC_values)
summary(model1)
summary(model2)
summary(model3)