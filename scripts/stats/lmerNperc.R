#install.packages("Matrix", dependencies = TRUE)
library(dplyr)
#library(tidyverse)
library(lme4)

df<-read.csv("data/combined/AllCombined.csv")

df$Latitude <- as.numeric(df$Latitude)


df2 <- df %>%
  filter(Habitat %in% c("marsh", "mangrove"), CN > 5, CN < 100, centerdepth<1)


modelN <- lmer(N_perc ~  OC_perc *Habitat +  (1 | study_id/core_id), data = df2) #plot nested within source
# variability derives from different experimenters sampling different sites.


modelCN <- lmer(CN~ BDclass *  centerdepth * Habitat + (1 | study_id/core_id), data = df2) #plot nested within source
# variability derives from different experimenters sampling different sites.

modelCN <- lmer(CN~ BDclass *  Habitat + (1 | study_id/core_id), data = df2) #remove centerdetph to give a depth-blind version of the model for extrapolation 


modelNdensity <- lmer(Ndensity~  centerdepth*Habitat + (1 | study_id/core_id), data = df2) #plot nested within source


summary(modelN)
summary(modelCN)
summary(modelNdensity)


