##################################
#   The Blue Nitrogen Project    #
#      predict N percent.        #
#          Validation            #
##################################
library(ggplot2)
library(dplyr)
library(car)

BN=read.csv("AllCombined1014.csv")
BN.full=within(BN,{
  Habitat=factor(Habitat)
  BDcat=as.factor(ifelse(BD_reported_g_cm3<=0.5,"low","high"))
  BDcat = relevel(BDcat, ref = "low")
  OC_depth=OC_perc*centerdepth
  OC_habitat=ifelse(Habitat=="mangrove",0,OC_perc)
  BDcat_habitat=as.factor(ifelse(Habitat=="marsh"&BDcat=="high",1,0))
})

rm(BN)


###. Leave-One-Out Cross-Validation. ###
library(lme4)  # For fitting mixed-effects models
library(Metrics)  # For calculating RMSE or other error metrics

# Initialize vectors to store predictions and actual values
predictions = numeric(nrow(BN.full))
actual = BN.full$N_perc  

for (i in 1:nrow(BN.full)) {
  # Split the dataset
  training_data = BN.full[-i, ]  # Training data (all except the i-th observation)
  test_data = BN.full[i, , drop = FALSE]  # Test data (only the i-th observation)
  
  # Fit the model on the training data
  model = lmer(N_perc ~ OC_perc + Habitat + OC_habitat + 
                  (1 | study_id/site_id/core_id), data = training_data)
  
  # Predict the left-out observation using fixed effects only
  pred_value = predict(model, newdata = test_data, re.form = NA)  # Use only fixed effects
  
  # Store the predicted value
  predictions[i] = pred_value
  
  if(i %% 500 == 0){print(i)}
}

# Compare predicted values to actual values
results <- data.frame(Actual = actual, Predicted = predictions)
head(results)
with(results, cor(Predicted, Actual))

#save(results, file="BN_Validation.RData")
#load("BN_Validation.RData")

ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "black", size = 1) +    # Black points
  labs(x = "Actual Soil N (%)", y = "Predicted Soild N (%)") +
  theme_classic() +
  theme(axis.line = element_line(color = "lightgray", size = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))+
  ylim(0, 3)


# Calculate error metrics
rmse_value = rmse(results$Actual, results$Predicted)  # Root Mean Square Error
mae_value = mae(results$Actual, results$Predicted)  # Mean Absolute Error

# Display error metrics
cat("RMSE:", rmse_value, "\n")
cat("MAE:", mae_value, "\n")











