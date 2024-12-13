# Load necessary libraries
library(dplyr)
library(lme4)
library(lmerTest)  # This package provides p-values for mixed models
library(broom.mixed)
library(officer)
library(writexl)

# Read data
df <- read.csv("data/combined/AllCombined.csv")
shallow <- df %>% filter(centerdepth < 0.3)

# Fit models with lmerTest to include p-values
modelN1 <- lmer(N_perc ~ OC_perc * Habitat + (1 | study_id/site_id/core_id), data = df)
modelN2 <- lmer(N_perc ~ OC_perc * Habitat + centerdepth + (1 | study_id/site_id/core_id), data = df)
modelCN1 <- lmer(CN ~ Habitat + (1 | study_id/site_id/core_id), data = df)
modelCN2 <- lmer(CN ~ BDclass * Habitat + (1 | study_id/site_id/core_id), data = df)
modelCN3 <- lmer(CN ~ BDclass * Habitat + centerdepth + (1 | study_id/site_id/core_id), data = df)
modelNdensity <- lmer(Ndensity ~ Habitat + (1 | study_id/site_id/core_id), data = df)
modelNdensity2 <- lmer(Ndensity ~ Habitat + centerdepth + (1 | study_id/site_id/core_id), data = df)

# Get the BIC for each model
model_bics <- tibble(
  Model = c("modelN1", "modelN2", "modelCN1", "modelCN2", "modelCN3", "modelNdensity", "modelNdensity2"),
  BIC = c(BIC(modelN1), BIC(modelN2), BIC(modelCN1), BIC(modelCN2), BIC(modelCN3), BIC(modelNdensity), BIC(modelNdensity2))
)

# Convert model summaries to tidy data frames, which will now include p-values from lmerTest
model_results <- list(
  modelN1 = tidy(modelN1),
  modelN2 = tidy(modelN2),
  modelCN1 = tidy(modelCN1),
  modelCN2 = tidy(modelCN2),
  modelCN3 = tidy(modelCN3),
  modelNdensity = tidy(modelNdensity),
  modelNdensity2 = tidy(modelNdensity2)
)

# Add BIC to model results
# Here we will add the BIC value as a new column in the model results
for (model_name in names(model_results)) {
  model_results[[model_name]] <- model_results[[model_name]] %>%
    mutate(BIC = model_bics$BIC[model_bics$Model == model_name][1])
}

# Combine all results into one data frame for Excel
all_results <- bind_rows(model_results, .id = "Model")

# Save the results to an Excel file
write_xlsx(all_results, path = "outputs/model_results.xlsx")

# Create a Word document
doc <- read_docx()

# Add each model's results as a separate table
for (model_name in names(model_results)) {
  doc <- doc %>%
    body_add_par(paste("Results for", model_name), style = "heading 1") %>%
    body_add_table(value = model_results[[model_name]], style = "table_template")
}

# Save the document
print(doc, target = "outputs/Model_Results.docx")
