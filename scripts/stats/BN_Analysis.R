##################################
#   The Blue Nitrogen Project    #
#      predict N percent.        #
##################################
library(ggplot2)
library(lme4)
library(dplyr)
library(car)
library(lmerTest) #find df for lmer


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



#################################################
#           Mixed effects Models.               #
#.          response = N percent.               #
# random effect = core in site nested in study. #
#################################################
model1.1 = lmer(N_perc ~ OC_perc  + Habitat + OC_habitat+
               (1 | study_id/site_id/core_id), data = BN.full)
res1.1=summary(model1.1, ddf = "Kenward-Roger") 
res1.1
BIC(model1.1)


###################################
# Find slope of N versus C (SE)   #
# .  for mangrove and marsh       #
###################################
# Extract the fixed-effect coefficients
coef = fixef(model1.1)
# Extract the variance-covariance matrix of the fixed effects
vcov_matrix = vcov(model1.1)
combined_slope = coef["OC_perc"] + coef["OC_habitat"]

combined_variance = vcov_matrix["OC_perc", "OC_perc"] +
  vcov_matrix["OC_habitat", "OC_habitat"] +
  2 * vcov_matrix["OC_perc", "OC_habitat"]
combined_se = sqrt(combined_variance)

# Print the combined slope and its standard error
combined_slope
combined_se


###  Add depth to the fixed effects ###
model1.2 = lmer(N_perc ~ OC_perc  + Habitat + OC_habitat+ centerdepth+
              (1 | study_id/site_id/core_id), data = BN.full)
res1.2=summary(model1.2, ddf = "Kenward-Roger") 
BIC(model1.2)

# Extract the fixed-effect coefficients
coef = fixef(model1.2)
vcov_matrix = vcov(model1.2)
combined_slope = coef["OC_perc"] + coef["OC_habitat"]

combined_variance = vcov_matrix["OC_perc", "OC_perc"] +
  vcov_matrix["OC_habitat", "OC_habitat"] +
  2 * vcov_matrix["OC_perc", "OC_habitat"]
combined_se = sqrt(combined_variance)

# Print the combined slope and its standard error
combined_slope
combined_se



###############################
# linear mixed effects model. #
#     response = C/N          #
###############################
model3.1 = lmer(CN ~ Habitat + BDcat + BDcat_habitat +
                (1 | study_id/site_id/core_id), data = BN.full)
res3.1=summary(model3.1, ddf = "Kenward-Roger") 
res3.1
BIC(model3.1)


#Find mean C:N 
fixed_effects <- fixef(model3.1)
# Calculate the estimated mean CN for each combination
mean_mangrove_lowBDcat = fixed_effects["(Intercept)"]
mean_marsh_lowBDcat = fixed_effects["(Intercept)"] + fixed_effects["Habitatmarsh"]
mean_mangrove_highBDcat = fixed_effects["(Intercept)"] + fixed_effects["BDcathigh"]
mean_marsh_highBDcat = fixed_effects["(Intercept)"] + fixed_effects["Habitatmarsh"] + fixed_effects["BDcathigh"] + fixed_effects["BDcat_habitat1"]

# Display the estimated means
mean_mangrove_lowBDcat
mean_marsh_lowBDcat
mean_mangrove_highBDcat
mean_marsh_highBDcat

# Extract the variance-covariance matrix of the fixed effects
vcov_matrix = vcov(model3.1)
se_mangrove_lowBDcat = sqrt(vcov_matrix["(Intercept)", "(Intercept)"])
se_marsh_lowBDcat = sqrt(vcov_matrix["(Intercept)", "(Intercept)"] +
                           vcov_matrix["Habitatmarsh", "Habitatmarsh"] +
                           2 * vcov_matrix["(Intercept)", "Habitatmarsh"])
se_mangrove_highBDcat = sqrt(vcov_matrix["(Intercept)", "(Intercept)"] +
                               vcov_matrix["BDcathigh", "BDcathigh"] +
                               2 * vcov_matrix["(Intercept)", "BDcathigh"])
se_marsh_highBDcat = sqrt(vcov_matrix["(Intercept)", "(Intercept)"] +
                            vcov_matrix["Habitatmarsh", "Habitatmarsh"] +
                            vcov_matrix["BDcathigh", "BDcathigh"] +
                            vcov_matrix["BDcat_habitat1", "BDcat_habitat1"] +
                            2 * vcov_matrix["(Intercept)", "Habitatmarsh"] +
                            2 * vcov_matrix["(Intercept)", "BDcathigh"] +
                            2 * vcov_matrix["Habitatmarsh", "BDcathigh"] +
                            2 * vcov_matrix["(Intercept)", "BDcat_habitat1"] +
                            2 * vcov_matrix["Habitatmarsh", "BDcat_habitat1"] +
                            2 * vcov_matrix["BDcathigh", "BDcat_habitat1"])

# Display the standard errors
se_mangrove_lowBDcat
se_marsh_lowBDcat
se_mangrove_highBDcat
se_marsh_highBDcat


### a second model including depth ###
model3.2 = lmer(CN ~ Habitat + BDcat + BDcat_habitat + centerdepth+
              (1 | study_id/site_id/core_id), data = BN.full)
res3.2=summary(model3.2, ddf = "Kenward-Roger") 
res3.2
BIC(model3.2)

fixed_effects <- fixef(model3.2)
# Calculate the estimated mean CN for each combination
mean_mangrove_lowBDcat = fixed_effects["(Intercept)"]
mean_marsh_lowBDcat = fixed_effects["(Intercept)"] + fixed_effects["Habitatmarsh"]
mean_mangrove_highBDcat = fixed_effects["(Intercept)"] + fixed_effects["BDcathigh"]
mean_marsh_highBDcat = fixed_effects["(Intercept)"] + fixed_effects["Habitatmarsh"] + fixed_effects["BDcathigh"] + fixed_effects["BDcat_habitat1"]

# Display the estimated means
mean_mangrove_lowBDcat
mean_marsh_lowBDcat
mean_mangrove_highBDcat
mean_marsh_highBDcat

vcov_matrix = vcov(model3.2)
se_mangrove_lowBDcat = sqrt(vcov_matrix["(Intercept)", "(Intercept)"])
se_marsh_lowBDcat = sqrt(vcov_matrix["(Intercept)", "(Intercept)"] +
                           vcov_matrix["Habitatmarsh", "Habitatmarsh"] +
                           2 * vcov_matrix["(Intercept)", "Habitatmarsh"])
se_mangrove_highBDcat = sqrt(vcov_matrix["(Intercept)", "(Intercept)"] +
                               vcov_matrix["BDcathigh", "BDcathigh"] +
                               2 * vcov_matrix["(Intercept)", "BDcathigh"])
se_marsh_highBDcat = sqrt(vcov_matrix["(Intercept)", "(Intercept)"] +
                            vcov_matrix["Habitatmarsh", "Habitatmarsh"] +
                            vcov_matrix["BDcathigh", "BDcathigh"] +
                            vcov_matrix["BDcat_habitat1", "BDcat_habitat1"] +
                            2 * vcov_matrix["(Intercept)", "Habitatmarsh"] +
                            2 * vcov_matrix["(Intercept)", "BDcathigh"] +
                            2 * vcov_matrix["Habitatmarsh", "BDcathigh"] +
                            2 * vcov_matrix["(Intercept)", "BDcat_habitat1"] +
                            2 * vcov_matrix["Habitatmarsh", "BDcat_habitat1"] +
                            2 * vcov_matrix["BDcathigh", "BDcat_habitat1"])

# Display the standard errors
se_mangrove_lowBDcat
se_marsh_lowBDcat
se_mangrove_highBDcat
se_marsh_highBDcat



### a simple model to estimate mean C:N ratios in mangroves and marshes ###
model3.3 = lmer(CN ~ Habitat + 
                  (1 | study_id/site_id/core_id), data = BN.full)
res3.3=summary(model3.3, ddf = "Kenward-Roger") 
res3.3
BIC(model3.3)

# Extract fixed effects estimates
fixed_effects = fixef(model3.3)
mean_mangrove = fixed_effects[1]
mean_marsh = fixed_effects[1] + fixed_effects[2]

# Extract the variance-covariance matrix of the fixed effects
vcov_matrix = vcov(model3.3, method = "Kenward-Roger")
se_mangrove = sqrt(vcov_matrix[1, 1])
se_marsh = sqrt(vcov_matrix[1, 1] + vcov_matrix[2, 2] + 2 * vcov_matrix[1, 2])

# Display the results
mean_mangrove
se_mangrove
mean_marsh
se_marsh






####################################################################
#                      linear mixed effects model.                 #
#  response = N density = (N concentration/100)*bulk density*1000. #
####################################################################
#N density unit: mg/cm^3
#N_perc of 1% is recorded as 1
BN.full=within(BN.full,{
  Ndensity=N_perc*BD_reported_g_cm3*10
})

model4 = lmer(Ndensity ~ Habitat + 
                (1 | study_id/site_id/core_id), data = BN.full)
res4=summary(model4, ddf = "Kenward-Roger") 
res4
BIC(model4)

# Extract fixed effects estimates
fixed_effects = fixef(model4)
mean_mangrove = fixed_effects[1]
mean_marsh = fixed_effects[1] + fixed_effects[2]

# Extract the variance-covariance matrix of the fixed effects
vcov_matrix = vcov(model4, method = "Kenward-Roger")
se_mangrove = sqrt(vcov_matrix[1, 1])
se_marsh = sqrt(vcov_matrix[1, 1] + vcov_matrix[2, 2] + 2 * vcov_matrix[1, 2])

# Display the results
mean_mangrove
se_mangrove
mean_marsh
se_marsh


# Second model: inluding depth
model4.1 = lmer(Ndensity ~ Habitat + centerdepth+
                (1 | study_id/site_id/core_id), data = BN.full)
res4.1=summary(model4.1, ddf = "Kenward-Roger") 
res4.1
BIC(model4.1)

fixed_effects = fixef(model4.1)
vcov_matrix = vcov(model4.1)

mean_mangrove = fixed_effects["(Intercept)"]
se_mangrove = sqrt(vcov_matrix["(Intercept)", "(Intercept)"])

# Adjusted mean for marsh (Habitat = 1)
mean_marsh = fixed_effects["(Intercept)"] + fixed_effects["Habitatmarsh"]
se_marsh = sqrt(vcov_matrix["(Intercept)", "(Intercept)"] +
                   vcov_matrix["Habitatmarsh", "Habitatmarsh"] + 
                   2 * vcov_matrix["(Intercept)", "Habitatmarsh"])

# Display results
mean_mangrove_se <- c(Mean = mean_mangrove, SE = se_mangrove)
mean_marsh_se <- c(Mean = mean_marsh, SE = se_marsh)

mean_mangrove_se
mean_marsh_se






###################################
#           Prediction.           #
###################################
#Carbon estimates by Wang et al., 2021, across 103 sites
carbon=read.csv("Wang_et_al_2021_car.csv")

#use model3.3 (CN ~ Habitat) to predict N
carbon=within(carbon,{
  Habitat=ifelse(Vegetation=="Mangrove","mangrove","marsh")
  Habitat=as.factor(Habitat)
})

estCN=predict(model3.3,newdata=data.frame(Habitat=carbon$Habitat),
             re.form = NA, se.fit = TRUE)
carbon$predictedCN=estCN$fit
carbon$predicted_N=with(carbon,CAR/predictedCN)

#write.csv(carbon, file = "carbon_LW_11142024.csv", row.names = FALSE)
#summary statistics
carbon_man=subset(carbon,Habitat=="mangrove")
carbon_mar=subset(carbon,Habitat=="marsh")
median(carbon_man$predicted_N);summary(carbon_man$predicted_N)
median(carbon_mar$predicted_N);summary(carbon_mar$predicted_N)




##################################################
# calculate carbon for each region and the total #
##################################################
#carbon by country
c_cont=read.csv("Wang et al. 2021 CAR.csv")
#region code
rcode=read.csv("country_regions.csv") 

Asia=rcode$alpha.3[rcode$Region=="Asia"]
Europe=rcode$alpha.3[rcode$Region=="Europe"]
Africa=rcode$alpha.3[rcode$Region=="Africa"]
Oceania=rcode$alpha.3[rcode$Region=="Oceania"]
NC.America=rcode$alpha.3[rcode$Region=="N&C America"]
S.America=rcode$alpha.3[rcode$Region=="South America"]

Region=rep(c("Asia","Europe","Africa","Oceania",
         "NC.America","S.America","Total"),2)
Habitat=c(rep("mangrove",7),rep("marsh",7))


mangroveCAR=c(sum(subset(c_cont,country %in% Asia)$mangroveCAR),
                sum(subset(c_cont,country %in% Europe)$mangroveCAR),
                sum(subset(c_cont,country %in% Africa)$mangroveCAR),
                sum(subset(c_cont,country %in% Oceania)$mangroveCAR),
                sum(subset(c_cont,country %in% NC.America)$mangroveCAR),
                sum(subset(c_cont,country %in% S.America)$mangroveCAR))
total.man=sum(mangroveCAR)
marshCAR=c(sum(subset(c_cont,country %in% Asia)$marshCAR),
                sum(subset(c_cont,country %in% Europe)$marshCAR),
                sum(subset(c_cont,country %in% Africa)$marshCAR),
                sum(subset(c_cont,country %in% Oceania)$marshCAR),
                sum(subset(c_cont,country %in% NC.America)$marshCAR),
                sum(subset(c_cont,country %in% S.America)$marshCAR))
total.mar=sum(marshCAR)
#carbon by regions (continents)
CAR=c(mangroveCAR,total.man,marshCAR,total.mar)

#use model3.3 to predict N
new=data.frame(Region=Region, Carbon=CAR,
               Habitat=as.factor(Habitat))
levels(new$Habitat)

est = predict(model3.3, newdata = new, re.form = NA, se.fit = TRUE)
new$predicted_CN=est$fit
new$predicted_CN_var=(est$se.fit)^2

#N=C/(C:N)
new$predicted_N=with(new, Carbon/predicted_CN)
#Use Delta Method to calculate the variance of N from C:N
new$predicted_N_se=with(new, sqrt(predicted_CN_var*(-Carbon/(predicted_CN^2))^2 ))

global.total=2447.643216+772.012259
global.se=sqrt(118.6757002^2+36.4792668^2)
#write.csv(new, file = "total nitrogen by region_LW_11142024.csv", row.names = FALSE)



