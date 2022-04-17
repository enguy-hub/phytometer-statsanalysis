# -- Prerequisites ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "PerformanceAnalytics", "MASS", 
                   "car", "effects")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Hien/Garden/MyGithub/Phytometer_StatisticalAnalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
fa_path = "./analysis_data/FA/FA_Data_2021_4analysis_garden_transformed.xlsx"
FA_data <- read_excel(fa_path, sheet = 1)


# Check structure and summaries of the data
str(FA_data)
summary(FA_data)


# Remove "Non-normal distributed" variables
FA_data <- FA_data %>%
  dplyr::select(-c("pol_abundance", "pol_shannon", "flo_abundance", "flo_richness",
                   "urbanclass100", "urbanclass200", "urbanclass500", "urbanclass1000"))


# Check correlation between "Response variables"
resp_vars <- c(2,3,4,5)
resp_corr <- FA_data[,resp_vars]
chart.Correlation(resp_corr, histogram=TRUE)


# Check correlation between "Predictor variables"
pred_vars <- c(6,7,8,9,10,11,12,13,14,15,16,17)
pred_corr <- FA_data[,pred_vars]
chart.Correlation(pred_corr, histogram=TRUE)


# ------------------------------------------------------------------------------


# ---- Quick Note ----

# -- Only lm() function will be used in this script -- #

# -- !! All variables are normally distributed !! -- #
# ! Response (dependent): 
#   + mass_meandiff, mass_meanopen, ratio_meandiff, ratio_meanopen
# ! Predictor (independent): 
#   + temp, lux, imperv100, imperv200, imperv500, imperv1000
#   + pol_abundance.yj, pol_richness, pol_shannon.yj
#   + flo_abundance.yj, flo_richness.yj, flo_shannon

# -- Guide for reading lm() function's output -- #
# RSE: Lower is better
# Adjusted R-squared: Higher is better
# F-statistic p-value: Below 0.05 and more significant (*) is better
# ---------------------------------------------------------------------------- #


# -- Working with: mass_meandiff ----


# -- Making single lm() models ----
mmd.lm.s0 <- lm(mass_meandiff ~ temp, data=FA_data)
summary(mmd.lm.s0) # RSE: 2.84 ; Adj-R2: -0.055 ; p: 0.556 *

mmd.lm.s1 <- lm(mass_meandiff ~ lux, data=FA_data)
summary(mmd.lm.s1) # RSE: 2.89 ; Adj-R2: -0.09 p: 0.962

mmd.lm.s2 <- lm(mass_meandiff ~ imperv100, data=FA_data)
summary(mmd.lm.s2) # RSE: 2.79 ; Adj-R2: -0.01 ; p: 0.39

mmd.lm.s3 <- lm(mass_meandiff ~ imperv200, data=FA_data)
summary(mmd.lm.s3) # RSE: 2.73 ; Adj-R2: 0.02 ; p: 0.28 *

mmd.lm.s4 <- lm(mass_meandiff ~ imperv500, data=FA_data)
summary(mmd.lm.s4) # RSE: 2.69 ; Adj-R2: 0.054 ; p: 0.22 * 

mmd.lm.s5 <- lm(mass_meandiff ~ imperv1000, data=FA_data)
summary(mmd.lm.s5) # RSE: 2.71 ; Adj-R2: 0.041 ; p: 0.243 *

mmd.lm.s6 <- lm(mass_meandiff ~ pol_abundance.yj, data=FA_data)
summary(mmd.lm.s6) # RSE: 2.64 ; Adj-R2: 0.09 ; p: 0.16 *

mmd.lm.s7 <- lm(mass_meandiff ~ pol_richness, data=FA_data)
summary(mmd.lm.s7) # RSE: 2.5 ; Adj-R2: 0.18 ; p: 0.081 *

mmd.lm.s8 <- lm(mass_meandiff ~ pol_shannon.yj, data=FA_data)
summary(mmd.lm.s8) # RSE: 2.11 ; Adj-R2: 0.42 ; p: 0.009 *

mmd.lm.s9 <- lm(mass_meandiff ~ flo_abundance.yj, data=FA_data)
summary(mmd.lm.s9) # RSE: 2.8 ; Adj-R2: -0.027 ; p: 0.43 *

mmd.lm.s10 <- lm(mass_meandiff ~ flo_richness.yj, data=FA_data)
summary(mmd.lm.s10) # RSE: 2.7 ; Adj-R2: 0.004 ; p: 0.326 *

mmd.lm.s11 <- lm(mass_meandiff ~ flo_shannon, data=FA_data)
summary(mmd.lm.s11) # RSE: 2.84 ; Adj-R2: -0.054 ; p: 0.55 *


# -- Check correlation of dependent and independent vars again ----
mmd_vars <- c(2,6,7,8,9,10,11,12,13,14,15,16,17)
mmd_corr <- FA_data[,mmd_vars]
chart.Correlation(mmd_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
mmd.lm0 <- lm(mass_meandiff ~ temp + lux + imperv1000 + 
              pol_shannon.yj + # pol_abundance.yj + pol_richness +
              flo_abundance.yj + flo_shannon, data=FA_data) # flo_richness.yj
summary(mmd.lm0) # Adj-R2: 0.7299; p: 0.0099


# Find best model with stepAIC()
step.mmd.lm <- MASS::stepAIC(mmd.lm0, direction = "both", trace = FALSE)
summary(step.mmd.lm) # Adj-R2: 0.738; p: 0.0039


# Check model's call
step.mmd.lm$call # temp + lux + imperv1000 + pol_shannon.yj


# Check for multi-collinerity
vif(step.mmd.lm) %>% 
  knitr::kable() # All < 3: Pass


# ------------------------------------------------------------------------------


# -- Working with: mass_meanopen ----


# -- Create single lm() models ----
mmo.lm.s0 <- lm(mass_meanopen ~ temp, data=FA_data)
summary(mmo.lm.s0) # RSE: 2.72 ; Adj-R2: -0.075 ; p: 0.698 *

mmo.lm.s1 <- lm(mass_meanopen ~ lux, data=FA_data)
summary(mmo.lm.s1) # RSE: 2.72 ; Adj-R2: -0.07 p: 0.66 *

mmo.lm.s2 <- lm(mass_meanopen ~ imperv100, data=FA_data)
summary(mmo.lm.s2) # RSE: 2.72 ; Adj-R2: -0.073 ; p: 0.6769

mmo.lm.s3 <- lm(mass_meanopen ~ imperv200, data=FA_data)
summary(mmo.lm.s3) # RSE: 2.69 ; Adj-R2: -0.05 ; p: 0.529

mmo.lm.s4 <- lm(mass_meanopen ~ imperv500, data=FA_data)
summary(mmo.lm.s4) # RSE: 2.66 ; Adj-R2: -0.03 ; p: 0.439 

mmo.lm.s5 <- lm(mass_meanopen ~ imperv1000, data=FA_data)
summary(mmo.lm.s5) # RSE: 2.57 ; Adj-R2: 0.043 ; p: 0.24 *

mmo.lm.s6 <- lm(mass_meanopen ~ pol_abundance.yj, data=FA_data)
summary(mmo.lm.s6) # RSE: 2.56 ; Adj-R2: 0.049 ; p: 0.23 *

mmo.lm.s7 <- lm(mass_meanopen ~ pol_richness, data=FA_data)
summary(mmo.lm.s7) # RSE: 2.56 ; Adj-R2: 0.044 ; p: 0.24 *

mmo.lm.s8 <- lm(mass_meanopen ~ pol_shannon.yj, data=FA_data)
summary(mmo.lm.s8) # RSE: 2.45 ; Adj-R2: 0.13 ; p: 0.125 *

mmo.lm.s9 <- lm(mass_meanopen ~ flo_abundance.yj, data=FA_data)
summary(mmo.lm.s9) # RSE: 2.74 ; Adj-R2: -0.089 ; p: 0.89

mmo.lm.s10 <- lm(mass_meanopen ~ flo_richness.yj, data=FA_data)
summary(mmo.lm.s10) # RSE: 2.65 ; Adj-R2: -0.017 ; p: 0.39 *

mmo.lm.s11 <- lm(mass_meanopen ~ flo_shannon, data=FA_data)
summary(mmo.lm.s11) # RSE: 2.7 ; Adj-R2: -0.059 ; p: 0.577 *


# -- Check correlation of dependent and independent vars again ----
mmo_vars <- c(3,6,7,8,9,10,11,12,13,14,15,16,17)
mmo_corr <- FA_data[,mmo_vars]
chart.Correlation(mmo_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
mmo.lm0 <- lm(mass_meanopen ~ temp + lux + imperv1000 + 
              pol_shannon.yj + # pol_abundance.yj + pol_richness +    
              flo_abundance.yj + flo_shannon, data=FA_data) # flo_richness.yj +
summary(mmo.lm0) # Adj-R2: 0.3672; p: 0.185


# Find best model with stepAIC()
step.mmo.lm <- MASS::stepAIC(mmo.lm0, direction = "both", trace = FALSE)
summary(step.mmo.lm) # Adj-R2: 0.479 ; p: 0.031


# Check model's call
step.mmo.lm$call # temp + imp1000 + pol_shannon.yj


# Check for multi-collinerity
vif(step.mmo.lm) %>% 
  knitr::kable() # All < 3: Pass


# ------------------------------------------------------------------------------


# -- Working with: ratio_meandiff ----


# -- Create single lm() models ----
rmd.lm.s0 <- lm(ratio_meandiff ~ temp, data=FA_data)
summary(rmd.lm.s0) # RSE: 0.41 ; Adj-R2: 0.028 ; p: 0.27 *

rmd.lm.s1 <- lm(ratio_meandiff ~ lux, data=FA_data)
summary(rmd.lm.s1) # RSE: 0.43 ; Adj-R2: -0.057 p: 0.56 *

rmd.lm.s2 <- lm(ratio_meandiff ~ imperv100, data=FA_data)
summary(rmd.lm.s2) # RSE: 0.43 ; Adj-R2: -0.073 ; p: 0.68

rmd.lm.s3 <- lm(ratio_meandiff ~ imperv200, data=FA_data)
summary(rmd.lm.s3) # RSE: 0.42 ; Adj-R2: -0.014 ; p: 0.38 *

rmd.lm.s4 <- lm(ratio_meandiff ~ imperv500, data=FA_data)
summary(rmd.lm.s4) # RSE: 0.4 ; Adj-R2: 0.06 ; p: 0.2 *

rmd.lm.s5 <- lm(ratio_meandiff ~ imperv1000, data=FA_data)
summary(rmd.lm.s5) # RSE: 0.38 ; Adj-R2: 0.156 ; p: 0.1 *

rmd.lm.s6 <- lm(ratio_meandiff ~ pol_abundance.yj, data=FA_data)
summary(rmd.lm.s6) # RSE: 0.434 ; Adj-R2: -0.09 ; p: 0.96 

rmd.lm.s7 <- lm(ratio_meandiff ~ pol_richness, data=FA_data)
summary(rmd.lm.s7) # RSE: 0.435 ; Adj-R2: -0.09 ; p: 0.98

rmd.lm.s8 <- lm(ratio_meandiff ~ pol_shannon.yj, data=FA_data)
summary(rmd.lm.s8) # RSE: 0.43 ; Adj-R2: -0.06 ; p: 0.62 *

rmd.lm.s9 <- lm(ratio_meandiff ~ flo_abundance.yj, data=FA_data)
summary(rmd.lm.s9) # RSE: 0.42 ; Adj-R2: 0.0002 ; p: 0.34 *

rmd.lm.s10 <- lm(ratio_meandiff ~ flo_richness.yj, data=FA_data)
summary(rmd.lm.s10) # RSE: 0.426 ; Adj-R2: -0.049 ; p: 0.525 *

rmd.lm.s11 <- lm(ratio_meandiff ~ flo_shannon, data=FA_data)
summary(rmd.lm.s11) # RSE: 0.414 ; Adj-R2: 0.008 ; p: 0.316 *


# -- Check correlation of dependent and independent vars again ----
rmd_vars <- c(4,6,7,8,9,10,11,12,13,14,15,16,17)
rmd_corr <- FA_data[,rmd_vars]
chart.Correlation(rmd_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
rmd.lm0 <- lm(ratio_meandiff ~ temp + lux + imperv1000 + 
              pol_shannon.yj + # pol_abundance.yj + pol_richness +    
              flo_abundance.yj + flo_shannon, data=FA_data) # flo_richness.yj +
summary(rmd.lm0) # Adj-R2: 0.094; p: 0.412


# Double check with stepAIC()
step.rmd.lm <- MASS::stepAIC(rmd.lm0, direction = "both", trace = FALSE)
summary(step.rmd.lm) # Adj-R2: 0.31 ; p: 0.099


# Check model's call
step.rmd.lm$call # imp1000 + pol_shannon.yj + flo_shannon


# Check for multi-collinerity
vif(step.rmd.lm) %>% 
  knitr::kable()


# ------------------------------------------------------------------------------


# -- Working with: ratio_meanopen ----


# -- Create single lm() models ----
rmo.lm.s0 <- lm(ratio_meanopen ~ temp, data=FA_data)
summary(rmo.lm.s0) # RSE: 0.4 ; Adj-R2: -0.039 ; p: 0.477 *

rmo.lm.s1 <- lm(ratio_meanopen ~ lux, data=FA_data)
summary(rmo.lm.s1) # RSE: 0.4 ; Adj-R2: -0.05 ; p: 0.526 *

rmo.lm.s2 <- lm(ratio_meanopen ~ imperv100, data=FA_data)
summary(rmo.lm.s2) # RSE: 0.4 ; Adj-R2: -0.07 ; p: 0.643 *

rmo.lm.s3 <- lm(ratio_meanopen ~ imperv200, data=FA_data)
summary(rmo.lm.s3) # RSE: 0.4 ; Adj-R2: -0.05 ; p: 0.55 *

rmo.lm.s4 <- lm(ratio_meanopen ~ imperv500, data=FA_data)
summary(rmo.lm.s4) # RSE: 0.38 ; Adj-R2: 0.028 ; p: 0.269 *

rmo.lm.s5 <- lm(ratio_meanopen ~ imperv1000, data=FA_data)
summary(rmo.lm.s5) # RSE: 0.37 ; Adj-R2: 0.111 ; p: 0.141 *

rmo.lm.s6 <- lm(ratio_meanopen ~ pol_abundance.yj, data=FA_data)
summary(rmo.lm.s6) # RSE: 0.399 ; Adj-R2: -0.036 ; p: 0.464 *

rmo.lm.s7 <- lm(ratio_meanopen ~ pol_richness, data=FA_data)
summary(rmo.lm.s7) # RSE: 0.4 ; Adj-R2: -0.088 ; p: 0.88

rmo.lm.s8 <- lm(ratio_meanopen ~ pol_shannon.yj, data=FA_data)
summary(rmo.lm.s8) # RSE: 0.4 ; Adj-R2: -0.088 ; p: 0.87

rmo.lm.s9 <- lm(ratio_meanopen ~ flo_abundance.yj, data=FA_data)
summary(rmo.lm.s9) # RSE: 0.4 ; Adj-R2: -0.066 ; p: 0.625 *

rmo.lm.s10 <- lm(ratio_meanopen ~ flo_richness.yj, data=FA_data)
summary(rmo.lm.s10) # RSE: 0.4 ; Adj-R2: -0.08 ; p: 0.76

rmo.lm.s11 <- lm(ratio_meanopen ~ flo_shannon, data=FA_data)
summary(rmo.lm.s11) # RSE: 0.4 ; Adj-R2: -0.05 ; p: 0.52 *


# -- Check correlation of dependent and independent vars again ----
rmo_vars <- c(5,6,7,8,9,10,11,12,13,14,15,16,17)
rmo_corr <- FA_data[,rmo_vars]
chart.Correlation(rmo_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
rmo.lm0 <- lm(ratio_meanopen ~ temp + lux + imperv1000 + 
              pol_shannon.yj +  #  pol_richness + pol_abundance.yj + 
              flo_abundance.yj + flo_shannon, data=FA_data) # flo_richness.yj 
summary(rmo.lm0) # Adj-R2: -0.135; p: 0.625


# Find best model with stepAIC()
step.rmo.lm <- MASS::stepAIC(rmo.lm0, direction = "both", trace = FALSE)
summary(step.rmo.lm) # Adj-R2: 0.23; p: 0.1


# Check model's call
step.rmo.lm$call # temp + imp1000


# Check for multi-collinerity
vif(step.rmo.lm) %>% 
  knitr::kable() # All < 3: Pass


# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------


# -- Best models ----
# + step.mmd.lm
# + step.mmo.lm

# Check summaries of best models
summary(step.mmd.lm)
# ~ temp + lux + imperv1000 + pol_shannon.yj
# Adj-R2: 0.738; p: 0.0039

summary(step.mmo.lm) 
# ~ temp + imperv1000 + pol_shannon.yj
# Adj-R2: 0.48; p: 0.031


# ------------------------------------------------------------------------------


# -- Clean-up environment for the next script ----
rm(list=ls())
