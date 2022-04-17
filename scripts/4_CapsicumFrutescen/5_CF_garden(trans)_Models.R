# ---- Prerequisite ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", "PerformanceAnalytics")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Hien/Garden/MyGithub/Phytometer_StatisticalAnalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
cf_path = "./analysis_data/CF/CF_Data_2021_4analysis_garden_transformed.xlsx"
CF_data <- read_excel(cf_path, sheet = 1)


# Check structure and summaries of the data sets
str(CF_data)
summary(CF_data)


# Remove "Non-normal distributed" variables
CF_data <- CF_data %>%
  dplyr::select(-c("fruimass_meandiff", "fruimass_meanopen", "ratio_meanopen", "flo_abundance", "pol_shannon", 
                   "urbanclass100", "urbanclass200", "urbanclass500", "urbanclass1000"))


# Check correlation of Response variables
resp_vars <- c(2,3,4,5,6,7,8)
resp_cors <- CF_data[,resp_vars]
chart.Correlation(resp_cors, histogram=TRUE)


# Check correlation of Predictor variables
pred_vars <- c(9,10,11,12,13,14,15,16,17,18,19,20)
pred_cors <- CF_data[,pred_vars]
chart.Correlation(pred_cors, histogram=TRUE)


# ------------------------------------------------------------------------------


# ---- Quick Note ----

# ----- lm() function will be used in this script ----- #

# -- !! All variables are normally distributed !! -- #
# ! Response (dependent): 
#   + fruimass_meandiff.yj, fruimass_meanopen.yj
#   + seedmass_meandiff, seedmass_meanopen, mass_pseed_meanopen
#   + ratio_meandiff, ratio_meanopen.yj
# ! Predictor (independent): 
#   + temp, lux, imperv100, imperv200, imperv500, imperv1000
#   + pol_abundance, pol_richness, pol_shannon
#   + flo_abundance.yj.yj, flo_richness, flo_shannon

# -- Guide for reading lm() function's output -- #
# RSE: Lower is better
# Adjusted R-squared: Higher is better
# F-statistic p-value: Below 0.05 and more significant (*) is better
# ------------------------------------------------------------------------------


# -- Working with: fruimass_meandiff.yj ----


# -- Making single lm() models ----
fmmd.lm.s0 <- lm(fruimass_meandiff.yj ~ temp, data=CF_data)
summary(fmmd.lm.s0) # RSE: 0.071 ; Adj-R2: -0.06 ; p: 0.62 

fmmd.lm.s1 <- lm(fruimass_meandiff.yj ~ lux, data=CF_data)
summary(fmmd.lm.s1) # RSE: 0.066 ; Adj-R2: 0.08 ; p: 0.178 *

fmmd.lm.s2 <- lm(fruimass_meandiff.yj ~ imperv100, data=CF_data)
summary(fmmd.lm.s2) # RSE: 0.070 ; Adj-R2: -0.036 ; p: 0.46 *

fmmd.lm.s3 <- lm(fruimass_meandiff.yj ~ imperv200, data=CF_data)
summary(fmmd.lm.s3) # RSE: 0.072 ; Adj-R2: -0.069 ; p: 0.647 

fmmd.lm.s4 <- lm(fruimass_meandiff.yj ~ imperv500, data=CF_data)
summary(fmmd.lm.s4) # RSE: 0.0002 ; Adj-R2: -0.09 ; p: 0.955 

fmmd.lm.s5 <- lm(fruimass_meandiff.yj ~ imperv1000, data=CF_data)
summary(fmmd.lm.s5) # RSE: 0.070 ; Adj-R2: -0.035 ; p: 0.4589 *

fmmd.lm.s6 <- lm(fruimass_meandiff.yj ~ pol_abundance, data=CF_data)
summary(fmmd.lm.s6) # RSE: 0.0722 ; Adj-R2: -0.08 ; p: 0.771

fmmd.lm.s7 <- lm(fruimass_meandiff.yj ~ pol_richness, data=CF_data)
summary(fmmd.lm.s7) # RSE: 0.0725 ; Adj-R2: -0.09 ; p: 0.994 

fmmd.lm.s8 <- lm(fruimass_meandiff.yj ~ pol_shannon, data=CF_data)
summary(fmmd.lm.s8) # RSE: 0.0721 ; Adj-R2: -0.077 ; p: 0.72

fmmd.lm.s9 <- lm(fruimass_meandiff.yj ~ flo_abundance.yj, data=CF_data)
summary(fmmd.lm.s9) # RSE: 0.069 ; Adj-R2: 0.0006 ; p: 0.337 *

fmmd.lm.s10 <- lm(fruimass_meandiff.yj ~ flo_richness, data=CF_data)
summary(fmmd.lm.s10) # RSE: 0.072 ; Adj-R2: -0.082 ; p: 0.776 

fmmd.lm.s11 <- lm(fruimass_meandiff.yj ~ flo_shannon, data=CF_data)
summary(fmmd.lm.s11) # RSE: 0.066 ; Adj-R2: 0.087 ; p: 0.1707 *


# -- Check correlation of dependent and independent vars again ----
fmmd_vars <- c(2,9,10,11,12,13,14,15,16,17,18,19,20)
fmmd_corr <- CF_data[,fmmd_vars]
chart.Correlation(fmmd_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
fmmd.lm0 <- lm(fruimass_meandiff.yj ~ lux + imperv100 + # temp + 
               pol_abundance + pol_shannon + # pol_richness + flo_richness + 
               flo_abundance.yj + flo_shannon, data=CF_data)
summary(fmmd.lm0) # Adj-R2: -0.18; p: 0.654


# Best model with stepAIC()
step.fmmd.lm0 <- MASS::stepAIC(fmmd.lm0, direction="both", trace=F)
summary(step.fmmd.lm0) # Adj-R2: 0.2338; p: 0.106


# Check model's call
step.fmmd.lm0$call # ~ lux + imperv100


# Check multi-collinerity
vif(step.fmmd.lm0) %>% 
  knitr::kable() # All < 3: Pass


# ---------------------------------------------------------------------------- #


# -- Working with: fruimass_meanopen.yj ----


# -- Making single lm() models ----
fmmo.lm.s0 <- lm(fruimass_meanopen.yj ~ temp, data=CF_data)
summary(fmmo.lm.s0) # RSE: 0.58 ; Adj-R2: 0.06 ; p: 0.21 *

fmmo.lm.s1 <- lm(fruimass_meanopen.yj ~ lux, data=CF_data)
summary(fmmo.lm.s1) # RSE: 0.59 ; Adj-R2: 0.03 ; p: 0.259 * 

fmmo.lm.s2 <- lm(fruimass_meanopen.yj ~ imperv100, data=CF_data)
summary(fmmo.lm.s2) # RSE: 0.57 ; Adj-R2: 0.07 ; p: 0.186 *

fmmo.lm.s3 <- lm(fruimass_meanopen.yj ~ imperv200, data=CF_data)
summary(fmmo.lm.s3) # RSE: 0.59 ; Adj-R2: 0.01 ; p: 0.303 *

fmmo.lm.s4 <- lm(fruimass_meanopen.yj ~ imperv500, data=CF_data)
summary(fmmo.lm.s4) # RSE: 0.62 ; Adj-R2: -0.07 ; p: 0.6762 

fmmo.lm.s5 <- lm(fruimass_meanopen.yj ~ imperv1000, data=CF_data)
summary(fmmo.lm.s5) # RSE: 0.628 ; Adj-R2: -0.085 ; p: 0.8143

fmmo.lm.s6 <- lm(fruimass_meanopen.yj ~ pol_abundance, data=CF_data)
summary(fmmo.lm.s6) # RSE: 0.625 ; Adj-R2: -0.075 ; p: 0.7025

fmmo.lm.s7 <- lm(fruimass_meanopen.yj ~ pol_richness, data=CF_data)
summary(fmmo.lm.s7) # RSE: 0.627 ; Adj-R2: -0.083 ; p: 0.7855 

fmmo.lm.s8 <- lm(fruimass_meanopen.yj ~ pol_shannon, data=CF_data)
summary(fmmo.lm.s8) # RSE: 0.622 ; Adj-R2: -0.066 ; p: 0.624

fmmo.lm.s9 <- lm(fruimass_meanopen.yj ~ flo_abundance.yj, data=CF_data)
summary(fmmo.lm.s9) # RSE: 0.615 ; Adj-R2: -0.043 ; p: 0.54 *

fmmo.lm.s10 <- lm(fruimass_meanopen.yj ~ flo_richness, data=CF_data)
summary(fmmo.lm.s10) # RSE: 0.624 ; Adj-R2: -0.073 ; p: 0.676 

fmmo.lm.s11 <- lm(fruimass_meanopen.yj ~ flo_shannon, data=CF_data)
summary(fmmo.lm.s11) # RSE: 0.563 ; Adj-R2: 0.127 ; p: 0.1256 *


# -- Check correlation of dependent and independent vars again ----
fmmo_vars <- c(3,9,10,11,12,13,14,15,16,17,18,19,20)
fmmo_corr <- CF_data[,fmmo_vars]
chart.Correlation(fmmo_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
fmmo.lm0 <- lm(fruimass_meanopen.yj ~ lux + imperv100 + # temp + 
               pol_abundance + pol_shannon + # pol_richness + flo_richness + 
               flo_abundance.yj + flo_shannon, data=CF_data)
summary(fmmo.lm0) # Adj-R2: 0.083; p: 0.418


# Best model with stepAIC()
step.fmmo.lm0 <- MASS::stepAIC(fmmo.lm0, direction="both", trace=FALSE)
summary(step.fmmo.lm0) # Adj-R2: 0.3998; p: 0.0313


# Check model's call
step.fmmo.lm0$call # ~ lux + imperv100


# Check multi-collinerity
vif(step.fmmo.lm0) %>% 
  knitr::kable()

# ---------------------------------------------------------------------------- #


# -- Working with: seedmass_meandiff ----


# -- Making single lm() models ----
smmd.lm.s0 <- lm(seedmass_meandiff ~ temp, data=CF_data)
summary(smmd.lm.s0) # RSE: 0.006 ; Adj-R2: 0.198 ; p: 0.053 *

smmd.lm.s1 <- lm(seedmass_meandiff ~ lux, data=CF_data)
summary(smmd.lm.s1) # RSE: 0.007 ; Adj-R2: -0.02 ; p: 0.299 *

smmd.lm.s2 <- lm(seedmass_meandiff ~ imperv100, data=CF_data)
summary(smmd.lm.s2) # RSE: 0.006 ; Adj-R2: 0.2019 ; p: 0.075 *

smmd.lm.s3 <- lm(seedmass_meandiff ~ imperv200, data=CF_data)
summary(smmd.lm.s3) # RSE: 0.006 ; Adj-R2: 0.2095 ; p: 0.08 *

smmd.lm.s4 <- lm(seedmass_meandiff ~ imperv500, data=CF_data)
summary(smmd.lm.s4) # RSE: 0.007 ; Adj-R2: 0.1387 ; p: 0.13 *

smmd.lm.s5 <- lm(seedmass_meandiff ~ imperv1000, data=CF_data)
summary(smmd.lm.s5) # RSE: 0.007 ; Adj-R2: -0.01 ; p: 0.421 *

smmd.lm.s6 <- lm(seedmass_meandiff ~ pol_abundance, data=CF_data)
summary(smmd.lm.s6) # RSE: 0.007 ; Adj-R2: -0.019 ; p: 0.524 *

smmd.lm.s7 <- lm(seedmass_meandiff ~ pol_richness, data=CF_data)
summary(smmd.lm.s7) # RSE: 0.007 ; Adj-R2: 0.022 ; p: 0.43 *

smmd.lm.s8 <- lm(seedmass_meandiff ~ pol_shannon, data=CF_data)
summary(smmd.lm.s8) # RSE: 0.007 ; Adj-R2: -0.027 ; p: 0.58 *

smmd.lm.s9 <- lm(seedmass_meandiff ~ flo_abundance.yj, data=CF_data)
summary(smmd.lm.s9) # RSE: 0.0069 ; Adj-R2: 0.1558 ; p: 0.37 *

smmd.lm.s10 <- lm(seedmass_meandiff ~ flo_richness, data=CF_data)
summary(smmd.lm.s10) # RSE: 0.007 ; Adj-R2: -0.083 ; p: 0.62 

smmd.lm.s11 <- lm(seedmass_meandiff ~ flo_shannon, data=CF_data)
summary(smmd.lm.s11) # RSE: 0.0065 ; Adj-R2: 0.2372 ; p: 0.06 *


# -- Check correlation of dependent and independent vars again ----
smmd_vars <- c(4,9,10,11,12,13,14,15,16,17,18,19,20)
smmd_corr <- CF_data[,smmd_vars]
chart.Correlation(smmd_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
smmd.lm0 <- lm(seedmass_meandiff ~ lux + imperv100 + temp + 
               pol_abundance + pol_shannon + pol_richness + # flo_richness + 
               flo_abundance.yj + flo_shannon, data=CF_data)
summary(smmd.lm0) # Adj-R2: 0.3; p: 0.33


# Best model with stepAIC()
step.smmd.lm0 <- MASS::stepAIC(smmd.lm0, direction = "both", trace = FALSE)
summary(step.smmd.lm0) # Adj-R2: 0.634; p: 0.0067


# Check model's call
step.smmd.lm0$call # ~ lux + imperv100 + pol_shannon


# Check multi-collinerity
vif(step.smmd.lm0) %>% 
  knitr::kable()


# ---------------------------------------------------------------------------- #


# -- Working with: seedmass_meanopen ----


# -- Making single lm() models ----
smmo.lm.s0 <- lm(seedmass_meanopen ~ temp, data=CF_data)
summary(smmo.lm.s0) # RSE: 0.0199 ; Adj-R2: 0.3674 ; p: 0.016 *

smmo.lm.s1 <- lm(seedmass_meanopen ~ lux, data=CF_data)
summary(smmo.lm.s1) # RSE: 0.0245 ; Adj-R2: 0.0383 ; p: 0.25 *

smmo.lm.s2 <- lm(seedmass_meanopen ~ imperv100, data=CF_data)
summary(smmo.lm.s2) # RSE: 0.0236 ; Adj-R2: 0.11 ; p: 0.143 *

smmo.lm.s3 <- lm(seedmass_meanopen ~ imperv200, data=CF_data)
summary(smmo.lm.s3) # RSE: 0.0230 ; Adj-R2: 0.152 ; p: 0.102 *

smmo.lm.s4 <- lm(seedmass_meanopen ~ imperv500, data=CF_data)
summary(smmo.lm.s4) # RSE: 0.0230 ; Adj-R2: 0.154 ; p: 0.101 *

smmo.lm.s5 <- lm(seedmass_meanopen ~ imperv1000, data=CF_data)
summary(smmo.lm.s5) # RSE: 0.02367 ; Adj-R2: 0.1087 ; p: 0.1448 *

smmo.lm.s6 <- lm(seedmass_meanopen ~ pol_abundance, data=CF_data)
summary(smmo.lm.s6) # RSE: 0.026 ; Adj-R2: -0.076 ; p: 0.7115

smmo.lm.s7 <- lm(seedmass_meanopen ~ pol_richness, data=CF_data)
summary(smmo.lm.s7) # RSE: 0.0261 ; Adj-R2: -0.090 ; p: 0.9535 

smmo.lm.s8 <- lm(seedmass_meanopen ~ pol_shannon, data=CF_data)
summary(smmo.lm.s8) # RSE: 0.0260 ; Adj-R2: -0.081 ; p: 0.7605

smmo.lm.s9 <- lm(seedmass_meanopen ~ flo_abundance.yj, data=CF_data)
summary(smmo.lm.s9) # RSE: 0.0256 ; Adj-R2: -0.047 ; p: 0.513 *

smmo.lm.s10 <- lm(seedmass_meanopen ~ flo_richness, data=CF_data)
summary(smmo.lm.s10) # RSE: 0.0259 ; Adj-R2: -0.068 ; p: 0.644 

smmo.lm.s11 <- lm(seedmass_meanopen ~ flo_shannon, data=CF_data)
summary(smmo.lm.s11) # RSE: 0.0239 ; Adj-R2: 0.087 ; p: 0.171 *


# -- Check correlation of dependent and independent vars again ----
smmo_vars <- c(5,9,10,11,12,13,14,15,16,17,18,19,20)
smmo_corr <- CF_data[,smmo_vars]
chart.Correlation(smmo_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
smmo.lm0 <- lm(seedmass_meanopen ~ lux + imperv100 + temp + 
               pol_abundance + pol_shannon + # pol_richness + flo_richness + 
               flo_abundance.yj + flo_shannon, data=CF_data)
summary(smmo.lm0) # RSE: 0.017 ; Adj-R2: 0.528 ; p: 0.0365 


# Best model with stepAIC()
step.smmo.lm0 <- MASS::stepAIC(smmo.lm0, direction = "both", trace = FALSE)
summary(step.smmo.lm0) # Adj-R2: 0.668; p: 0.0098


# Check model's call
step.smmo.lm0$call # ~ lux + temp + pol_abundance + pol_shannon


# Check multi-collinerity
vif(step.smmo.lm0) %>% 
  knitr::kable()


# ---------------------------------------------------------------------------- #


# -- Working with: ratio_meandiff ----


# -- Making single lm() models ----
rmd.lm.s0 <- lm(ratio_meandiff ~ temp, data=CF_data)
summary(rmd.lm.s0) # RSE: 1.033 ; Adj-R2: 0.081 ; p: 0.1791 *

rmd.lm.s1 <- lm(ratio_meandiff ~ lux, data=CF_data)
summary(rmd.lm.s1) # RSE: 1.02 ; Adj-R2: 0.1044 ; p: 0.1497 *

rmd.lm.s2 <- lm(ratio_meandiff ~ imperv100, data=CF_data)
summary(rmd.lm.s2) # RSE: 0.98 ; Adj-R2: 0.1735 ; p: 0.087 *

rmd.lm.s3 <- lm(ratio_meandiff ~ imperv200, data=CF_data)
summary(rmd.lm.s3) # RSE: 0.92 ; Adj-R2: 0.2603 ; p: 0.043 *

rmd.lm.s4 <- lm(ratio_meandiff ~ imperv500, data=CF_data)
summary(rmd.lm.s4) # RSE: 0.80 ; Adj-R2: 0.4394 ; p: 0.008 *

rmd.lm.s5 <- lm(ratio_meandiff ~ imperv1000, data=CF_data)
summary(rmd.lm.s5) # RSE: 0.91 ; Adj-R2: 0.2854 ; p: 0.034 *

rmd.lm.s6 <- lm(ratio_meandiff ~ pol_abundance, data=CF_data)
summary(rmd.lm.s6) # RSE: 1.126 ; Adj-R2: -0.09 ; p: 0.9589

rmd.lm.s7 <- lm(ratio_meandiff ~ pol_richness, data=CF_data)
summary(rmd.lm.s7) # RSE: 1.122 ; Adj-R2: -0.084 ; p: 0.8 

rmd.lm.s8 <- lm(ratio_meandiff ~ pol_shannon, data=CF_data)
summary(rmd.lm.s8) # RSE: 1.118 ; Adj-R2: -0.075 ; p: 0.6944

rmd.lm.s9 <- lm(ratio_meandiff ~ flo_abundance.yj.yj, data=CF_data)
summary(rmd.lm.s9) # RSE: 1.06 ; Adj-R2: 0.032 ; p: 0.2603 *

rmd.lm.s10 <- lm(ratio_meandiff ~ flo_richness, data=CF_data)
summary(rmd.lm.s10) # RSE: 1.078 ; Adj-R2: 0.0007 ; p: 0.3368 *

rmd.lm.s11 <- lm(ratio_meandiff ~ flo_shannon, data=CF_data)
summary(rmd.lm.s11) # RSE: 1.062 ; Adj-R2: 0.029 ; p: 0.2669 *


# -- Check correlation of dependent and independent vars again ----
rmd_vars <- c(6,9,10,11,12,13,14,15,16,17,18,19,20)
rmd_corr <- CF_data[,rmd_vars]
chart.Correlation(rmd_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
rmd.lm0 <- lm(ratio_meandiff ~ lux + imperv500 + # temp + 
              pol_abundance + pol_shannon + # pol_richness + flo_richness + 
              flo_abundance.yj + flo_shannon, data=CF_data)
summary(rmd.lm0) # Adj-R2: 0.286; p: 0.29


# Best model with stepAIC()
step.rmd.lm0 <- MASS::stepAIC(rmd.lm0, direction = "both", trace = FALSE)
summary(step.rmd.lm0) # Adj-R2: 0.464; p: 0.035


# Check model's call 
step.rmd.lm0$call # ~ lux + imperv500 + pol_abundance


# Check multi-collinerity
vif(step.rmd.lm0) %>% 
  knitr::kable()


# => Best single models
# + rmd.lm.s3: imp200
# + rmd.lm.s4: imp500
# + rmd.lm.s5: imp1000

# ---------------------------------------------------------------------------- #


# -- Working with: ratio_meanopen.yj ----


# -- Making single lm() models ----
rmo.lm.s0 <- lm(ratio_meanopen.yj ~ temp, data=CF_data)
summary(rmo.lm.s0) # RSE: 15.99 ; Adj-R2: 0.2293 ; p: 0.0558 *

rmo.lm.s1 <- lm(ratio_meanopen.yj ~ lux, data=CF_data)
summary(rmo.lm.s1) # RSE: 18.98 ; Adj-R2: -0.086 ; p: 0.832

rmo.lm.s2 <- lm(ratio_meanopen.yj ~ imperv100, data=CF_data)
summary(rmo.lm.s2) # RSE: 18.02 ; Adj-R2: 0.02 ; p: 0.286 *

rmo.lm.s3 <- lm(ratio_meanopen.yj ~ imperv200, data=CF_data)
summary(rmo.lm.s3) # RSE: 17.26 ; Adj-R2: 0.1016 ; p: 0.153 *

rmo.lm.s4 <- lm(ratio_meanopen.yj ~ imperv500, data=CF_data)
summary(rmo.lm.s4) # RSE: 15.45 ; Adj-R2: 0.28 ; p: 0.0363 *

rmo.lm.s5 <- lm(ratio_meanopen.yj ~ imperv1000, data=CF_data)
summary(rmo.lm.s5) # RSE: 14.81 ; Adj-R2: 0.3384 ; p: 0.0217 *

rmo.lm.s6 <- lm(ratio_meanopen.yj ~ pol_abundance, data=CF_data)
summary(rmo.lm.s6) # RSE: 18.42 ; Adj-R2: -0.0234 ; p: 0.4126 *

rmo.lm.s7 <- lm(ratio_meanopen.yj ~ pol_richness, data=CF_data)
summary(rmo.lm.s7) # RSE: 18.94 ; Adj-R2: -0.081 ; p: 0.7636

rmo.lm.s8 <- lm(ratio_meanopen.yj ~ pol_shannon, data=CF_data)
summary(rmo.lm.s8) # RSE: 19.02 ; Adj-R2: -0.09 ; p: 0.9946

rmo.lm.s9 <- lm(ratio_meanopen.yj ~ flo_abundance.yj, data=CF_data)
summary(rmo.lm.s9) # RSE: 18.89 ; Adj-R2: -0.075 ; p: 0.65

rmo.lm.s10 <- lm(ratio_meanopen.yj ~ flo_richness, data=CF_data)
summary(rmo.lm.s10) # RSE: 18.71 ; Adj-R2: -0.056 ; p: 0.59 *

rmo.lm.s11 <- lm(ratio_meanopen.yj ~ flo_shannon, data=CF_data)
summary(rmo.lm.s11) # RSE: 18.91 ; Adj-R2: -0.077 ; p: 0.75


# -- Check correlation of dependent and independent vars again ----
rmo_vars <- c(7,9,10,11,12,13,14,15,16,17,18,19,20)
rmo_corr <- CF_data[,rmo_vars]
chart.Correlation(rmo_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
rmo.lm0 <- lm(ratio_meanopen.yj ~ lux + imperv1000 + # temp + 
              pol_abundance + pol_shannon + # + flo_shannon pol_richness + flo_richness + 
              flo_abundance.yj , data=CF_data)
summary(rmo.lm0) # Adj-R2: 0.294; p: 0.2


# Best model with stepAIC()
step.rmo.lm0 <- MASS::stepAIC(rmo.lm0, direction = "both", trace = FALSE)
summary(step.rmo.lm0) # Adj-R2: 0.4378; p: 0.022


# Check model's call
step.rmo.lm0$call # ~ imperv1000 + pol_abundance


# Check multi-collinerity
vif(step.rmo.lm0) %>% 
  knitr::kable()


# => Best single models
# + rmo.lm.s4: imperv500
# + rmo.lm.s5: imperv1000


# ---------------------------------------------------------------------------- #


# -- Working with: mass_pseed_meanopen ----


# -- Making single lm() models ----
mpsmo.lm.s0 <- lm(mass_pseed_meanopen ~ temp, data=CF_data)
summary(mpsmo.lm.s0) # RSE: 0.0008 ; Adj-R2: -0.040 ; p: 0.481 *

mpsmo.lm.s1 <- lm(mass_pseed_meanopen ~ lux, data=CF_data)
summary(mpsmo.lm.s1) # RSE: 0.00083 ; Adj-R2: -0.089 ; p: 0.893

mpsmo.lm.s2 <- lm(mass_pseed_meanopen ~ imperv100, data=CF_data)
summary(mpsmo.lm.s2) # RSE: 0.00082 ; Adj-R2: -0.055 ; p: 0.554 *

mpsmo.lm.s3 <- lm(mass_pseed_meanopen ~ imperv200, data=CF_data)
summary(mpsmo.lm.s3) # RSE: 0.00082 ; Adj-R2: -0.075 ; p: 0.696

mpsmo.lm.s4 <- lm(mass_pseed_meanopen ~ imperv500, data=CF_data)
summary(mpsmo.lm.s4) # RSE: 0.00083 ; Adj-R2: -0.09 ; p: 0.968

mpsmo.lm.s5 <- lm(mass_pseed_meanopen ~ imperv1000, data=CF_data)
summary(mpsmo.lm.s5) # RSE: 0.00083 ; Adj-R2: -0.089 ; p: 0.8993

mpsmo.lm.s6 <- lm(mass_pseed_meanopen ~ pol_abundance, data=CF_data)
summary(mpsmo.lm.s6) # RSE: 0.00083 ; Adj-R2: -0.086 ; p: 0.8413

mpsmo.lm.s7 <- lm(mass_pseed_meanopen ~ pol_richness, data=CF_data)
summary(mpsmo.lm.s7) # RSE: 0.00082 ; Adj-R2: -0.069 ; p: 0.6497 *

mpsmo.lm.s8 <- lm(mass_pseed_meanopen ~ pol_shannon.yj, data=CF_data)
summary(mpsmo.lm.s8) # RSE: 0.00082 ; Adj-R2: -0.054 ; p: 0.3461 *

mpsmo.lm.s9 <- lm(mass_pseed_meanopen ~ flo_abundance.yj, data=CF_data)
summary(mpsmo.lm.s9) # RSE: 0.00081 ; Adj-R2: -0.038 ; p: 0.47 *

mpsmo.lm.s10 <- lm(mass_pseed_meanopen ~ flo_richness, data=CF_data)
summary(mpsmo.lm.s10) # RSE: 0.00079 ; Adj-R2: -0.0033 ; p: 0.3483 *

mpsmo.lm.s11 <- lm(mass_pseed_meanopen ~ flo_shannon, data=CF_data)
summary(mpsmo.lm.s11) # RSE: 0.00080 ; Adj-R2: -0.0161 ; p: 0.3874 *


# -- Check correlation of dependent and independent vars again ----
mpsmo_vars <- c(8,9,10,11,12,13,14,15,16,17,18,19,20)
mpsmo_corr <- CF_data[,mpsmo_vars]
chart.Correlation(mpsmo_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
mpsmo.lm0 <- lm(mass_pseed_meanopen ~ lux + imperv1000 + temp + 
                pol_abundance + pol_shannon.yj + flo_richness + pol_richness +  
                flo_abundance.yj + flo_shannon, data=CF_data)
summary(mpsmo.lm0) # Adj-R2: 0.113; p: 0.277


# Best model with stepAIC()
step.mpsmo.lm0 <- MASS::stepAIC(mpsmo.lm0, direction = "both", trace = FALSE)
summary(step.mpsmo.lm0) # Adj-R2: 0.113; p: 0.277


# Check model's call
step.mpsmo.lm0$call # ~ temp + pol_shannon + flo_shannon


# Check multi-collinerity
vif(step.mpsmo.lm0) %>% 
  knitr::kable()


# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------


# !!-- Best multiple regression models --!! #
# + step.fmmo.lm0
# + step.smmd.lm0
# + step.smmo.lm0
# + step.rmd.lm0
# + step.rmo.lm0

summary(step.fmmo.lm0)
# ~ lux + imperv100
# Adj-R2: 0.3998; p: 0.031

summary(step.smmd.lm0)
# ~ lux + imperv100 + pol_shannon
# Adj-R2: 0.6344; p: 0.0067

summary(step.smmo.lm0)
# ~ temp + lux + pol_abundance + pol_shannon
# Adj-R2: 0.668; p: 0.0098

summary(step.rmd.lm0)
# ~ lux + imperv500 + pol_abundance
# Adj-R2: 0.464; p: 0.035

summary(step.rmo.lm0)
# ~ imperv1000 + pol_abundance
# Adj-R2: 0.4378; p: 0.022

# ---------------------------------------------------------------------------- #

# !!-- Best single models --!! #
# + rmd.lm.s3
# + rmd.lm.s4
# + rmd.lm.s5
# + rmo.lm.s4
# + rmo.lm.s5

summary(rmd.lm.s3) 
# ~ imperv200; Adj-R2: 0.26; p: 0.043

summary(rmd.lm.s5) 
# ~ imperv1000 ; Adj-R2: 0.28; p: 0.035

summary(rmd.lm.s4)
# ~ imperv500; Adj-R2: 0.4394; p: 0.008 => Best

summary(rmo.lm.s4)
# ~ imperv500; Adj-R2: 0.28; p: 0.036

summary(rmo.lm.s5)
# ~ imperv1000; Adj-R2: 0.34; p: 0.022 => Best


# ------------------------------------------------------------------------------


# ---- Clean-up environment for the next script ----
rm(list=ls())

