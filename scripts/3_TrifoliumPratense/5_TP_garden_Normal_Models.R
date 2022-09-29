# ---- Prerequisite ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "PerformanceAnalytics", 
                   "MASS", "car", "effects")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Hien/Garden/MyGithub/Phytometer_StatisticalAnalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
tp_path = "./analysis_data/TP/TP_Data_2021_4analysis_garden.xlsx"
TP_data <- read_excel(tp_path, sheet = 1)


# Check structure and summaries of the data sets
str(TP_data)
summary(TP_data)


# Remove "Non-normal distributed" variables
TP_data <- TP_data %>%
  dplyr::select(-c("urbanclass100", "urbanclass200", "urbanclass500", "urbanclass1000"))


# Check correlation between Response variables
resp_vars <- c(2,3,4,5,6,7)
resp_cors <- TP_data[,resp_vars]
chart.Correlation(resp_cors, histogram=TRUE)


# Check correlation between Predictor variables
pred_vars <- c(8,9,10,11,12,13,14,15,16,17,18,19)
pred_cors <- TP_data[,pred_vars]
chart.Correlation(pred_cors, histogram=TRUE)


# ------------------------------------------------------------------------------


# ---- Quick Note ----

# ----- lm() function will be used in this script ----- #

# -- !! All variables are normally distributed - Except "flo_shannon" !! -- #
# ! Response (dependent): 
#   + flowmass_meandiff, flowmass_meanopen, 
#   + seedmass_meandiff, seedmass_meanopen,
#   + mass_pseed_meandiff, mass_pseed_meanopen
# ! Predictor (independent): 
#   + temp, lux, imperv100, imperv200, imperv500, imperv1000
#   + pol_abundance, pol_richness, pol_shannon.yj
#   + flo_abundance, flo_richness, flo_shannon (not norm-dist)

# -- Guide for reading lm() function's output -- #
# RSE: Lower is better
# Adjusted R-squared: Higher is better
# F-statistic p-value: Below 0.05 and more significant (*) is better
# ---------------------------------------------------------------------------- #


# -- Working with: flowmass_meandiff ----


# -- Making single lm() models ----
fmmd.lm.s0 <- lm(flowmass_meandiff ~ temp, data=TP_data)
summary(fmmd.lm.s0) # RSE: 0.0174 ; Adj-R2: -0.0127 ; p: 0.3767 * 

fmmd.lm.s1 <- lm(flowmass_meandiff ~ lux, data=TP_data)
summary(fmmd.lm.s1) # RSE: 0.0179 ; Adj-R2: -0.072 ; p: 0.6728

fmmd.lm.s2 <- lm(flowmass_meandiff ~ imperv100, data=TP_data)
summary(fmmd.lm.s2) # RSE: 0.01615 ; Adj-R2: 0.128 ; p: 0.1248 *

fmmd.lm.s3 <- lm(flowmass_meandiff ~ imperv200, data=TP_data)
summary(fmmd.lm.s3) # RSE: 0.01671 ; Adj-R2: 0.066 ; p: 0.2011 *

fmmd.lm.s4 <- lm(flowmass_meandiff ~ imperv500, data=TP_data)
summary(fmmd.lm.s4) # RSE: 0.01637 ; Adj-R2: 0.1039 ; p: 0.1503 *

fmmd.lm.s5 <- lm(flowmass_meandiff ~ imperv1000, data=TP_data)
summary(fmmd.lm.s5) # RSE: 0.0175 ; Adj-R2: -0.035 ; p: 0.4555 *

fmmd.lm.s6 <- lm(flowmass_meandiff ~ pol_abundance, data=TP_data)
summary(fmmd.lm.s6) # RSE: 0.0164 ; Adj-R2: 0.0978 ; p: 0.0799 *

fmmd.lm.s7 <- lm(flowmass_meandiff ~ pol_richness, data=TP_data)
summary(fmmd.lm.s7) # RSE: 0.0140 ; Adj-R2: 0.3376 ; p: 0.02189 *

fmmd.lm.s8 <- lm(flowmass_meandiff ~ pol_shannon, data=TP_data)
summary(fmmd.lm.s8) # RSE: 0.0172 ; Adj-R2: 0.008 ; p: 0.3173 *

fmmd.lm.s9 <- lm(flowmass_meandiff ~ flo_abundance, data=TP_data)
summary(fmmd.lm.s9) # RSE: 0.018 ; Adj-R2: -0.08 ; p: 0.5363

fmmd.lm.s10 <- lm(flowmass_meandiff ~ flo_richness, data=TP_data)
summary(fmmd.lm.s10) # RSE: 0.018 ; Adj-R2: -0.089 ; p: 0.9085 

fmmd.lm.s11 <- lm(flowmass_meandiff ~ flo_shannon, data=TP_data)
summary(fmmd.lm.s11) # RSE: 0.0155 ; Adj-R2: 0.198 ; p: 0.072 *


# -- Check correlation of dependent and independent vars again ----
fmmd_vars <- c(2,8,9,10,11,12,13,14,15,16,17,18,19)
fmmd_corr <- TP_data[,fmmd_vars]
chart.Correlation(fmmd_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
fmmd.lm0 <- lm(flowmass_meandiff ~ temp + lux + imperv100 + 
               pol_abundance + pol_richness + pol_shannon + 
               flo_abundance + flo_richness + flo_shannon, data=TP_data)
summary(fmmd.lm0) # Adj-R2: 0.366; p: 0.348


# Find best model with stepAIC()
step.fmmd.lm0 <- MASS::stepAIC(fmmd.lm0, direction="both", trace = FALSE)
summary(step.fmmd.lm0) # Adj-R2: 0.705; p: 0.0063


# Check model's call
step.fmmd.lm0$call # ~ temp + pol_abundance + pol_richness + flo_richness


# Check multi-collinerity
vif(step.fmmd.lm0) %>% 
  knitr::kable() # All < 3: Pass


# ---------------------------------------------------------------------------- #


# -- Working with: flowmass_meanopen ----


# -- Making single lm() models ----
fmmo.lm.s0 <- lm(flowmass_meanopen ~ temp, data=TP_data)
summary(fmmo.lm.s0) # RSE: 0.01589 ; Adj-R2: -0.077 ; p: 0.72 

fmmo.lm.s1 <- lm(flowmass_meanopen ~ lux, data=TP_data)
summary(fmmo.lm.s1) # RSE: 0.0147 ; Adj-R2: 0.076 ; p: 0.1856 *

fmmo.lm.s2 <- lm(flowmass_meanopen ~ imperv100, data=TP_data)
summary(fmmo.lm.s2) # RSE: 0.0143 ; Adj-R2: 0.128 ; p: 0.1239 *

fmmo.lm.s3 <- lm(flowmass_meanopen ~ imperv200, data=TP_data)
summary(fmmo.lm.s3) # RSE: 0.0145 ; Adj-R2: 0.1035 ; p: 0.1507 *

fmmo.lm.s4 <- lm(flowmass_meanopen ~ imperv500, data=TP_data)
summary(fmmo.lm.s4) # RSE: 0.015 ; Adj-R2: 0.032 ; p: 0.26 *

fmmo.lm.s5 <- lm(flowmass_meanopen ~ imperv1000, data=TP_data)
summary(fmmo.lm.s5) # RSE: 0.0157 ; Adj-R2: -0.062 ; p: 0.5949 *

fmmo.lm.s6 <- lm(flowmass_meanopen ~ pol_abundance, data=TP_data)
summary(fmmo.lm.s6) # RSE: 0.0114 ; Adj-R2: 0.45 ; p: 0.0028 *

fmmo.lm.s7 <- lm(flowmass_meanopen ~ pol_richness, data=TP_data)
summary(fmmo.lm.s7) # RSE: 0.0147 ; Adj-R2: 0.077 ; p: 0.1843 *

fmmo.lm.s8 <- lm(flowmass_meanopen ~ pol_shannon, data=TP_data)
summary(fmmo.lm.s8) # RSE: 0.01583 ; Adj-R2: -0.069 ; p: 0.647

fmmo.lm.s9 <- lm(flowmass_meanopen ~ flo_abundance, data=TP_data)
summary(fmmo.lm.s9) # RSE: 0.0138 ; Adj-R2: 0.1781 ; p: 0.373 *

fmmo.lm.s10 <- lm(flowmass_meanopen ~ flo_richness, data=TP_data)
summary(fmmo.lm.s10) # RSE: 0.0152 ; Adj-R2: 0.007 ; p: 0.3191 *

fmmo.lm.s11 <- lm(flowmass_meanopen ~ flo_shannon, data=TP_data)
summary(fmmo.lm.s11) # RSE: 0.0121 ; Adj-R2: 0.3753 ; p: 0.0153 *


# -- Check correlation of dependent and independent vars again ----
fmmo_vars <- c(3,8,9,10,11,12,13,14,15,16,17,18,19)
fmmo_corr <- TP_data[,fmmo_vars]
chart.Correlation(fmmo_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
fmmo.lm0 <- lm(flowmass_meanopen ~ temp + lux + imperv100 + 
               pol_abundance + pol_richness + pol_shannon + 
               flo_abundance + flo_richness + flo_shannon, data=TP_data)
summary(fmmo.lm0) # Adj-R2: 0.259 ; p: 0.415


# Find best model with stepAIC()
step.fmmo.lm0 <- MASS::stepAIC(fmmo.lm0, direction="both", trace=F)
summary(step.fmmo.lm0) # Adj-R2: 0.7; p: 0.0009


# Check model's call
step.fmmo.lm0$call # ~ pol_abundance + flo_shannon


# Check multi-collinerity
vif(step.fmmo.lm0) %>% 
  knitr::kable() # All < 3: Pass


# ---------------------------------------------------------------------------- #


# -- Working with: seedmass_meandiff ----


# -- Making single lm() models ----
smmd.lm.s0 <- lm(seedmass_meandiff ~ temp, data=TP_data)
summary(smmd.lm.s0) # RSE: 0.0072 ; Adj-R2: -0.09 ; p: 0.9438

smmd.lm.s1 <- lm(seedmass_meandiff ~ lux, data=TP_data)
summary(smmd.lm.s1) # RSE: 0.0071 ; Adj-R2: -0.055 ; p: 0.5531 *

smmd.lm.s2 <- lm(seedmass_meandiff ~ imperv100, data=TP_data)
summary(smmd.lm.s2) # RSE: 0.0071 ; Adj-R2: -0.064 ; p: 0.6128 *

smmd.lm.s3 <- lm(seedmass_meandiff ~ imperv200, data=TP_data)
summary(smmd.lm.s3) # RSE: 0.0071 ; Adj-R2: -0.081 ; p: 0.7607

smmd.lm.s4 <- lm(seedmass_meandiff ~ imperv500, data=TP_data)
summary(smmd.lm.s4) # RSE: 0.0072 ; Adj-R2: -0.09 ; p: 0.9539

smmd.lm.s5 <- lm(seedmass_meandiff ~ imperv1000, data=TP_data)
summary(smmd.lm.s5) # RSE: 0.0072 ; Adj-R2: -0.089 ; p: 0.9173

smmd.lm.s6 <- lm(seedmass_meandiff ~ pol_abundance, data=TP_data)
summary(smmd.lm.s6) # RSE: 0.0068 ; Adj-R2: 0.02357 ; p: 0.1989 *

smmd.lm.s7 <- lm(seedmass_meandiff ~ pol_richness, data=TP_data)
summary(smmd.lm.s7) # RSE: 0.0069 ; Adj-R2: -0.02 ; p: 0.4028 *

smmd.lm.s8 <- lm(seedmass_meandiff ~ pol_shannon, data=TP_data)
summary(smmd.lm.s8) # RSE: 0.0072 ; Adj-R2: -0.079 ; p: 0.7376

smmd.lm.s9 <- lm(seedmass_meandiff ~ flo_abundance, data=TP_data)
summary(smmd.lm.s9) # RSE: 0.0071 ; Adj-R2: -0.066 ; p: 0.7084

smmd.lm.s10 <- lm(seedmass_meandiff ~ flo_richness, data=TP_data)
summary(smmd.lm.s10) # RSE: 0.0071 ; Adj-R2: -0.079 ; p: 0.7405 

smmd.lm.s11 <- lm(seedmass_meandiff ~ flo_shannon, data=TP_data)
summary(smmd.lm.s11) # RSE: 0.0067 ; Adj-R2: 0.046 ; p: 0.2345 *


# -- Check correlation of dependent and independent vars again ----
smmd_vars <- c(4,8,9,10,11,12,13,14,15,16,17,18,19)
smmd_corr <- TP_data[,smmd_vars]
chart.Correlation(smmd_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
smmd.lm0 <- lm(seedmass_meandiff ~ temp + lux + imperv1000 + 
               pol_abundance + pol_shannon + flo_richness + pol_richness + 
               flo_abundance + flo_shannon, data=TP_data)
summary(smmd.lm0) # Adj-R2: -1.681; p: 0.985


# Find best model with stepAIC()
step.smmd.lm0 <- MASS::stepAIC(smmd.lm0, direction="both", trace=F)
summary(step.smmd.lm0) # Adj-R2: 0.067; p: 0.1989


# Check model's call
step.smmd.lm0$call # ~ pol_abundance


# ---------------------------------------------------------------------------- #


# -- Working with: seedmass_meanopen ----


# -- Making single lm() models ----
smmo.lm.s0 <- lm(seedmass_meanopen ~ temp, data=TP_data)
summary(smmo.lm.s0) # RSE: 0.0073 ; Adj-R2: -0.09 ; p: 0.9437

smmo.lm.s1 <- lm(seedmass_meanopen ~ lux, data=TP_data)
summary(smmo.lm.s1) # RSE: 0.0071 ; Adj-R2: -0.052 ; p: 0.5369 *

smmo.lm.s2 <- lm(seedmass_meanopen ~ imperv100, data=TP_data)
summary(smmo.lm.s2) # RSE: 0.0072 ; Adj-R2: -0.06 ; p: 0.5843 *

smmo.lm.s3 <- lm(seedmass_meanopen ~ imperv200, data=TP_data)
summary(smmo.lm.s3) # RSE: 0.0072 ; Adj-R2: -0.079 ; p: 0.7432

smmo.lm.s4 <- lm(seedmass_meanopen ~ imperv500, data=TP_data)
summary(smmo.lm.s4) # RSE: 0.0073 ; Adj-R2: -0.09 ; p: 0.9668

smmo.lm.s5 <- lm(seedmass_meanopen ~ imperv1000, data=TP_data)
summary(smmo.lm.s5) # RSE: 0.0073 ; Adj-R2: -0.089 ; p: 0.922

smmo.lm.s6 <- lm(seedmass_meanopen ~ pol_abundance, data=TP_data)
summary(smmo.lm.s6) # RSE: 0.0069 ; Adj-R2: 0.018 ; p: 0.217 *

smmo.lm.s7 <- lm(seedmass_meanopen ~ pol_richness, data=TP_data)
summary(smmo.lm.s7) # RSE: 0.007 ; Adj-R2: -0.021 ; p: 0.4056 *

smmo.lm.s8 <- lm(seedmass_meanopen ~ pol_shannon, data=TP_data)
summary(smmo.lm.s8) # RSE: 0.0072 ; Adj-R2: -0.08 ; p: 0.7526

smmo.lm.s9 <- lm(seedmass_meanopen ~ flo_abundance, data=TP_data)
summary(smmo.lm.s9) # RSE: 0.0072 ; Adj-R2: -0.068 ; p: 0.7344

smmo.lm.s10 <- lm(seedmass_meanopen ~ flo_richness, data=TP_data)
summary(smmo.lm.s10) # RSE: 0.0073 ; Adj-R2: -0.078 ; p: 0.7267 

smmo.lm.s11 <- lm(seedmass_meanopen ~ flo_shannon, data=TP_data)
summary(smmo.lm.s11) # RSE: 0.0068 ; Adj-R2: 0.032 ; p: 0.2602 *


# -- Check correlation of dependent and independent vars again ----
smmo_vars <- c(5,8,9,10,11,12,13,14,15,16,17,18,19)
smmo_corr <- TP_data[,smmo_vars]
chart.Correlation(smmo_corr, histogram=TRUE)


shapiro.test(TP_data$seedmass_meanopen)

# -- Create multiple regression lm() models ----
smmo.lm0 <- lm(seedmass_meanopen ~ lux + imperv100 + # temp + 
               pol_abundance + pol_shannon + flo_richness + pol_richness + 
               flo_abundance + flo_shannon, data=TP_data)
summary(smmo.lm0) # Adj-R2: -1.86; p: 0.99


# Find best model with stepAIC()
step.smmo.lm0 <- MASS::stepAIC(smmo.lm0, direction = "both", trace = FALSE)
summary(step.smmo.lm0) # Null model is best


# Check model's call
step.smmo.lm0$call # ~ 1


# ---------------------------------------------------------------------------- #


# -- Working with: mass_pseed_meandiff ----


# -- Making single lm() models ----
mpsmd.lm.s0 <- lm(mass_pseed_meandiff ~ temp, data=TP_data)
summary(mpsmd.lm.s0) # RSE: 0.00012 ; Adj-R2: 0.011 ; p: 0.3087 *

mpsmd.lm.s1 <- lm(mass_pseed_meandiff ~ lux, data=TP_data)
summary(mpsmd.lm.s1) # RSE: 0.00012 ; Adj-R2: -0.06 ; p: 0.6051

mpsmd.lm.s2 <- lm(mass_pseed_meandiff ~ imperv100, data=TP_data)
summary(mpsmd.lm.s2) # RSE: 0.00012 ; Adj-R2: -0.028 ; p: 0.4299 *

mpsmd.lm.s3 <- lm(mass_pseed_meandiff ~ imperv200, data=TP_data)
summary(mpsmd.lm.s3) # RSE: 0.00012 ; Adj-R2: -0.026 ; p: 0.4236 *

mpsmd.lm.s4 <- lm(mass_pseed_meandiff ~ imperv500, data=TP_data)
summary(mpsmd.lm.s4) # RSE: 0.00012 ; Adj-R2: -0.016 ; p: 0.3879 *

mpsmd.lm.s5 <- lm(mass_pseed_meandiff ~ imperv1000, data=TP_data)
summary(mpsmd.lm.s5) # RSE: 0.00012 ; Adj-R2: -0.088 ; p: 0.8686

mpsmd.lm.s6 <- lm(mass_pseed_meandiff ~ pol_abundance, data=TP_data)
summary(mpsmd.lm.s6) # RSE: 0.00012 ; Adj-R2: -0.066 ; p: 0.605

mpsmd.lm.s7 <- lm(mass_pseed_meandiff ~ pol_richness, data=TP_data)
summary(mpsmd.lm.s7) # RSE: 0.00012 ; Adj-R2: -0.081 ; p: 0.7586

mpsmd.lm.s8 <- lm(mass_pseed_meandiff ~ pol_shannon, data=TP_data)
summary(mpsmd.lm.s8) # RSE: 0.00012 ; Adj-R2: -0.049 ; p: 0.5261 *

mpsmd.lm.s9 <- lm(mass_pseed_meandiff ~ flo_abundance, data=TP_data)
summary(mpsmd.lm.s9) # RSE: 0.00012 ; Adj-R2: -0.09 ; p: 0.7469

mpsmd.lm.s10 <- lm(mass_pseed_meandiff ~ flo_richness, data=TP_data)
summary(mpsmd.lm.s10) # RSE: 0.00012 ; Adj-R2: -0.084 ; p: 0.7977 

mpsmd.lm.s11 <- lm(mass_pseed_meandiff ~ flo_shannon, data=TP_data)
summary(mpsmd.lm.s11) # RSE: 0.00012 ; Adj-R2: -0.086 ; p: 0.8385


# -- Check correlation of dependent and independent vars again ----
mpsmd_vars <- c(6,8,9,10,11,12,13,14,15,16,17,18,19)
mpsmd_corr <- TP_data[,mpsmd_vars]
chart.Correlation(mpsmd_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
mpsmd.lm0 <- lm(mass_pseed_meandiff ~ temp + lux + imperv1000 + 
                pol_abundance + pol_shannon + # pol_richness + flo_richness +
                flo_abundance + flo_shannon, data=TP_data)
summary(mpsmd.lm0) # Adj-R2: -0.786; p: 0.95


# Find best model with stepAIC()
step.mpsmd.lm0 <- MASS::stepAIC(mpsmd.lm0, direction="both", trace=F)
summary(step.mpsmd.lm0) # Null model is best


# Check model's call
step.mpsmd.lm0$call # ~ 1


# ---------------------------------------------------------------------------- #


# -- Working with: mass_pseed_meanopen ----


# -- Making single lm() models ----
mpsmo.lm.s0 <- lm(mass_pseed_meanopen ~ temp, data=TP_data)
summary(mpsmo.lm.s0) # RSE: 0.00013 ; Adj-R2: 0.098 ; p: 0.1572 *

mpsmo.lm.s1 <- lm(mass_pseed_meanopen ~ lux, data=TP_data)
summary(mpsmo.lm.s1) # RSE: 0.00013 ; Adj-R2: 0.0487 ; p: 0.2301 *

mpsmo.lm.s2 <- lm(mass_pseed_meanopen ~ imperv100, data=TP_data)
summary(mpsmo.lm.s2) # RSE: 0.00012 ; Adj-R2: 0.2419 ; p: 0.05 *

mpsmo.lm.s3 <- lm(mass_pseed_meanopen ~ imperv200, data=TP_data)
summary(mpsmo.lm.s3) # RSE: 0.00013 ; Adj-R2: 0.129 ; p: 0.1236 *

mpsmo.lm.s4 <- lm(mass_pseed_meanopen ~ imperv500, data=TP_data)
summary(mpsmo.lm.s4) # RSE: 0.00014 ; Adj-R2: -0.01 ; p: 0.3722 *

mpsmo.lm.s5 <- lm(mass_pseed_meanopen ~ imperv1000, data=TP_data)
summary(mpsmo.lm.s5) # RSE: 0.00014 ; Adj-R2: -0.074 ; p: 0.6855

mpsmo.lm.s6 <- lm(mass_pseed_meanopen ~ pol_abundance, data=TP_data)
summary(mpsmo.lm.s6) # RSE: 0.00014 ; Adj-R2: -0.0079 ; p: 0.32 *

mpsmo.lm.s7 <- lm(mass_pseed_meanopen ~ pol_richness, data=TP_data)
summary(mpsmo.lm.s7) # RSE: 0.00014 ; Adj-R2: -0.09 ; p: 0.98

mpsmo.lm.s8 <- lm(mass_pseed_meanopen ~ pol_shannon, data=TP_data)
summary(mpsmo.lm.s8) # RSE: 0.00014 ; Adj-R2: -0.09 ; p: 0.9328

mpsmo.lm.s9 <- lm(mass_pseed_meanopen ~ flo_abundance, data=TP_data)
summary(mpsmo.lm.s9) # RSE: 0.00014 ; Adj-R2: -0.09 ; p: 0.814

mpsmo.lm.s10 <- lm(mass_pseed_meanopen ~ flo_richness, data=TP_data)
summary(mpsmo.lm.s10) # RSE: 0.00014 ; Adj-R2: -0.047 ; p: 0.5157 * 

mpsmo.lm.s11 <- lm(mass_pseed_meanopen ~ flo_shannon, data=TP_data)
summary(mpsmo.lm.s11) # RSE: 0.00013 ; Adj-R2: 0.1387 ; p: 0.1148 *


# -- Check correlation of dependent and independent vars again ----
mpsmo_vars <- c(7,8,9,10,11,12,13,14,15,16,17,18,19)
mpsmo_corr <- TP_data[,mpsmo_vars]
chart.Correlation(mpsmo_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
mpsmo.lm0 <- lm(mass_pseed_meanopen ~ temp + lux + imperv100 + 
                pol_abundance + pol_shannon + flo_richness + pol_richness + # flo_shannon + 
                flo_abundance, data=TP_data)
summary(mpsmo.lm0) # Adj-R2: -0.31; p: 0.744


# Find best model with stepAIC()
step.mpsmo.lm0 <- MASS::stepAIC(mpsmo.lm0, direction="both", trace=F)
summary(step.mpsmo.lm0) # Adj-R2: 0.434; p: 0.1414


# Check multi-collinerity
vif(step.mpsmo.lm0) %>% 
  knitr::kable() # All < 3: Pass


# Check model's call
step.mpsmo.lm0$call # ~ imperv100 + pol_abundance + pol_shannon + pol_richness + flo_richness + flo_abundance


# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------


# ---- Best models ----
# + step.fmmd.lm0
# + step.fmmo.lm0


summary(step.fmmd.lm0)
# ~ temp + pol_abundance + pol_richness + flo_richness
# Adj-R2: 0.7; p: 0.0063


summary(step.fmmo.lm0)
# ~ pol_abundance + flo_shannon
# Adj-R2: 0.7; p: 0.0009


# ------------------------------------------------------------------------------


# ---- Clean-up environment for the next script ----
rm(list=ls())
