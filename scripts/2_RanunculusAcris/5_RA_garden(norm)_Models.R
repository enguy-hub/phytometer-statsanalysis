# -- Prerequisites ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "PerformanceAnalytics", 
                   "MASS", "car", "effects")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Garden/MyGithub/Phytometer_StatisticalAnalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
ra_path = "./analysis_data/RA/RA_Data_2021_4analysis_garden.xlsx"
RA_data <- read_excel(ra_path, sheet = 1)


# Check structure and summaries of the data
str(RA_data)
summary(RA_data)


# Remove "Non-normal distributed" variables
RA_data <- RA_data %>%
  dplyr::select(-c("urbanclass100", "urbanclass200", "urbanclass500", "urbanclass1000"))
 

# Check correlation between "Response Variables"
resp_vars <- c(2,3,4,5,6,7,8,9,10,11)
resp_corr <- RA_data[,resp_vars]
chart.Correlation(resp_corr, histogram=TRUE)


# Check correlation between "Predictor Variables"
pred_vars <- c(12,13,14,15,16,17,18,19,20,21,22,23)
pred_corr <- RA_data[,pred_vars]
chart.Correlation(pred_corr, histogram=TRUE)


# ------------------------------------------------------------------------------


# ---- Quick Note ----

# -- Only lm() function will be used in this script -- #

# -- !! All variables are normally distributed - Except "flo_shannon" !! -- #
# ! Response (dependent): 
#   + fremass_meandiff, drymass_meandiff, fremass_meanopen, drymass_meanopen,
#   + avgfremass_pseed_meandiff, avgdrymass_pseed_meandiff, 
#   + avgfremass_pseed_meanopen, avgdrymass_pseed_meanopen,
#   + avgfremass_ferseed, avgdrymass_ferseed
# ! Predictor (independent): 
#   + temp, lux, imperv100, imperv200, imperv500, imperv1000
#   + pol_abundance, pol_richness, pol_shannon
#   + flo_abundance, flo_richness, flo_shannon (not norm-dist)

# -- Guide for reading lm() function's output -- #
# RSE: Lower is better
# Adjusted R-squared: Higher is better
# F-statistic p-value: Below 0.05 and more significant (*) is better
# ---------------------------------------------------------------------------- #


# -- Working with: fremass_meandiff ----


# -- Making single lm() models ----
fmmd.lm.s0 <- lm(fremass_meandiff ~ temp, data=RA_data)
summary(fmmd.lm.s0) # RSE: 0.008 ; Adj-R2: -0.05 ; p:: 0.5421 *

fmmd.lm.s1 <- lm(fremass_meandiff ~ lux, data=RA_data)
summary(fmmd.lm.s1) # RSE: 0.008 ; Adj-R2: -0.09 ; p:: 0.985

fmmd.lm.s2 <- lm(fremass_meandiff ~ imperv100, data=RA_data)
summary(fmmd.lm.s2) # RSE: 0.008 ; Adj-R2: -0.09 ; p:: 0.9517

fmmd.lm.s3 <- lm(fremass_meandiff ~ imperv200, data=RA_data)
summary(fmmd.lm.s3) # RSE: 0.008 ; Adj-R2: -0.09 ; p:: 0.9385

fmmd.lm.s4 <- lm(fremass_meandiff ~ imperv500, data=RA_data)
summary(fmmd.lm.s4) # RSE: 0.008 ; Adj-R2: -0.07 ; p:: 0.6701

fmmd.lm.s5 <- lm(fremass_meandiff ~ imperv1000, data=RA_data)
summary(fmmd.lm.s5) # RSE: 0.008 ; Adj-R2: -0.074 ; p:: 0.6874 

fmmd.lm.s6 <- lm(fremass_meandiff ~ pol_abundance, data=RA_data)
summary(fmmd.lm.s6) # RSE: 0.008 ; Adj-R2: -0.07 ; p:: 0.6 *

fmmd.lm.s7 <- lm(fremass_meandiff ~ pol_richness, data=RA_data)
summary(fmmd.lm.s7) # RSE: 0.008 ; Adj-R2: -0.07 ; p:: 0.6532 *

fmmd.lm.s8 <- lm(fremass_meandiff ~ pol_shannon, data=RA_data)
summary(fmmd.lm.s8) # RSE: 0.008 ; Adj-R2: -0.09 ; p:: 0.9273

fmmd.lm.s9 <- lm(fremass_meandiff ~ flo_abundance, data=RA_data)
summary(fmmd.lm.s9) # RSE: 0.008 ; Adj-R2: -0.08 ; p:: 0.7

fmmd.lm.s10 <- lm(fremass_meandiff ~ flo_richness, data=RA_data)
summary(fmmd.lm.s10) # RSE: 0.008 ; Adj-R2: -0.04 ; p:: 0.4805 *

fmmd.lm.s11 <- lm(fremass_meandiff ~ flo_shannon, data=RA_data)
summary(fmmd.lm.s11) # RSE: 0.008 ; Adj-R2: -0.089 ; p:: 0.9135


# -- Check correlation of dependent and independent vars again ----
fmmd_vars <- c(2,12,13,14,15,16,17,18,19,20,21,22,23)
fmmd_corr <- RA_data[,fmmd_vars]
chart.Correlation(fmmd_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
fmmd.lm0 <- lm(fremass_meandiff ~ temp + lux + imperv1000 +
               pol_abundance + pol_richness + pol_shannon + 
               flo_abundance + flo_richness + flo_shannon, data=RA_data)
summary(fmmd.lm0) # Adj-R2: -1.989; p: 0.995


# Find best model with stepAIC()
step.fmmd.lm0 <- MASS::stepAIC(fmmd.lm0, direction = "both", trace = FALSE)
summary(step.fmmd.lm0) # Null model is the best


# Check model's call
step.fmmd.lm0$call # ~ 1


# ------------------------------------------------------------------------------


# -- Working with: fremass_meanopen ----


# -- Create single lm() models ----
fmmo.lm.s0 <- lm(fremass_meanopen ~ temp, data=RA_data)
summary(fmmo.lm.s0) # RSE: 0.008 ; Adj-R2: 0.033 ; p:: 0.26 *

fmmo.lm.s1 <- lm(fremass_meanopen ~ lux, data=RA_data)
summary(fmmo.lm.s1) # RSE: 0.009 ; Adj-R2: -0.09 ; p:: 0.9482

fmmo.lm.s2 <- lm(fremass_meanopen ~ imperv100, data=RA_data)
summary(fmmo.lm.s2) # RSE: 0.009 ; Adj-R2: -0.07 ; p:: 0.6697

fmmo.lm.s3 <- lm(fremass_meanopen ~ imperv200, data=RA_data)
summary(fmmo.lm.s3) # RSE: 0.009 ; Adj-R2: -0.08 ; p:: 0.7531

fmmo.lm.s4 <- lm(fremass_meanopen ~ imperv500, data=RA_data)
summary(fmmo.lm.s4) # RSE: 0.009 ; Adj-R2: -0.036 ; p:: 0.4621 *

fmmo.lm.s5 <- lm(fremass_meanopen ~ imperv1000, data=RA_data)
summary(fmmo.lm.s5) # RSE: 0.009 ; Adj-R2: -0.06 ; p:: 0.5841 *

fmmo.lm.s6 <- lm(fremass_meanopen ~ pol_abundance, data=RA_data)
summary(fmmo.lm.s6) # RSE: 0.009 ; Adj-R2: -0.06 ; p:: 0.55 *

fmmo.lm.s7 <- lm(fremass_meanopen ~ pol_richness, data=RA_data)
summary(fmmo.lm.s7) # RSE: 0.009 ; Adj-R2: -0.089 ; p:: 0.9

fmmo.lm.s8 <- lm(fremass_meanopen ~ pol_shannon, data=RA_data)
summary(fmmo.lm.s8) # RSE: 0.009 ; Adj-R2: -0.031 ; p:: 0.443 *

fmmo.lm.s9 <- lm(fremass_meanopen ~ flo_abundance, data=RA_data)
summary(fmmo.lm.s9) # RSE: 0.009 ; Adj-R2: -0.08 ; p:: 0.77

fmmo.lm.s10 <- lm(fremass_meanopen ~ flo_richness, data=RA_data)
summary(fmmo.lm.s10) # RSE: 0.009 ; Adj-R2: -0.089 ; p:: 0.92

fmmo.lm.s11 <- lm(fremass_meanopen ~ flo_shannon, data=RA_data)
summary(fmmo.lm.s11) # RSE: 0.009 ; Adj-R2: -0.06 ; p:: 0.59 *


# -- Check correlation of dependent and independent vars again ----
fmmo_vars <- c(3,12,13,14,15,16,17,18,19,20,21,22,23)
fmmo_corr <- RA_data[,fmmo_vars]
chart.Correlation(fmmo_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
fmmo.lm0 <- lm(fremass_meanopen ~ temp + lux + imperv1000 +
               pol_abundance + pol_richness + pol_shannon + 
               flo_abundance + flo_richness + flo_shannon, data=RA_data)
summary(fmmo.lm0) # Adj-R2: -0.149; p: 0.666 


# Best model with stepAIC()
step.fmmo.lm0 <- MASS::stepAIC(fmmo.lm0, direction = "both", trace = FALSE)
summary(step.fmmo.lm0) # Null model is the best


# Check model's call
step.fmmo.lm0$call # ~ 1


# ------------------------------------------------------------------------------


# -- Working with: drymass_meandiff ----


# -- Create single lm() models ----
dmmd.lm.s0 <- lm(drymass_meandiff ~ temp, data=RA_data)
summary(dmmd.lm.s0) # RSE: 0.005 ; Adj-R2: -0.082 ; p:: 0.77 

dmmd.lm.s1 <- lm(drymass_meandiff ~ lux, data=RA_data)
summary(dmmd.lm.s1) # RSE: 0.005 ; Adj-R2: -0.09 ; p:: 0.9462

dmmd.lm.s2 <- lm(drymass_meandiff ~ imperv100, data=RA_data)
summary(dmmd.lm.s2) # RSE: 0.0049 ; Adj-R2: 0.065 ; p:: 0.2 *

dmmd.lm.s3 <- lm(drymass_meandiff ~ imperv200, data=RA_data)
summary(dmmd.lm.s3) # RSE: 0.0049 ; Adj-R2: 0.058 ; p:: 0.213 *

dmmd.lm.s4 <- lm(drymass_meandiff ~ imperv500, data=RA_data)
summary(dmmd.lm.s4) # RSE: 0.0052 ; Adj-R2: -0.042 ; p:: 0.4913 *

dmmd.lm.s5 <- lm(drymass_meandiff ~ imperv1000, data=RA_data)
summary(dmmd.lm.s5) # RSE: 0.0053 ; Adj-R2: -0.083 ; p:: 0.7897

dmmd.lm.s6 <- lm(drymass_meandiff ~ pol_abundance, data=RA_data)
summary(dmmd.lm.s6) # RSE: 0.0053 ; Adj-R2: -0.09 ; p:: 0.98

dmmd.lm.s7 <- lm(drymass_meandiff ~ pol_richness, data=RA_data)
summary(dmmd.lm.s7) # RSE: 0.0053 ; Adj-R2: -0.069 ; p:: 0.6463

dmmd.lm.s8 <- lm(drymass_meandiff ~ pol_shannon, data=RA_data)
summary(dmmd.lm.s8) # RSE: 0.0052 ; Adj-R2: -0.053 ; p:: 0.542 *

dmmd.lm.s9 <- lm(drymass_meandiff ~ flo_abundance, data=RA_data)
summary(dmmd.lm.s9) # RSE: 0.0053 ; Adj-R2: -0.072 ; p:: 0.67

dmmd.lm.s10 <- lm(drymass_meandiff ~ flo_richness, data=RA_data)
summary(dmmd.lm.s10) # RSE: 0.0053 ; Adj-R2: -0.06 ; p:: 0.6329

dmmd.lm.s11 <- lm(drymass_meandiff ~ flo_shannon, data=RA_data)
summary(dmmd.lm.s11) # RSE: 0.0051 ; Adj-R2: -0.01 ; p:: 0.3753 *


# -- Check correlation of dependent and independent vars again ----
dmmd_vars <- c(4,12,13,14,15,16,17,18,19,20,21,22,23)
dmmd_corr <- RA_data[,dmmd_vars]
chart.Correlation(dmmd_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
dmmd.lm0 <- lm(drymass_meandiff ~ temp + lux + imperv1000 +
               pol_abundance + pol_richness + pol_shannon + 
               flo_abundance + flo_richness + flo_shannon, data=RA_data)
summary(dmmd.lm0) # Adj-R2: -2.015; p: 0.996 


# Best model with stepAIC()
step.dmmd.lm0 <- MASS::stepAIC(dmmd.lm0, direction = "both", trace = FALSE)
summary(step.dmmd.lm0) # Null model is the best


# Check model's call
step.dmmd.lm0$call # ~ 1


# ------------------------------------------------------------------------------


# -- Working with: drymass_meanopen ----


# -- Create single lm() models ----
dmmo.lm.s0 <- lm(drymass_meanopen ~ temp, data=RA_data)
summary(dmmo.lm.s0) # RSE: 0.006 ; Adj-R2: -0.08 ; p:: 0.8296 

dmmo.lm.s1 <- lm(drymass_meanopen ~ lux, data=RA_data)
summary(dmmo.lm.s1) # RSE: 0.006 ; Adj-R2: -0.08 ; p:: 0.7921

dmmo.lm.s2 <- lm(drymass_meanopen ~ imperv100, data=RA_data)
summary(dmmo.lm.s2) # RSE: 0.006 ; Adj-R2: -0.03 ; p:: 0.4708 *

dmmo.lm.s3 <- lm(drymass_meanopen ~ imperv200, data=RA_data)
summary(dmmo.lm.s3) # RSE: 0.006 ; Adj-R2: -0.04 ; p:: 0.5115 *

dmmo.lm.s4 <- lm(drymass_meanopen ~ imperv500, data=RA_data)
summary(dmmo.lm.s4) # RSE: 0.006 ; Adj-R2: -0.08 ; p:: 0.8092

dmmo.lm.s5 <- lm(drymass_meanopen ~ imperv1000, data=RA_data)
summary(dmmo.lm.s5) # RSE: 0.006 ; Adj-R2: -0.09 ; p:: 0.9256

dmmo.lm.s6 <- lm(drymass_meanopen ~ pol_abundance, data=RA_data)
summary(dmmo.lm.s6) # RSE: 0.006 ; Adj-R2: -0.087 ; p:: 0.98

dmmo.lm.s7 <- lm(drymass_meanopen ~ pol_richness, data=RA_data)
summary(dmmo.lm.s7) # RSE: 0.006 ; Adj-R2: 0.033 ; p:: 0.2584 *

dmmo.lm.s8 <- lm(drymass_meanopen ~ pol_shannon, data=RA_data)
summary(dmmo.lm.s8) # RSE: 0.0056 ; Adj-R2: 0.1485 ; p:: 0.1 *

dmmo.lm.s9 <- lm(drymass_meanopen ~ flo_abundance, data=RA_data)
summary(dmmo.lm.s9) # RSE: 0.006 ; Adj-R2: -0.089 ; p:: 0.55

dmmo.lm.s10 <- lm(drymass_meanopen ~ flo_richness, data=RA_data)
summary(dmmo.lm.s10) # RSE: 0.006 ; Adj-R2: -0.089 ; p:: 0.913

dmmo.lm.s11 <- lm(drymass_meanopen ~ flo_shannon, data=RA_data)
summary(dmmo.lm.s11) # RSE: 0.006 ; Adj-R2: -0.078 ; p:: 0.7324


# -- Check correlation of dependent and independent vars again ----
dmmo_vars <- c(5,12,13,14,15,16,17,18,19,20,21,22,23)
dmmo_corr <- RA_data[,dmmo_vars]
chart.Correlation(dmmo_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
dmmo.lm0 <- lm(drymass_meanopen ~ imperv100 + lux + # temp + 
               pol_abundance + pol_shannon + pol_richness + flo_richness + flo_shannon +
               flo_abundance , data=RA_data)
summary(dmmo.lm0) # Adj-R2: -1.773 ; p:: 0.989


# Best model with stepAIC()
step.dmmo.lm0 <- MASS::stepAIC(dmmo.lm0, direction = "both", trace = FALSE)
summary(step.dmmo.lm0) # Adj-R2: 0.1485; p: 0.106


# Check model's call
step.dmmo.lm0$call # ~ pol_shannon


# ------------------------------------------------------------------------------


# -- lm() models for: avgfremass_pseed_meandiff ----


# -- Create single lm() models ----
afmpsmd.lm.s0 <- lm(avgfremass_pseed_meandiff ~ temp, data=RA_data)
summary(afmpsmd.lm.s0) # RSE: 0.0003 ; Adj-R2: -0.09 ; p:: 0.9381

afmpsmd.lm.s1 <- lm(avgfremass_pseed_meandiff ~ lux, data=RA_data)
summary(afmpsmd.lm.s1) # RSE: 0.0003 ; Adj-R2: -0.06 ; p:: 0.5845 *

afmpsmd.lm.s2 <- lm(avgfremass_pseed_meandiff ~ imperv100, data=RA_data)
summary(afmpsmd.lm.s2) # RSE: 0.0003 ; Adj-R2: 0.01 ; p:: 0.311 *

afmpsmd.lm.s3 <- lm(avgfremass_pseed_meandiff ~ imperv200, data=RA_data)
summary(afmpsmd.lm.s3) # RSE: 0.0003 ; Adj-R2: 0.015 ; p:: 0.2989 *

afmpsmd.lm.s4 <- lm(avgfremass_pseed_meandiff ~ imperv500, data=RA_data)
summary(afmpsmd.lm.s4) # RSE: 0.0003 ; Adj-R2: -0.06 ; p:: 0.637

afmpsmd.lm.s5 <- lm(avgfremass_pseed_meandiff ~ imperv1000, data=RA_data)
summary(afmpsmd.lm.s5) # RSE: 0.0003 ; Adj-R2: -0.088 ; p:: 0.8753

afmpsmd.lm.s6 <- lm(avgfremass_pseed_meandiff ~ pol_abundance, data=RA_data)
summary(afmpsmd.lm.s6) # RSE: 0.0003 ; Adj-R2: -0.08 ; p:: 0.74

afmpsmd.lm.s7 <- lm(avgfremass_pseed_meandiff ~ pol_richness, data=RA_data)
summary(afmpsmd.lm.s7) # RSE: 0.0003 ; Adj-R2: -0.08 ; p:: 0.8486

afmpsmd.lm.s8 <- lm(avgfremass_pseed_meandiff ~ pol_shannon, data=RA_data)
summary(afmpsmd.lm.s8) # RSE: 0.0003 ; Adj-R2: -0.075 ; p:: 0.6948 *

afmpsmd.lm.s9 <- lm(avgfremass_pseed_meandiff ~ flo_abundance, data=RA_data)
summary(afmpsmd.lm.s9) # RSE: 0.0003 ; Adj-R2: -0.045 ; p:: 0.79 *

afmpsmd.lm.s10 <- lm(avgfremass_pseed_meandiff ~ flo_richness, data=RA_data)
summary(afmpsmd.lm.s10) # RSE: 0.0003 ; Adj-R2: -0.085 ; p:: 0.8112

afmpsmd.lm.s11 <- lm(avgfremass_pseed_meandiff ~ flo_shannon, data=RA_data)
summary(afmpsmd.lm.s11) # RSE: 0.0003 ; Adj-R2: -0.036 ; p:: 0.4639 *


# -- Check correlation of dependent and independent vars again ----
afmpsmd_vars <- c(6,12,13,14,15,16,17,18,19,20,21,22,23)
afmpsmd_corr <- RA_data[,afmpsmd_vars]
chart.Correlation(afmpsmd_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
afmpsmd.lm0 <- lm(avgfremass_pseed_meandiff ~ temp + lux + imperv1000 +
                  pol_abundance + pol_richness + pol_shannon + 
                  flo_abundance + flo_richness + flo_shannon, data=RA_data)
summary(afmpsmd.lm0) # Adj-R2: -1.559; p: 0.978


# Best model with stepAIC()
step.afmpsmd.lm0 <- MASS::stepAIC(afmpsmd.lm0, direction = "both", trace = FALSE)
summary(step.afmpsmd.lm0) # Null model is the best


# Check model's call
step.afmpsmd.lm0$call # ~ 1


# ---------------------------------------------------------------------------- #


# -- lm() models for: avgfremass_pseed_meanopen ----


# -- Create single lm() models ----
afmpsmo.lm.s0 <- lm(avgfremass_pseed_meanopen ~ temp, data=RA_data)
summary(afmpsmo.lm.s0) # RSE: 0.0004 ; Adj-R2: -0.027 ; p:: 0.426 *

afmpsmo.lm.s1 <- lm(avgfremass_pseed_meanopen ~ lux, data=RA_data)
summary(afmpsmo.lm.s1) # RSE: 0.0004 ; Adj-R2: -0.078 ; p:: 0.728

afmpsmo.lm.s2 <- lm(avgfremass_pseed_meanopen ~ imperv100, data=RA_data)
summary(afmpsmo.lm.s2) # RSE: 0.0004 ; Adj-R2: -0.08 ; p:: 0.806

afmpsmo.lm.s3 <- lm(avgfremass_pseed_meanopen ~ imperv200, data=RA_data)
summary(afmpsmo.lm.s3) # RSE: 0.0004 ; Adj-R2: -0.08 ; p:: 0.78

afmpsmo.lm.s4 <- lm(avgfremass_pseed_meanopen ~ imperv500, data=RA_data)
summary(afmpsmo.lm.s4) # RSE: 0.0004 ; Adj-R2: -0.09 ; p:: 0.95

afmpsmo.lm.s5 <- lm(avgfremass_pseed_meanopen ~ imperv1000, data=RA_data)
summary(afmpsmo.lm.s5) # RSE: 0.0004 ; Adj-R2: -0.09 ; p:: 0.95

afmpsmo.lm.s6 <- lm(avgfremass_pseed_meanopen ~ pol_abundance, data=RA_data)
summary(afmpsmo.lm.s6) # RSE: 0.0004 ; Adj-R2: -0.06 ; p:: 0.64 *

afmpsmo.lm.s7 <- lm(avgfremass_pseed_meanopen ~ pol_richness, data=RA_data)
summary(afmpsmo.lm.s7) # RSE: 0.0004 ; Adj-R2: -0.02 ; p:: 0.414 *

afmpsmo.lm.s8 <- lm(avgfremass_pseed_meanopen ~ pol_shannon, data=RA_data)
summary(afmpsmo.lm.s8) # RSE: 0.0004 ; Adj-R2: 0.057 ; p:: 0.21 *

afmpsmo.lm.s9 <- lm(avgfremass_pseed_meanopen ~ flo_abundance, data=RA_data)
summary(afmpsmo.lm.s9) # RSE: 0.0004 ; Adj-R2: -0.06 ; p:: 0.9 *

afmpsmo.lm.s10 <- lm(avgfremass_pseed_meanopen ~ flo_richness, data=RA_data)
summary(afmpsmo.lm.s10) # RSE: 0.0004 ; Adj-R2: -0.08 ; p:: 0.74

afmpsmo.lm.s11 <- lm(avgfremass_pseed_meanopen ~ flo_shannon, data=RA_data)
summary(afmpsmo.lm.s11) # RSE: 0.0004 ; Adj-R2: -0.09 ; p:: 0.93


# -- Check correlation of dependent and independent vars again ----
afmpsmo_vars <- c(7,12,13,14,15,16,17,18,19,20,21,22,23)
afmpsmo_corr <- RA_data[,afmpsmo_vars]
chart.Correlation(afmpsmo_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
afmpsmo.lm0 <- lm(avgfremass_pseed_meanopen ~ lux + imperv100 + # temp + 
                  pol_abundance + pol_richness + pol_shannon + 
                  flo_abundance + flo_richness + flo_shannon, data=RA_data)
summary(afmpsmo.lm0) # Adj-R2: -1.594; p: 0.98


# Best model with stepAIC()
step.afmpsmo.lm0 <- MASS::stepAIC(afmpsmo.lm0, direction = "both", trace = FALSE)
summary(step.afmpsmo.lm0) # null is best


# Check model's call
step.afmpsmo.lm0$call # ~ 1


# ---------------------------------------------------------------------------- #


# -- lm() models for: avgdrymass_pseed_meandiff ----


# -- Create single lm() models ----
admpsmd.lm.s0 <- lm(avgdrymass_pseed_meandiff ~ temp, data=RA_data)
summary(admpsmd.lm.s0) # RSE: 0.0002 ; Adj-R2: -0.07 ; p:: 0.65

admpsmd.lm.s1 <- lm(avgdrymass_pseed_meandiff ~ lux, data=RA_data)
summary(admpsmd.lm.s1) # RSE: 0.0002 ; Adj-R2: -0.08 ; p:: 0.78

admpsmd.lm.s2 <- lm(avgdrymass_pseed_meandiff ~ imperv100, data=RA_data)
summary(admpsmd.lm.s2) # RSE: 0.0002 ; Adj-R2: 0.166 ; p:: 0.092 *

admpsmd.lm.s3 <- lm(avgdrymass_pseed_meandiff ~ imperv200, data=RA_data)
summary(admpsmd.lm.s3) # RSE: 0.0002 ; Adj-R2: 0.125 ; p:: 0.127 *

admpsmd.lm.s4 <- lm(avgdrymass_pseed_meandiff ~ imperv500, data=RA_data)
summary(admpsmd.lm.s4) # RSE: 0.0002 ; Adj-R2: 0.032 ; p:: 0.2605 *

admpsmd.lm.s5 <- lm(avgdrymass_pseed_meandiff ~ imperv1000, data=RA_data)
summary(admpsmd.lm.s5) # RSE: 0.0002 ; Adj-R2: -0.06 ; p:: 0.6 *

admpsmd.lm.s6 <- lm(avgdrymass_pseed_meandiff ~ pol_abundance, data=RA_data)
summary(admpsmd.lm.s6) # RSE: 0.0002 ; Adj-R2: -0.09 ; p:: 0.89

admpsmd.lm.s7 <- lm(avgdrymass_pseed_meandiff ~ pol_richness, data=RA_data)
summary(admpsmd.lm.s7) # RSE: 0.0002 ; Adj-R2: 0.026 ; p:: 0.2753 *

admpsmd.lm.s8 <- lm(avgdrymass_pseed_meandiff ~ pol_shannon, data=RA_data)
summary(admpsmd.lm.s8) # RSE: 0.0002 ; Adj-R2: -0.009 ; p:: 0.3677 *

admpsmd.lm.s9 <- lm(avgdrymass_pseed_meandiff ~ flo_abundance, data=RA_data)
summary(admpsmd.lm.s9) # RSE: 0.0002 ; Adj-R2: -0.087 ; p:: 0.5

admpsmd.lm.s10 <- lm(avgdrymass_pseed_meandiff ~ flo_richness, data=RA_data)
summary(admpsmd.lm.s10) # RSE: 0.0002 ; Adj-R2: -0.088 ; p:: 0.88

admpsmd.lm.s11 <- lm(avgdrymass_pseed_meandiff ~ flo_shannon, data=RA_data)
summary(admpsmd.lm.s11) # RSE: 0.0002 ; Adj-R2: 0.034 ; p:: 0.257 *


# -- Check correlation of dependent and independent vars again ----
admpsmd_vars <- c(8,12,13,14,15,16,17,18,19,20,21,22,23)
admpsmd_corr <- RA_data[,admpsmd_vars]
chart.Correlation(admpsmd_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
admpsmd.lm0 <- lm(avgdrymass_pseed_meandiff ~ lux + imperv100 + # temp + 
                  pol_abundance + pol_richness + pol_shannon + 
                  flo_abundance + flo_richness + flo_shannon, data=RA_data)
summary(admpsmd.lm0) # Adj-R2: -0.3516; p: 0.745


# Best model with stepAIC()
step.admpsmd.lm0 <- MASS::stepAIC(admpsmd.lm0, direction = "both", trace = FALSE)
summary(step.admpsmd.lm0) # Adj-R2: 0.3425; p: 0.0493


# Check model's call
step.admpsmd.lm0$call # ~ imperv100 + flo_abundance


# Check multi-collinearity
vif(step.admpsmd.lm0) %>% 
  knitr::kable()


# ---------------------------------------------------------------------------- #


# -- lm() models for: avgdrymass_pseed_meanopen ----


# Create single lm() models
admpsmo.lm.s0 <- lm(avgdrymass_pseed_meanopen ~ temp, data=RA_data)
summary(admpsmo.lm.s0) # RSE: 0.0003 ; Adj-R2: -0.09 ; p:: 0.9395

admpsmo.lm.s1 <- lm(avgdrymass_pseed_meanopen ~ lux, data=RA_data)
summary(admpsmo.lm.s1) # RSE: 0.0003 ; Adj-R2: -0.08 ; p:: 0.764

admpsmo.lm.s2 <- lm(avgdrymass_pseed_meanopen ~ imperv100, data=RA_data)
summary(admpsmo.lm.s2) # RSE: 0.0003 ; Adj-R2: 0.005 ; p:: 0.324 *

admpsmo.lm.s3 <- lm(avgdrymass_pseed_meanopen ~ imperv200, data=RA_data)
summary(admpsmo.lm.s3) # RSE: 0.0003 ; Adj-R2: -0.014 ; p:: 0.3824 *

admpsmo.lm.s4 <- lm(avgdrymass_pseed_meanopen ~ imperv500, data=RA_data)
summary(admpsmo.lm.s4) # RSE: 0.0003 ; Adj-R2: -0.05 ; p:: 0.5369

admpsmo.lm.s5 <- lm(avgdrymass_pseed_meanopen ~ imperv1000, data=RA_data)
summary(admpsmo.lm.s5) # RSE: 0.0003 ; Adj-R2: -0.07 ; p:: 0.6706

admpsmo.lm.s6 <- lm(avgdrymass_pseed_meanopen ~ pol_abundance, data=RA_data)
summary(admpsmo.lm.s6) # RSE: 0.0003 ; Adj-R2: -0.08 ; p:: 0.97

admpsmo.lm.s7 <- lm(avgdrymass_pseed_meanopen ~ pol_richness, data=RA_data)
summary(admpsmo.lm.s7) # RSE: 0.0002 ; Adj-R2: 0.144 ; p:: 0.1 *

admpsmo.lm.s8 <- lm(avgdrymass_pseed_meanopen ~ pol_shannon, data=RA_data)
summary(admpsmo.lm.s8) # RSE: 0.0002 ; Adj-R2: 0.2322 ; p:: 0.054 * => Best

admpsmo.lm.s9 <- lm(avgdrymass_pseed_meanopen ~ flo_abundance, data=RA_data)
summary(admpsmo.lm.s9) # RSE: 0.0003 ; Adj-R2: -0.09 ; p:: 0.62 *

admpsmo.lm.s10 <- lm(avgdrymass_pseed_meanopen ~ flo_richness, data=RA_data)
summary(admpsmo.lm.s10) # RSE: 0.0003 ; Adj-R2: -0.081 ; p:: 0.766

admpsmo.lm.s11 <- lm(avgdrymass_pseed_meanopen ~ flo_shannon, data=RA_data)
summary(admpsmo.lm.s11) # RSE: 0.0003 ; Adj-R2: -0.058 ; p:: 0.575 *


# -- Check correlation of dependent and independent vars again ----
admpsmo_vars <- c(9,12,13,14,15,16,17,18,19,20,21,22,23)
admpsmo_corr <- RA_data[,admpsmo_vars]
chart.Correlation(admpsmo_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
admpsmo.lm0 <- lm(avgdrymass_pseed_meanopen ~ lux + imperv100 + # temp + 
                  pol_abundance + pol_richness + pol_shannon + flo_richness + flo_shannon +
                  flo_abundance , data=RA_data)
summary(admpsmo.lm0) # Adj-R2: -0.64; p: 0.865


# Best model with stepAIC()
step.admpsmo.lm0 <- MASS::stepAIC(admpsmo.lm0, direction = "both", trace = FALSE)
summary(step.admpsmo.lm0) # Adj-R2: 0.232; p: 0.054


# Check model's call
step.admpsmo.lm0$call # ~ pol_shannon


# ---------------------------------------------------------------------------- #


# -- lm() models for: avgfremass_ferseed ----


# Create single lm() models
afmfs.lm.s0 <- lm(avgfremass_ferseed ~ temp, data=RA_data)
summary(afmfs.lm.s0) # RSE: 0.0004 ; Adj-R2: -0.06 ; p:: 0.61

afmfs.lm.s1 <- lm(avgfremass_ferseed ~ lux, data=RA_data)
summary(afmfs.lm.s1) # RSE: 0.0003 ; Adj-R2: 0.01 ; p:: 0.31 *

afmfs.lm.s2 <- lm(avgfremass_ferseed ~ imperv100, data=RA_data)
summary(afmfs.lm.s2) # RSE: 0.0004 ; Adj-R2: -0.08 ; p:: 0.812

afmfs.lm.s3 <- lm(avgfremass_ferseed ~ imperv200, data=RA_data)
summary(afmfs.lm.s3) # RSE: 0.0004 ; Adj-R2: -0.085 ; p:: 0.827

afmfs.lm.s4 <- lm(avgfremass_ferseed ~ imperv500, data=RA_data)
summary(afmfs.lm.s4) # RSE: 0.0004 ; Adj-R2: -0.09 ; p:: 0.954

afmfs.lm.s5 <- lm(avgfremass_ferseed ~ imperv1000, data=RA_data)
summary(afmfs.lm.s5) # RSE: 0.0004 ; Adj-R2: -0.08 ; p:: 0.856

afmfs.lm.s6 <- lm(avgfremass_ferseed ~ pol_abundance, data=RA_data)
summary(afmfs.lm.s6) # RSE: 0.0004 ; Adj-R2: -0.07 ; p:: 0.31 *

afmfs.lm.s7 <- lm(avgfremass_ferseed ~ pol_richness, data=RA_data)
summary(afmfs.lm.s7) # RSE: 0.0004 ; Adj-R2: -0.09 ; p:: 0.99

afmfs.lm.s8 <- lm(avgfremass_ferseed ~ pol_shannon, data=RA_data)
summary(afmfs.lm.s8) # RSE: 0.0004 ; Adj-R2: -0.09 ; p:: 0.97

afmfs.lm.s9 <- lm(avgfremass_ferseed ~ flo_abundance, data=RA_data)
summary(afmfs.lm.s9) # RSE: 0.00039 ; Adj-R2: -0.04 ; p:: 0.94 *

afmfs.lm.s10 <- lm(avgfremass_ferseed ~ flo_richness, data=RA_data)
summary(afmfs.lm.s10) # RSE: 0.00039 ; Adj-R2: -0.04 ; p:: 0.49 *

afmfs.lm.s11 <- lm(avgfremass_ferseed ~ flo_shannon, data=RA_data)
summary(afmfs.lm.s11) # RSE: 0.00039 ; Adj-R2: 0.0009 ; p:: 0.34 *


# -- Check correlation of dependent and independent vars again ----
afmfs_vars <- c(10,12,13,14,15,16,17,18,19,20,21,22,23)
afmfs_corr <- RA_data[,afmfs_vars]
chart.Correlation(afmfs_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
afmfs.lm0 <- lm(avgfremass_ferseed ~ lux + imperv1000 + temp + 
                pol_abundance + pol_shannon + pol_richness + flo_shannon + 
                flo_abundance + flo_richness, data=RA_data)
summary(afmfs.lm0) # Adj-R2: 0.343; p: 0.3


# Best model with stepAIC()
step.afmfs.lm0 <- MASS::stepAIC(afmfs.lm0, direction = "both", trace = FALSE)
summary(step.afmfs.lm0) # Adj-R2: 0.18; p: 0.14


# Check model's call
step.afmfs.lm0$call # ~ lux + pol_abundance


# Check multi-collinearity
vif(step.afmfs.lm0) %>% 
  knitr::kable() # All < 3: Pass


# ---------------------------------------------------------------------------- #


# -- lm() models for: avgdrymass_ferseed ----


# Create single lm() models
admfs.lm.s0 <- lm(avgdrymass_ferseed ~ temp, data=RA_data)
summary(admfs.lm.s0) # RSE: 0.0003 ; Adj-R2: -0.05 ; p:: 0.55 *

admfs.lm.s1 <- lm(avgdrymass_ferseed ~ lux, data=RA_data)
summary(admfs.lm.s1) # RSE: 0.0003 ; Adj-R2: -0.02 ; p:: 0.4 *

admfs.lm.s2 <- lm(avgdrymass_ferseed ~ imperv100, data=RA_data)
summary(admfs.lm.s2) # RSE: 0.0003 ; Adj-R2: 0.18 ; p:: 0.07 *

admfs.lm.s3 <- lm(avgdrymass_ferseed ~ imperv200, data=RA_data)
summary(admfs.lm.s3) # RSE: 0.0003 ; Adj-R2: 0.12 ; p:: 0.13 *

admfs.lm.s4 <- lm(avgdrymass_ferseed ~ imperv500, data=RA_data)
summary(admfs.lm.s4) # RSE: 0.0003 ; Adj-R2: 0.08 ; p:: 0.17 *

admfs.lm.s5 <- lm(avgdrymass_ferseed ~ imperv1000, data=RA_data)
summary(admfs.lm.s5) # RSE: 0.0003 ; Adj-R2: 0.016 ; p:: 0.2955 *

admfs.lm.s6 <- lm(avgdrymass_ferseed ~ pol_abundance, data=RA_data)
summary(admfs.lm.s6) # RSE: 0.0003 ; Adj-R2: -0.08 ; p:: 0.85

admfs.lm.s7 <- lm(avgdrymass_ferseed ~ pol_richness, data=RA_data)
summary(admfs.lm.s7) # RSE: 0.0003 ; Adj-R2: 0.18 ; p:: 0.078 *

admfs.lm.s8 <- lm(avgdrymass_ferseed ~ pol_shannon, data=RA_data)
summary(admfs.lm.s8) # RSE: 0.0003 ; Adj-R2: 0.11 ; p:: 0.14 *

admfs.lm.s9 <- lm(avgdrymass_ferseed ~ flo_abundance, data=RA_data)
summary(admfs.lm.s9) # RSE: 0.0003 ; Adj-R2: -0.08 ; p:: 0.46

admfs.lm.s10 <- lm(avgdrymass_ferseed ~ flo_richness, data=RA_data)
summary(admfs.lm.s10) # RSE: 0.0003 ; Adj-R2: -0.05 ; p:: 0.55 *

admfs.lm.s11 <- lm(avgdrymass_ferseed ~ flo_shannon, data=RA_data)
summary(admfs.lm.s11) # RSE: 0.0003 ; Adj-R2: -0.06 ; p:: 0.64 *


# -- Check correlation of dependent and independent vars again ----
admfs_vars <- c(11,12,13,14,15,16,17,18,19,20,21,22,23)
admfs_corr <- RA_data[,admfs_vars]
chart.Correlation(admfs_corr, histogram=TRUE)


# -- Create multiple regression lm() models ----
admfs.lm0 <- lm(avgdrymass_ferseed ~ lux + imperv100 + # temp + 
                pol_abundance + pol_shannon + pol_richness + flo_richness + flo_shannon +
                flo_abundance, data=RA_data)
summary(admfs.lm0) # Adj-R2: 0.386; p: 0.22


# Best model with stepAIC()
step.admfs.lm0 <- MASS::stepAIC(admfs.lm0, direction = "both", trace = FALSE)
summary(step.admfs.lm0) # Adj-R2: 0.5848; p: 0.03972


# Check model's call
step.admfs.lm0$call # ~ imperv100 + pol_abundance + pol_richness + flo_abundance + flo_richness


# Check multi-collinearity
vif(step.admfs.lm0) %>% 
  knitr::kable() # All < 3: Pass


# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------


# ---- Best models ----
# + step.admpsmd.lm0
# + step.admfs.lm0

summary(step.admpsmd.lm0) 
# ~ imperv100 + flo_abundance
# Adj-R2: 0.34; p: 0.049

summary(step.admfs.lm0) 
# ~ imperv100 + pol_abundance + flo_abundance + flo_richness
# Adj-R2: 0.458; p: 0.0367


# ------------------------------------------------------------------------------


# ---- Clean-up environment for the next script ----
rm(list=ls())