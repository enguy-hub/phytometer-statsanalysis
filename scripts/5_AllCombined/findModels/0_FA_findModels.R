# ---- Prerequisites ----

# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
setwd(pdir)

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", 
                   "jtools", "PerformanceAnalytics", "sjPlot", "ggpubr")
lapply(list_packages, library, character.only = TRUE)

# Set path and read the data
fa_path = "./analysis_data/FA/FA_Data_2021_4analysis_garden_transformed.xlsx"
FA_data <- read_excel(fa_path, sheet=1)

# Remove "Non-normal distributed" variables
FA_data <- FA_data %>%
  dplyr::select(-c("urbanclass100", "urbanclass200", "urbanclass500", "urbanclass1000"))


# ------------------------------------------------------------------------------

# -- Function: fitted_vs_actual ----
fitted_vs_actual <- function(models, df_respvar, title){
  ggplot(models, aes(x=df_respvar, y=fit)) +
    geom_point()+
    geom_smooth(aes(color = 'model')) +
    geom_line(aes(x=seq(min(df_respvar),max(df_respvar), length.out = 13), 
                  y=seq(min(df_respvar),max(df_respvar), length.out = 13), 
                  color = 'ideal'))+
    labs(x= "actual values", y= "fitted values") + 
    scale_color_manual('linear relation', values = c('red', 'blue')) +
    theme(legend.position = c(0.25, 0.8)) +
    ggtitle(title)
}

# ---- Function: PRESS - predicted residual sums of squares
PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}

# ---- Function: pred_r_squared
pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-PRESS(linear.model)/(tss)
  
  return(pred.r.squared)
}


# ------------------------------------------------------------------------------
# --------------- Find model for: FA_norm | mass_meanopen ----------------------
# ------------------------------------------------------------------------------

# Create new dataframe, which remove "non-related" vars
FA_mmo <- FA_data %>%
  dplyr::select(-c("mass_meandiff", "ratio_meandiff", "ratio_meanopen",
                   # "imperv200", "imperv500",
                   "pol_abundance", "pol_abundance.yj",
                   "flo_abundance", "flo_abundance.yj"))

# Check correlation of dependent and independent vars again
mmo_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14)
mmo_corr <- FA_mmo[,mmo_vars]
chart.Correlation(mmo_corr, histogram=TRUE)

# -------------------------------------

# Model-0: Temp + Imperv1000 + pol_ric + flo_ric
norm_mmo.lm0 <- lm(mass_meanopen ~ temp + imperv1000 + 
                   pol_richness + flo_richness
                   , data=FA_mmo)
summ(norm_mmo.lm0) # Adj.R2: 0.23; p: 0.20
pred_r_squared(norm_mmo.lm0) # -0.3388

# Initial model
norm_mmo.lm0.init <- MASS::stepAIC(norm_mmo.lm0, direction="both", trace=F)
summ(norm_mmo.lm0.init, digits= 4) # Adj-R2: 0.3124; p: 0.0997
pred_r_squared(norm_mmo.lm0.init) # -0.206

# Interaction model
norm_mmo.lm0.inter <- stepAIC(norm_mmo.lm0, ~.^2, trace=F)
summ(norm_mmo.lm0.inter,digits=4) # Adj-R2: 0.55; p: 0.03
pred_r_squared(norm_mmo.lm0.inter) # -1.6

# -------------------------------------

# Model-1: Temp + Imperv1000 + pol_sha + flo_sha
norm_mmo.lm1 <- lm(mass_meanopen ~ temp + imperv1000 +
                     pol_shannon + flo_shannon
                   , data=FA_mmo)
summ(norm_mmo.lm1) # Adj-R2: 0.38; p: 0.10
pred_r_squared(norm_mmo.lm1) # -0.5518

# Initial model
norm_mmo.lm1.init <- MASS::stepAIC(norm_mmo.lm1, direction="both", trace=F)
summ(norm_mmo.lm1.init, digits= 4) # Adj-R2: 0.3793; p: 0.0981
pred_r_squared(norm_mmo.lm1.init) # -0.5518

# !!! Interaction model
norm_mmo.lm1.inter <- stepAIC(norm_mmo.lm1, ~.^2, trace=F)
summ(norm_mmo.lm1.inter,digits=4) # Adj-R2: 0.6824; p: 0.0168
pred_r_squared(norm_mmo.lm1.inter) # 0.2268

# -------------------------------------

# Model-2: Lux + Imperv100 + pol_ric + flo_ric
norm_mmo.lm2 <- lm(mass_meanopen ~ lux + imperv100 +
                     pol_richness + flo_richness
                   , data=FA_mmo)
summ(norm_mmo.lm2) # Adj-R2: -0.19; p: 0.73
pred_r_squared(norm_mmo.lm2) # -1.097

# Initial model
norm_mmo.lm2.init <- MASS::stepAIC(norm_mmo.lm2, direction="both", trace=F)
summ(norm_mmo.lm2.init, digits= 4) # NULL

# Interaction model
norm_mmo.lm2.inter <- stepAIC(norm_mmo.lm2, ~.^2, trace=F)
summ(norm_mmo.lm2.inter,digits=4) # Adj-R2: 0.16; p: 0.2236
pred_r_squared(norm_mmo.lm2.inter) # -0.494

# -------------------------------------

# Model-3: Lux + Imperv100 + pol_sha + flo_sha
norm_mmo.lm3 <- lm(mass_meanopen ~ lux + imperv100 +
                     pol_shannon + flo_shannon
                   , data=FA_mmo)
summ(norm_mmo.lm3) # Adj-R2: -0.28; p: 0.84
pred_r_squared(norm_mmo.lm3) # -1.454

# Initial model
norm_mmo.lm3.init <- MASS::stepAIC(norm_mmo.lm3, direction="both", trace=F)
summ(norm_mmo.lm3.init, digits= 4) # NULL

# Interaction model
norm_mmo.lm3.inter <- stepAIC(norm_mmo.lm3, ~.^2, trace=F)
summ(norm_mmo.lm3.inter,digits=4) # NULL

# -------------------------------------

# Model-4: Lux + Imperv200 + pol_ric + flo_ric
norm_mmo.lm4 <- lm(mass_meanopen ~ lux + imperv200 +
                   pol_richness + flo_richness
                   , data=FA_mmo)
summ(norm_mmo.lm4) # Adj-R2: -0.16; p: 0.68
pred_r_squared(norm_mmo.lm4) # -0.95

# Initial model
norm_mmo.lm4.init <- MASS::stepAIC(norm_mmo.lm4, direction="both", trace=F)
summ(norm_mmo.lm4.init, digits= 4) # NULL

# Interaction model
norm_mmo.lm4.inter <- stepAIC(norm_mmo.lm4, ~.^2, trace=F)
summ(norm_mmo.lm4.inter,digits=4) # Adj-R2: 0.2656; p: 0.2185

# -------------------------------------

# Model-5: Lux + Imperv200 + pol_sha + flo_sha
norm_mmo.lm5 <- lm(mass_meanopen ~ lux + imperv200 +
                   pol_shannon + flo_shannon
                   , data=FA_mmo)
summ(norm_mmo.lm5) # Adj-R2: -0.26; p: 0.82
pred_r_squared(norm_mmo.lm5) # -1.44

# Initial model
norm_mmo.lm5.init <- MASS::stepAIC(norm_mmo.lm5, direction="both", trace=F)
summ(norm_mmo.lm5.init, digits= 4) # NULL

# Interaction model
norm_mmo.lm5.inter <- stepAIC(norm_mmo.lm5, ~.^2, trace=F)
summ(norm_mmo.lm3.inter,digits=4) # NULL

# -------------------------------------

# Model-6: Lux + Imperv500 + pol_ric + flo_ric
norm_mmo.lm6 <- lm(mass_meanopen ~ lux + imperv500 +
                     pol_richness + flo_richness
                   , data=FA_mmo)
summ(norm_mmo.lm6) # Adj-R2: -0.12; p: 0.62
pred_r_squared(norm_mmo.lm6) # -0.793

# Initial model
norm_mmo.lm6.init <- MASS::stepAIC(norm_mmo.lm6, direction="both", trace=F)
summ(norm_mmo.lm6.init, digits= 4) # NULL

# Interaction model
norm_mmo.lm6.inter <- stepAIC(norm_mmo.lm6, ~.^2, trace=F)
summ(norm_mmo.lm6.inter,digits=4) # Adj-R2: 0.1719; p: 0.2119

# -------------------------------------

# Model-7: Lux + Imperv200 + pol_sha + flo_sha
norm_mmo.lm7 <- lm(mass_meanopen ~ lux + imperv500 +
                   pol_shannon + flo_shannon
                   , data=FA_mmo)
summ(norm_mmo.lm7) # Adj-R2: -0.20; p: 0.74
pred_r_squared(norm_mmo.lm7) # -1.4976

# Initial model
norm_mmo.lm7.init <- MASS::stepAIC(norm_mmo.lm7, direction="both", trace=F)
summ(norm_mmo.lm7.init, digits= 4) # NULL

# Interaction model
norm_mmo.lm7.inter <- stepAIC(norm_mmo.lm7, ~.^2, trace=F)
summ(norm_mmo.lm7.inter,digits=4) # NULL


# ------------------------------------------------------------------------------
# --------------- Find model for: FA_trans | mass_meanopen ---------------------
# ------------------------------------------------------------------------------

# Model-0: Temp + Imperv1000 + pol_ric + flo_ric.yj
trans_mmo.lm0 <- lm(mass_meanopen ~ temp + imperv1000 +
                     pol_richness + flo_richness.yj
                   , data=FA_mmo)
summ(trans_mmo.lm0) # Adj-R2: 0.24; p: 0.20
pred_r_squared(trans_mmo.lm0) # -0.3712

# Initial model
trans_mmo.lm0.init <- MASS::stepAIC(trans_mmo.lm0, direction="both", trace=F)
summ(trans_mmo.lm0.init, digits= 4) # Adj-R2: 0.3124; p: 0.0997
pred_r_squared(trans_mmo.lm0.init) # -0.206

# Interaction model
trans_mmo.lm0.inter <- stepAIC(trans_mmo.lm0, ~.^2, trace=F)
summ(trans_mmo.lm0.inter,digits=4) # Adj-R2: 0.55; p: 0.03
pred_r_squared(trans_mmo.lm0.inter) # -1.6

# -------------------------------------

# Model-1: Temp + Imperv1000 + pol_sha.yj + flo_sha
trans_mmo.lm1 <- lm(mass_meanopen ~ temp + imperv1000 +
                     pol_shannon.yj + flo_shannon 
                   , data=FA_mmo)
summ(trans_mmo.lm1) # Adj-R2: 0.46; p: 0.06
pred_r_squared(trans_mmo.lm1) # -0.524

# Initial model
trans_mmo.lm1.init <- MASS::stepAIC(trans_mmo.lm1, direction="both", trace=F)
summ(trans_mmo.lm1.init, digits= 4) # Adj-R2: 0.479; p: 0.031
pred_r_squared(trans_mmo.lm1.init) # -0.016

# !!! Interaction model
trans_mmo.lm1.inter <- stepAIC(trans_mmo.lm1, ~.^2, trace=F)
summ(trans_mmo.lm1.inter,digits=4) # Adj-R2: 0.7785; p: 0.0021
pred_r_squared(trans_mmo.lm1.inter) # 0.4768

# -------------------------------------

# Model-2: Lux + Imperv100 + pol_ric + flo_ric.yj
trans_mmo.lm2 <- lm(mass_meanopen ~ lux + imperv100 +
                      pol_richness + flo_richness.yj
                    , data=FA_mmo)
summ(trans_mmo.lm2) # Adj-R2: -0.18; p: 0.72
pred_r_squared(trans_mmo.lm2) # -1.1455

# Initial model
trans_mmo.lm2.init <- MASS::stepAIC(trans_mmo.lm2, direction="both", trace=F)
summ(trans_mmo.lm2.init, digits= 4) # NULL

# Interaction model
trans_mmo.lm2.inter <- stepAIC(trans_mmo.lm2, ~.^2, trace=F)
summ(trans_mmo.lm2.inter,digits=4) # Adj-R2: 0.1605; p: 0.2236
pred_r_squared(trans_mmo.lm2.inter) # -0.494

# -------------------------------------

# Model-3: Lux + Imperv100 + pol_sha.yj + flo_sha
trans_mmo.lm3 <- lm(mass_meanopen ~ lux + imperv100 +
                      pol_shannon.yj + flo_shannon 
                    , data=FA_mmo)
summ(trans_mmo.lm3) # Adj-R2: -0.17; p: 0.7
pred_r_squared(trans_mmo.lm3) # -1.61

# Initial model
trans_mmo.lm3.init <- MASS::stepAIC(trans_mmo.lm3, direction="both", trace=F)
summ(trans_mmo.lm3.init, digits= 4) # Adj-R2: 0.1273; p: 0.1254
pred_r_squared(trans_mmo.lm3.init) # -0.219

# Interaction model
trans_mmo.lm3.inter <- stepAIC(trans_mmo.lm3, ~.^2, trace=F)
summ(trans_mmo.lm3.inter,digits=4) # Adj-R2: 0.216; p: 0.17
pred_r_squared(trans_mmo.lm3.inter) # -0.591

# -------------------------------------

# Model-4: Lux + Imperv200 + pol_ric + flo_ric
trans_mmo.lm4 <- lm(mass_meanopen ~ lux + imperv200 +
                    pol_richness + flo_richness.yj
                   , data=FA_mmo)
summ(trans_mmo.lm4) # Adj-R2: -0.15; p: 0.67
pred_r_squared(trans_mmo.lm4) # -1.005

# Initial model
trans_mmo.lm4.init <- MASS::stepAIC(trans_mmo.lm4, direction="both", trace=F)
summ(trans_mmo.lm4.init, digits= 4) # NULL

# Interaction model
trans_mmo.lm4.inter <- stepAIC(trans_mmo.lm4, ~.^2, trace=F)
summ(trans_mmo.lm4.inter,digits=4) # Adj-R2: 0.3143; p: 0.1803

# -------------------------------------

# Model-5: Lux + Imperv200 + pol_sha + flo_sha
trans_mmo.lm5 <- lm(mass_meanopen ~ lux + imperv200 +
                    pol_shannon.yj + flo_shannon
                   , data=FA_mmo)
summ(trans_mmo.lm5) # Adj-R2: -0.15; p: 0.67
pred_r_squared(trans_mmo.lm5) # -1.526

# Initial model
trans_mmo.lm5.init <- MASS::stepAIC(trans_mmo.lm5, direction="both", trace=F)
summ(trans_mmo.lm5.init, digits= 4) # Adj-R2: 0.1273; p: 0.1254
pred_r_squared(trans_mmo.lm5.init) # -0.2194

# Interaction model
trans_mmo.lm5.inter <- stepAIC(trans_mmo.lm5, ~.^2, trace=F)
summ(trans_mmo.lm5.inter, digits= 4) # Adj-R2: 0.2441; p: 0.1469
pred_r_squared(trans_mmo.lm5.inter) # -0.568

# -------------------------------------

# Model-6: Lux + Imperv500 + pol_ric + flo_ric
trans_mmo.lm6 <- lm(mass_meanopen ~ lux + imperv500 +
                    pol_richness + flo_richness.yj
                    , data=FA_mmo)
summ(trans_mmo.lm6) # Adj-R2: -0.11; p: 0.62
pred_r_squared(trans_mmo.lm6) # -0.831

# Initial model
trans_mmo.lm6.init <- MASS::stepAIC(trans_mmo.lm6, direction="both", trace=F)
summ(trans_mmo.lm6.init, digits= 4) # NULL

# Interaction model
trans_mmo.lm6.inter <- stepAIC(trans_mmo.lm6, ~.^2, trace=F)
summ(trans_mmo.lm6.inter,digits=4) # Adj-R2: 0.1719; p: 0.2119
pred_r_squared(trans_mmo.lm6.inter) # -1.0196

# -------------------------------------

# Model-7: Lux + Imperv500 + pol_sha + flo_sha
trans_mmo.lm7 <- lm(mass_meanopen ~ lux + imperv500 +
                     pol_shannon.yj + flo_shannon
                   , data=FA_mmo)
summ(trans_mmo.lm7) # Adj-R2: -0.08; p: 0.57
pred_r_squared(trans_mmo.lm7) # -1.53

# Initial model
trans_mmo.lm7.init <- MASS::stepAIC(trans_mmo.lm7, direction="both", trace=F)
summ(trans_mmo.lm7.init, digits= 4) # Adj-R2: 0.1273; p: 0.1254

# Interaction model
trans_mmo.lm7.inter <- stepAIC(trans_mmo.lm7, ~.^2, trace=F)
summ(trans_mmo.lm7.inter,digits=4) # Adj-R2: 0.7456; p: 0.0327
pred_r_squared(trans_mmo.lm7.inter) # -0.221


# ------------------------------------------------------------------------------


# -- Clean-up environment for the next script ----
rm(list=ls())
