# ---- Prerequisites ----

# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
setwd(pdir)

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", 
                   "jtools", "PerformanceAnalytics", "sjPlot", "ggpubr")
lapply(list_packages, library, character.only = TRUE)

# Set path and read the data
cf_path = "./analysis_data/CF/CF_Data_2021_4analysis_garden_transformed.xlsx"
CF_data <- read_excel(cf_path, sheet = 1)

# Remove "Non-normal distributed" variables
CF_data <- CF_data %>%
  dplyr::select(-c("fruimass_meandiff", "fruimass_meanopen", 
                   "ratio_meanopen", "flo_abundance.yj",
                   "urbanclass100", "urbanclass200", 
                   "urbanclass500", "urbanclass1000"))


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
# ---------------- Best TRANS model for: fruimass_meanopen.yj ------------------
# ------------------------------------------------------------------------------

# Create new dataframe, which remove "non-related" vars
CF_fmmo <- CF_data %>%
  dplyr::select(-c("fruimass_meandiff.yj", "seedmass_meandiff", 
                   "ratio_meandiff", "ratio_meanopen.yj", 
                   "seedmass_meanopen", "mass_pseed_meanopen",
                   "imperv200", "imperv500",
                   "pol_abundance", "flo_abundance"))

# Check correlation of dependent and independent vars again
fmmo_vars <- c(2,3,4,5,6,7,8,9,10,11)
fmmo_corr <- CF_fmmo[,fmmo_vars]
chart.Correlation(fmmo_corr, histogram=TRUE)


# ---------------------------------------------

# Model-0: Temp + Imperv1000 + pol_ric + flo_ric
fmmo.lm0 <- lm(fruimass_meanopen.yj ~ temp + imperv1000 + 
                       pol_richness + flo_richness
                     , data=CF_fmmo)
summ(fmmo.lm0, digits=4) # Adj-R2: -0.09; p: 0.5848
pred_r_squared(fmmo.lm0) # -0.947

# Initial model
fmmo.lm0.init <- MASS::stepAIC(fmmo.lm0, direction = "both", trace = FALSE)
summ(fmmo.lm0.init, digits= 4) # Adj-R2: 0.0711; p: 0.1935
pred_r_squared(fmmo.lm0.init) # -0.3215

# Interaction model
fmmo.lm0.inter <- stepAIC(fmmo.lm0, ~.^2, trace=F)
summ(fmmo.lm0.inter,digits=4) # Adj-R2: 0.553; p: 0.05
pred_r_squared(fmmo.lm0.inter) # -0.157


# ---------------------------------------------

# Model-1: Temp + Imperv1000 + pol_sha + flo_sha
fmmo.lm1 <- lm(fruimass_meanopen.yj ~ temp + imperv1000 + 
                       pol_shannon + flo_shannon
                     , data=CF_fmmo)
summ(fmmo.lm1, digits=4) # Adj-R2: 0.1496; p: 0.2823
pred_r_squared(fmmo.lm1) # -0.3128

# Initial model
fmmo.lm1.init <- MASS::stepAIC(fmmo.lm1, direction = "both", trace = FALSE)
summ(fmmo.lm1.init, digits= 4) # Adj-R2: 0.2167; p: 0.1696
pred_r_squared(fmmo.lm1.init) # -0.2678

# Interaction model
fmmo.lm1.inter <- stepAIC(fmmo.lm1, ~.^2, trace=F)
summ(fmmo.lm1.init, digits= 4) # Same as initial

# ---------------------------------------------

# Model-2: Temp + Imperv1000 + pol_sha.yj + flo_sha
fmmo.lm2 <- lm(fruimass_meanopen.yj ~ temp + imperv1000 + 
                       pol_shannon.yj + flo_shannon
                     , data=CF_fmmo)
summ(fmmo.lm2, digits=4) # Adj-R2: 0.1528; p: 0.279
pred_r_squared(fmmo.lm2) # -0.2766

# Initial model
fmmo.lm2.init <- MASS::stepAIC(fmmo.lm2, direction = "both", trace = FALSE)
summ(fmmo.lm2.init, digits= 4) # Adj-R2: 2167; p: 1696
pred_r_squared(fmmo.lm1.init) # -0.2678

# Interaction model
fmmo.lm2.inter <- stepAIC(fmmo.lm2, ~.^2, trace=F)
summ(fmmo.lm2.init, digits= 4) # Same as initial


# ---------------------------------------------

# Model-3: Lux + Imperv100 + pol_ric
fmmo.lm3 <- lm(fruimass_meanopen.yj ~ lux + imperv100 + pol_richness
                     , data=CF_fmmo)
summ(fmmo.lm3, digits=4) # Adj-R2: 0.3338; p: 0.0874
pred_r_squared(fmmo.lm3) # 0.188

# !!! Initial model
fmmo.lm3.init <- MASS::stepAIC(fmmo.lm3, direction = "both", trace = FALSE)
summ(fmmo.lm3.init, digits= 4) # Adj-R2: 0.3998; p: 0.0313
pred_r_squared(fmmo.lm3.init) # 0.26

# Interaction model
fmmo.lm3.inter <- stepAIC(fmmo.lm3, ~.^2, trace=F) 
summ(fmmo.lm3.inter, digits= 4) # Same as initial


# ---------------------------------------------

# Model-4: Lux + Imperv100 + pol_sha
fmmo.lm4 <- lm(fruimass_meanopen.yj ~ lux + imperv100 + pol_shannon
                     , data=CF_fmmo)
summ(fmmo.lm4, digits=4) # Adj-R2: 0.3338; p: 0.0874
pred_r_squared(fmmo.lm4) # 0.179

# Initial model
fmmo.lm4.init <- MASS::stepAIC(fmmo.lm4, direction = "both", trace = FALSE)
summ(fmmo.lm4.init, digits= 4) # Adj-R2: 0.3998; p: 0.0313
pred_r_squared(fmmo.lm4.init) # 0.26

# Interaction model
fmmo.lm4.inter <- stepAIC(fmmo.lm4, ~.^2, trace=F) # Same as initial
summ(fmmo.lm4.inter, digits= 4) # Same as initial


# ---------------------------------------------

# Model-5: Lux + Imperv100 + pol_sha.yj
fmmo.lm5 <- lm(fruimass_meanopen.yj ~ lux + imperv100 + pol_shannon.yj
                     , data=CF_fmmo)
summ(fmmo.lm5, digits=4) # Adj-R2: 0.3358; p: 0.0864
pred_r_squared(fmmo.lm5) # 0.142

# Initial model
fmmo.lm5.init <- MASS::stepAIC(fmmo.lm5, direction = "both", trace = FALSE)
summ(fmmo.lm5.init, digits= 4) # Adj-R2: 0.3998; p: 0.0313
pred_r_squared(fmmo.lm5.init) # 0.26

# Interaction model
fmmo.lm5.inter <- stepAIC(fmmo.lm5, ~.^2, trace=F) # Same as initial
summ(fmmo.lm5.inter, digits= 4) # Same as initial


# ------------------------------------------------------------------------------
# ---------------- Best TRANS model for: seedmass_meanopen ---------------------
# ------------------------------------------------------------------------------

# Create new dataframe, which remove "non-related" vars
CF_smmo <- CF_data %>%
  dplyr::select(-c("fruimass_meandiff.yj", "fruimass_meanopen.yj",
                   "ratio_meandiff", "ratio_meanopen.yj", 
                   "seedmass_meandiff", "mass_pseed_meanopen"))

# Check correlation of dependent and independent vars again
smmo_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
smmo_corr <- CF_smmo[,smmo_vars]
chart.Correlation(smmo_corr, histogram=TRUE)


# ---------------------------------------------

# Model-0: Temp + Imperv1000 + pol_ric + flo_ric
smmo.lm0 <- lm(seedmass_meanopen ~ temp + imperv1000 +
                       pol_richness + flo_richness
                     , data=CF_smmo)
summ(smmo.lm0, digits=4) # Adj-R2: 0.1397; p: 0.2928
pred_r_squared(smmo.lm0) # -0.281

# Initial model
smmo.lm0.init <- MASS::stepAIC(smmo.lm0, direction="both", trace=F)
summ(smmo.lm0.init, digits= 4) # Adj-R2: 0.3674; p: 0.0166
pred_r_squared(smmo.lm0.init) # 0.113

# !!! Interaction model
smmo.lm0.inter <- stepAIC(smmo.lm0, ~.^2, trace=F)
summ(smmo.lm0.inter,digits=4) # Adj-R2: 0.4929; p: 0.0277
pred_r_squared(smmo.lm0.inter) # 0.265


# ---------------------------------------------

# Model-1: Temp + Imperv1000 + pol_sha + flo_sha
smmo.lm1 <- lm(seedmass_meanopen ~ temp + imperv1000 + 
                       pol_shannon + flo_shannon
                     , data=CF_smmo)
summ(smmo.lm1, digits=4) # Adj-R2: 0.1718; p: 0.2594
pred_r_squared(smmo.lm1) # -0.5389

# !!! Initial model
smmo.lm1.init <- MASS::stepAIC(smmo.lm1, direction="both", trace=F)
summ(smmo.lm1.init, digits= 4) # Adj-R2: 0.3674; p: 0.0166
pred_r_squared(smmo.lm1.init) # 0.113

# Interaction model (from initial model) using stepAIC()
smmo.lm1.inter <- stepAIC(smmo.lm1, ~.^2, trace=F)
summ(smmo.lm1.inter,digits=4) # Same as initial


# ---------------------------------------------

# Model-2: Temp + Imperv1000 + pol_sha.yj + flo_sha
smmo.lm2 <- lm(seedmass_meanopen ~ temp + imperv1000 +
                       pol_shannon.yj + flo_shannon
                     , data=CF_smmo)
summ(smmo.lm2, digits=4) # Adj-R2: 0.1893; p: 0.2421
pred_r_squared(smmo.lm2) # -0.488

# Initial model
smmo.lm2.init <- MASS::stepAIC(smmo.lm2, direction="both", trace=F)
summ(smmo.lm2.init, digits= 4) # Adj-R2: 0.3674; p: 0.0166
pred_r_squared(smmo.lm2.init) # 0.113

# Interaction model (from initial model) using stepAIC()
smmo.lm2.inter <- stepAIC(smmo.lm2, ~.^2, trace=F)
summ(smmo.lm2.inter,digits=4) # Same as initial


# ---------------------------------------------

# Model-3: Lux + Imperv100 + pol_ric
smmo.lm3 <- lm(seedmass_meanopen ~ lux + imperv100 + pol_richness
                     , data=CF_smmo)
summ(smmo.lm3, digits=4) # Adj-R2: 0.3974; p: 0.0575
pred_r_squared(smmo.lm3) # -0.0679

# !!! Initial model
smmo.lm3.init <- MASS::stepAIC(smmo.lm3, direction="both", trace=F)
summ(smmo.lm3.init, digits= 4) # Adj-R2: 0.455; p: 0.0193
pred_r_squared(smmo.lm3.init) # 0.306

# Interaction model
smmo.lm3.inter <- stepAIC(smmo.lm3, ~.^2, trace=F)
summ(smmo.lm3.inter,digits=4) # Same as initial


# ---------------------------------------------

# Model-4: Lux + Imperv100 + pol_sha
smmo.lm4 <- lm(seedmass_meanopen ~ lux + imperv100 + pol_shannon
                     , data=CF_smmo)
summ(smmo.lm4, digits=4) # Adj-R2: 0.4745; p: 0.0322
pred_r_squared(smmo.lm4) # 0.2856

# !!! Initial model
smmo.lm4.init <- MASS::stepAIC(smmo.lm4, direction="both", trace=F)
summ(smmo.lm4.init, digits= 4) # Adj-R2: 0.455; p: 0.0193
pred_r_squared(smmo.lm4.init) # 0.306

# Interaction model
smmo.lm4.inter <- stepAIC(smmo.lm4, ~.^2, trace=F)
summ(smmo.lm4.inter,digits=4) # Same as initial


# ---------------------------------------------

# Model-5: Lux + Imperv100 + pol_sha.yj
smmo.lm5 <- lm(seedmass_meanopen ~ lux + imperv100 + pol_shannon.yj
                     , data=CF_smmo)
summ(smmo.lm5, digits=4) # Adj-R2: 0.4645; p: 0.0349
pred_r_squared(smmo.lm5) # 0.2244

# !!! Initial model
smmo.lm5.init <- MASS::stepAIC(smmo.lm5, direction="both", trace=F)
summ(smmo.lm5.init, digits= 4) # Adj-R2: 0.455; p: 0.0193
pred_r_squared(smmo.lm5.init) # 0.306

# Interaction model (from initial model) using stepAIC()
smmo.lm5.inter <- stepAIC(smmo.lm5, ~.^2, trace=F)
summ(smmo.lm5.inter,digits=4) # Same as initial


# ------------------------------------------------------------------------------


# ---- Clean-up environment for the next script ----
rm(list=ls())

