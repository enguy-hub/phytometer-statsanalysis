# ---- Prerequisite ----

# Set path and read the data
cf_trans_path = "./analysis_data/CF/CF_Data_2021_4analysis_garden_transformed.xlsx"
CF_trans_data <- read_excel(cf_trans_path, sheet = 1)

# Remove "Non-normal distributed" variables
CF_trans_data <- CF_trans_data %>%
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
CF_trans_fmmo <- CF_trans_data %>%
  dplyr::select(-c("fruimass_meandiff.yj", "seedmass_meandiff", 
                   "seedmass_meanopen",
                   "ratio_meandiff", "ratio_meanopen.yj", 
                   "mass_pseed_meanopen"))

# Check correlation of dependent and independent vars again
fmmo_trans_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
fmmo_trans_corr <- CF_trans_fmmo[,fmmo_trans_vars]
chart.Correlation(fmmo_trans_corr, histogram=TRUE)

# ---------------------------------------------

# Model-0: Temp + Imperv1000 + pol_ric + flo_ric
fmmo_trans.lm0 <- lm(fruimass_meanopen.yj ~ temp + imperv1000 + 
                       pol_richness + flo_richness
                     , data=CF_trans_data)
summ(fmmo_trans.lm0, digits=4) # Adj-R2: -0.09; p: 0.5848
pred_r_squared(fmmo_trans.lm0) # -0.947

# Initial model
fmmo.lm0.init <- MASS::stepAIC(fmmo_trans.lm0, direction = "both", trace = FALSE)
summ(fmmo.lm0.init, digits= 4) # Adj-R2: 0.0711; p: 0.1935
pred_r_squared(fmmo.lm0.init) # -0.3215

# Interaction model
fmmo.lm0.inter <- stepAIC(fmmo_trans.lm0, ~.^2, trace=F)
summ(fmmo.lm0.inter,digits=4) # Adj-R2: 0.553; p: 0.05
pred_r_squared(fmmo.lm0.inter) # -0.157

# ---------------------------------------------

# Model-1: Temp + Imperv1000 + pol_sha + flo_sha
fmmo_trans.lm1 <- lm(fruimass_meanopen.yj ~ temp + imperv1000 + 
                       pol_shannon + flo_shannon
                     , data=CF_trans_data)
summ(fmmo_trans.lm1, digits=4) # Adj-R2: 0.1496; p: 0.2823
pred_r_squared(fmmo_trans.lm1) # -0.3128

# Initial model
fmmo.lm1.init <- MASS::stepAIC(fmmo_trans.lm1, direction = "both", trace = FALSE)
summ(fmmo.lm1.init, digits= 4) # Adj-R2: 0.2167; p: 0.1696
pred_r_squared(fmmo.lm1.init) # -0.2678

# Interaction model
fmmo.lm1.inter <- stepAIC(fmmo_trans.lm1, ~.^2, trace=F)
summ(fmmo.lm1.init, digits= 4) # Same as initial

# ---------------------------------------------

# Model-2: Temp + Imperv1000 + pol_sha.yj + flo_sha
fmmo_trans.lm2 <- lm(fruimass_meanopen.yj ~ temp + imperv1000 + 
                       pol_shannon.yj + flo_shannon
                     , data=CF_trans_data)
summ(fmmo_trans.lm2, digits=4) # Adj-R2: 0.1528; p: 0.279
pred_r_squared(fmmo_trans.lm2) # -0.2766

# Initial model
fmmo.lm2.init <- MASS::stepAIC(fmmo_trans.lm2, direction = "both", trace = FALSE)
summ(fmmo.lm2.init, digits= 4) # Adj-R2: 2167; p: 1696
pred_r_squared(fmmo.lm1.init) # -0.2678

# Interaction model
fmmo.lm2.inter <- stepAIC(fmmo_trans.lm2, ~.^2, trace=F)
summ(fmmo.lm2.init, digits= 4) # Same as initial


# ---------------------------------------------

# Model-3: Lux + Imperv100 + pol_ric
fmmo_trans.lm3 <- lm(fruimass_meanopen.yj ~ lux + imperv100 + pol_richness
                     , data=CF_trans_data)
summ(fmmo_trans.lm3, digits=4) # Adj-R2: 0.3338; p: 0.0874
pred_r_squared(fmmo_trans.lm3) # 0.188

# !!! Initial model
fmmo.lm3.init <- MASS::stepAIC(fmmo_trans.lm3, direction = "both", trace = FALSE)
summ(fmmo.lm3.init, digits= 4) # Adj-R2: 0.3998; p: 0.0313
pred_r_squared(fmmo.lm3.init) # 0.26

# Interaction model
fmmo.lm3.inter <- stepAIC(fmmo_trans.lm3, ~.^2, trace=F) 
summ(fmmo.lm3.inter, digits= 4) # Same as initial

# ---------------------------------------------

# Model-4: Lux + Imperv100 + pol_sha
fmmo_trans.lm4 <- lm(fruimass_meanopen.yj ~ lux + imperv100 + pol_shannon
                     , data=CF_trans_data)
summ(fmmo_trans.lm4, digits=4) # Adj-R2: 0.3338; p: 0.0874
pred_r_squared(fmmo_trans.lm4) # 0.179

# Initial model
fmmo.lm4.init <- MASS::stepAIC(fmmo_trans.lm4, direction = "both", trace = FALSE)
summ(fmmo.lm4.init, digits= 4) # Adj-R2: 0.3998; p: 0.0313
pred_r_squared(fmmo.lm4.init) # 0.26

# Interaction model
fmmo.lm4.inter <- stepAIC(fmmo_trans.lm4, ~.^2, trace=F) # Same as initial
summ(fmmo.lm4.inter, digits= 4) # Same as initial

# ---------------------------------------------

# Model-5: Lux + Imperv100 + pol_sha.yj
fmmo_trans.lm5 <- lm(fruimass_meanopen.yj ~ lux + imperv100 + pol_shannon.yj
                     , data=CF_trans_data)
summ(fmmo_trans.lm5, digits=4) # Adj-R2: 0.3358; p: 0.0864
pred_r_squared(fmmo_trans.lm5) # 0.142

# Initial model
fmmo.lm5.init <- MASS::stepAIC(fmmo_trans.lm5, direction = "both", trace = FALSE)
summ(fmmo.lm5.init, digits= 4) # Adj-R2: 0.3998; p: 0.0313
pred_r_squared(fmmo.lm5.init) # 0.26

# Interaction model
fmmo.lm5.inter <- stepAIC(fmmo_trans.lm5, ~.^2, trace=F) # Same as initial
summ(fmmo.lm5.inter, digits= 4) # Same as initial


# ------------------------------------------------------------------------------
# ---------------- Best TRANS model for: seedmass_meanopen ---------------------
# ------------------------------------------------------------------------------

# Create new dataframe, which remove "non-related" vars
CF_trans_smmo <- CF_trans_data %>%
  dplyr::select(-c("fruimass_meandiff.yj", "fruimass_meanopen.yj",
                   "ratio_meandiff", "ratio_meanopen.yj", 
                   "seedmass_meandiff", "mass_pseed_meanopen"))

# Check correlation of dependent and independent vars again
smmo_trans_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
smmo_trans_corr <- CF_trans_smmo[,smmo_trans_vars]
chart.Correlation(smmo_trans_corr, histogram=TRUE)

# ---------------------------------------------

# Model-0: Temp + Imperv1000 + pol_ric + flo_ric
smmo_trans.lm0 <- lm(seedmass_meanopen ~ temp + imperv1000 +
                       pol_richness + flo_richness
                     , data=CF_trans_data)
summ(smmo_trans.lm0, digits=4) # Adj-R2: 0.1397; p: 0.2928
pred_r_squared(smmo_trans.lm0) # -0.281

# Initial model
smmo_trans.lm0.init <- MASS::stepAIC(smmo_trans.lm0, direction="both", trace=F)
summ(smmo_trans.lm0.init, digits= 4) # Adj-R2: 0.3674; p: 0.0166
pred_r_squared(smmo_trans.lm0.init) # 0.113

# !!! Interaction model
smmo_trans.lm0.inter <- stepAIC(smmo_trans.lm0, ~.^2, trace=F)
summ(smmo_trans.lm0.inter,digits=4) # Adj-R2: 0.4929; p: 0.0277
pred_r_squared(smmo_trans.lm0.inter) # 0.265

# ---------------------------------------------

# Model-1: Temp + Imperv1000 + pol_sha + flo_sha
smmo_trans.lm1 <- lm(seedmass_meanopen ~ temp + imperv1000 + 
                       pol_shannon + flo_shannon
                     , data=CF_trans_data)
summ(smmo_trans.lm1, digits=4) # Adj-R2: 0.1718; p: 0.2594
pred_r_squared(smmo_trans.lm1) # -0.5389

# !!! Initial model
smmo_trans.lm1.init <- MASS::stepAIC(smmo_trans.lm1, direction="both", trace=F)
summ(smmo_trans.lm1.init, digits= 4) # Adj-R2: 0.3674; p: 0.0166
pred_r_squared(smmo_trans.lm1.init) # 0.113

# Interaction model (from initial model) using stepAIC()
smmo_trans.lm1.inter <- stepAIC(smmo_trans.lm1, ~.^2, trace=F)
summ(smmo_trans.lm1.inter,digits=4) # Same as initial

# ---------------------------------------------

# Model-2: Temp + Imperv1000 + pol_sha.yj + flo_sha
smmo_trans.lm2 <- lm(seedmass_meanopen ~ temp + imperv1000 +
                       pol_shannon.yj + flo_shannon
                     , data=CF_trans_data)
summ(smmo_trans.lm2, digits=4) # Adj-R2: 0.1893; p: 0.2421
pred_r_squared(smmo_trans.lm2) # -0.488

# Initial model
smmo_trans.lm2.init <- MASS::stepAIC(smmo_trans.lm2, direction="both", trace=F)
summ(smmo_trans.lm2.init, digits= 4) # Adj-R2: 0.3674; p: 0.0166
pred_r_squared(smmo_trans.lm2.init) # 0.113

# Interaction model (from initial model) using stepAIC()
smmo_trans.lm2.inter <- stepAIC(smmo_trans.lm2, ~.^2, trace=F)
summ(smmo_trans.lm2.inter,digits=4) # Same as initial

# ---------------------------------------------

# Model-3: Lux + Imperv100 + pol_ric
smmo_trans.lm3 <- lm(seedmass_meanopen ~ lux + imperv100 + pol_richness
                     , data=CF_trans_data)
summ(smmo_trans.lm3, digits=4) # Adj-R2: 0.3974; p: 0.0575
pred_r_squared(smmo_trans.lm3) # -0.0679

# !!! Initial model
smmo_trans.lm3.init <- MASS::stepAIC(smmo_trans.lm3, direction="both", trace=F)
summ(smmo_trans.lm3.init, digits= 4) # Adj-R2: 0.455; p: 0.0193
pred_r_squared(smmo_trans.lm3.init) # 0.306

# Interaction model
smmo_trans.lm3.inter <- stepAIC(smmo_trans.lm3, ~.^2, trace=F)
summ(smmo_trans.lm3.inter,digits=4) # Same as initial

# ---------------------------------------------

# Model-4: Lux + Imperv100 + pol_sha
smmo_trans.lm4 <- lm(seedmass_meanopen ~ lux + imperv100 + pol_shannon
                     , data=CF_trans_data)
summ(smmo_trans.lm4, digits=4) # Adj-R2: 0.4745; p: 0.0322
pred_r_squared(smmo_trans.lm4) # 0.2856

# !!! Initial model
smmo_trans.lm4.init <- MASS::stepAIC(smmo_trans.lm4, direction="both", trace=F)
summ(smmo_trans.lm4.init, digits= 4) # Adj-R2: 0.455; p: 0.0193
pred_r_squared(smmo_trans.lm4.init) # 0.306

# Interaction model
smmo_trans.lm4.inter <- stepAIC(smmo_trans.lm4, ~.^2, trace=F)
summ(smmo_trans.lm4.inter,digits=4) # Same as initial

# ---------------------------------------------

# Model-5: Lux + Imperv100 + pol_sha.yj
smmo_trans.lm5 <- lm(seedmass_meanopen ~ lux + imperv100 + pol_shannon.yj
                     , data=CF_trans_data)
summ(smmo_trans.lm5, digits=4) # Adj-R2: 0.4645; p: 0.0349
pred_r_squared(smmo_trans.lm5) # 0.2244

# !!! Initial model
smmo_trans.lm5.init <- MASS::stepAIC(smmo_trans.lm5, direction="both", trace=F)
summ(smmo_trans.lm5.init, digits= 4) # Adj-R2: 0.455; p: 0.0193
pred_r_squared(smmo_trans.lm5.init) # 0.306

# Interaction model (from initial model) using stepAIC()
smmo_trans.lm5.inter <- stepAIC(smmo_trans.lm5, ~.^2, trace=F)
summ(smmo_trans.lm5.inter,digits=4) # Same as initial


# ------------------------------------------------------------------------------


# ---- Clean-up environment for the next script ----
rm(list=ls())

