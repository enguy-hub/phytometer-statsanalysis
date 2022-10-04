# ---- Prerequisite procedures ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", 
                   "jtools", "PerformanceAnalytics", "sjPlot")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
tp_norm_path = "./analysis_data/TP/TP_Data_2021_4analysis_garden.xlsx"
TP_norm_data <- read_excel(tp_norm_path, sheet = 1)

tp_trans_path = "./analysis_data/TP/TP_Data_2021_4analysis_garden_transformed.xlsx"
TP_trans_data <- read_excel(tp_trans_path, sheet = 1)


# Remove "Non-normal distributed" variables
TP_norm_data <- TP_norm_data %>%
  dplyr::select(-c("urbanclass100", "urbanclass200", 
                   "urbanclass500", "urbanclass1000"))

TP_trans_data <- TP_trans_data %>%
  dplyr::select(-c("pol_abundance", "flo_abundance", "urbanclass100", 
                   "urbanclass200", "urbanclass500", "urbanclass1000"))


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
# ------------------ Best NORM model for: mass_pseed_meanopen ------------------
# ------------------------------------------------------------------------------

# Create new dataframe, which remove "non-related" vars
TP_norm_mpsmo <- TP_norm_data %>%
  dplyr::select(-c("flowmass_meandiff", "flowmass_meanopen", 
                   "seedmass_meandiff", "seedmass_meanopen",
                   "mass_pseed_meandiff"))

# Check correlation of dependent and independent vars again
mpsmo_norm_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14)
mpsmo_norm_corr <- TP_norm_mpsmo[,mpsmo_norm_vars]
chart.Correlation(mpsmo_norm_corr, histogram=T)

# -------------------------------------

# Model: Temp + Imperv1000 + pol_ric + flo_ric
mpsmo_norm.lm0 <- lm(mass_pseed_meanopen ~ temp + # lux + 
                       imperv1000 + # imperv100 + 
                       pol_richness + # pol_shannon + pol_abundance +   
                       flo_richness # + flo_shannon + flo_abundance
                  , data=TP_norm_mpsmo)
summ(mpsmo_norm.lm0, digits=4) # Adj-R2: 0.5; p: -0.029
pred_r_squared(mpsmo_norm.lm0) # -1.45

# Initial model: Temp + Imperv1000 + pol_ric + flo_ric
mpsmo_norm.lm0.init <- MASS::stepAIC(mpsmo_norm.lm0, direction="both", trace=F)
summ(mpsmo_norm.lm0.init, digits= 4) # Adj-R2: 0.0981; p: 0.1572
pred_r_squared(mpsmo_norm.lm0.init) # -0.163

# Interaction model: Temp + Imperv1000 + pol_ric + flo_ric
mpsmo_norm.lm0.inter <- stepAIC(mpsmo_norm.lm0, ~.^2, trace=F)
summ(mpsmo_norm.lm0.inter,digits=4) # Adj-R2: 0.307; p: 0.1029
pred_r_squared(mpsmo_norm.lm0.inter) # 0.081

# -------------------------------------

# Model: Temp + Imperv1000 + pol_sha + flo_sha
mpsmo_norm.lm1 <- lm(mass_pseed_meanopen ~ temp + # lux + 
                       imperv1000 + # imperv100 + 
                       pol_shannon + # pol_richness + pol_abundance +   
                       flo_shannon # + flo_richness + flo_abundance
                     , data=TP_norm_mpsmo)
summ(mpsmo_norm.lm1, digits=4) # Adj-R2: -0.128; p: 0.637
pred_r_squared(mpsmo_norm.lm1) # -1.188

# Initial model: Temp + Imperv1000 + pol_sha + flo_sha
mpsmo_norm.lm1.init <- MASS::stepAIC(mpsmo_norm.lm1, direction="both", trace=F)
summ(mpsmo_norm.lm1.init, digits= 4) # Adj-R2: 0.1387; p: 0.1148
pred_r_squared(mpsmo_norm.lm1.init) # -0.116

# Interaction model: Temp + Imperv1000 + pol_sha + flo_sha
mpsmo_norm.lm1.inter <- stepAIC(mpsmo_norm.lm1, ~.^2, trace=F)
summ(mpsmo_norm.lm1.inter,digits=4) # Adj-R2: 0.667; p: 0.0601
pred_r_squared(mpsmo_norm.lm1.inter) # -1.05

# -------------------------------------

# Model: Lux + Imperv100 + pol_ric
mpsmo_norm.lm2 <- lm(mass_pseed_meanopen ~ lux + # temp + 
                       imperv100 + # imperv1000 + 
                       pol_richness # pol_shannon + pol_abundance +   
                     , data=TP_norm_mpsmo)
summ(mpsmo_norm.lm2, digits=4) # Adj-R2: 0.1343; p: 0.2524
pred_r_squared(mpsmo_norm.lm2) # -0.669

# Initial model
mpsmo_norm.lm2.init <- MASS::stepAIC(mpsmo_norm.lm2, direction="both", trace=F)
summ(mpsmo_norm.lm2.init, digits= 4) # Adj-R2: 0.242; p: 0.05
pred_r_squared(mpsmo_norm.lm2.init) # 0.062

# Interaction model
mpsmo_norm.lm2.inter <- stepAIC(mpsmo_norm.lm2, ~.^2, trace=F)
summ(mpsmo_norm.lm2.inter,digits=4) # Same as initial model


# ------------------------------------------------------------------------------


# ---- Clean-up environment for the next script ----
rm(list=ls())

