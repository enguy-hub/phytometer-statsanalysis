# ---- Prerequisites ----

# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
setwd(pdir)

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", 
                   "jtools", "PerformanceAnalytics", "sjPlot", "ggpubr")
lapply(list_packages, library, character.only = TRUE)

# Set path and read the data
tp_path = "./analysis_data/TP/TP_Data_2021_4analysis_garden_transformed.xlsx"
TP_data <- read_excel(tp_path, sheet = 1)

# Remove "Non-normal distributed" variables
TP_data <- TP_data %>%
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
# ------------------ Best NORM model for: mass_pseed_meanopen ------------------
# ------------------------------------------------------------------------------

# Create new dataframe, which remove "non-related" vars
TP_mpsmo <- TP_data %>%
  dplyr::select(-c("flowmass_meandiff", "flowmass_meanopen", 
                   "seedmass_meandiff", "seedmass_meanopen",
                   "mass_pseed_meandiff",
                   "imperv200", "imperv500",
                   "pol_abundance", "pol_abundance.yj",
                   "flo_abundance", "flo_abundance.yj"))

# Check correlation of dependent and independent vars again
mpsmo_vars <- c(2,3,4,5,6,7,8,9,10)
mpsmo_corr <- TP_mpsmo[,mpsmo_vars]
chart.Correlation(mpsmo_corr, histogram=T)

# -------------------------------------

# Model-0: Temp + Imperv1000 + pol_ric + flo_ric
mpsmo.lm0 <- lm(mass_pseed_meanopen ~ temp + imperv1000 + 
                       pol_richness + flo_richness
                  , data=TP_mpsmo)
summ(mpsmo.lm0, digits=4) # Adj-R2: 0.5; p: -0.029
pred_r_squared(mpsmo.lm0) # -1.45

# Initial model
mpsmo.lm0.init <- MASS::stepAIC(mpsmo.lm0, direction="both", trace=F)
summ(mpsmo.lm0.init, digits= 4) # Adj-R2: 0.0981; p: 0.1572
pred_r_squared(mpsmo.lm0.init) # -0.163

# Interaction model
mpsmo.lm0.inter <- stepAIC(mpsmo.lm0, ~.^2, trace=F)
summ(mpsmo.lm0.inter,digits=4) # Adj-R2: 0.307; p: 0.1029
pred_r_squared(mpsmo.lm0.inter) # 0.081


# -------------------------------------

# Model-1: Temp + Imperv1000 + pol_sha + flo_sha
mpsmo.lm1 <- lm(mass_pseed_meanopen ~ temp + imperv1000 + 
                       pol_shannon + flo_shannon
                     , data=TP_mpsmo)
summ(mpsmo.lm1, digits=4) # Adj-R2: -0.128; p: 0.637
pred_r_squared(mpsmo.lm1) # -1.188

# Initial model
mpsmo.lm1.init <- MASS::stepAIC(mpsmo.lm1, direction="both", trace=F)
summ(mpsmo.lm1.init, digits= 4) # Adj-R2: 0.1387; p: 0.1148
pred_r_squared(mpsmo.lm1.init) # -0.116

# Interaction model
mpsmo.lm1.inter <- stepAIC(mpsmo.lm1, ~.^2, trace=F)
summ(mpsmo.lm1.inter,digits=4) # Adj-R2: 0.667; p: 0.0601
pred_r_squared(mpsmo.lm1.inter) # -1.05


# -------------------------------------

# Model-2: Lux + Imperv100 + pol_ric
mpsmo.lm2 <- lm(mass_pseed_meanopen ~ lux + imperv100 + pol_richness 
                     , data=TP_mpsmo)
summ(mpsmo.lm2, digits=4) # Adj-R2: 0.1343; p: 0.2524
pred_r_squared(mpsmo.lm2) # -0.669

# Initial model
mpsmo.lm2.init <- MASS::stepAIC(mpsmo.lm2, direction="both", trace=F)
summ(mpsmo.lm2.init, digits= 4) # Adj-R2: 0.242; p: 0.05
pred_r_squared(mpsmo.lm2.init) # 0.062

# Interaction model
mpsmo.lm2.inter <- stepAIC(mpsmo.lm2, ~.^2, trace=F)
summ(mpsmo.lm2.inter,digits=4) # Same initial


# -------------------------------------

# Model-3: Lux + Imperv100 + pol_sha
mpsmo.lm3 <- lm(mass_pseed_meanopen ~ lux + imperv100 + pol_shannon
                     , data=TP_mpsmo)
summ(mpsmo.lm3, digits=4) # Adj-R2: 0.1036; p: 0.289
pred_r_squared(mpsmo.lm3) # -1.278

# Initial model
mpsmo.lm3.init <- MASS::stepAIC(mpsmo.lm3, direction="both", trace=F)
summ(mpsmo.lm3.init, digits=4) # Adj-R2: 0.242; p: 0.05
pred_r_squared(mpsmo.lm3.init) # 0.062

# Interaction model
mpsmo.lm3.inter <- stepAIC(mpsmo.lm3, ~.^2, trace=F)
summ(mpsmo.lm3.inter, digits=4) # Adj-R2: 0.4089; p: 0.053
pred_r_squared(mpsmo.lm3.inter) # 0.0139


# ------------------------------------------------------------------------------


# ---- Clean-up environment for the next script ----
rm(list=ls())

