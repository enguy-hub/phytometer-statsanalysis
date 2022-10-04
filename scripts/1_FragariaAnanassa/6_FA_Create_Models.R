# -- Prerequisites ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", "jtools",
                   "PerformanceAnalytics", "sjPlot")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
fa_norm_path = "./analysis_data/FA/FA_Data_2021_4analysis_garden.xlsx"
FA_norm_data <- read_excel(fa_norm_path, sheet = 1)

fa_trans_path = "./analysis_data/FA/FA_Data_2021_4analysis_garden_transformed.xlsx"
FA_trans_data <- read_excel(fa_trans_path, sheet = 1)


# Remove "Non-normal distributed" variables
FA_norm_data <- FA_norm_data %>%
  dplyr::select(-c("urbanclass100", "urbanclass200", "urbanclass500", "urbanclass1000"))

FA_trans_data <- FA_trans_data %>%
  dplyr::select(-c("pol_abundance", "flo_abundance", "flo_richness", # "pol_shannon",
                   "urbanclass100", "urbanclass200", "urbanclass500", "urbanclass1000"))


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
FA_norm_mmo <- FA_norm_data %>%
  dplyr::select(-c("mass_meandiff", "ratio_meandiff", "ratio_meanopen"))

# Check correlation of dependent and independent vars again
norm_mmo_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14)
norm_mmo_corr <- FA_norm_mmo[,norm_mmo_vars]
chart.Correlation(norm_mmo_corr, histogram=TRUE)

# -------------------------------------

# Create multiple regression lm() model
norm_mmo.lm0 <- lm(mass_meanopen ~ temp + # lux + 
              imperv1000 + # imperv100 
              pol_richness + # pol_shannon + pol_abundance +
              flo_richness # + flo_shannon + flo_abundance
              , data=FA_norm_mmo)
summ(norm_mmo.lm0) # Adj.R2: 0.23; p: 0.20
pred_r_squared(norm_mmo.lm0) # -0.3388

# Create initial model with stepAIC()
norm_mmo.lm0.init <- MASS::stepAIC(norm_mmo.lm0, direction="both", trace=F)
summ(norm_mmo.lm0.init, digits= 4) # Adj-R2: 0.3124; p: 0.098
pred_r_squared(norm_mmo.lm0.init) # -0.206

# Create interaction model (from initial model) using stepAIC()
norm_mmo.lm0.inter <- stepAIC(norm_mmo.lm0.init, ~.^2, trace=F) # => Same as initial
summ(norm_mmo.lm0.inter,digits=4) # Adj-R2: 0.55; p: 0.03
pred_r_squared(norm_mmo.lm0.inter) # -1.6

# -------------------------------------

# Create multiple regression lm() model
norm_mmo.lm1 <- lm(mass_meanopen ~ temp + # lux + 
                     imperv1000 + # imperv100 
                     pol_shannon + # pol_richness + pol_abundance +
                     flo_shannon # + flo_richness + flo_abundance
                   , data=FA_norm_mmo)
summ(norm_mmo.lm1) # Adj-R2: 0.38; p: 0.10
pred_r_squared(norm_mmo.lm1) # -0.552

# Create initial model with stepAIC()
norm_mmo.lm1.init <- MASS::stepAIC(norm_mmo.lm1, direction="both", trace=F)
summ(norm_mmo.lm1.init, digits= 4) # Adj-R2: 0.3793; p: 0.0981
pred_r_squared(norm_mmo.lm1.init) # -0.5518

# Create interaction model (from initial model) using stepAIC()
norm_mmo.lm1.inter <- stepAIC(norm_mmo.lm1.init, ~.^2, trace=F) # => Same as initial
summ(norm_mmo.lm1.inter,digits=4) # Adj-R2: 0.6824; p: 0.0168
pred_r_squared(norm_mmo.lm1.inter) # 0.2268


# ------------------------------------------------------------------------------
# --------------- Find model for: FA_trans | mass_meanopen ---------------------
# ------------------------------------------------------------------------------

# Create new dataframe, which remove "non-related" vars
FA_trans_mmo <- FA_trans_data %>%
  dplyr::select(-c("mass_meandiff", "ratio_meandiff", "ratio_meanopen"))


# Check correlation of dependent and independent vars again
trans_mmo_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
trans_mmo_corr <- FA_trans_mmo[,trans_mmo_vars]
chart.Correlation(trans_mmo_corr, histogram=TRUE)

# -------------------------------------

# Create multiple regression lm() model
trans_mmo.lm0 <- lm(mass_meanopen ~ temp + # lux + 
                     imperv1000 + # imperv100 
                     pol_richness + # pol_shannon + pol_abundance +
                     flo_richness.yj # + flo_shannon + flo_abundance
                   , data=FA_trans_mmo)
summ(trans_mmo.lm0) # Adj-R2: 0.24; p: 0.20
pred_r_squared(trans_mmo.lm0) # -0.3712

# Create initial model with stepAIC()
trans_mmo.lm0.init <- MASS::stepAIC(trans_mmo.lm0, direction="both", trace=F)
summ(trans_mmo.lm0.init, digits= 4) # Adj-R2: 0.3124; p: 0.0997
pred_r_squared(trans_mmo.lm0.init) # - 0.206

# Create interaction model (from initial model) using stepAIC()
trans_mmo.lm0.inter <- stepAIC(trans_mmo.lm0.init, ~.^2, trace=F) # => Same as initial
summ(trans_mmo.lm0.inter,digits=4) # Adj-R2: 0.55; p: 0.03
pred_r_squared(trans_mmo.lm0.inter) # -1.6

# -------------------------------------

# Create multiple regression lm() model
trans_mmo.lm1 <- lm(mass_meanopen ~ temp + # lux + 
                     imperv1000 + # imperv100 
                     pol_shannon.yj + # pol_richness + pol_abundance +
                     flo_shannon # + flo_richness + flo_abundance
                   , data=FA_trans_mmo)
summ(trans_mmo.lm1) # Adj-R2: 0.46; p: 0.06
pred_r_squared(trans_mmo.lm1) # -0.524

# Create initial model with stepAIC()
trans_mmo.lm1.init <- MASS::stepAIC(trans_mmo.lm1, direction="both", trace=F)
summ(trans_mmo.lm1.init, digits= 4) # Adj-R2: 0.479; p: 0.031
pred_r_squared(trans_mmo.lm1.init) # -0.016

# Create interaction model (from initial model) using stepAIC()
trans_mmo.lm1.inter <- stepAIC(trans_mmo.lm1.init, ~.^2, trace=F) # => Same as initial
summ(trans_mmo.lm1.inter,digits=4) # Adj-R2: 0.7785; p: 0.0021
pred_r_squared(trans_mmo.lm1.inter) # 0.4768


# ------------------------------------------------------------------------------


# -- Clean-up environment for the next script ----
rm(list=ls())
