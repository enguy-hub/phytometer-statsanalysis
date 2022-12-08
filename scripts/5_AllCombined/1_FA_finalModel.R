# ------------------------------------------------------------------------------
# -------------------------- Prerequisites -------------------------------------
# ------------------------------------------------------------------------------

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", "jtools", 
                   "PerformanceAnalytics", "sjPlot", "ggpubr")
lapply(list_packages, library, character.only = TRUE)

# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
setwd(pdir)

# Set path and read the data
fa_path = "./analysis_data/FA/FA_Data_2021_4analysis_garden_transformed.xlsx"
FA_data <- read_excel(fa_path, sheet = 1)


# ------------------------------------------------------------------------------
# ------------------------- Functions corner -----------------------------------
# ------------------------------------------------------------------------------

# ----- Function: fitted_vs_actual -----
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

# ----- Function: PRESS - predicted residual sums of squares -----
PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}

# ----- Function: pred_r_squared -----
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
# -------------------------- F. ananassa ---------------------------------------
# -------------- Resp variable: fruit mass (mass_meanopen) ---------------------
# ------------------------------------------------------------------------------

# Check structure and summaries of the data
str(FA_data)
summary(FA_data)

# Remove "Unnecessary" variables
FA_data <- FA_data %>%
  dplyr::select(-c("urbanclass100", "urbanclass200", "urbanclass500", "urbanclass1000"))

# Create "FA_mmo" dataframe, remove "non-related" vars
FA_mmo <- FA_data %>%
  dplyr::select(-c("mass_meandiff", "ratio_meandiff", "ratio_meanopen"))

# Check correlation of dependent and independent vars again
mmo.t._vars <- c(2,3,4,5,6,7,8,10,11,13,15,17,18)
mmo.t._corr <- FA_mmo[,mmo.t._vars]
chart.Correlation(mmo.t._corr, histogram=TRUE)


# ----- Step 1: Create initial "full" model -----

# Create initial "full" multiple regression lm() model
mmo.t.lm0 <- lm(mass_meanopen ~ imperv1000 + # imperv100 +
                  temp + # lux +  
                  pol_richness + flo_richness.yj +
                  pol_shannon.yj + flo_shannon + 
                  pol_abundance.yj  + flo_abundance.yj, 
                data=FA_mmo)
summ(mmo.t.lm0) # Adj-R2: -0.05; p: 0.59


# ----- A - Step 2: Step-wise 'non-interaction' model -----

# Create step-wise with stepAIC() 
mmo.t.lm.init <- MASS::stepAIC(mmo.t.lm0, direction="both", trace=F)
summ(mmo.t.lm.init, digits= 4) # Adj-R2: 0.4792; p: 0.031
pred_r_squared(mmo.t.lm.init)

# Check model$call
mmo.t.lm.init$call # ~ temp + imp1000 + pol_shannon.yj


# ----- A - Step 3: Check assumptions -----

# Check for multi-collinerity: For all vars, less than 3 is good
vif(mmo.t.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mmo.t.lm.init) # p: 0.983 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mmo.t.lm.init) # p: 0.212 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mmo.t.lm.init)) # p: 0.773 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmo.t.lm.init))
qqline(residuals(mmo.t.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mmo.t.lm.init, type = "rstandard") # => slight non-linearity

# Check CERES plot
ceresPlots(mmo.t.lm.init)

# Initial model: ~ temp + imperv1000 + pol_shannon.yj
summ(mmo.t.lm.init, digits= 4) # Adj-R2: 0.479; p: 0.031


# ----- B - Step 2: Step-wise 'interaction' model -----

# Create interaction step-wise model using stepAIC()
mmo.t.lm.inter <- stepAIC(mmo.t.lm.init, ~.^2, trace=F)
summ(mmo.t.lm.inter,digits=4) # Adj-R2: 0.7785; p: 0.0021
pred_r_squared(mmo.t.lm.inter) # Pr-R2: 0.4768561

# Check model$call
mmo.t.lm.inter$call # ~ temp + imp1000 * pol_shannon.yj


# ----- B - Step 3: Check assumptions -----

# Check for multi-collinerity: For all vars, less than 3 is good
vif(mmo.t.lm.inter, type = 'predictor') %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mmo.t.lm.inter) # p: 0.448 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mmo.t.lm.inter) # p: 0.73 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mmo.t.lm.inter)) # p: 0.086 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmo.t.lm.inter))
qqline(residuals(mmo.t.lm.inter))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mmo.t.lm.inter, type = "rstandard") # => slight non-linearity

# Interaction model: ~ temp + imp1000 * pol_shannon.yj
summ(mmo.t.lm.inter, digits= 4) # Adj-R2: 0.7785; p: 0.0021


# ----- Compare Adj-R2, p-value, and ANOVA test of the models -----

# Initial model: ~ temp + imperv1000 + pol_shannon.yj
summ(mmo.t.lm.init, center=T, digits= 4) # Adj-R2: 0.479; p: 0.031

# Interaction model: ~ temp + imperv1000 * pol_shannon.yj
summ(mmo.t.lm.inter, center=T, digits= 4) # Adj-R2: 0.7785; p: 0.0021

# Anova testing between models
anova(mmo.t.lm.init, mmo.t.lm.inter, test="F") # p: 0.0067 => Improved


# ----- Linear graphs to compare initial models against best model(s) -----

# Initial model
mmo.t_init_fitval <- 
  predict(mmo.t.lm.init, FA_mmo, interval = "confidence") %>% data.frame() 
mmo.t_g1 <- 
  fitted_vs_actual(mmo.t_init_fitval, FA_mmo$mass_meanopen,
                   "mass_meanopen - Initial Model")

# Interaction model
mmo.t_inter_fitval <- 
  predict(mmo.t.lm.inter, FA_mmo, interval = "confidence") %>% data.frame()
mmo.t_g2 <- fitted_vs_actual(mmo.t_inter_fitval, FA_mmo$mass_meanopen,
                             "mass_meanopen - Interaction Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(mmo.t_g1, mmo.t_g2, ncol=2)

# ==> Use interaction model for the paper


# ------------------------------------------------------------------------------

# ---- Clean-up environment for the next script ----
rm(list=ls())