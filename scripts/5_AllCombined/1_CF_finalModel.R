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
cf_path = "./analysis_data/CF/CF_Data_2021_4analysis_garden_transformed.xlsx"

# Read the data
CF_data <- read_excel(cf_path, sheet = 1)


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
# -------------------------- C. frutescens -------------------------------------
# ------------- Resp variable: Seed mass (seedmass_meanopen) -------------------
# ------------------------------------------------------------------------------

# Check structure and summaries of the data sets
str(CF_data)
summary(CF_data)

# Remove "Non-normal distributed" variables
CF_data <- CF_data %>%
  dplyr::select(-c("urbanclass100", "urbanclass200", 
                   "urbanclass500", "urbanclass1000",
                   "fruimass_meandiff", "fruimass_meandiff.yj", 
                   "fruimass_meanopen", "seedmass_meandiff", 
                   "ratio_meandiff", "ratio_meanopen", 
                   "ratio_meanopen.yj", "mass_pseed_meanopen"))

# Create new dataframe, which remove "non-related" vars
CF_smmo <- CF_data %>%
  dplyr::select(-c("fruimass_meanopen.yj"))

# Check correlation of dependent and independent vars again
smmo.t_vars <- c(2,3,4,5,6,7,8,9,10,12,14,15,16)
smmo.t_corr <- CF_smmo[,smmo.t_vars]
chart.Correlation(smmo.t_corr, histogram=TRUE)


# ----- Step 1: Create initial "full" model -----

# Create initial "full" multiple regression lm() model
smmo.t.lm0 <- lm(seedmass_meanopen ~ lux + imperv100 + temp + 
                 pol_abundance + pol_shannon.yj + flo_richness + # pol_richness + 
                 flo_abundance.yj + flo_shannon, data=CF_data)
summ(smmo.t.lm0, digits=4) # Adj-R2: 0.8315; p: 0.0282


# ----- A - Step 2: Step-wise 'non-interaction' model -----

# Create step-wise with stepAIC() 
smmo.t.lm.init <- MASS::stepAIC(smmo.t.lm0, direction="both", trace=F)
summ(smmo.t.lm.init, digits= 4) # Adj-R2: 0.8845; p: 0.0006
pred_r_squared(smmo.t.lm.init) # Pr-R2: 0.7104604

# Check model$call
smmo.t.lm.init$call # ~ lux + temp + pol_abundance + pol_shannon.yj + flo_richness


# ----- A - Step 3: Check assumptions -----

# Check for multi-collinerity: For all vars, less than 3 is good
vif(smmo.t.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(smmo.t.lm.init) # p: 0.3713 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(smmo.t.lm.init) # p: 0.736 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(smmo.t.lm.init)) # p: 0.8393 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(smmo.t.lm.init))
qqline(residuals(smmo.t.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(smmo.t.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(smmo.t.lm.init)

# Initial model: ~ lux + temp + pol_abundance + pol_shannon + flo_richness
summ(smmo.t.lm.init, digits=4) # Adj-R2: 0.8845; p: 0.0006


# ----- B - Step 2: Step-wise 'interaction' model -----

# Create interaction step-wise model using stepAIC()
smmo.t.lm.inter <- stepAIC(smmo.t.lm.init, ~.^2, trace=F)
summ(smmo.t.lm.inter,digits=4)

# Can't make interaction model as initial was already perfect fit model => STOP


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: ~ temp + lux + pol_abundance + pol_shannon + flo_richness + flo_shannon
summ(smmo.t.lm.init, digits=4) # Adj-R2: 0.88; p: 0.0019


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
smmo.t_init_fitval <- predict(smmo.t.lm.init, CF_smmo, interval="confidence") %>%
  data.frame() 
smmo.t_g1 <- fitted_vs_actual(smmo.t_init_fitval, CF_smmo$seedmass_meanopen, 
                              "seedmass_meanopen - Initial Model")

# ==> Use initial step-wise model for the paper


# ------------------------------------------------------------------------------

# ---- Clean-up environment for the next script ----
rm(list=ls())
