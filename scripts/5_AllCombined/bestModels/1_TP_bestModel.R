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
tp_path = "./analysis_data/TP/TP_Data_2021_4analysis_garden_transformed.xlsx"
TP_data <- read_excel(tp_path, sheet = 1)


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
# -------------------------- T. pratense ---------------------------------------
# ------------- Resp variable: seed mass (mass_pseed_meanopen) -----------------
# ------------------------------------------------------------------------------

# Check structure and summaries of the data sets
str(TP_data)
summary(TP_data)

# Remove "Non-normal distributed" variables
TP_data <- TP_data %>%
  dplyr::select(-c("urbanclass100", "urbanclass200", "urbanclass500", "urbanclass1000"))

# Create new dataframe, which remove "non-related" vars
TP_mpsmo <- TP_data %>%
  dplyr::select(-c("flowmass_meandiff", "flowmass_meanopen", 
                   "seedmass_meandiff", "seedmass_meanopen",
                   "mass_pseed_meandiff"))

# Check correlation of dependent and independent vars again
mpsmo.t_vars <- c(2,3,4,5,6,7,8,10,11,12,14,15,16)
mpsmo.t_corr <- TP_mpsmo[,mpsmo.t_vars]
chart.Correlation(mpsmo.t_corr, histogram=T)


# ----- Step 1: Create initial "full" model -----

# Create initial "full" multiple regression lm() model
mpsmo.t.lm0 <- lm(mass_pseed_meanopen ~ imperv1000 + # imperv100 +
                    lux + # temp + 
                    flo_richness + pol_richness + 
                    pol_shannon + flo_shannon +
                    pol_abundance.yj + flo_abundance.yj
                  ,data=TP_mpsmo)
summ(mpsmo.t.lm0, digits=4) # Adj-R2: 0.38; p: 0.2765


# ----- A - Step 2: Step-wise 'non-interaction' model -----

# Create step-wise with stepAIC() 
mpsmo.t.lm.init <- MASS::stepAIC(mpsmo.t.lm0, direction="both", trace=F)
summ(mpsmo.t.lm.init, digits= 4) # Adj-R2: 0.5548; p: 0.0495
pred_r_squared(mpsmo.t.lm.init) # Pr-R2: 0.0489

# Check model$call
mpsmo.t.lm.init$call # ~ lux + flo_richness + pol_richness + flo_shannon + flo_abundance.yj


# ----- A - Step 3: Check assumptions -----

# Check for multi-collinerity: For all vars, less than 3 is good
vif(mpsmo.t.lm.init) %>% 
  knitr::kable() # All <= 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mpsmo.t.lm.init) # p: 0.7372 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mpsmo.t.lm.init) # p: 0.64 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mpsmo.t.lm.init)) # p: 0.04 --> Residuals NOT norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mpsmo.t.lm.init))
qqline(residuals(mpsmo.t.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mpsmo.t.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(mpsmo.t.lm.init)

# Initial model: ~ lux + flo_richness + pol_richness + flo_shannon + flo_abundance.yj
summ(mpsmo.t.lm.init, digits= 4) # Adj-R2: 0.5548; p: 0.0495


# ----- B - Step 2: Step-wise 'interaction' model -----

# Create interaction step-wise model using stepAIC()
mpsmo.t.lm.inter <- stepAIC(mpsmo.t.lm.init, ~.^2, trace=F)
summ(mpsmo.t.lm.inter,digits=4) # Adj-R2: 0.3794; p: 0.0651

# Can't make interaction model as initial was already perfect fit model => STOP


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: ~ lux + flo_richness + pol_richness + flo_abundance.yj + flo_shannon
summ(mpsmo.t.lm.init, digits=4, center=T) # Adj-R2: 0.5548; p: 0.0495


# ----- Linear graphs to compare initial models against best model(s) -----

# Initial model
mpsmo.t_init_fitval <- predict(mpsmo.t.lm.init, TP_mpsmo, interval="confidence") %>%
  data.frame() 
mpsmo.t_g1 <- fitted_vs_actual(mpsmo.t_init_fitval, TP_mpsmo$mass_pseed_meanopen, 
                               "mass_pseed_meanopen - Initial Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(mpsmo.t_g1, ncol=1)

# ==> Use initial model for the paper


# ------------------------------------------------------------------------------

# ---- Clean-up environment for the next script ----
rm(list=ls())
