# ------------------------------------------------------------------------------
# -------------------------- Prerequisites -------------------------------------
# ------------------------------------------------------------------------------

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", 
                   "jtools", "PerformanceAnalytics", "sjPlot", "ggpubr")
lapply(list_packages, library, character.only = TRUE)

# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
setwd(pdir)

# Set path and read the data
ra_path = "./analysis_data/RA/RA_Data_2021_4analysis_garden_transformed.xlsx"

# Read the data
RA_data <- read_excel(ra_path, sheet = 1)


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
# ---------------------------- R. acris ----------------------------------------
# -------------- Resp variable: fruit mass (mass_meanopen) ---------------------
# ------------------------------------------------------------------------------

# Check structure and summaries of the data
str(RA_data)
summary(RA_data)

# Remove "Unnecessary" variables
RA_data <- RA_data %>%
  dplyr::select(-c("urbanclass100", "urbanclass200", "urbanclass500", "urbanclass1000"))

# Create new dataframe, which remove "non-related" vars
RA_admfs <- RA_data %>%
  dplyr::select(-c("imperv200", "imperv500",
                   "fremass_meandiff", "drymass_meandiff", 
                   "fremass_meanopen", "drymass_meanopen",
                   "avgfremass_pseed_meandiff", "avgdrymass_pseed_meandiff",
                   "avgfremass_pseed_meanopen", "avgdrymass_pseed_meanopen",
                   "avgfremass_ferseed"))

# Check correlation of dependent and independent vars again
admfs_vars <- c(2,3,4,5,6,7,9,10,11,13,14)
admfs_corr <- RA_admfs[,admfs_vars]
chart.Correlation(admfs_corr, histogram=T)


# ----- Step 1: Create initial "full" model -----

# Create initial "full" multiple regression lm() model
admfs.lm0 <- lm(avgdrymass_ferseed ~ imperv100 + # imperv1000 +
                  lux + # temp + 
                  pol_shannon + # flo_shannon +
                  flo_richness + # pol_richness + 
                  flo_abundance + pol_abundance
                ,data=RA_admfs)
summ(admfs.lm0, digits=4) # Adj-R2: 0.3937; p: 0.1264


# ----- A - Step 2: Step-wise 'non-interaction' model -----

# Create step-wise with stepAIC()
admfs.lm.init <- MASS::stepAIC(admfs.lm0, direction="both", trace=F)
summ(admfs.lm.init, digits=4, center=T) # Adj-R2: 0.5341; p: 0.035
pred_r_squared(admfs.lm.init)

# Check model$call
admfs.lm.init$call # ~ imperv100 + pol_abun + flo_ric + flo_abundance 


# ----- A - Step 3: Check assumptions -----

# Check for multi-collinerity: For all vars, less than 3 is good
vif(admfs.lm.init) %>% 
  knitr::kable() # Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(admfs.lm.init) # p: 0.0092 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(admfs.lm.init) # p: 0.874 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(admfs.lm.init)) # p: 0.03495 --> Residuals ARE NOT norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(admfs.lm.init))
qqline(residuals(admfs.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(admfs.lm.init, type="rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(admfs.lm.init)

# Initial model: ~ imperv100 + pol_abundance + flo_abundance + flo_richness
summ(admfs.lm.init, digits=4) # Adj-R2: 0.46; p: 0.0361


# ----- B - Step 2: Step-wise 'interaction' model -----

# Create interaction model (from initial model) using stepAIC()
admfs.lm.inter <- stepAIC(admfs.lm.init, ~.^2, trace=F)
summ(admfs.lm.inter,digits=4) # Adj-R2: 0.772; p: 0.0255
pred_r_squared(admfs.lm.init)

# Check model$call
admfs.lm.inter$call # ~ imperv100 * pol_abundance * flo_richness + flo_abundance


# ----- B - Step 3: Check assumptions -----

# Check for multi-collinerity: For all vars, less than 3 is good
vif(admfs.lm.inter, type = 'predictor') %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(admfs.lm.inter) # p: 0.267 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(admfs.lm.inter) # p: 0.852 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(admfs.lm.inter)) # p: 0.57 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(admfs.lm.inter))
qqline(residuals(admfs.lm.inter))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(admfs.lm.inter, type = "rstandard") # curve --> slight non-linearity

# Initial model: avgdrymass_ferseed ~ imperv100 * pol_abundance * flo_richness + flo_abundance
summ(admfs.lm.inter, digits=4, center=T) # Adj-R2: 0.772; p: 0.0255


# ----- Compare Adj-R2, p-value, and ANOVA test of the models -----

# Initial model: ~ imperv100 + pol_abundance + flo_abundance + flo_richness
summ(admfs.lm.init, digits=4, center=T) # Adj-R2: 0.5341; p: 0.0350

# Interaction model: ~ imperv100 * pol_abundance * flo_richness + flo_abundance
summ(admfs.lm.inter, digits=4, center=T) # Adj-R2: 0.7717; p: 0.0255

# Anova testing between models
anova(admfs.lm.init, admfs.lm.inter, test="F") # p: 2.2e-16 => Improved


# ----- Linear graphs to compare initial models against best model(s) -----

# Initial model
admfs_init_fitval <- predict(admfs.lm.init, RA_admfs, interval="confidence") %>%
  data.frame() 
admfs_g1 <- fitted_vs_actual(admfs_init_fitval, RA_admfs$avgdrymass_ferseed, 
                             "avgdrymass_ferseed - Initial Model")

# Interaction model
admfs_inter_fitval <- predict(admfs.lm.inter, RA_admfs, interval="confidence") %>%
  data.frame() 
admfs_g2 <- fitted_vs_actual(admfs_inter_fitval, RA_admfs$avgdrymass_ferseed, 
                             "avgdrymass_ferseed - Interaction Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(admfs_g1,admfs_g2, ncol=2)

# ==> Use interaction model for the paper


# ------------------------------------------------------------------------------

# ---- Clean-up environment for the next script ----
rm(list=ls())
