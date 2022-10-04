# -- Prerequisites ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", "jtools",
                   "PerformanceAnalytics", "sjPlot")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data ###
fa_path = "./analysis_data/FA/FA_Data_2021_4analysis_garden_transformed.xlsx"
FA_data <- read_excel(fa_path, sheet = 1)


# Check structure and summaries of the data
str(FA_data)
summary(FA_data)


# Remove "Non-normal distributed" variables
FA_data <- FA_data %>%
  dplyr::select(-c("pol_abundance", "flo_abundance", "flo_richness", # , "pol_shannon"
                   "urbanclass100", "urbanclass200", "urbanclass500", "urbanclass1000")) # 


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

# ------------------ Best model for: mass_meanopen -----------------------------

# ---- Create new dataframe, without "non-related" vars ----
FA_mmo <- FA_data %>%
  dplyr::select(-c("mass_meandiff", "ratio_meandiff", "ratio_meanopen"))


# ---- Check correlation of dependent and independent vars again ----
mmo_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
mmo_corr <- FA_mmo[,mmo_vars]
chart.Correlation(mmo_corr, histogram=TRUE)


# ---- Create initial multiple regression lm() model ----
mmo.lm0 <- lm(mass_meanopen ~ temp + # lux + 
                imperv1000 + # imperv100 +
                pol_richness + # pol_shannon.yj + pol_abundance.yj +     
                flo_richness.yj # + flo_shannon + flo_abundance.yj
              , data=FA_mmo)
summ(mmo.lm0) # Adj-R2: 0.37; p: 0.19
pred_r_squared(mmo.lm0) # -0.37

# ---- Create initial model with stepAIC() ----
mmo.lm.init <- MASS::stepAIC(mmo.lm0, direction="both", trace=F)
summ(mmo.lm.init) # Adj-R2: 0.48 ; p: 0.031
pred_r_squared(mmo.lm.init)

# Check model$call
mmo.lm.init$call # mass_meanopen ~ temp + imp1000 + pol_shannon.yj

# Check for multi-collinerity: For all vars, less than 3 is good
vif(mmo.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mmo.lm.init, type = "rstandard") # => slight non-linearity

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmo.lm.init))
qqline(residuals(mmo.lm.init))

# Shapiro test
shapiro.test(residuals(mmo.lm.init)) # p: 0.77 --> Residuals are norm-dist

# Check CERES plot
ceresPlots(mmo.lm.init)

# Initial model: mass_meanopen ~ temp + imp1000 + pol_shannon.yj
summ(mmo.lm.init, digits= 4) # Adj-R2: 0.48; p: 0.031


# ---- Create interaction model ----

# Add interaction to initial model
mmo.lm.inter <- stepAIC(mmo.lm.init, ~.^2, trace=F)
summ(mmo.lm.inter, digits=4) # Adj-R2: 0.7785 ; p: 0.0021

# Check model$call
mmo.lm.inter$call # mass_meanopen ~ temp + imperv1000 + pol_shannon.yj + imperv1000:pol_shannon.yj

# Check residual plot
residualPlots(mmo.lm.inter, type = "rstandard") # => curve line

# Check if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmo.lm.inter))
qqline(residuals(mmo.lm.inter))

# Shapiro test
shapiro.test(residuals(mmo.lm.inter)) # p: 0.086 --> Residuals are norm-dist

# Interaction model: mass_meanopen ~ temp + imperv1000 * pol_shannon.yj
summ(mmo.lm.inter, digits= 4) # Adj-R2: 0.7785 ; p: 0.0021


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: mass_meanopen ~ temp + imperv1000 + pol_shannon.yj
summ(mmo.lm.init, digits= 4) # Adj-R2: 0.479 ; p: 0.031

# Interaction model: mass_meanopen ~ temp + imperv1000 * pol_shannon.yj
summ(mmo.lm.inter, digits= 4) # Adj-R2: 0.7785; p: 0.0021

# Anova testing between models
anova(mmo.lm.init, mmo.lm.inter, test="F") # p: 0.00067 => Much improved


# ---- Testing/Checking linear assumptions for best model(s) ----

# Constant variance (homoscedasticity) of errors testing: > 0.05 is pass --> Constant variance of errors
ncvTest(mmo.lm.inter) # p: 0.45 --> Pass 

# Auto correlated Errors test: p-value more than 0.05 is good
# Null: no correlation between the residuals --> the residuals are independent.
set.seed(1)
durbinWatsonTest(mmo.lm.inter) # p: 0.7 --> Pass

# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
mmo_init_fitval <- predict(mmo.lm.init, FA_mmo, interval = "confidence") %>%
  data.frame() 
mmo_g1 <- fitted_vs_actual(mmo_init_fitval, FA_mmo$mass_meanopen, 
                           "mass_meandiff - Initial Model")

# Interaction model
mmo_inter_fitval <- predict(mmo.lm.inter, FA_mmo, interval = "confidence") %>%
  data.frame()
mmo_g2 <- fitted_vs_actual(mmo_inter_fitval, FA_mmo$mass_meanopen,  
                           "mass_meanopen - Interaction Model")

# ---- Plotting the relationship of vars in the best model(s) ----

# ---- Interaction model ----

# Estimated coefficients of the predictors and their confidence intervals
summ(mmo.lm.inter, confint=T, digits=4, ci.width = .95, center=T, pvals=T)
# Call: mass_meanopen~temp+imperv1000*pol_shannon.yj
# Adj-R2: 0.7785; p: 0.0021

# Plot how predictor 'temp' is related to response var
plot_model(mmo.lm.inter, type="pred", terms='temp', show.data=T, line.size=1.3)

# Plot how predictor 'imperv1000' is related to response var
plot_model(mmo.lm.inter, type="pred", terms='imperv1000', show.data=T, line.size=1.3)

# Plot how predictor 'pol_shannon.yj' is related to response var
plot_model(mmo.lm.inter, type="pred", terms='pol_shannon.yj', show.data=T, line.size=1.3)


# Plot how 'imperv1000 * pol_shannon.yj' is related to the fitted values of the response var
p_mmo.inter_imp.pol <- plot_model(mmo.lm.inter, type="pred", line.size=1.3,
                                  terms=c("imperv1000", "pol_shannon.yj")) # [1.2, 4.4]  
p_mmo.inter_pol.imp <- plot_model(mmo.lm.inter, type="pred",  line.size=1.3,
                                  terms=c("pol_shannon.yj", "imperv1000")) # [0.52, 0.75]  

gridExtra::grid.arrange(p_mmo.inter_imp.pol, p_mmo.inter_pol.imp, ncol=2)


# ------------------------------------------------------------------------------


# -- Clean-up environment for the next script ----
rm(list=ls())

