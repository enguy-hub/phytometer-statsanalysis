# ---- Prerequisite procedures ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", "jtools",
                   "PerformanceAnalytics", "sjPlot")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Hien/Garden/MyGithub/Phytometer_StatisticalAnalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
tp_path = "./analysis_data/TP/TP_Data_2021_4analysis_garden.xlsx"
TP_data <- read_excel(tp_path, sheet = 1)


# Check structure and summaries of the data sets
str(TP_data)
summary(TP_data)


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


# ------------------------------------------------------------------------------

# ---- Guide for reading lm() output ----
# 1/ RSE: Lower is better
# 2/ Adjusted R-squared: Higher is better
# 3/ F-statistic p-value: Below 0.05 and more significant (*) is better


# ---- Assumptions in linear regression modelling ----

# 1/ In multiple regression, two or more predictors should NOT be related to each other, 
#    so that one predictor can be used to predict the value of the other, and hence the name `independent variables`
#
# 2/ There is a linear relationship between the predictors (independent vars)
#    and the outcome (residuals of response var)
#    * Residual: Actual values - Fitted values (Predicted values) of response var
#
# 3/ Constant variance (homoscedasticity) of errors is another assumption of a linear regression model.
#    The error terms may, for instance, change with the value of the response variable in case of 
#    non-constant variance (heteroscedasticity) of errors.
#
# 4/ Consecutive error terms are UNcorrelated. The standard errors of the estimated 
#    regression coefficients are calculated on the basis of this assumption
#    * If the consecutive error terms are correlated, the standard errors 
#      of the estimated regression coefficients may be much larger.


# ---- Guide for model validation: checking linear regression assumptions  ----

# 1/ Check multicollinearity among independent vars     
#   + Use vif() function from the "car" package with the following syntax:
#     > vif(`input_model`)

# 2/ Check for linear relationship between the predictors and the model outcome, by looking at:
#   a/ Residual plot of "fitted values" vs the "residuals" of the model.
#     + Use residualPlots() function from the "car" package with the following syntax:
#       > residualPlots(`input_model`, type = "rstandard")
#     * Explaining the output from running residualPlots():
#       ~ Blue line: represents the "smooth" pattern between the fitted values (of response var) 
#         and the standard residuals 
#       --> The straighter and more aligned with the "zero" dashed line 
#           the blue line is, the better/more linear the data is.
#
#   b/ QQ plot of the residuals of fitted values to see if it is normally distributed
#     + Use qqnorm() function from the "stats" package with the following syntax:
#       > qqnorm(residuals(`input_model`))
#       > qqline(residuals(`input_model`))
#     + Double check with Shapiro test as follow:
#       > shapiro.test(residuals(`input_model`))
#
#   c/ Component Residual plots (CR plots) of "each predictor" vs the "residuals"
#     + Use ceresPlots() function from the "car" package with the following syntax:
#       > ceresPlots(`input_model`)
#     * CR plot Ref: https://www.r-bloggers.com/2012/01/r-regression-diagnostics-part-1/
#     * Explaining the output from running ceresPlot():
#       ~ Pink line (residual line): represents the relationship between the predictor and residuals. 
#       ~ Blue dashed line (component line): line of best fit. 
#       --> Significant difference between the two lines for a predictor means that 
#           that predictor and the outcome (residuals of response var) don’t have a linear relationship.
#       !!! To fix this type of inconsistency, one could introduce a non-linear transformation 
#           to the "inconsistent predictor" and save it as a new model, with the following syntax:
#           > `new_model_name` <- update(`old_model`, .~.+I(`inconsistent predictor`^1.25))
#
#   e/ Repeat steps 2a, 2b, and 2c to the newly "non-linear transformed" model to see if there is an improvement
#
# 3/ Perform anova() test the new model against the old model, with the following syntax:
#   > anova(`new_model`, `old_model`, test = "F")
#
# 4/ Testing the constant variance (homoscedasticity) of errors using the Breusch-Pagan Test, 
#    with the following syntax:
#   > ncvTest('input_model')
#    H0: Constant variance of errors (p >= 0.05)
#    H1: Error variance changes with the level of the response or with a linear combination of predictors (p < 0.05)
#
# 5/ Testing the correlation of error terms, with the following syntax:
#   > durbinWatsonTest('input_model')
#    H0: The consecutive errors have NO auto-correlation (p >= 0.05)
#    H1: The consecutive errors have auto-correlation (p < 0.05)

# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------

# -- Best lm() model for: flowmass_meandiff ----


# -- Create new dataframe, which remove "non-related" vars ----
TP_fmmd <- TP_data %>%
  dplyr::select(-c("flowmass_meanopen", 
                   "seedmass_meandiff", "seedmass_meanopen",
                   "mass_pseed_meandiff", "mass_pseed_meanopen"))


# -- Check correlation of dependent and independent vars again ----
fmmd_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14)
fmmd_corr <- TP_fmmd[,fmmd_vars]
chart.Correlation(fmmd_corr, histogram=TRUE)


# -- Create multiple regression lm() model ----
fmmd.lm0 <- lm(flowmass_meandiff ~ temp + lux + imperv100 + 
               pol_abundance + pol_richness + pol_shannon + 
               flo_abundance + flo_richness + flo_shannon, data=TP_fmmd)
summ(fmmd.lm0, digits=4) # Adj-R2: 0.3663; p: 0.348


# ---- Create initial model with stepAIC() ---- 
fmmd.lm.init <- MASS::stepAIC(fmmd.lm0, direction = "both", trace = FALSE)
summ(fmmd.lm.init, digits= 4) # Adj-R2: 0.7; p: 0.0063

# Check model$call
fmmd.lm.init$call # flowmass_meandiff ~ temp + pol_abundance + pol_richness + flo_richness

# Check for multi-collinerity: For all vars, less than 3 is good
vif(fmmd.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(fmmd.lm.init) # p: 0.542 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(fmmd.lm.init) # p: 0.196 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(fmmd.lm.init)) # p: 0.964 --> Residuals are norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(fmmd.lm.init))
qqline(residuals(fmmd.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(fmmd.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(fmmd.lm.init)

# Initial model: flowmass_meandiff ~ temp + pol_abundance + pol_richness + flo_richness
summ(fmmd.lm.init, digits= 4) # Adj-R2: 0.7; p: 0.006


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
fmmd.lm.inter <- stepAIC(fmmd.lm.init, ~.^2, trace=F)
summ(fmmd.lm.inter,digits=4) # Adj-R2: 0.7; p: 0.0063

# Same as initial => No significant interaction terms found


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
TP_fmmd <- TP_fmmd %>%
  mutate(sq.temp = temp^2,
         sq.pol_abundance = pol_abundance^2,
         sq.pol_richness = pol_richness^2,
         sq.flo_richness = flo_richness^2)

# Create transformed model
fmmd.lm.init.trans <- lm(flowmass_meandiff ~ sq.temp + sq.pol_abundance + sq.pol_richness + sq.flo_richness, data=TP_fmmd)
summ(fmmd.lm.init.trans, digits=4) # Adj-R2: 0.5677; p: 0.0265

# Use stepAIC() to find the best model and override the old one
fmmd.lm.init.trans <- stepAIC(fmmd.lm.init.trans, direction="both", trace=F)
summ(fmmd.lm.init.trans,digits=4) # Same model as above ==> Adj-R2: 0.5677; p: 0.0265

# Check model$call
fmmd.lm.init.trans$call # flowmass_meandiff ~ sq.temp + sq.pol_abundance + sq.pol_richness + sq.flo_richness

# Check for multi-collinerity: For all vars, less than 3 is good
vif(fmmd.lm.init.trans) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(fmmd.lm.init.trans) # p: 0.477 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(fmmd.lm.init.trans) # p: 0.11 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(fmmd.lm.init.trans)) # p: 0.472 --> Residuals are norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(fmmd.lm.init.trans))
qqline(residuals(fmmd.lm.init.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(fmmd.lm.init.trans, type = "rstandard") # straight line

# Check CERES plot
ceresPlots(fmmd.lm.init.trans)

# Initial model: flowmass_meandiff ~ sq.temp + sq.pol_abundance + sq.pol_richness + sq.flo_richness
summ(fmmd.lm.init.trans, digits= 4) # Adj-R2: 0.5677; p: 0.0265


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
fmmd.lm.inter.trans <- stepAIC(fmmd.lm.init.trans, ~.^2, trace=F)
summ(fmmd.lm.inter.trans,digits=4) # Adj-R2: 0.5677; p: 0.0265

# Same as initial (transformed) model => No significant interaction terms found 


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
fmmd_init_fitval <- predict(fmmd.lm.init, TP_fmmd, interval="confidence") %>%
  data.frame() 
fmmd_g1 <- fitted_vs_actual(fmmd_init_fitval, TP_fmmd$flowmass_meandiff, 
                               "flowmass_meandiff - Initial Model")

# Initial (trans) model
fmmd_init_trans_fitval <- predict(fmmd.lm.init.trans, TP_fmmd, interval="confidence") %>%
  data.frame()
fmmd_g2 <- fitted_vs_actual(fmmd_init_trans_fitval, TP_fmmd$flowmass_meandiff, 
                               "flowmass_meandiff - Initial (Transformed) Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(fmmd_g1,fmmd_g2, ncol=2)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: flowmass_meandiff ~ temp + pol_abundance + pol_richness + flo_richness
summ(fmmd.lm.init, digits= 4) # Adj-R2: 0.7; p: 0.0063

# Initial (trans) model: flowmass_meandiff ~ sq.temp + sq.pol_abundance + sq.pol_richness + sq.flo_richness
summ(fmmd.lm.init.trans, digits= 4) # Adj-R2: 0.5677; p: 0.0265

# No ANOVA tests: The best two models have different predictors


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Initial model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(fmmd.lm.init, confint=T, digits=4, ci.width=.95, center=T, pvals=T)
# Call: flowmass_meandiff ~ temp + pol_abundance + pol_richness + flo_richness
# Adj-R2: 0.7; p: 0.0063

# Plot how predictor 'pol_abundance' is related to response var
plot_model(fmmd.lm.init, type="pred", terms='pol_abundance', show.data=T, line.size=1.3,
           title="Flowerhead Mass vs Pollinator Abundance",
           axis.title=c("pollinator abundance", "fitted values | flowerhead mass of 'open - bagged' flowers"))

# Plot how predictor 'pol_richness' is related to response var
plot_model(fmmd.lm.init, type="pred", terms='pol_richness', show.data=T, line.size=1.3,
           title="Flowerhead Mass vs Pollinator Richness",
           axis.title=c("pollinator richness", "fitted values | flowerhead mass of 'open - bagged' flowers"))

# Plot how predictor 'flo_richness' is related to response var
plot_model(fmmd.lm.init, type="pred", terms='flo_richness', show.data=T, line.size=1.3,
           title="Flowerhead Mass vs Floral Richness",
           axis.title=c("floral richness", "fitted values | flowerhead mass of 'open - bagged'flowers"))


# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------


# -- Best lm() model for: flowmass_meanopen----


# -- Create new dataframe, which remove "non-related" vars ----
TP_fmmo <- TP_data %>%
  dplyr::select(-c("flowmass_meandiff", 
                   "seedmass_meandiff", "seedmass_meanopen",
                   "mass_pseed_meandiff", "mass_pseed_meanopen"))


# -- Check correlation of dependent and independent vars again ----
fmmo_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14)
fmmo_corr <- TP_fmmo[,fmmo_vars]
chart.Correlation(fmmo_corr, histogram=TRUE)


# -- Create multiple regression lm() model ----
fmmo.lm0 <- lm(flowmass_meanopen ~ temp + lux + imperv100 + 
               pol_abundance + pol_richness + pol_shannon + 
               flo_abundance + flo_richness + flo_shannon, data=TP_fmmo)
summ(fmmo.lm0, digits=4) # Adj-R2: 0.259; p: 0.4149


# ---- Create initial model with stepAIC() ---- 
fmmo.lm.init <- MASS::stepAIC(fmmo.lm0, direction = "both", trace = FALSE)
summ(fmmo.lm.init, digits= 4) # Adj-R2: 0.7; p: 0.0009

# Check model$call
fmmo.lm.init$call # flowmass_meanopen ~ pol_abundance + flo_shannon

# Check for multi-collinerity: For all vars, less than 3 is good
vif(fmmo.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(fmmo.lm.init) # p: 0.888 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(fmmo.lm.init) # p: 0.838 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(fmmo.lm.init)) # p: 0.337 --> Residuals are norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(fmmo.lm.init))
qqline(residuals(fmmo.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(fmmo.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(fmmo.lm.init)

# Initial model: flowmass_meanopen ~ pol_abundance + flo_shannon
summ(fmmo.lm.init, digits= 4) # Adj-R2: 0.7; p: 0.0009


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
fmmo.lm.inter <- stepAIC(fmmo.lm.init, ~.^2, trace=F)
summ(fmmo.lm.inter,digits=4) # Adj-R2: 0.7; p: 0.0009

# Same as initial => No significant interaction terms found


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
TP_fmmo <- TP_fmmo %>%
  mutate(sq.pol_abundance = pol_abundance^2,
         sq.flo_shannon = flo_shannon^2)

# Create transformed model
fmmo.lm.init.trans <- lm(flowmass_meanopen ~ sq.pol_abundance + sq.flo_shannon, data=TP_fmmo)
summ(fmmo.lm.init.trans, digits=4) # Adj-R2: 0.7140; p: 0.0008

# Check model$call
fmmo.lm.init.trans$call # flowmass_meanopen ~ sq.pol_abundance + sq.flo_shannon

# Check for multi-collinerity: For all vars, less than 3 is good
vif(fmmo.lm.init.trans) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(fmmo.lm.init.trans) # p: 0.434 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(fmmo.lm.init.trans) # p: 0.92 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(fmmo.lm.init.trans)) # p: 0.2525 --> Residuals are norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(fmmo.lm.init.trans))
qqline(residuals(fmmo.lm.init.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(fmmo.lm.init.trans, type = "rstandard") # straight line

# Check CERES plot
ceresPlots(fmmo.lm.init.trans)

# Initial (transformed) model: flowmass_meanopen ~ sq.pol_abundance + sq.flo_shannon
summ(fmmo.lm.init.trans, digits= 4) # Adj-R2: 0.7140; p: 0.0008


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
fmmo.lm.inter.trans <- stepAIC(fmmo.lm.init.trans, ~.^2, trace=F)
summ(fmmo.lm.inter.trans,digits=4) # Adj-R2: 0.7140; p: 0.0008

# Same as initial (transformed) model => No significant interaction terms found 


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
fmmo_init_fitval <- predict(fmmo.lm.init, TP_fmmo, interval="confidence") %>%
  data.frame() 
fmmo_g1 <- fitted_vs_actual(fmmo_init_fitval, TP_fmmo$flowmass_meanopen, 
                            "flowmass_meanopen - Initial Model")

# Initial (trans) model
fmmo_init_trans_fitval <- predict(fmmo.lm.init.trans, TP_fmmo, interval="confidence") %>%
  data.frame()
fmmo_g2 <- fitted_vs_actual(fmmo_init_trans_fitval, TP_fmmo$flowmass_meanopen, 
                            "flowmass_meanopen - Initial (Transformed) Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(fmmo_g1,fmmo_g2, ncol=2)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: flowmass_meanopen ~ pol_abundance + flo_shannon
summ(fmmo.lm.init, digits= 4) # Adj-R2: 0.7; p: 0.0009

# Initial (trans) model: flowmass_meanopen ~ sq.pol_abundance + sq.flo_shannon
summ(fmmo.lm.init.trans, digits= 4) # Adj-R2: 0.714; p: 0.0008

# No ANOVA tests: The best two models have different predictors


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Initial model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(fmmo.lm.init, confint = TRUE, digits=4, ci.width = .95, center=T, pvals=T)
# Call: flowmass_meanopen ~ pol_abundance + flo_shannon
# Adj-R2: 0.71; p: 0.0009

# Plot how predictor 'pol_abundance' is related to response var
plot_model(fmmo.lm.init, type="pred", terms='pol_abundance', show.data=T, line.size=1.3,
           title="Flowerhead Mass vs Pollinator Abundance",
           axis.title=c("pollinator abundance", "fitted values | flowerhead mass of 'open flowers'"))

# Plot how predictor 'flo_shannon' is related to response var
plot_model(fmmo.lm.init, type="pred", terms='flo_shannon', show.data=T, line.size=1.3,
           title="Flowerhead Mass vs Floral Diversity Shannon Index",
           axis.title=c("floral diversity shannon index", "fitted values | flowerhead mass of 'open flowers'"))


# ---- Initial (transformed) model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(fmmo.lm.init.trans, confint = TRUE, digits=4, ci.width = .95, center=T, pvals=T)
# Call: flowmass_meanopen ~ sq.pol_abundance + sq.flo_shannon
# Adj-R2: 0.714; p: 0.0008

# Plot how predictor 'sq.pol_abundance' is related to response var
plot_model(fmmo.lm.init.trans, type="pred", terms='sq.pol_abundance', show.data=T, line.size=1.3,
           title="Flowerhead Mass vs Pollinator Abundance (Squared Transformed)",
           axis.title=c("pollinator abundance (squared transformed)", "fitted values | flowerhead mass of 'open flowers'"))

# Plot how predictor 'sq.flo_shannon' is related to response var
plot_model(fmmo.lm.init.trans, type="pred", terms='sq.flo_shannon', show.data=T, line.size=1.3,
           title="Flowerhead Mass vs Floral Diversity Shannon Index (Squared Transformed)",
           axis.title=c("floral diversity shannon index (squared transformed)", "fitted values | flowerhead mass of 'open flowers'"))


# ------------------------------------------------------------------------------


# ---- Clean-up environment for the next script ----
rm(list=ls())

