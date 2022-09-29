# -- Prerequisites ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", "jtools",
                   "PerformanceAnalytics", "sjPlot")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Garden/MyGithub/Phytometer_StatisticalAnalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
fa_path = "./analysis_data/FA/FA_Data_2021_4analysis_garden.xlsx"
FA_data <- read_excel(fa_path, sheet = 1)


# Check structure and summaries of the data
str(FA_data)
summary(FA_data)


# Remove "Non-normal distributed" variables
FA_data <- FA_data %>%
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

# ------------- Best lm() model for: mass_meandiff -----------------------------

# ---- Create new dataframe, without "non-related" vars ----
FA_mmd <- FA_data %>%
  dplyr::select(-c("mass_meanopen", "ratio_meandiff", "ratio_meanopen"))


# ---- Check correlation of dependent and independent vars again ----
mmd_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14)
mmd_corr <- FA_mmd[,mmd_vars]
chart.Correlation(mmd_corr, histogram=TRUE)


# ---- Create multiple regression lm() model ----
mmd.lm0 <- lm(mass_meandiff ~ temp + lux + imperv1000 + 
              pol_shannon + pol_abundance + pol_richness +  
              flo_abundance + flo_richness + flo_shannon, data=FA_mmd) # 
summ(mmd.lm0, digits=4) # Adj-R2: 0.442; p: 0.18


# ---- Create initial model with stepAIC() ----
mmd.lm.init <- MASS::stepAIC(mmd.lm0, direction="both", trace=FALSE)
summ(mmd.lm.init, digits= 4) # Adj-R2: 0.59; p: 0.021

# Check model$call
mmd.lm.init$call # mass_meandiff ~ temp+imperv1000+pol_shannon+flo_shannon

# Check for multi-collinerity: For all vars, less than 3 is good
vif(mmd.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (p > 0.05 = pass):
ncvTest(mmd.lm.init) # p: 0.467 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mmd.lm.init) # p: 0.65 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mmd.lm.init)) # p: 0.556 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmd.lm.init))
qqline(residuals(mmd.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mmd.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(mmd.lm.init)

# Initial model: mass_meandiff ~ temp+imperv1000+pol_shannon+flo_shannon
summ(mmd.lm.init, digits= 4) # Adj-R2: 0.5913 ; p: 0.0216


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
mmd.lm.inter <- stepAIC(mmd.lm.init, ~.^2, trace=F)
summ(mmd.lm.inter,digits=4) # Adj-R2: 0.591; p: 0.0216 

# Same as initial => No significant interaction terms found


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
FA_mmd <- FA_mmd %>%
  mutate(sq.temp = temp^2,
         sq.imperv1000 = imperv1000^2,
         sq.pol_shannon = pol_shannon^2,
         sq.flo_shannon = flo_shannon^2)

# Create transformed model
mmd.lm.init.trans <- lm(mass_meandiff ~ sq.temp + sq.imperv1000 + 
                        sq.pol_shannon + sq.flo_shannon, data=FA_mmd)
summ(mmd.lm.init.trans, digits=4) # Adj-R2: 0.705 ; p: 0.0063

# Use stepAIC() to find the best model and override the old one
mmd.lm.init.trans <- stepAIC(mmd.lm.init.trans, direction="both", trace=F)
summ(mmd.lm.init.trans,digits=4) # Adj-R2: 0.72; p: 0.0007

# Check model$call
mmd.lm.init.trans$call # mass_meandiff ~ sq.imperv1000 + sq.pol_shannon

# Check for multi-collinerity: For all vars, less than 3 is good
vif(mmd.lm.init.trans) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (p > 0.05 = pass):
ncvTest(mmd.lm.init.trans) # p: 0.8877 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mmd.lm.init.trans) # p: 0.796 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mmd.lm.init.trans)) # p: 0.082 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmd.lm.init.trans))
qqline(residuals(mmd.lm.init.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mmd.lm.init.trans, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(mmd.lm.init.trans)

# Initial model: mass_meandiff ~ sq.imperv1000 + sq.pol_shannon
summ(mmd.lm.init.trans, digits= 4) # Adj-R2: 0.72 ; p: 0.0007


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
mmd.lm.inter.trans <- stepAIC(mmd.lm.init.trans, ~.^2, trace=F)
summ(mmd.lm.inter.trans,digits=4) # Adj-R2: 0.72 ; p: 0.0007

# Same as initial (transformed) model => No significant interaction terms found 


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
mmd_init_fitval <- predict(mmd.lm.init, FA_mmd, interval="confidence") %>%
  data.frame() 
mmd_g1 <- fitted_vs_actual(mmd_init_fitval, FA_mmd$mass_meandiff, 
                           "mass_meandiff - Initial Model")

# Initial (trans) model
mmd_init_trans_fitval <- predict(mmd.lm.init.trans, FA_mmd, interval="confidence") %>%
  data.frame()
mmd_g2 <- fitted_vs_actual(mmd_init_trans_fitval, FA_mmd$mass_meandiff, 
                           "mass_meandiff - Initial (Transformed) Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(mmd_g1,mmd_g2, ncol=2)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: mass_meandif ~ temp + imperv1000 + pol_shannon + flo_shannon
summ(mmd.lm.init, digits=4) # Adj-R2: 0.591; p: 0.022

# Initial (trans) model: mass_meandiff ~ sq.imperv1000 + sq.pol_shannon
summ(mmd.lm.init.trans, digits=4) # Adj-R2: 0.717; p: 0.0007

# No ANOVA tests: The best two models have different predictors


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Initial model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(mmd.lm.init, confint=T, digits=4, ci.width=.95, center=T, pvals=T)
# Call: mass_meandiff~ temp + imperv1000 + pol_shannon + flo_shannon
# Adj-R2: 0.59; p: 0.022

tab_model(mmd.lm.init)

# Plot how predictor 'sq.imperv1000' is related to response var
plot_model(mmd.lm.init, type="pred", terms='imperv1000', show.data=T, line.size=1.3,
           title="Fragaria Fruit Mass vs Impervious Surface",
           axis.title=c("impervious surface (%) of 1000m buffer", "fitted values | fruit mass of `open - bagged` flowers"))

# Plot how predictor 'pol_shannon' is related to response var
plot_model(mmd.lm.init, type="pred", terms='pol_shannon', show.data=T, line.size=1.3,
           title="Fragaria Fruit Mass vs Pollinator Diversity Shannon Index",
           axis.title=c("pollinator diversity shannon index", "fitted values | fruit mass of `open - bagged` flowers"))


# ---- Initial (transformed) model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(mmd.lm.init.trans, confint=T, digits=4, ci.width=.95, center=T, pvals=T)
# Call: mass_meandiff ~ sq.imperv1000 + sq.pol_shannon
# Adj-R2: 0.717; p: 0.0007

tab_model(mmd.lm.init.trans)

# Plot how predictor 'sq.imperv1000' is related to response var
plot_model(mmd.lm.init.trans, type="pred", terms='sq.imperv1000', show.data=T, line.size=1.3,
           title="Fragaria Fruit Mass vs Impervious Surface (Squared Transformed)",
           axis.title=c("impervious surface (%) of 1000m buffer (squared transformed)", "fitted values | fruit mass of `open - bagged` flowers"))

# Plot how predictor 'sq.pol_shannon' is related to response var
plot_model(mmd.lm.init.trans, type="pred", terms='sq.pol_shannon', show.data=T, line.size=1.3,
           title="Fragaria Fruit Mass vs Pollinator Diversity Shannon Index (Squared Transformed)",
           axis.title=c("pollinator diversity shannon index (squared transformed)", "fitted values | fruit mass of `open - bagged` flowers"))

# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------

# ------------- Best lm() model for: mass_meanopen -----------------------------


# -- Create new dataframe, which remove "non-related" vars ----
FA_mmo <- FA_data %>%
  dplyr::select(-c("mass_meandiff", "ratio_meandiff", "ratio_meanopen"))


# -- Check correlation of dependent and independent vars again ----
mmo_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14)
mmo_corr <- FA_mmo[,mmo_vars]
chart.Correlation(mmo_corr, histogram=TRUE)


# -- Create multiple regression lm() model ----
mmo.lm0 <- lm(mass_meanopen ~ temp + lux + imperv1000 + 
              pol_shannon +  # pol_abundance + pol_richness +  
              flo_abundance + flo_richness + flo_shannon, data=FA_mmo)
summ(mmo.lm0) # Adj-R2: 0.10; p: 0.44


# ---- Create initial model with stepAIC() ----
mmo.lm.init <- MASS::stepAIC(mmo.lm0, direction="both", trace=F)
summ(mmo.lm.init, digits= 4) # Adj-R2: 0.379; p: 0.098

# Check model$call
mmo.lm.init$call # ~ temp + imp1000 + pol_shannon + flo_shannon

# Check for multi-collinerity: For all vars, less than 3 is good
vif(mmo.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mmo.lm.init) # p: 0.32 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mmo.lm.init) # p: 0.16 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mmo.lm.init)) # p: 0.421 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmo.lm.init))
qqline(residuals(mmo.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mmo.lm.init, type = "rstandard") # => slight non-linearity

# Check CERES plot
ceresPlots(mmo.lm.init)

# Initial model: ~ temp + imperv1000 + pol_shannon + flo_shannon
summ(mmo.lm.init, digits= 4) # Adj-R2: 0.379 ; p: 0.0981


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
mmo.lm.inter <- stepAIC(mmo.lm.init, ~.^2, trace=F)
summ(mmo.lm.inter,digits=4) # Adj-R2: 0.682; p: 0.0168 

# Check model$call
mmo.lm.inter$call # ~ temp + imp1000 * pol_shannon + flo_shannon

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mmo.lm.inter) # p: 0.93 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mmo.lm.inter) # p: 0.7 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mmo.lm.inter)) # p: 0.157 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmo.lm.inter))
qqline(residuals(mmo.lm.inter))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mmo.lm.inter, type = "rstandard") # => slight non-linearity

# Interaction model: ~ temp + imp1000 * pol_shannon + flo_shannon
summ(mmo.lm.inter, digits= 4) # Adj-R2: 0.6824 ; p: 0.0168


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
FA_mmo <- FA_mmo %>%
  mutate(sq.temp = temp^2,
         sq.imperv1000 = imperv1000^2,
         sq.pol_shannon = pol_shannon^2,
         sq.flo_shannon = flo_shannon^2)

# Create transformed model
mmo.lm.init.trans <- lm(mass_meanopen ~ sq.temp + sq.imperv1000 + 
                        sq.pol_shannon + sq.flo_shannon, data=FA_mmo)
summ(mmo.lm.init.trans, digits=4) # Adj-R2: 0.492 ; p: 0.0478

# Use stepAIC() to find the best model and override the old one
mmo.lm.init.trans <- stepAIC(mmo.lm.init.trans, direction="both", trace=F)
summ(mmo.lm.init.trans,digits=4) # Adj-R2: 0.5398; p: 0.0183

# Check model$call
mmo.lm.init.trans$call # ~ sq.temp + sq.imperv1000 + sq.pol_shannon

# Check for multi-collinerity: For all vars, less than 3 is good
vif(mmo.lm.init.trans) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mmo.lm.init.trans) # p: 0.7 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mmo.lm.init.trans) # p: 0.16 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mmo.lm.init.trans)) # p: 0.7675 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmo.lm.init.trans))
qqline(residuals(mmo.lm.init.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mmo.lm.init.trans, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(mmo.lm.init.trans)

# Initial (transformed) model: ~ sq.temp + sq.imperv1000 + sq.pol_shannon
summ(mmo.lm.init.trans, digits= 4) # Adj-R2: 0.5398 ; p: 0.0183


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
mmo.lm.inter.trans <- stepAIC(mmo.lm.init.trans, ~.^2, trace=F)
summ(mmo.lm.inter.trans,digits=4) # Adj-R2: 0.792 ; p: 0.0016

# Check model$call
mmo.lm.inter.trans$call # ~ sq.temp + sq.imperv1000 * sq.pol_shannon

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mmo.lm.inter.trans) # p: 0.28 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mmo.lm.inter.trans) # p: 0.95 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mmo.lm.inter.trans)) # p: 0.192 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmo.lm.inter.trans))
qqline(residuals(mmo.lm.inter.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mmo.lm.inter.trans, type = "rstandard") # curve --> slight non-linearity

# Interaction (trans) model: ~ sq.temp + sq.imperv1000 * sq.pol_shannon
summ(mmo.lm.inter.trans, digits= 4) # Adj-R2: 0.792 ; p: 0.0016


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
mmo_init_fitval <- predict(mmo.lm.init, FA_mmo, interval = "confidence") %>%
  data.frame() 
mmo_g1 <- fitted_vs_actual(mmo_init_fitval, FA_mmo$mass_meanopen,
                           "mass_meanopen - Initial Model")

# Interaction model
mmo_inter_fitval <- predict(mmo.lm.inter, FA_mmo, interval = "confidence") %>%
  data.frame()
mmo_g2 <- fitted_vs_actual(mmo_inter_fitval, FA_mmo$mass_meanopen,
                           "mass_meanopen - Interaction Model")

# Initial (trans) model
mmo_init_trans_fitval <- predict(mmo.lm.init.trans, FA_mmo, interval = "confidence") %>%
  data.frame()
mmo_g3 <- fitted_vs_actual(mmo_init_trans_fitval, FA_mmo$mass_meanopen,
                           "mass_meanopen - Initial (Transformed) Model")

# Interaction (trans) model
mmo_inter_trans_fitval <- predict(mmo.lm.inter.trans, FA_mmo, interval = "confidence") %>%
  data.frame()
mmo_g4 <- fitted_vs_actual(mmo_inter_trans_fitval, FA_mmo$mass_meanopen,
                           "mass_meanopen - Interaction (Transformed) Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(mmo_g1, mmo_g2, mmo_g3, mmo_g4, ncol=2)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: ~ temp + imperv1000 + pol_shannon + flo_shannon
summ(mmo.lm.init, digits= 4) # Adj-R2: 0.379; p: 0.098

# Interaction model: ~ temp + imperv1000 * pol_shannon + flo_shannon
summ(mmo.lm.inter, digits= 4) # Adj-R2: 0.682; p: 0.0168

# Initial (trans) model: ~ sq.temp + sq.imperv1000 + sq.pol_shannon
summ(mmo.lm.init.trans, digits= 4) # Adj-R2: 0.5398; p: 0.0183

# Interaction (trans) model: ~ sq.temp + sq.imperv1000 * sq.pol_shannon
summ(mmo.lm.inter.trans, digits= 4) # Adj-R2: 0.792; p: 0.0016

# Anova testing between models
anova(mmo.lm.inter.trans, mmo.lm.init.trans, test="F") # p: 0.0086 => Improved
anova(mmo.lm.init, mmo.lm.inter, test="F") # p: 0.0217 => Improved


# ---- Plotting the relationship of vars in the best model(s) ----

# ---- Interaction model ----

# Estimated coefficients of the predictors and their confidence intervals
summ(mmo.lm.inter, confint=T, digits=4, ci.width=.95, center=T, pvals=T)
# Call: mass_meanopen ~ temp + imperv1000 * pol_shannon + flo_shannon
# Adj-R2: 0.68; p: 0.0168

# Plot how predictor 'temp' is related to response var
plot_model(mmo.lm.inter, type="pred", terms='temp', show.data=T, line.size=1.3,
           title="Fragaria Fruit Mass vs Temperature",
           axis.title=c("temperature celsius", "fitted values | fruit mass of `open` flowers"))

# Plot how predictor 'imperv1000' is related to response var
plot_model(mmo.lm.inter, type="pred", terms='imperv1000', show.data=T, line.size=1.3,
           title="Fragaria Fruit Mass vs Impervious Surface",
           axis.title=c("impervious surface (%) of 1000m buffer", "fitted values | fruit mass of `open` flowers"))

# Plot how predictor 'pol_shannon' is related to response var
plot_model(mmo.lm.inter, type="pred", terms='pol_shannon', show.data=T, line.size=1.3,
           title="Fragaria Fruit Mass vs Pollinator Diversity Shannon Index",
           axis.title=c("pollinator diversity shannon index", "fitted values | fruit mass of `open` flowers"))


# Plot how 'imperv1000 * pol_shannon' is related to the fitted values of the response var
p_mmo.inter_imp.pol <- 
  plot_model(mmo.lm.inter, type="pred", line.size=1.3,
             terms=c("imperv1000", "pol_shannon"), # [1.16, 1.72]
             title="Fragaria Fruit Mass vs\nImpervious Surface\n(%) of 1000m Buffer
                    \nInteraction: Pollinator Diversity\nShannon Index",
             axis.title=c("impervious surface (%) of 1000m\nbuffer", "fitted values | fruit mass of `open` flowers"),
             legend.title="pollinator\ndiversity\nshannon\nindex")
p_mmo.inter_imp.pol

p_mmo.inter_pol.imp <- 
  plot_model(mmo.lm.inter, type="pred", line.size=1.3,
             terms=c("pol_shannon", "imperv1000"), # [0.52, 0.64]
             title="Fragaria Fruit Mass vs\nPollinator Diversity\nShannon Index
                    \nInteraction: Impervious\nSurface (%) of\n1000m Buffer",
             axis.title=c("pollinator diversity shannon index", "fitted values | fruit mass of `open` flowers"),
             legend.title="impervious\nsurface\n(%) of\n1000m buffer")    
p_mmo.inter_pol.imp

gridExtra::grid.arrange(p_mmo.inter_imp.pol, p_mmo.inter_pol.imp, ncol=2)


# ---- Interaction (Transformed) model ----

# Estimated coefficients of predictors and their confidence intervals
summ(mmo.lm.inter.trans, confint=T, digits=4, ci.width=.95, center=T, pvals=T)
# Call: mass_meanopen ~ sq.temp + sq.imperv1000 * sq.pol_shannon
# Adj-R2: 0.792; p: 0.0016

# Plot how predictor 'sq.temp' is related to response var
plot_model(mmo.lm.inter.trans, type="pred", terms='sq.temp', show.data=T, line.size=1.3,
           title="Fragaria Fruit Mass vs Temperature (Squared Transformed)",
           axis.title=c("temperature celsius (squared transformed)", "fitted values | fruit mass of `open` flowers"))

# Plot how predictor 'sq.imperv1000' is related to response var
plot_model(mmo.lm.inter.trans, type="pred", terms='sq.imperv1000', show.data=T, line.size=1.3,
           title="Fragaria Fruit Mass vs Impervious Surface (Squared Transformed)",
           axis.title=c("impervious surface (%) of 1000m buffer (squared transformed)", "fitted values | fruit mass of `open` flowers"))

# Plot how predictor 'sq.pol_shannon' is related to response var
plot_model(mmo.lm.inter.trans, type="pred", terms='sq.pol_shannon', show.data=T, line.size=1.3,
           title="Fragaria Fruit Mass vs Pollinator Diversity Shannon Index (Squared Transformed)",
           axis.title=c("pollinator diversity shannon index (squared transformed)", "fitted values | fruit mass of `open` flowers"))


# Plot how 'imperv1000 * pol_shannon' is related to the fitted values of the response var
p_mmo.inter.trans_imp.pol <- 
  plot_model(mmo.lm.inter.trans, type="pred", line.size=1.3,
             terms = c("sq.imperv1000", "sq.pol_shannon"), #  [1.63, 2.64]
             title="Fragaria Fruit Mass vs Impervious Surface (%)\nof 1000m Buffer(Squared Transformed)
                    \nInteraction: Pollinator Diversity\nShannon Index (Squared Transformed)",
             axis.title=c("impervious surface (%) of 1000m buffer (squared transformed)", "fitted values | fruit mass of `open` flowers"),
             legend.title="pollinator diversity\nshannon index\n(squared transformed)")
p_mmo.inter.trans_imp.pol

p_mmo.inter.trans_pol.imp <- 
  plot_model(mmo.lm.inter.trans, type="pred", line.size=1.3,
             terms=c("sq.pol_shannon", "sq.imperv1000"), #  [0.27, 0.57]
             title="Fragaria Fruit Mass vs Pollinator Diversity\nShannon Index(Squared Transformed)
                    \nInteraction: Impervious Surface (%)\nof 1000m Buffer (Squared Transformed)",
             axis.title=c("pollinator diversity shannon index (squared transformed)", "fitted values | fruit mass of `open` flowers"),
             legend.title="impervious surface (%)\n of 1000m buffer\n(squared transformed)")
p_mmo.inter.trans_pol.imp

gridExtra::grid.arrange(p_mmo.inter.trans_imp.pol, p_mmo.inter.trans_pol.imp, ncol=2)


# ------------------------------------------------------------------------------


# -- Clean-up environment for the next script ----
rm(list=ls())
