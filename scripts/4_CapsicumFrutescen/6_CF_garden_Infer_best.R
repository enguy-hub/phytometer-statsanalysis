# ---- Prerequisite procedures ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", 
                   "jtools", "PerformanceAnalytics", "sjPlot")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Hien/Garden/MyGithub/phytometer-statsanalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
cf_path = "./analysis_data/CF/CF_Data_2021_4analysis_garden_transformed.xlsx"
CF_data <- read_excel(cf_path, sheet = 1)


# Check structure and summaries of the data sets
str(CF_data)
summary(CF_data)


# Remove "Non-normal distributed" variables
CF_data <- CF_data %>%
  dplyr::select(-c("fruimass_meandiff", "fruimass_meanopen", "ratio_meanopen", "flo_abundance.yj",
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


# ------------------------------------------------------------------------------


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


# -- Best lm() model for: fruimass_meanopen.yj ----


# -- Create new dataframe, which remove "non-related" vars ----
CF_fmmo <- CF_data %>%
  dplyr::select(-c("fruimass_meandiff.yj", "seedmass_meandiff", "seedmass_meanopen",
                   "ratio_meandiff", "ratio_meanopen.yj", "mass_pseed_meanopen"))


# -- Check correlation of dependent and independent vars again ----
fmmo_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14)
fmmo_corr <- CF_fmmo[,fmmo_vars]
chart.Correlation(fmmo_corr, histogram=TRUE)


# -- Create multiple regression lm() model ----
fmmo.lm0 <- lm(fruimass_meanopen.yj ~ lux + imperv100 + # temp + 
               pol_abundance + pol_shannon + # pol_richness + flo_richness + 
               flo_abundance + flo_shannon, data=CF_data)
summ(fmmo.lm0, digits=4) # Adj-R2: 0.422; p: 0.083


# ---- Create initial model with stepAIC() ---- 
fmmo.lm.init <- MASS::stepAIC(fmmo.lm0, direction = "both", trace = FALSE)
summ(fmmo.lm.init, digits= 4) # Adj-R2: 0.3998; p: 0.31

# Check model$call
fmmo.lm.init$call # fruimass_meanopen.yj ~ lux + imperv100

# Check for multi-collinerity: For all vars, less than 3 is good
vif(fmmo.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(fmmo.lm.init) # p: 0.423 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(fmmo.lm.init) # p: 0.968 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(fmmo.lm.init)) # p: 0.01 --> Residuals NOT norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(fmmo.lm.init))
qqline(residuals(fmmo.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(fmmo.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(fmmo.lm.init)

# Initial model: fruimass_meanopen.yj ~ lux + imperv100
summ(fmmo.lm.init, digits= 4) # Adj-R2: 0.3998; p: 0.31


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
fmmo.lm.inter <- stepAIC(fmmo.lm.init, ~.^2, trace=F)
summ(fmmo.lm.inter,digits=4) # Adj-R2: 0.3998; p: 0.031

# Same as initial => No significant interaction terms found


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
CF_fmmo <- CF_fmmo %>%
  mutate(sq.lux = lux^2,
         sq.imperv100 = imperv100^2)

# Create transformed model
fmmo.lm.init.trans <- lm(fruimass_meanopen.yj ~ sq.lux + sq.imperv100, data=CF_fmmo)
summ(fmmo.lm.init.trans, digits=4) # Adj-R2: 0.1867; p: 0.143

# Check model$call
fmmo.lm.init.trans$call # fruimass_meanopen.yj ~ sq.lux + sq.imperv100

# Check for multi-collinerity: For all vars, less than 3 is good
vif(fmmo.lm.init.trans) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(fmmo.lm.init.trans) # p: 0.731 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(fmmo.lm.init.trans) # p: 0.49 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(fmmo.lm.init.trans)) # p: 0.021 --> Residuals NOT norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(fmmo.lm.init.trans))
qqline(residuals(fmmo.lm.init.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(fmmo.lm.init.trans, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(fmmo.lm.init.trans)

# Initial model: fruimass_meanopen.yj ~ sq.lux + sq.imperv100
summ(fmmo.lm.init.trans, digits=4) # Adj-R2: 0.1867; p: 0.143


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
fmmo.lm.inter.trans <- stepAIC(fmmo.lm.init.trans, ~.^2, trace=F)
summ(fmmo.lm.inter.trans,digits=4) # Adj-R2: 0.1867; p: 0.143

# Same as initial (transformed) model => No significant interaction terms found 


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
fmmo_init_fitval <- predict(fmmo.lm.init, CF_fmmo, interval="confidence") %>%
  data.frame() 
fmmo_g1 <- fitted_vs_actual(fmmo_init_fitval, CF_fmmo$fruimass_meanopen.yj, 
                            "fruimass_meanopen.yj - Initial Model")

# Initial (trans) model
fmmo_init_trans_fitval <- predict(fmmo.lm.init.trans, CF_fmmo, interval="confidence") %>%
  data.frame()
fmmo_g2 <- fitted_vs_actual(fmmo_init_trans_fitval, CF_fmmo$fruimass_meanopen.yj, 
                            "fruimass_meanopen.yj - Initial (Transformed) Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(fmmo_g1,fmmo_g2, ncol=2)


 # ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: fruimass_meanopen.yj ~ lux + imperv100
summ(fmmo.lm.init, digits=4) # Adj-R2: 0.3998; p: 0.031

# Initial (trans) model: fruimass_meanopen.yj ~ sq.lux + sq.imperv100
summ(fmmo.lm.init.trans, digits=4) # Adj-R2: 0.1867; p: 0.143

# No ANOVA tests: The best two models have different predictors


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Initial model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(fmmo.lm.init, confint=T, digits=4, ci.width=.95, center=T, pvals=T)
# Call: fruimass_meanopen.yj ~ lux + imperv100
# Adj-R2: 0.3998; p: 0.031

# Plot how predictor 'lux' is related to response var
plot_model(fmmo.lm.init, type="pred", terms='lux', show.data=T, line.size=1.3,
           title="Fruit Mass vs Light Intensity",
           axis.title=c("light intensity in lux", "fitted values | fruit mass of 'open' flowers (yj)"))

# Plot how predictor 'imperv100' is related to response var
plot_model(fmmo.lm.init, type="pred", terms='imperv100', show.data=T, line.size=1.3,
           title="Fruit Mass vs Impervious Surface",
           axis.title=c("impervious surface (%) of 100m buffer", "fitted values | fruit mass of 'open' flowers (yj)"))


# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------


# -- Best lm() model for: seedmass_meandiff ----


# -- Create new dataframe, which remove "non-related" vars ----
CF_smmd <- CF_data %>%
  dplyr::select(-c("fruimass_meandiff.yj", "fruimass_meanopen.yj", "seedmass_meanopen",
                   "ratio_meandiff", "ratio_meanopen.yj", "mass_pseed_meanopen"))


# -- Check correlation of dependent and independent vars again ----
smmd_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14)
smmd_corr <- CF_smmd[,smmd_vars]
chart.Correlation(smmd_corr, histogram=TRUE)


# -- Create multiple regression lm() model ----
smmd.lm0 <- lm(seedmass_meandiff ~ lux + imperv100 + temp + 
               pol_abundance + pol_shannon + pol_richness + flo_richness + 
               flo_abundance + flo_shannon, data=CF_data)
summ(smmd.lm0, digits=4) # Adj-R2: 0.3; p: 0.33


# ---- Create initial model with stepAIC() ---- 
smmd.lm.init <- MASS::stepAIC(smmd.lm0, direction="both", trace=F)
summ(smmd.lm.init, digits= 4) # Adj-R2: 0.6344; p: 0.067

# Check model$call
smmd.lm.init$call # seedmass_meandiff ~ lux + imperv100 + pol_shannon

# Check for multi-collinerity: For all vars, less than 3 is good
vif(smmd.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(smmd.lm.init) # p: 0.846 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(smmd.lm.init) # p: 0.386 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(smmd.lm.init)) # p: 0.627 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(smmd.lm.init))
qqline(residuals(smmd.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(smmd.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(smmd.lm.init)

# Initial model: seedmass_meandiff ~ lux + imperv100 + pol_shannon
summ(smmd.lm.init, digits= 4) # Adj-R2: 0.6344; p: 0.0067


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
smmd.lm.inter <- stepAIC(smmd.lm.init, ~.^2, trace=F)
summ(smmd.lm.inter,digits=4) # Adj-R2: 0.6936; p: 0.015

# Check model$call
smmd.lm.inter$call # seedmass_meandiff ~ lux + imperv100 + pol_shannon + lux:imperv100 + lux:pol_shannon

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(smmd.lm.inter) # p: 0.75 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(smmd.lm.inter) # p: 0.418 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(smmd.lm.inter)) # p: 0.51 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(smmd.lm.inter))
qqline(residuals(smmd.lm.inter))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(smmd.lm.inter, type = "rstandard") # curve --> slight non-linearity

# Interaction model: seedmass_meandiff ~ lux + imperv100 + pol_shannon + lux:imperv100 + lux:pol_shannon
summ(smmd.lm.inter, digits= 4) # Adj-R2: 0.6936; p: 0.015


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
CF_smmd <- CF_smmd %>%
  mutate(sq.lux = lux^2,
         sq.imperv100 = imperv100^2,
         sq.pol_shannon = pol_shannon^2)

# Create transformed model
smmd.lm.init.trans <- lm(seedmass_meandiff ~ sq.lux + sq.imperv100 + sq.pol_shannon, data=CF_smmd)
summ(smmd.lm.init.trans, digits=4) # Adj-R2: 0.54; p: 0.018

# Use stepAIC() to find the best model and override the old one
smmd.lm.init.trans <- stepAIC(smmd.lm.init.trans, direction="both", trace=F)
summ(smmd.lm.init.trans,digits=4) # Adj-R2: 0.559; p: 0.0067

# Check model$call
smmd.lm.init.trans$call # seedmass_meandiff ~ sq.lux + sq.imperv100

# Check for multi-collinerity: For all vars, less than 3 is good
vif(smmd.lm.init.trans) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(smmd.lm.init.trans) # p: 0.68 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(smmd.lm.init.trans) # p: 0.6 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(smmd.lm.init.trans)) # p: 0.35 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(smmd.lm.init.trans))
qqline(residuals(smmd.lm.init.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(smmd.lm.init.trans, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(smmd.lm.init.trans)

# Initial model: seedmass_meandiff ~ sq.lux + sq.imperv100
summ(smmd.lm.init.trans, digits=4) # Adj-R2: 0.559; p: 0.0067


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
smmd.lm.inter.trans <- stepAIC(smmd.lm.init.trans, ~.^2, trace=F)
summ(smmd.lm.inter.trans,digits=4) # Adj-R2: 0.559; p: 0.0067

# Same as initial (transformed) model => No significant interaction terms found 


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
smmd_init_fitval <- predict(smmd.lm.init, CF_smmd, interval="confidence") %>%
  data.frame() 
smmd_g1 <- fitted_vs_actual(smmd_init_fitval, CF_smmd$seedmass_meandiff, 
                            "seedmass_meandiff - Initial Model")

# Initial (trans) model
smmd_init_trans_fitval <- predict(smmd.lm.init.trans, CF_smmd, interval="confidence") %>%
  data.frame()
smmd_g2 <- fitted_vs_actual(smmd_init_trans_fitval, CF_smmd$seedmass_meandiff, 
                            "seedmass_meandiff - Initial (Transformed) Model")

# Interaction model
smmd_inter_fitval <- predict(smmd.lm.inter, CF_smmd, interval="confidence") %>%
  data.frame()
smmd_g3 <- fitted_vs_actual(smmd_init_trans_fitval, CF_smmd$seedmass_meandiff, 
                            "seedmass_meandiff - Interaction Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(smmd_g1,smmd_g2,smmd_g3, ncol=2)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: seedmass_meandiff ~ lux + imperv100 + pol_shannon
summ(smmd.lm.init, digits=4) # Adj-R2: 0.6344; p: 0.0067

# Initial (trans) model: seedmass_meandiff ~ sq.lux + sq.imperv100
summ(smmd.lm.init.trans, digits=4) # Adj-R2: 0.559; p: 0.0067

# Interaction model: seedmass_meandiff ~ lux + imperv100 + pol_shannon + lux:imperv100 + lux:pol_shannon
summ(smmd.lm.inter, digits=4) # Adj-R2: 0.694; p: 0.015

# Anova testing between models
anova(smmd.lm.init, smmd.lm.inter, test="F") # p: 0.223 => Not much improvement


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Interaction model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(smmd.lm.inter, confint=T, digits=4, ci.width=.95, center=T, pvals=T)
# Call: seedmass_meandiff ~ lux + imperv100 + pol_shannon + lux:imperv100 + lux:pol_shannon
# Adj-R2: 0.6936; p: 0.015

# Plot how predictor 'lux' is related to response var
plot_model(smmd.lm.inter, type="pred", terms='lux', show.data=T, line.size=1.3,
           title="Seed Mass vs Light Intensity",
           axis.title=c("light intensity in lux", "fitted values | seed mass of 'open - bagged' flowers"))

# Plot how predictor 'imperv100' is related to response var
plot_model(smmd.lm.inter, type="pred", terms='imperv100', show.data=T, line.size=1.3,
           title="Seed Mass vs Impervious Surface",
           axis.title=c("impervious surface (%) of 100m buffer", "fitted values | seed mass of 'open - bagged' flowers"))


# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------


# -- Best lm() model for: seedmass_meanopen ----


# -- Create new dataframe, which remove "non-related" vars ----
CF_smmo <- CF_data %>%
  dplyr::select(-c("fruimass_meandiff.yj", "fruimass_meanopen.yj", "seedmass_meandiff",
                   "ratio_meandiff", "ratio_meanopen.yj", "mass_pseed_meanopen"))


# -- Check correlation of dependent and independent vars again ----
smmo_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14)
smmo_corr <- CF_smmo[,smmo_vars]
chart.Correlation(smmo_corr, histogram=TRUE)


# -- Create multiple regression lm() model ----
smmo.lm0 <- lm(seedmass_meanopen ~ lux + imperv100 + temp + 
               pol_abundance + pol_shannon + # pol_richness + flo_richness + 
               flo_abundance + flo_shannon, data=CF_data)
summ(smmo.lm0, digits=4) # Adj-R2: 0.484; p: 0.154


# ---- Create initial model with stepAIC() ---- 
smmo.lm.init <- MASS::stepAIC(smmo.lm0, direction="both", trace=F)
summ(smmo.lm.init, digits= 4) # Adj-R2: 0.668; p: 0.0098

# Check model$call
smmo.lm.init$call # seedmass_meanopen ~ lux + temp + pol_abundance + pol_shannon

# Check for multi-collinerity: For all vars, less than 3 is good
vif(smmo.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(smmo.lm.init) # p: 0.226 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(smmo.lm.init) # p: 0.746 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(smmo.lm.init)) # p: 0.8666 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(smmo.lm.init))
qqline(residuals(smmo.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(smmo.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(smmo.lm.init)

# Initial model: seedmass_meanopen ~ lux + temp + pol_abundance + pol_shannon
summ(smmo.lm.init, digits=4) # Adj-R2: 0.668; p: 0.0098


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
smmo.lm.inter <- stepAIC(smmo.lm.init, ~.^2, trace=F)
summ(smmo.lm.inter,digits=4) # Adj-R2: 0.84; p: 0.0044

# Check model$call
smmo.lm.inter$call # seedmass_meanopen ~ lux + temp + pol_abundance + pol_shannon + lux:pol_abundance + temp:pol_abundance

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(smmo.lm.inter) # p: 0.133 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(smmo.lm.inter) # p: 0.344 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(smmo.lm.inter)) # p: 0.0044 --> Residuals NOT norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(smmo.lm.inter))
qqline(residuals(smmo.lm.inter))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(smmo.lm.inter, type = "rstandard") # curve --> slight non-linearity

# Interaction model: seedmass_meanopen ~ lux + temp + pol_abundance + pol_shannon + lux:pol_abundance + temp:pol_abundance
summ(smmo.lm.inter, digits=4) # Adj-R2: 0.8412; p: 0.0044


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
CF_smmo <- CF_smmo %>%
  mutate(sq.lux = lux^2,
         sq.imperv100 = imperv100^2,
         sq.pol_abundance = pol_abundance^2,
         sq.pol_shannon = pol_shannon^2)

# Create transformed model
smmo.lm.init.trans <- lm(seedmass_meanopen ~ sq.lux + sq.imperv100 + sq.pol_abundance + sq.pol_shannon, data=CF_smmo)
summ(smmo.lm.init.trans, digits=4) # Adj-R2: 0.459; p: 0.06

# Use stepAIC() to find the best model and override the old one
smmo.lm.init.trans <- stepAIC(smmo.lm.init.trans, direction="both", trace=F)
summ(smmo.lm.init.trans,digits=4) # Adj-R2: 0.442; p: 0.041

# Check model$call
smmo.lm.init.trans$call # seedmass_meanopen ~ sq.lux + sq.imperv100 + sq.pol_abundance

# Check for multi-collinerity: For all vars, less than 3 is good
vif(smmo.lm.init.trans) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(smmo.lm.init.trans) # p: 0.864 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(smmo.lm.init.trans) # p: 0.33 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(smmo.lm.init.trans)) # p: 0.25 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(smmo.lm.init.trans))
qqline(residuals(smmo.lm.init.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(smmo.lm.init.trans, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(smmo.lm.init.trans)

# Initial model: seedmass_meanopen ~ sq.lux + sq.imperv100 + sq.pol_abundance
summ(smmo.lm.init.trans, digits=4) # Adj-R2: 0.443; p: 0.041


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
smmo.lm.inter.trans <- stepAIC(smmo.lm.init.trans, ~.^2, trace=F)
summ(smmo.lm.inter.trans,digits=4) # Adj-R2: 0.7; p: 0.0065

# Check model$call
smmo.lm.inter.trans$call # seedmass_meanopen ~ sq.lux + sq.imperv100 + sq.pol_abundance + sq.lux:sq.pol_abundance

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(smmo.lm.inter.trans) # p: 0.489 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(smmo.lm.inter.trans) # p: 0.258 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(smmo.lm.inter.trans)) # p: 0.84 --> Residuals NOT norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(smmo.lm.inter.trans))
qqline(residuals(smmo.lm.inter.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(smmo.lm.inter.trans, type = "rstandard") # curve --> slight non-linearity

# Initial model: seedmass_meandiff ~ sq.lux + sq.imperv100 + sq.pol_abundance + sq.lux:sq.pol_abundance
summ(smmo.lm.inter.trans, digits=4) # Adj-R2: 0.7; p: 0.0065


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
smmo_init_fitval <- predict(smmo.lm.init, CF_smmo, interval="confidence") %>%
  data.frame() 
smmo_g1 <- fitted_vs_actual(smmo_init_fitval, CF_smmo$seedmass_meanopen, 
                            "seedmass_meanopen - Initial Model")

# Initial (trans) model
smmo_init_trans_fitval <- predict(smmo.lm.init.trans, CF_smmo, interval="confidence") %>%
  data.frame()
smmo_g2 <- fitted_vs_actual(smmo_init_trans_fitval, CF_smmo$seedmass_meanopen, 
                            "seedmass_meanopen - Initial (Transformed) Model")

# Initial (trans) model
smmo_inter_fitval <- predict(smmo.lm.inter, CF_smmo, interval="confidence") %>%
  data.frame()
smmo_g3 <- fitted_vs_actual(smmo_init_trans_fitval, CF_smmo$seedmass_meanopen, 
                            "seedmass_meanopen - Interaction Model")

# Initial (trans) model
smmo_inter_trans_fitval <- predict(smmo.lm.init.trans, CF_smmo, interval="confidence") %>%
  data.frame()
smmo_g4 <- fitted_vs_actual(smmo_init_trans_fitval, CF_smmo$seedmass_meanopen, 
                            "seedmass_meanopen - Interaction (Transformed) Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(smmo_g1,smmo_g2,smmo_g3,smmo_g4, ncol=2)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: seedmass_meanopen ~ temp + lux + pol_abundance + pol_shannon
summ(smmo.lm.init, digits=4) # Adj-R2: 0.668; p: 0.0098

# Initial (trans) model: seedmass_meanopen ~ sq.lux + sq.imperv100 + sq.pol_abundance
summ(smmo.lm.init.trans, digits=4) # Adj-R2: 0.443; p: 0.041

# Interaction model: seedmass_meanopen ~ temp + lux + pol_abundance + pol_shannon + lux:pol_abundance + temp:pol_abundance
summ(smmo.lm.inter, digits=4) # Adj-R2: 0.841; p: 0.0044

# Interaction (trans) model: seedmass_meanopen ~ sq.lux + sq.imperv100 + sq.pol_abundance + sq.lux:sq.pol_abundance
summ(smmo.lm.inter.trans, digits=4) # Adj-R2: 0.7; p: 0.0065

# Anova testing between models
anova(smmo.lm.init, smmo.lm.inter, test="F") # p: 0.046 => Much improvement
anova(smmo.lm.init.trans, smmo.lm.inter.trans, test="F") # p: 0.018 => Much improvement


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Initial model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(smmo.lm.init, confint=T, digits=4, ci.width=.95, center=T, pvals=T)
# Call: seedmass_meanopen ~ lux + temp + pol_abundance + pol_shannon
# Adj-R2: 0.668; p: 0.0098

# Plot how predictor 'lux' is related to response var
plot_model(smmo.lm.init, type="pred", terms='lux', show.data=T, line.size=1.3,
           title="Seed Mass vs Light Intensity",
           axis.title=c("light intensity in lux", "fitted values | seed mass of 'open' flowers"))

# Plot how predictor 'temp' is related to response var
plot_model(smmo.lm.init, type="pred", terms='temp', show.data=T, line.size=1.3,
           title="Seed Mass vs Temperature",
           axis.title=c("temperature in celsius", "fitted values | seed mass of 'open' flowers"))
 
# Plot how predictor 'pol_shannon' is related to response var
plot_model(smmo.lm.init, type="pred", terms='pol_shannon', show.data=T, line.size=1.3,
           title="Seed Mass vs Pollinator Diversity Shannon Index",
           axis.title=c("pollinator diversity shannon index", "fitted values | seed mass of 'open' flowers"))


# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------


# -- Best lm() model for: ratio_meandiff ----


# -- Create new dataframe, which remove "non-related" vars ----
CF_rmd <- CF_data %>%
  dplyr::select(-c("fruimass_meandiff.yj", "fruimass_meanopen.yj", "seedmass_meandiff",
                   "seedmass_meanopen", "ratio_meanopen.yj", "mass_pseed_meanopen"))


# -- Check correlation of dependent and independent vars again ----
rmd_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14)
rmd_corr <- CF_rmd[,rmd_vars]
chart.Correlation(rmd_corr, histogram=TRUE)


# -- Create multiple regression lm() model ----
rmd.lm0 <- lm(ratio_meandiff ~ lux + imperv500 + temp + 
              pol_abundance + pol_shannon + # pol_richness + flo_richness + 
              flo_abundance + flo_shannon, data=CF_data)
summ(rmd.lm0, digits=4) # Adj-R2: 0.286; p: 0.292


# ---- Create initial model with stepAIC() ---- 
rmd.lm.init <- MASS::stepAIC(rmd.lm0, direction="both", trace=F)
summ(rmd.lm.init, digits= 4) # Adj-R2: 0.495; p: 0.047

# Check model$call
rmd.lm.init$call # ratio_meandiff ~ lux + imperv500 + pol_abundance + flo_abundance

# Check for multi-collinerity: For all vars, less than 3 is good
vif(rmd.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(rmd.lm.init) # p: 0.423 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(rmd.lm.init) # p: 0.116 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(rmd.lm.init)) # p: 0.457 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(rmd.lm.init))
qqline(residuals(rmd.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(rmd.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(rmd.lm.init)

# Initial model: ratio_meandiff ~ lux + imperv500 + pol_abundance + flo_abundance
summ(rmd.lm.init, digits=4) # Adj-R2: 0.495; p: 0.047


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
rmd.lm.inter <- stepAIC(rmd.lm.init, ~.^2, trace=F)
summ(rmd.lm.inter,digits=4) # Adj-R2: 0.8; p: 0.0084

# Check model$call
rmd.lm.inter$call # ratio_meandiff ~ lux + imperv500 + pol_abundance + flol_abundance + pol_abundance:flo_abundance + imperv500:pol_abundance

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(rmd.lm.inter) # p: 0.115 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(rmd.lm.inter) # p: 0.166 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(rmd.lm.inter)) # p: 0.246 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(rmd.lm.inter))
qqline(residuals(rmd.lm.inter))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(rmd.lm.inter, type = "rstandard") # curve --> slight non-linearity

# Interaction model: ratio_meandiff ~ lux + imperv500 + pol_abundance + flol_abundance + pol_abundance:flo_abundance + imperv500:pol_abundance
summ(rmd.lm.inter, digits=4) # Adj-R2: 0.8; p: 0.0084


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
CF_rmd <- CF_rmd %>%
  mutate(sq.lux = lux^2,
         sq.imperv500 = imperv500^2,
         sq.pol_abundance = pol_abundance^2,
         sq.flo_abundance = flo_abundance^2)

# Create transformed model
rmd.lm.init.trans <- lm(ratio_meandiff ~ sq.lux + sq.imperv500 + sq.pol_abundance + sq.flo_abundance, data=CF_rmd)
summ(rmd.lm.init.trans, digits=4) # Adj-R2: 0.473; p: 0.055

# Check model$call
rmd.lm.init.trans$call # ratio_meandiff ~ sq.lux + sq.imperv500 + sq.pol_abundance + sq.flo_abundance

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(rmd.lm.init.trans) # p: 0.4 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(rmd.lm.init.trans) # p: 0.112 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(rmd.lm.init.trans)) # p: 0.817 --> Residuals NOT norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(rmd.lm.init.trans))
qqline(residuals(rmd.lm.init.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(rmd.lm.init.trans, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(rmd.lm.init.trans)

# Initial model: ratio_meandiff ~ sq.lux + sq.imperv500 + sq.pol_abundance + sq.flo_abundance
summ(rmd.lm.init.trans, digits=4) # Adj-R2: 0.473; p: 0.055


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
rmd.lm.inter.trans <- stepAIC(rmd.lm.init.trans, ~.^2, trace=F)
summ(rmd.lm.inter.trans,digits=4) # Adj-R2: 0.84; p: 0.0046

# Check model$call
rmd.lm.inter.trans$call # ratio_meandiff ~ sq.lux + sq.imperv500 + sq.pol_abundance + sq.flo_abundance + sq.pol_abundance:sq.flo_abundance + sq.imperv500:sq.pol_abundance

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(rmd.lm.inter.trans) # p: 0.037 --> NOT Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(rmd.lm.inter.trans) # p: 0.084 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(rmd.lm.inter.trans)) # p: 0.13 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(rmd.lm.inter.trans))
qqline(residuals(rmd.lm.inter.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(rmd.lm.inter.trans, type = "rstandard") # curve --> slight non-linearity

# Initial model: ratio_meandiff ~ sq.lux + sq.imperv500 + sq.pol_abundance + sq.flo_abundance + sq.pol_abundance:sq.flo_abundance + sq.imperv500:sq.pol_abundance
summ(rmd.lm.inter.trans, digits=4) # Adj-R2: 0.84; p: 0.0046


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
rmd_init_fitval <- predict(rmd.lm.init, CF_rmd, interval="confidence") %>%
  data.frame() 
rmd_g1 <- fitted_vs_actual(rmd_init_fitval, CF_rmd$ratio_meandiff, 
                           "ratio_meandiff - Initial Model")

# Initial (trans) model
rmd_init_trans_fitval <- predict(rmd.lm.init.trans, CF_rmd, interval="confidence") %>%
  data.frame()
rmd_g2 <- fitted_vs_actual(rmd_init_trans_fitval, CF_rmd$ratio_meandiff, 
                           "ratio_meandiff - Initial (Transformed) Model")

# Interaction model
rmd_inter_fitval <- predict(rmd.lm.inter, CF_rmd, interval="confidence") %>%
  data.frame()
rmd_g3 <- fitted_vs_actual(rmd_init_trans_fitval, CF_rmd$ratio_meandiff, 
                           "ratio_meandiff - Interaction Model")

# Interaction (trans) model
rmd_inter_trans_fitval <- predict(rmd.lm.init.trans, CF_rmd, interval="confidence") %>%
  data.frame()
rmd_g4 <- fitted_vs_actual(rmd_init_trans_fitval, CF_rmd$ratio_meandiff, 
                           "ratio_meandiff - Interaction (Transformed) Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(rmd_g1,rmd_g2,rmd_g3,rmd_g4, ncol=2)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: ratio_meandiff ~ lux + imperv500 + pol_abundance + flo_abundance
summ(rmd.lm.init, digits=4) # Adj-R2: 0.495; p: 0.047

# Initial (trans) model: ratio_meandiff ~ sq.lux + sq.imperv500 + sq.pol_abundance + sq.flo_abundance
summ(rmd.lm.init.trans, digits=4) # Adj-R2: 0.473; p: 0.055

# Interaction model: ratio_meandiff ~ lux + imperv500 + pol_abundance + flo_abundance + pol_abundance:flo_abundance + imperv500:pol_abundance
summ(rmd.lm.inter, digits=4) # Adj-R2: 0.8; p: 0.008

# Interaction (trans) model: ratio_meandiff ~ sq.lux + sq.imperv500 + sq.pol_abundance + sq.flo_abundance + sq.pol_abundance:sq.flo_abundance + sq.imperv500:sq.pol_abundance
summ(rmd.lm.inter.trans, digits=4) # Adj-R2: 0.84; p: 0.005


# Anova testing between models
anova(rmd.lm.init, rmd.lm.inter, test="F") # p: 0.025 => Much improvement
anova(rmd.lm.init.trans, rmd.lm.inter.trans, test="F") # p: 0.012 => Much improvement


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Interaction model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(rmd.lm.inter, confint=T, digits=4, ci.width=.95, center=T, pvals=T)
# Call: ratio_meandiff ~ lux + imperv500 + pol_abundance + flo_abundance + pol_abundance:flo_abundance + imperv500:pol_abundance
# Adj-R2: 0.8; p: 0.008

# Plot how predictor 'lux' is related to response var
plot_model(rmd.lm.inter, type="pred", terms='lux', show.data=T, line.size=1.3,
           title="Fruit Ratio (Height/Width) vs Light Intensity",
           axis.title=c("light intensity in lux", "fitted values | fruit ratio (height/width) of 'open - bagged' flowers"))

# Plot how predictor 'imperv500' is related to response var
plot_model(rmd.lm.inter, type="pred", terms='imperv500', show.data=T, line.size=1.3,
           title="Ratio of Fruit (Height/Width) vs Impervious Surface",
           axis.title=c("impervious surface (%) of 500m buffer", "fitted values | ratio of fruit (height/width) of 'open - bagged' flowers"))

# Plot how predictor 'pol_abundance' is related to response var
plot_model(rmd.lm.inter, type="pred", terms='pol_abundance', show.data=T, line.size=1.3,
           title="Fruit Ratio (Height/Width) vs Pollinator Abundance",
           axis.title=c("pollinator abundance", "fitted values | fruit ratio (height/width) of 'open - bagged' flowers"))


# Plot how 'pol_abundance*flo_abundance' is related to the fitted values of the response var
p_rmd.inter_pol.flo <- 
  plot_model(rmd.lm.inter, type="pred", line.size=1.3,
             terms=c("pol_abundance", "flo_abundance"), # [1.16, 1.72]
             title="Ratio of Fruit (Height/Width) vs Pollinator Abundance
                    \nInteraction: Floral Abundance",
             axis.title=c("pollinator abundance", "fitted values | fruit ratio (height/width) of 'open - bagged' flowers"),
             legend.title="floral abundance")
p_rmd.inter_pol.flo

p_rmd.inter_flo.pol <- 
  plot_model(rmd.lm.inter, type="pred", line.size=1.3,
             terms=c("flo_abundance", "pol_abundance"), # [1.16, 1.72]
             title="Ratio of Fruit (Height/Width) vs Floral Abundance
                    \nInteraction: Pollinator Abundance",
             axis.title=c("floral abundance", "fitted values | fruit ratio (height/width) of 'open - bagged' flowers"),
             legend.title="pollinator abundance")
p_rmd.inter_flo.pol

gridExtra::grid.arrange(p_rmd.inter_pol.flo, p_rmd.inter_flo.pol, ncol=2)


# Plot how 'imperv500*pol_abundance' is related to the fitted values of the response var
p_rmd.inter_imp.pol <- 
  plot_model(rmd.lm.inter, type="pred", line.size=1.3,
             terms=c("imperv500", "pol_abundance"), # [1.16, 1.72]
             title="Ratio of Fruit (Height/Width) vs Impervious Surface
                    \nInteraction: Pollinator Abundance",
             axis.title=c("impervious surface (%) of 500m buffer", "fitted values | fruit ratio (height/width) of 'open - bagged' flowers"),
             legend.title="pollinator abundance")
p_rmd.inter_imp.pol

p_rmd.inter_pol.imp <- 
  plot_model(rmd.lm.inter, type="pred", line.size=1.3,
             terms=c("pol_abundance", "imperv500"), # [1.16, 1.72]
             title="Ratio of Fruit (Height/Width) vs Pollinator Abundance
                    \nInteraction: Impervious Surface",
             axis.title=c("pollinator abundance", "fitted values | fruit ratio (height/width) of 'open - bagged' flowers"),
             legend.title="impervious surface\n(%) of 500m buffer")
p_rmd.inter_pol.imp

gridExtra::grid.arrange(p_rmd.inter_imp.pol, p_rmd.inter_pol.imp, ncol=2)


# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------


# -- Best lm() model for: ratio_meanopen.yj ----


# -- Create new dataframe, which remove "non-related" vars ----
CF_rmo <- CF_data %>%
  dplyr::select(-c("fruimass_meandiff.yj", "fruimass_meanopen.yj", "seedmass_meandiff",
                   "seedmass_meanopen", "ratio_meandiff", "mass_pseed_meanopen"))


# -- Check correlation of dependent and independent vars again ----
rmo_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14)
rmo_corr <- CF_rmo[,rmo_vars]
chart.Correlation(rmo_corr, histogram=TRUE)


# -- Create multiple regression lm() model ----
rmo.lm0 <- lm(ratio_meanopen.yj ~ lux + imperv1000 + # temp + 
              pol_abundance + pol_shannon + # pol_richness + flo_richness + 
              flo_abundance + flo_shannon, data=CF_data)
summ(rmo.lm0, digits=4) # Adj-R2: 0.12; p: 0.543


# ---- Create initial model with stepAIC() ---- 
rmo.lm.init <- MASS::stepAIC(rmo.lm0, direction="both", trace=F)
summ(rmo.lm.init, digits= 4) # Adj-R2: 0.4378; p: 0.022

# Check model$call
rmo.lm.init$call # ratio_meanopen.yj ~ imperv1000 + pol_abundance

# Check for multi-collinerity: For all vars, less than 3 is good
vif(rmo.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(rmo.lm.init) # p: 0.261 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(rmo.lm.init) # p: 0.86 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(rmo.lm.init)) # p: 0.23 --> Residuals NOT norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(rmo.lm.init))
qqline(residuals(rmo.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(rmo.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(rmo.lm.init)

# Initial model: ratio_meanopen.yj ~ imperv1000 + pol_abundance
summ(rmo.lm.init, digits=4) # Adj-R2: 0.438; p: 0.023


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
rmo.lm.inter <- stepAIC(rmo.lm.init, ~.^2, trace=F)
summ(rmo.lm.inter,digits=4) # Adj-R2: 0.438; p: 0.023

# Same as initial => No significant interaction terms found


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
CF_rmo <- CF_rmo %>%
  mutate(sq.imperv1000 = imperv500^2,
         sq.pol_abundance = pol_abundance^2)

# Create transformed model
rmo.lm.init.trans <- lm(ratio_meanopen.yj ~ sq.imperv1000 + sq.pol_abundance, data=CF_rmo)
summ(rmo.lm.init.trans, digits=4) # Adj-R2: 0.4789; p: 0.015

# Check model$call
rmo.lm.init.trans$call # ratio_meanopen.yj ~ sq.imperv1000 + sq.pol_abundance

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(rmo.lm.init.trans) # p: 0.15 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(rmo.lm.init.trans) # p: 0.6 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(rmo.lm.init.trans)) # p: 0.6 --> Residuals NOT norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(rmo.lm.init.trans))
qqline(residuals(rmo.lm.init.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(rmo.lm.init.trans, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(rmo.lm.init.trans)

# Initial model: ratio_meanopen.yj ~ sq.imperv1000 + sq.pol_abundance
summ(rmo.lm.init.trans, digits=4) # Adj-R2: 0.4789; p: 0.015


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
rmo.lm.inter.trans <- stepAIC(rmo.lm.init.trans, ~.^2, trace=F)
summ(rmo.lm.inter.trans,digits=4) # Adj-R2: 0.438; p: 0.023

# Same as initial => No significant interaction terms found


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
rmo_init_fitval <- predict(rmo.lm.init, CF_rmo, interval="confidence") %>%
  data.frame() 
rmo_g1 <- fitted_vs_actual(rmo_init_fitval, CF_rmo$ratio_meanopen.yj, 
                           "ratio_meanopen.yj - Initial Model")

# Initial (trans) model
rmo_init_trans_fitval <- predict(rmo.lm.init.trans, CF_rmo, interval="confidence") %>%
  data.frame()
rmo_g2 <- fitted_vs_actual(rmo_init_trans_fitval, CF_rmo$ratio_meanopen.yj, 
                           "ratio_meanopen.yj - Initial (Transformed) Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(rmo_g1,rmo_g2, ncol=2)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: ratio_meanopen.yj ~ imperv1000 + pol_abundance
summ(rmo.lm.init, digits=4) # Adj-R2: 0.438; p: 0.023

# Initial (trans) model: ratio_meanopen.yj ~ sq.imperv1000 + sq.pol_abundance
summ(rmo.lm.init.trans, digits=4) # Adj-R2: 0.4789; p: 0.015

# No possible Anova testing


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Initial model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(rmo.lm.init, confint=T, digits=4, ci.width=.95, center=T, pvals=T)
# Call: ratio_meanopen.yj ~ imperv1000 + pol_abundance
# Adj-R2: 0.44; p: 0.023

# Plot how predictor 'imperv1000' is related to response var
plot_model(rmo.lm.init, type="pred", terms='imperv1000', show.data=T, line.size=1.3,
           title="Ratio of Fruit (Height/Width) vs Impervious Surface",
           axis.title=c("impervious surface (%) of 1000m buffer", "fitted values | ratio of fruit (height/width) of 'open' flowers (yj)"))


# ------------------------------------------------------------------------------


# ---- Clean-up environment for the next script ----
rm(list=ls())

