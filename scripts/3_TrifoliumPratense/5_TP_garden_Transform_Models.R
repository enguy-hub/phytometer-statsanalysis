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
tp_path = "./analysis_data/TP/TP_Data_2021_4analysis_garden_transformed.xlsx"
TP_data <- read_excel(tp_path, sheet = 1)


# Check structure and summaries of the data sets
str(TP_data)
summary(TP_data)


# Remove "Non-normal distributed" variables
TP_data <- TP_data %>%
  dplyr::select(-c("pol_abundance", "flo_abundance", 
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
                 pol_abundance.yj + pol_richness + # pol_shannon + flo_richness + 
                 flo_abundance.yj + flo_shannon, data=TP_fmmd)
summ(fmmd.lm0, digits=4) # Adj-R2: 0.4776; p: 0.1583


# ---- Create initial model with stepAIC() ---- 
fmmd.lm.init <- MASS::stepAIC(fmmd.lm0, direction = "both", trace = FALSE)
summ(fmmd.lm.init, digits= 4) # Adj-R2: 0.657; p: 0.011

# Check model$call
fmmd.lm.init$call # flowmass_meandiff ~ pol_abundance.yj + pol_richness + flo_abundance.yj + flo_shannon

# Check for multi-collinerity: For all vars, less than 3 is good
vif(fmmd.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(fmmd.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(fmmd.lm.init))
qqline(residuals(fmmd.lm.init))

# Shapiro test
shapiro.test(residuals(fmmd.lm.init)) # p: 0.845 --> Residuals are norm-dist

# Check CERES plot
ceresPlots(fmmd.lm.init)

# Initial model: flowmass_meandiff ~ pol_abundance.yj + pol_richness + flo_abundance.yj + flo_shannon
summ(fmmd.lm.init, digits= 4) # Adj-R2: 0.657; p: 0.011


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
fmmd.lm.inter <- stepAIC(fmmd.lm.init, ~.^2, trace=F)
summ(fmmd.lm.inter,digits=4) # Adj-R2: 0.657; p: 0.011

# Same as initial => No significant interaction terms found


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
TP_fmmd <- TP_fmmd %>%
  mutate(sq.pol_abundance.yj = pol_abundance.yj^2,
         sq.pol_richness = pol_richness^2,
         sq.flo_abundance.yj = flo_abundance.yj^2,
         sq.flo_shannon = flo_shannon^2)

# Create transformed model
fmmd.lm.init.trans <- lm(flowmass_meandiff ~ sq.pol_abundance.yj + sq.pol_richness + 
                         sq.flo_abundance.yj + sq.flo_shannon, data=TP_fmmd)
summ(fmmd.lm.init.trans, digits=4) # Adj-R2: 0.6426; p: 0.013

# Use stepAIC() to find the best model and override the old one
fmmd.lm.init.trans <- stepAIC(fmmd.lm.init.trans, direction="both", trace=F)
summ(fmmd.lm.init.trans,digits=4) # Same model as above ==> # Adj-R2: 0.6426; p: 0.013

# Check model$call
fmmd.lm.init.trans$call # flowmass_meandiff ~ sq.pol_abundance.yj + sq.pol_richness + sq.flo_abundance.yj + sq.flo_shannon

# Check for multi-collinerity: For all vars, less than 3 is good
vif(fmmd.lm.init.trans) %>% 
  knitr::kable() # All < 3: Pass

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(fmmd.lm.init.trans, type = "rstandard") # straighter line

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(fmmd.lm.init.trans))
qqline(residuals(fmmd.lm.init.trans))

# Shapiro test
shapiro.test(residuals(fmmd.lm.init.trans)) # p: 0.6195 --> Residuals are norm-dist

# Check CERES plot
ceresPlots(fmmd.lm.init.trans)

# Initial model: flowmass_meandiff ~ sq.pol_abundance.yj + sq.pol_richness + sq.flo_abundance + sq.flo_shannon
summ(fmmd.lm.init.trans, digits= 4) # Adj-R2: 0.643; p: 0.013


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
fmmd.lm.inter.trans <- stepAIC(fmmd.lm.init.trans, ~.^2, trace=F)
summ(fmmd.lm.inter.trans,digits=4) # Adj-R2: 0.643; p: 0.013

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

# Initial model: flowmass_meandiff ~ pol_abundance.yj + pol_richness + flo_abundance.yj + flo_shannon
summ(fmmd.lm.init, digits= 4) # Adj-R2: 0.657; p: 0.0111

# Initial (trans) model: flowmass_meandiff ~ sq.pol_abundance.yj + sq.pol_richness + sq.flo_abundance.yj + sq.flo_shannon
summ(fmmd.lm.init.trans, digits= 4) # Adj-R2: 0.643; p: 0.013

# No ANOVA tests: The best two models have different predictors


# ---- Testing/Checking linear assumptions for best model(s) ----

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(fmmd.lm.init) # p: 0.832 --> Pass
ncvTest(fmmd.lm.init.trans) # p: 0.653 --> Pass

# Auto correlated Errors test - H0: consecutive errors are not correlated 
# => p-value more than 0.05 is good
set.seed(1)
durbinWatsonTest(fmmd.lm.init) # p: 0.57 --> Consecutive errors are independent of each other
durbinWatsonTest(fmmd.lm.init.trans) # p: 0.49 --> Consecutive errors are independent of each other


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Initial model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(fmmd.lm.init, confint = TRUE, digits=4, ci.width = .95, center=T, pvals=T)
# Call: flowmass_meandiff ~ pol_abundance.yj + pol_richness + flo_abundance.yj + flo_shannon
# Adj-R2: 0.657; p: 0.011

# Plot how predictor 'pol_richness' is related to response var
plot_model(fmmd.lm.init, type="pred", terms='pol_richness', show.data=T, line.size=1.3)

# Plot how predictor 'flo_abundance.yj' is related to response var
plot_model(fmmd.lm.init, type="pred", terms='flo_abundance.yj', show.data=T, line.size=1.3)

# Plot how predictor 'flo_shannon' is related to response var
plot_model(fmmd.lm.init, type="pred", terms='flo_shannon', show.data=T, line.size=1.3)


# ---- Initial (transformed) model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(fmmd.lm.init.trans, confint = TRUE, digits=4, ci.width = .95, center=T, pvals=T)
# Call: flowmass_meandiff ~ sq.pol_abundance.yj + sq.pol_richness + sq.flo_abundance.yj + sq.flo_shannon
# Adj-R2: 0.643; p: 0.013

# Plot how predictor 'sq.flo_abundance.yj' is related to response var
plot_model(fmmd.lm.init.trans, type="pred", terms='sq.flo_abundance.yj', show.data=T, line.size=1.3)

# Plot how predictor 'sq.flo_shannon' is related to response var
plot_model(fmmd.lm.init.trans, type="pred", terms='sq.flo_shannon', show.data=T, line.size=1.3)


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
               pol_abundance.yj + pol_richness + pol_shannon + 
               flo_abundance.yj + flo_richness + flo_shannon, data=TP_fmmo)
summ(fmmo.lm0, digits=4) # Adj-R2: 0.12; p: 0.4977


# ---- Create initial model with stepAIC() ---- 
fmmo.lm.init <- MASS::stepAIC(fmmo.lm0, direction = "both", trace = FALSE)
summ(fmmo.lm.init, digits= 4) # Adj-R2: 0.686; p: 0.0012

# Check model$call
fmmo.lm.init$call # flowmass_meanopen ~ pol_abundance.yj + flo_shannon

# Check for multi-collinerity: For all vars, less than 3 is good
vif(fmmo.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(fmmo.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(fmmo.lm.init))
qqline(residuals(fmmo.lm.init))

# Shapiro test
shapiro.test(residuals(fmmo.lm.init)) # p: 0.6485 --> Residuals are norm-dist

# Check CERES plot
ceresPlots(fmmo.lm.init)

# Initial model: flowmass_meandiff ~ temp + pol_abundance + pol_richness + flo_richness
summ(fmmo.lm.init, digits= 4) # Adj-R2: 0.686; p: 0.0012


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
fmmo.lm.inter <- stepAIC(fmmo.lm.init, ~.^2, trace=F)
summ(fmmo.lm.inter,digits=4) # Adj-R2: 0.715; p: 0.0023

# Check model$call
fmmo.lm.inter$call # flowmass_meanopen ~ pol_abundance.yj * flo_shannon 

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(fmmo.lm.inter, type = "rstandard") # curve --> slight non-linearity

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(fmmo.lm.inter))
qqline(residuals(fmmo.lm.inter))

# Shapiro test
shapiro.test(residuals(fmmo.lm.inter)) # p: 0.324 --> Residuals are norm-dist

# Initial model: flowmass_meanopen ~ pol_abundance.yj * flo_shannon 
summ(fmmo.lm.inter, digits= 4) # Adj-R2: 0.715; p: 0.0023


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
TP_fmmo <- TP_fmmo %>%
  mutate(sq.pol_abundance.yj = pol_abundance.yj^2,
         sq.flo_shannon = flo_shannon^2)

# Create transformed model
fmmo.lm.init.trans <- lm(flowmass_meanopen ~ sq.pol_abundance.yj + sq.flo_shannon, data=TP_fmmo)
summ(fmmo.lm.init.trans, digits=4) # Adj-R2: 0.697; p: 0.001

# Use stepAIC() to find the best model and override the old one
fmmo.lm.init.trans <- stepAIC(fmmo.lm.init.trans, direction="both", trace=F)
summ(fmmo.lm.init.trans,digits=4) # Same model as above ==> # Adj-R2: 0.697; p: 0.001

# Check model$call
fmmo.lm.init.trans$call # flowmass_meanopen ~ sq.pol_abundance.yj + sq.flo_shannon

# Check for multi-collinerity: For all vars, less than 3 is good
vif(fmmo.lm.init.trans) %>% 
  knitr::kable() # All < 3: Pass

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(fmmo.lm.init.trans, type = "rstandard") # curve --> slight non-linearity

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(fmmo.lm.init.trans))
qqline(residuals(fmmo.lm.init.trans))

# Shapiro test
shapiro.test(residuals(fmmo.lm.init.trans)) # p: 0.44 --> Residuals are norm-dist

# Check CERES plot
ceresPlots(fmmo.lm.init.trans)

# Initial (transformed) model: flowmass_meanopen ~ sq.pol_abundance + sq.flo_shannon
summ(fmmo.lm.init.trans, digits= 4) # Adj-R2: 0.697; p: 0.001


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
fmmo.lm.inter.trans <- stepAIC(fmmo.lm.init.trans, ~.^2, trace=F)
summ(fmmo.lm.inter.trans,digits=4) # Adj-R2: 0.7178; p: 0.0022

# Check model$call
fmmo.lm.inter.trans$call # flowmass_meanopen ~ sq.pol_abundance.yj + sq.flo_shannon + sq.pol_abundance.yj:sq.flo_shannon

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(fmmo.lm.inter.trans, type = "rstandard") # curve --> slight non-linearity

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(fmmo.lm.inter.trans))
qqline(residuals(fmmo.lm.inter.trans))

# Shapiro test
shapiro.test(residuals(fmmo.lm.inter.trans)) # p: 0.1935 --> Residuals are norm-dist

# Interaction (transformed) model: flowmass_meanopen ~ sq.pol_abundance.yj + sq.flo_shannon + sq.pol_abundance.yj:sq.flo_shannon
summ(fmmo.lm.inter.trans, digits= 4) # Adj-R2: 0.718; p: 0.0022


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
fmmo_init_fitval <- predict(fmmo.lm.init, TP_fmmo, interval="confidence") %>%
  data.frame() 
fmmo_g1 <- fitted_vs_actual(fmmo_init_fitval, TP_fmmo$flowmass_meanopen, 
                            "flowmass_meanopen - Initial Model")

# Interaction model
fmmo_inter_fitval <- predict(fmmo.lm.inter, TP_fmmo, interval="confidence") %>%
  data.frame() 
fmmo_g2 <- fitted_vs_actual(fmmo_inter_fitval, TP_fmmo$flowmass_meanopen, 
                            "flowmass_meanopen - Interaction Model")

# Initial (trans) model
fmmo_init_trans_fitval <- predict(fmmo.lm.init.trans, TP_fmmo, interval="confidence") %>%
  data.frame()
fmmo_g3 <- fitted_vs_actual(fmmo_init_trans_fitval, TP_fmmo$flowmass_meanopen, 
                            "flowmass_meanopen - Initial (Transformed) Model")

# Interaction (trans) model
fmmo_inter_trans_fitval <- predict(fmmo.lm.inter.trans, TP_fmmo, interval="confidence") %>%
  data.frame()
fmmo_g4 <- fitted_vs_actual(fmmo_inter_trans_fitval, TP_fmmo$flowmass_meanopen, 
                            "flowmass_meanopen - Interaction (Transformed) Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(fmmo_g1,fmmo_g2,fmmo_g3,fmmo_g4, ncol=4)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: flowmass_meanopen ~ pol_abundance.yj + flo_shannon
summ(fmmo.lm.init, digits= 4) # Adj-R2: 0.686; p: 0.0012

# Interaction model: flowmass_meanopen ~ pol_abundance.yj * flo_shannon
summ(fmmo.lm.inter, digits= 4) # Adj-R2: 0.715; p: 0.0023

# Initial (trans) model: flowmass_meanopen ~ sq.pol_abundance + sq.flo_shannon
summ(fmmo.lm.init.trans, digits= 4) # Adj-R2: 0.697; p: 0.001

# Interaction (trans) model: flowmass_meanopen ~ sq.pol_abundance * sq.flo_shannon
summ(fmmo.lm.inter.trans, digits= 4) # Adj-R2: 0.718; p: 0.0022

# Anova testing between models
anova(fmmo.lm.init.trans, fmmo.lm.inter.trans, test="F") # p: 0.22 => Not much improvement
anova(fmmo.lm.init, fmmo.lm.inter, test = "F") # p: 0.19 => Not much improvement


# ---- Testing/Checking linear assumptions for best model(s) ----

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(fmmo.lm.inter) # p: 0.276 --> Pass
ncvTest(fmmo.lm.inter.trans) # p: 0.44 --> Pass

# Auto correlated Errors test - H0: consecutive errors are not correlated 
# => p-value more than 0.05 is good
set.seed(1)
durbinWatsonTest(fmmo.lm.inter) # p: 0.946 --> Consecutive errors are independent of each other
durbinWatsonTest(fmmo.lm.inter.trans) # p: 0.92 --> Consecutive errors are independent of each other


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Interaction model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(fmmo.lm.inter, confint = TRUE, digits=4, ci.width = .95, center=T, pvals=T)
# Call: flowmass_meanopen ~ pol_abundance.yj * flo_shannon
# Adj-R2: 0.715; p: 0.0023

# Plot how predictor 'pol_abundance.yj' is related to response var
plot_model(fmmo.lm.inter, type="pred", terms='pol_abundance.yj', show.data=T, line.size=1.3)

# Plot how predictor 'flo_shannon' is related to response var
plot_model(fmmo.lm.inter, type="pred", terms='flo_shannon', show.data=T, line.size=1.3)


# ---- Interaction (transformed) model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(fmmo.lm.inter.trans, confint = TRUE, digits=4, ci.width = .95, center=T, pvals=T)
# Call: flowmass_meanopen ~ sq.pol_abundance.yj + sq.flo_shannon
# Adj-R2: 0.718; p: 0.0022

# Plot how predictor 'sq.pol_abundance.yj' is related to response var
plot_model(fmmo.lm.init.trans, type="pred", terms='sq.pol_abundance.yj', show.data=T, line.size=1.3)

# Plot how predictor 'sq.flo_shannon' is related to response var
plot_model(fmmo.lm.init.trans, type="pred", terms='sq.flo_shannon', show.data=T, line.size=1.3)


# ------------------------------------------------------------------------------


# ---- Clean-up environment for the next script ----
rm(list=ls())

