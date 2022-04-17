# ---- Prerequisite procedures ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", 
                   "jtools", "PerformanceAnalytics", "sjPlot")
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

# -- Best lm() model for: mass_pseed_meanopen ----


# -- Create new dataframe, which remove "non-related" vars ----
TP_mpsmo <- TP_data %>%
  dplyr::select(-c("flowmass_meandiff", "flowmass_meanopen", 
                   "seedmass_meandiff", "seedmass_meanopen",
                   "mass_pseed_meandiff"))


# ----------- mass_pseed_meanopen & non-transformed predictor vars -------------

# -- Check correlation of dependent and independent vars again ----
mpsmo_vars <- c(2,3,4,5,6,7,8,9,11,12,13,15,16)
mpsmo_corr <- TP_mpsmo[,mpsmo_vars]
chart.Correlation(mpsmo_corr, histogram=T)


# -- Create multiple regression lm() model ----
mpsmo.lm0 <- lm(mass_pseed_meanopen ~ temp + lux + imperv1000 + 
                pol_abundance + pol_shannon + flo_richness + flo_shannon + # pol_richness +  
                flo_abundance, data=TP_mpsmo)
summ(mpsmo.lm0, digits=4) # Adj-R2: -0.6275; p: 0.8611


# ---- Create initial model with stepAIC() ---- 
mpsmo.lm.init <- MASS::stepAIC(mpsmo.lm0, direction="both", trace=F)
summ(mpsmo.lm.init, digits= 4) # Adj-R2: 0.1946; p: 0.1362

# Check model$call
mpsmo.lm.init$call # ~ temp + lux

# Check for multi-collinerity: For all vars, less than 3 is good
vif(mpsmo.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mpsmo.lm.init) # p: 0.63696 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mpsmo.lm.init) # p: 0.99 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mpsmo.lm.init)) # p: 0.9283 --> Residuals are norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mpsmo.lm.init))
qqline(residuals(mpsmo.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mpsmo.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(mpsmo.lm.init)

# Initial model: ~ temp + lux
summ(mpsmo.lm.init, digits= 4) # Adj-R2: 0.657; p: 0.011


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
mpsmo.lm.inter <- stepAIC(mpsmo.lm.init, ~.^2, trace=F)
summ(mpsmo.lm.inter,digits=4) # Adj-R2: 0.3794; p: 0.0651

# Check model$call
mpsmo.lm.inter$call # ~ temp * lux

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mpsmo.lm.inter) # p: 0.867 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mpsmo.lm.inter) # p: 0.88 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mpsmo.lm.inter)) # p: 0.0811 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mpsmo.lm.inter))
qqline(residuals(mpsmo.lm.inter))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mpsmo.lm.inter, type = "rstandard") # curve --> slight non-linearity

# Initial model: ~ temp * lux
summ(mpsmo.lm.inter, digits= 4) # Adj-R2: 0.3794; p: 0.0651


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
TP_mpsmo <- TP_mpsmo %>%
  mutate(sq.temp = temp^2,
         sq.lux = lux^2)

# Create transformed model
mpsmo.lm.init.trans <- lm(mass_pseed_meanopen ~ sq.temp + sq.lux, data=TP_mpsmo)
summ(mpsmo.lm.init.trans, digits=4) # Adj-R2: 0.2177; p: 0.1177

# Check model$call
mpsmo.lm.init.trans$call # ~ sq.temp + sq.lux

# Check for multi-collinerity: For all vars, less than 3 is good
vif(mpsmo.lm.init.trans) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mpsmo.lm.init.trans) # p: 0.528 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mpsmo.lm.init.trans) # p: 0.936 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mpsmo.lm.init.trans)) # p: 0.9853 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mpsmo.lm.init.trans))
qqline(residuals(mpsmo.lm.init.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mpsmo.lm.init.trans, type = "rstandard") # straighter line

# Check CERES plot
ceresPlots(mpsmo.lm.init.trans)

# Initial model: ~ sq.temp + sq.lux
summ(mpsmo.lm.init.trans, digits= 4) # Adj-R2: 0.2177; p: 0.1177


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
mpsmo.lm.inter.trans <- stepAIC(mpsmo.lm.init.trans, ~.^2, trace=F)
summ(mpsmo.lm.inter.trans,digits=4) # Adj-R2: 0.3938; p: 0.0590

# Check model$call
mpsmo.lm.inter.trans$call # ~ sq.temp * sq.lux

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mpsmo.lm.inter.trans) # p: 0.993 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mpsmo.lm.inter.trans) # p: 0.79 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mpsmo.lm.inter.trans)) # p: 0.2559 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mpsmo.lm.inter.trans))
qqline(residuals(mpsmo.lm.inter.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mpsmo.lm.inter.trans, type = "rstandard") # curve --> slight non-linearity

# Initial model: ~ temp * lux
summ(mpsmo.lm.inter.trans, digits= 4) # Adj-R2: 0.3938; p: 0.0590


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
mpsmo_init_fitval <- predict(mpsmo.lm.init, TP_mpsmo, interval="confidence") %>%
  data.frame() 
mpsmo_g1 <- fitted_vs_actual(mpsmo_init_fitval, TP_mpsmo$mass_pseed_meanopen, 
                             "mass_pseed_meanopen - Initial Model")

# Interaction model
mpsmo_inter_fitval <- predict(mpsmo.lm.inter, TP_mpsmo, interval="confidence") %>%
  data.frame() 
mpsmo_g2 <- fitted_vs_actual(mpsmo_inter_fitval, TP_mpsmo$mass_pseed_meanopen, 
                             "mass_pseed_meanopen - Interaction Model")

# Initial (trans) model
mpsmo_init_trans_fitval <- predict(mpsmo.lm.init.trans, TP_mpsmo, interval="confidence") %>%
  data.frame()
mpsmo_g3 <- fitted_vs_actual(mpsmo_init_trans_fitval, TP_mpsmo$mass_pseed_meanopen, 
                             "mass_pseed_meanopen - Initial (Transformed) Model")

# Interaction (trans) model
mpsmo_inter_trans_fitval <- predict(mpsmo.lm.inter.trans, TP_mpsmo, interval="confidence") %>%
  data.frame()
mpsmo_g4 <- fitted_vs_actual(mpsmo_inter_trans_fitval, TP_mpsmo$mass_pseed_meanopen, 
                             "mass_pseed_meanopen - Interaction (Transformed) Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(mpsmo_g1, mpsmo_g2, mpsmo_g3, mpsmo_g4, ncol=2)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: ~ temp + lux
summ(mpsmo.lm.init, digits=4, center=T) # Adj-R2: 0.1946; p: 0.1362

# Interaction model: ~ temp * lux
summ(mpsmo.lm.inter, digits=4, center=T) # Adj-R2: 0.3794; p: 0.0651

# Initial (trans) model: ~ sq.temp + sq.lux
summ(mpsmo.lm.init.trans, digits=4, center=T) # Adj-R2: 0.2177; p: 0.1177

# Interaction (trans) model: ~ 
summ(mpsmo.lm.inter.trans, digits=4, center=T) # Adj-R2: 0.3938; p: 0.0590


# Anova testing between models
anova(mpsmo.lm.init.trans, mpsmo.lm.inter.trans, test="F") # p: 0.07956 => Improved
anova(mpsmo.lm.init, mpsmo.lm.inter, test="F") # p: 0.077 => Improved


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Interaction model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(mpsmo.lm.inter, confint=T, digits=4, ci.width=.95, center=T)
# Call: ~ temp * lux
# Adj-R2: 0.3794; p: 0.0651

# Plot how predictor 'lux' is related to response var
plot_model(mpsmo.lm.inter, type="pred", terms='lux', show.data=T, line.size=1.3,
           title="Trifolium Pratense | Dry Mass Per Seed of Open Flowers vs Light Intensity",
           axis.title=c("light intensity [lx]", "fitted values | dry mass per seed [g]"))


# ---- Interaction (transformed) model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(mpsmo.lm.inter.trans, confint=T, digits=4, ci.width=.95, center=T)
# Call: ~ sq.temp + sq.lux
# Adj-R2: 0.3938; p: 0.0590

# Plot how predictor 'sq.lux' is related to response var
plot_model(mpsmo.lm.inter.trans, type="pred", terms='sq.lux', show.data=T, line.size=1.3,
           title="Trifolium Pratense | Dry Mass Per Seed of Open Flowers vs Light Intensity (^2)",
           axis.title=c("light intensity (^2) [lx]", "fitted values | dry mass per seed [g]"))


# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------


# ------------- mass_pseed_meanopen & transformed predictor vars ---------------

# -- Check correlation of dependent and independent vars again ----
mpsmo.t_vars <- c(2,3,4,5,6,7,8,10,11,12,14,15,16)
mpsmo.t_corr <- TP_mpsmo[,mpsmo.t_vars]
chart.Correlation(mpsmo.t_corr, histogram=T)


# -- Create multiple regression lm() model ----
mpsmo.t.lm0 <- lm(mass_pseed_meanopen ~ imperv1000 + lux + # temp +   
                  flo_richness + pol_richness + pol_shannon + flo_abundance.yj + flo_shannon + 
                  pol_abundance.yj, data=TP_mpsmo)
summ(mpsmo.t.lm0, digits=4) # Adj-R2: 0.38; p: 0.2765


# ---- Create initial model with stepAIC() ---- 
mpsmo.t.lm.init <- MASS::stepAIC(mpsmo.t.lm0, direction="both", trace=F)
summ(mpsmo.t.lm.init, digits= 4) # Adj-R2: 0.5548; p: 0.0495

# Check model$call
mpsmo.t.lm.init$call # ~ lux + flo_richness + pol_richness + flo_shannon + flo_abundance.yj

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


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
mpsmo.t.lm.inter <- stepAIC(mpsmo.t.lm.init, ~.^2, trace=F)
summ(mpsmo.t.lm.inter,digits=4) # Adj-R2: 0.3794; p: 0.0651

# Can't make interaction model as initial was already perfect fit model => STOP


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
TP_mpsmo <- TP_mpsmo %>%
  mutate(sq.lux = lux^2,
         sq.flo_richness = flo_richness^2,
         sq.pol_richness = pol_richness^2,
         sq.flo_shannon = flo_shannon^2,
         sq.flo_abundance.yj = flo_abundance.yj^2)

# Create transformed model
mpsmo.t.lm.init.trans <- lm(mass_pseed_meanopen ~ sq.lux + sq.flo_richness + 
                            sq.pol_richness + sq.flo_shannon + 
                            sq.flo_abundance.yj, data=TP_mpsmo)
summ(mpsmo.t.lm.init.trans, digits=4) # Adj-R2: 0.4675; p: 0.0858

# Check model$call
mpsmo.t.lm.init.trans$call # ~ sq.lux + sq.flo_richness + sq.pol_richness + sq.flo_shannon + sq.flo_abundance.yj

# Check for multi-collinerity: For all vars, less than 3 is good
vif(mpsmo.t.lm.init.trans) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mpsmo.t.lm.init.trans) # p: 0.3157 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mpsmo.t.lm.init.trans) # p: 0.406 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mpsmo.t.lm.init.trans)) # p: 0.1594 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mpsmo.t.lm.init.trans))
qqline(residuals(mpsmo.t.lm.init.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mpsmo.t.lm.init.trans, type = "rstandard") # straighter line

# Check CERES plot
ceresPlots(mpsmo.t.lm.init.trans)

# Initial model: ~ sq.temp + sq.lux
summ(mpsmo.t.lm.init.trans, digits= 4) # Adj-R2: 0.4675; p: 0.0858


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
mpsmo.t.lm.inter.trans <- stepAIC(mpsmo.t.lm.init.trans, ~.^2, trace=F)
summ(mpsmo.t.lm.inter.trans,digits=4) # Adj-R2: 0.7706; p: 0.0500

# Check model$call
mpsmo.t.lm.inter.trans$call # ~ sq.lux * sq.flo_richness * sq.pol_richness * sq.flo_shannon + sq.flo_abundance.yj

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mpsmo.t.lm.inter.trans) # p: 0.2844 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mpsmo.t.lm.inter.trans) # p: 0.992 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mpsmo.t.lm.inter.trans)) # p: 0.6735 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mpsmo.t.lm.inter.trans))
qqline(residuals(mpsmo.t.lm.inter.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mpsmo.t.lm.inter.trans, type = "rstandard") # curve --> slight non-linearity

# Initial model: ~ temp * lux
summ(mpsmo.t.lm.inter.trans, digits= 4) # Adj-R2: 0.7706; p: 0.0500


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
mpsmo.t_init_fitval <- predict(mpsmo.t.lm.init, TP_mpsmo, interval="confidence") %>%
  data.frame() 
mpsmo.t_g1 <- fitted_vs_actual(mpsmo.t_init_fitval, TP_mpsmo$mass_pseed_meanopen, 
                               "mass_pseed_meanopen - Initial Model")

# Initial (trans) model
mpsmo.t_init_trans_fitval <- predict(mpsmo.t.lm.init.trans, TP_mpsmo, interval="confidence") %>%
  data.frame()
mpsmo.t_g3 <- fitted_vs_actual(mpsmo.t_init_trans_fitval, TP_mpsmo$mass_pseed_meanopen, 
                               "mass_pseed_meanopen - Initial (Transformed) Model")

# Interaction (trans) model
mpsmo.t_inter_trans_fitval <- predict(mpsmo.t.lm.inter.trans, TP_mpsmo, interval="confidence") %>%
  data.frame()
mpsmo.t_g4 <- fitted_vs_actual(mpsmo.t_inter_trans_fitval, TP_mpsmo$mass_pseed_meanopen, 
                               "mass_pseed_meanopen - Interaction (Transformed) Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(mpsmo.t_g1, mpsmo.t_g3, mpsmo.t_g4, ncol=2)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: ~ lux + flo_richness + pol_richness + flo_abundance.yj + flo_shannon
summ(mpsmo.t.lm.init, digits=4, center=T) # Adj-R2: 0.5548; p: 0.0495

# Initial (trans) model: ~ sq.lux + sq.flo_richness + sq.pol_richness + sq.flo_shannon + sq.flo_abundance.yj
summ(mpsmo.t.lm.init.trans, digits=4, center=T) # Adj-R2: 0.4675; p: 0.0858

# Interaction (trans) model: ~ sq.flo_shannon * sq.lux * sq. pol_richness * sq.flo_richness + sq.flo_abundance.yj
summ(mpsmo.t.lm.inter.trans, digits=4, center=T) # Adj-R2: 0.7706; p: 0.0500


# Anova testing between models
anova(mpsmo.t.lm.init.trans, mpsmo.t.lm.inter.trans, test="F") # p: 0.1038 => NOT Improved


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Initial model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(mpsmo.t.lm.init, confint=T, digits=4, ci.width=.95, center=T)
# Call: ~ lux + flo_richness + pol_richness + flo_abundance.yj + flo_shannon
# Adj-R2: 0.5548; p: 0.0495

# Plot how predictor 'pol_richness' is related to response var
plot_model(mpsmo.t.lm.init, type="pred", terms='pol_richness', show.data=T, line.size=1.3,
           title="Trifolium Pratense | Dry Mass Per Seed of Open Flowers vs Pollinator Richness",
           axis.title=c("pollinator richness", "fitted values | dry mass per seed [g]"))

# Plot how predictor 'flo_abundance.yj' is related to response var
plot_model(mpsmo.t.lm.init, type="pred", terms='flo_abundance.yj', show.data=T, line.size=1.3,
           title="Trifolium Pratense | Dry Mass Per Seed of Open Flowers vs Floral Abundance (yj)",
           axis.title=c("floral abundance (yj)", "fitted values | dry mass per seed [g]"))

# Plot how predictor 'flo_shannon' is related to response var
plot_model(mpsmo.t.lm.init, type="pred", terms='flo_shannon', show.data=T, line.size=1.3,
           title="Trifolium Pratense | Dry Mass Per Seed of Open Flowers vs Floral Shannon Index",
           axis.title=c("floral shannon index", "fitted values | dry mass per seed [g]"))


# ---- Interaction (transformed) model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(mpsmo.t.lm.inter.trans, confint=T, digits=4, ci.width=.95, center=T)
# Call: ~ sq.flo_shannon * sq.lux * sq. pol_richness * sq.flo_richness + sq.flo_abundance.yj
# Adj-R2: 0.7706; p: 0.0500

# Plot how predictor 'sq.lux' is related to response var
plot_model(mpsmo.t.lm.inter.trans, type="pred", terms='sq.lux', show.data=T, line.size=1.3,
           title="Trifolium Pratense | Dry Mass Per Seed of Open Flowers vs Light Intensity (^2)",
           axis.title=c("light intensity (^2) [lx]", "fitted values | dry mass per seed [g]"))

# Plot how predictor 'sq.pol_richness' is related to response var
plot_model(mpsmo.t.lm.inter.trans, type="pred", terms='sq.pol_richness', show.data=T, line.size=1.3,
           title="Trifolium Pratense | Dry Mass Per Seed of Open Flowers vs Pollinator Richness (^2)",
           axis.title=c("pollinator richness (^2)", "fitted values | dry mass per seed [g]"))

# Plot how predictor 'sq.flo_shannon' is related to response var
plot_model(mpsmo.t.lm.inter.trans, type="pred", terms='sq.flo_shannon', show.data=T, line.size=1.3,
           title="Trifolium Pratense | Dry Mass Per Seed of Open Flowers vs Floral Shannon Index (^2)",
           axis.title=c("floral shannon index (^2)", "fitted values | dry mass per seed [g]"))


# ------------------------------------------------------------------------------


# ---- Clean-up environment for the next script ----
rm(list=ls())

