# ---- Prerequisite procedures ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", "ggpubr",
                   "jtools", "PerformanceAnalytics", "sjPlot")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Hien/Garden/MyGithub/Phytometer_StatisticalAnalysis"
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
  dplyr::select(-c("urbanclass100", "urbanclass200", "urbanclass500", "urbanclass1000",
                   "fruimass_meandiff", "fruimass_meandiff.yj", "fruimass_meanopen", "seedmass_meandiff", 
                   "ratio_meandiff", "ratio_meanopen", "ratio_meanopen.yj", "mass_pseed_meanopen"))

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
  dplyr::select(-c("seedmass_meanopen"))


# ----------- fruimass_meanopen.yj & non-transformed predictor vars ------------

# -- Check correlation of dependent and independent vars again ----
fmmo_vars <- c(2,3,4,5,6,7,8,9,10,11,13,15,16)
fmmo_corr <- CF_fmmo[,fmmo_vars]
chart.Correlation(fmmo_corr, histogram=TRUE)


# -- Create multiple regression lm() model ----
fmmo.lm0 <- lm(fruimass_meanopen.yj ~ lux + imperv100 + # temp + 
               pol_abundance + pol_shannon + flo_richness + # pol_richness +
               flo_abundance + flo_shannon, data=CF_data)
summ(fmmo.lm0, digits=4) # Adj-R2: 0.2109; p: 0.3504


# ---- Create initial model with stepAIC() ---- 
fmmo.lm.init <- MASS::stepAIC(fmmo.lm0, direction ="both", trace=F)
summ(fmmo.lm.init, digits=4) # Adj-R2: 0.4727; p: 0.0327

# Check model$call
fmmo.lm.init$call # ~ lux + imperv100 + flo_richness

# Check for multi-collinerity: For all vars, less than 3 is good
vif(fmmo.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(fmmo.lm.init) # p: 0.166 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(fmmo.lm.init) # p: 0.894 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(fmmo.lm.init)) # p: 0.01 --> Residuals NOT norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(fmmo.lm.init))
qqline(residuals(fmmo.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(fmmo.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(fmmo.lm.init)

# Initial model: ~ lux + imperv100 + flo_richness
summ(fmmo.lm.init, digits=4) # Adj-R2: 0.4727; p: 0.0327


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
fmmo.lm.inter <- stepAIC(fmmo.lm.init, ~.^2, trace=F)
summ(fmmo.lm.inter,digits=4) # Adj-R2: 0.3998; p: 0.031

# Same as initial => No significant interaction terms found


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
CF_fmmo <- CF_fmmo %>%
  mutate(sq.lux = lux^2,
         sq.imperv100 = imperv100^2,
         sq.flo_richness = flo_richness^2)

# Create transformed model
fmmo.lm.init.trans <- lm(fruimass_meanopen.yj ~ sq.lux + sq.imperv100 + sq.flo_richness, data=CF_fmmo)
summ(fmmo.lm.init.trans, digits=4) # Adj-R2: 0.2330; p: 0.1558

# ---- Create initial model with stepAIC() ---- 
fmmo.lm.init.trans <- MASS::stepAIC(fmmo.lm.init.trans, direction="both", trace=F)
summ(fmmo.lm.init.trans, digits=4) # Adj-R2: 0.2330; p: 0.1558

# Check model$call
fmmo.lm.init.trans$call # ~ sq.lux + sq.imperv100 + sq.flo_richness

# Check for multi-collinerity: For all vars, less than 3 is good
vif(fmmo.lm.init.trans) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(fmmo.lm.init.trans) # p: 0.937 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(fmmo.lm.init.trans) # p: 0.618 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(fmmo.lm.init.trans)) # p: 0.0271 --> Residuals NOT norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(fmmo.lm.init.trans))
qqline(residuals(fmmo.lm.init.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(fmmo.lm.init.trans, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(fmmo.lm.init.trans)

# Initial model: ~ sq.lux + sq.imperv100 + sq.flo_richness
summ(fmmo.lm.init.trans, digits=4) # Adj-R2: 0.2330; p: 0.1558


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
fmmo.lm.inter.trans <- stepAIC(fmmo.lm.init.trans, ~.^2, trace=F)
summ(fmmo.lm.inter.trans,digits=4) # Adj-R2: 0.2641; p: 0.176

# Worse than initial (transformed) model => No significant interaction terms found 


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

# Initial model: ~ lux + imperv100 + flo_richness
summ(fmmo.lm.init, digits=4) # Adj-R2: 0.4727; p: 0.0327

# Initial (trans) model: ~ sq.lux + sq.imperv100 + sq.flo_richness
summ(fmmo.lm.init.trans, digits=4) # Adj-R2: 0.2330; p: 0.1558

# No ANOVA tests: The best two models have different predictors


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Initial model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(fmmo.lm.init, confint=T, digits=4, ci.width=.95, center=T)
# Call: ~ lux + imperv100 + flo_richness
# Adj-R2: 0.4727; p: 0.0327

# Plot how predictor 'lux' is related to response var
plot_model(fmmo.lm.init, type="pred", terms='lux', show.data=T, line.size=1.3,
           title="Capsicum Frutescens | Fruit Mass of Open Flowers (yj) vs Light Intensity",
           axis.title=c("light intensity [lx]", "fitted values | fruit mass of open flowers (yj) [g]"))

# Plot how predictor 'imperv100' is related to response var
plot_model(fmmo.lm.init, type="pred", terms='imperv100', show.data=T, line.size=1.3,
           title="Capsicum Frutescens | Fruit Mass of Open Flowers (yj) vs Impervious Surface",
           axis.title=c("impervious surface of 100m buffer [%]", "fitted values | fruit mass of open flowers (yj) [g]"))

# ------------------------------------------------------------------------------


# ------------- fruimass_meanopen.yj & transformed predictor vars --------------

# -- Check correlation of dependent and independent vars again ----
fmmo.t_vars <- c(2,3,4,5,6,7,8,9,10,12,14,15,16)
fmmo.t_corr <- CF_fmmo[,fmmo.t_vars]
chart.Correlation(fmmo.t_corr, histogram=TRUE)


# -- Create multiple regression lm() model ----
fmmo.t.lm0 <- lm(fruimass_meanopen.yj ~ lux + imperv100 + # temp + 
                 pol_abundance + pol_shannon.yj + flo_richness + # pol_richness +
                 flo_abundance.yj + flo_shannon, data=CF_data)
summ(fmmo.t.lm0, digits=4) # Adj-R2: 0.2597; p: 0.3123


# ---- Create initial model with stepAIC() ---- 
fmmo.t.lm.init <- MASS::stepAIC(fmmo.t.lm0, direction = "both", trace = FALSE)
summ(fmmo.t.lm.init, digits= 4) # Adj-R2: 0.4727; p: 0.0327

# Check model$call
fmmo.t.lm.init$call # ~ lux + imperv100 + flo_richness

# Same as fruimass_meanopen.yj & non-transformed predictor vars model => STOP

# ------------------------------------------------------------------------------




# ------------------------------------------------------------------------------

# -- Best lm() model for: seedmass_meanopen ----


# -- Create new dataframe, which remove "non-related" vars ----
CF_smmo <- CF_data %>%
  dplyr::select(-c("fruimass_meanopen.yj"))


# ------------- seedmass_meanopen & non-transformed predictor vars -------------

# -- Check correlation of dependent and independent vars again ----
smmo_vars <- c(2,3,4,5,6,7,8,9,10,11,13,15,16)
smmo_corr <- CF_smmo[,smmo_vars]
chart.Correlation(smmo_corr, histogram=TRUE)


# -- Create multiple regression lm() model ----
smmo.lm0 <- lm(seedmass_meanopen ~ lux + imperv100 + temp + 
               pol_abundance + pol_shannon + flo_richness + pol_richness + 
               flo_abundance + flo_shannon, data=CF_data)
summ(smmo.lm0, digits=4) # Adj-R2: 0.7715; p: 0.093


# ---- Create initial model with stepAIC() ----
smmo.lm.init <- MASS::stepAIC(smmo.lm0, direction="both", trace=F)
summ(smmo.lm.init, digits= 4) # Adj-R2: 0.8808; p: 0.0019

# Check model$call
smmo.lm.init$call # ~ lux + temp + pol_abundance + pol_shannon + flo_richness + flo_shannon

# Check for multi-collinerity: For all vars, less than 3 is good
vif(smmo.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(smmo.lm.init) # p: 0.95477 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(smmo.lm.init) # p: 0.596 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(smmo.lm.init)) # p: 0.5013 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(smmo.lm.init))
qqline(residuals(smmo.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(smmo.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(smmo.lm.init)

# Initial model: ~ lux + temp + pol_abundance + pol_shannon + flo_richness + flo_shannon
summ(smmo.lm.init, digits=4) # Adj-R2: 0.88; p: 0.0019


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
smmo.lm.inter <- stepAIC(smmo.lm.init, ~.^2, trace=F)
summ(smmo.lm.inter,digits=4)

# Can't make interaction model as initial was already perfect fit model => STOP


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
CF_smmo <- CF_smmo %>%
  mutate(sq.lux = lux^2,
         sq.imperv100 = imperv100^2,
         sq.pol_abundance = pol_abundance^2,
         sq.pol_shannon = pol_shannon^2,
         sq.flo_richness = flo_richness^2,
         sq.flo_shannon = flo_shannon^2)

# Create transformed model
smmo.lm.init.trans <- lm(seedmass_meanopen ~ sq.lux + sq.imperv100 + sq.pol_abundance + 
                         sq.pol_shannon + sq.flo_richness + sq.flo_shannon, data=CF_smmo)
summ(smmo.lm.init.trans, digits=4) # Adj-R2: 0.7145; p: 0.0232

# Find best model with stepAIC()
smmo.lm.init.trans <- MASS::stepAIC(smmo.lm.init.trans, direction="both", trace=F)
summ(smmo.lm.init.trans, digits= 4) # Adj-R2: 0.7145; p: 0.0232

# Check model$call
smmo.lm.init.trans$call # ~ sq.lux + sq.imperv100 + sq.pol_abundance + sq.pol_shannon + sq.flo_richness + sq.flo_shannon

# Check for multi-collinerity: For all vars, less than 3 is good
vif(smmo.lm.init.trans) %>% 
  knitr::kable() # All <= 5: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(smmo.lm.init.trans) # p: 0.3529 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(smmo.lm.init.trans) # p: 0.936 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(smmo.lm.init.trans)) # p: 0.3558 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(smmo.lm.init.trans))
qqline(residuals(smmo.lm.init.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(smmo.lm.init.trans, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(smmo.lm.init.trans)

# Initial model: ~ sq.lux + sq.imperv100 + sq.pol_abundance + sq.flo_richness + sq.flo_shannon
summ(smmo.lm.init.trans, digits=4) # Adj-R2: 0.7145; p: 0.0232


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
smmo.lm.inter.trans <- stepAIC(smmo.lm.init.trans, ~.^2, trace=F)
summ(smmo.lm.inter.trans,digits=4)

# Can't make interaction model as initial was already perfect fit model => STOP


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

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(smmo_g1, smmo_g2, ncol=2)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: ~ temp + lux + pol_abundance + pol_shannon + flo_richness + flo_shannon
summ(smmo.lm.init, digits=4) # Adj-R2: 0.88; p: 0.0019

# Initial (trans) model: ~ sq.lux + sq.imperv100 + sq.pol_abundance + sq.flo_richness + sq.flo_shannon
summ(smmo.lm.init.trans, digits=4) # Adj-R2: 0.7145; p: 0.0232


# No possible Anova test available


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Initial model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(smmo.lm.init, confint=T, digits=4, ci.width=.95, center=T, pvals=T)
# Call: ~ temp + lux + pol_abundance + pol_shannon + flo_richness + flo_shannon
# Adj-R2: 0.88; p: 0.0019

# Plot how predictor 'lux' is related to response var
plot_model(smmo.lm.init, type="pred", terms='lux', show.data=T, line.size=1.3,
           title="Capsicum Frutescens | Seed Mass of Open Flowers vs Light Intensity",
           axis.title=c("light intensity [lx]", "fitted values | seed mass of open flowers [g]"))

# Plot how predictor 'temp' is related to response var
plot_model(smmo.lm.init, type="pred", terms='temp', show.data=T, line.size=1.3,
           title="Capsicum Frutescens | Seed Mass of Open Flowers vs Temperature",
           axis.title=c("temperature [°C]", "fitted values | seed mass of open flowers [g]"))
 
# Plot how predictor 'pol_shannon' is related to response var
plot_model(smmo.lm.init, type="pred", terms='pol_shannon', show.data=T, line.size=1.3,
           title="Capsicum Frutescens | Seed Mass of Open Flowers vs Pollinator Shannon Index",
           axis.title=c("pollinator shannon index", "fitted values | seed mass of open flowers [g]"))

# Plot how predictor 'flo_richness' is related to response var
plot_model(smmo.lm.init, type="pred", terms='flo_richness', show.data=T, line.size=1.3,
           title="Capsicum Frutescens | Seed Mass of Open Flowers vs Floral Richness",
           axis.title=c("floral richness", "fitted values | seed mass of open flowers [g]"))

# ------------------------------------------------------------------------------



# --------------- seedmass_meanopen & transformed predictor vars ---------------

# -- Check correlation of dependent and independent vars again ----
smmo.t_vars <- c(2,3,4,5,6,7,8,9,10,12,14,15,16)
smmo.t_corr <- CF_smmo[,smmo.t_vars]
chart.Correlation(smmo.t_corr, histogram=TRUE)


# -- Create multiple regression lm() model ----
smmo.t.lm0 <- lm(seedmass_meanopen ~ lux + imperv100 + temp + 
                 pol_abundance + pol_shannon.yj + flo_richness + # pol_richness + 
                 flo_abundance.yj + flo_shannon, data=CF_data)
summ(smmo.t.lm0, digits=4) # Adj-R2: 0.8315; p: 0.0282


# ---- Create initial model with stepAIC() ----
smmo.t.lm.init <- MASS::stepAIC(smmo.t.lm0, direction="both", trace=F)
summ(smmo.t.lm.init, digits= 4) # Adj-R2: 0.8845; p: 0.0006

# Check model$call
smmo.t.lm.init$call # ~ lux + temp + pol_abundance + pol_shannon.yj + flo_richness

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


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
smmo.t.lm.inter <- stepAIC(smmo.t.lm.init, ~.^2, trace=F)
summ(smmo.t.lm.inter,digits=4)

# Can't make interaction model as initial was already perfect fit model => STOP


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
CF_smmo <- CF_smmo %>%
  mutate(sq.lux = lux^2,
         sq.temp = temp^2,
         sq.pol_abundance = pol_abundance^2,
         sq.pol_shannon.yj = pol_shannon.yj^2,
         sq.flo_richness = flo_richness^2)

# Create transformed model
smmo.t.lm.init.trans <- lm(seedmass_meanopen ~ sq.lux + sq.temp + sq.pol_abundance + 
                           sq.pol_shannon.yj + sq.flo_richness, data=CF_smmo)
summ(smmo.t.lm.init.trans, digits=4) # Adj-R2: 0.9262; p: 0.0001

# Find best model with stepAIC()
smmo.t.lm.init.trans <- MASS::stepAIC(smmo.t.lm.init.trans, direction="both", trace=F)
summ(smmo.t.lm.init.trans, digits= 4) # Adj-R2: 0.9262; p: 0.0001

# Check model$call
smmo.t.lm.init.trans$call # ~ sq.lux + sq.temp + sq.pol_abundance + sq.pol_shannon.yj + sq.flo_richness

# Check for multi-collinerity: For all vars, less than 3 is good
vif(smmo.t.lm.init.trans) %>% 
  knitr::kable() # All <= 5: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(smmo.t.lm.init.trans) # p: 0.0365 --> NOT Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(smmo.t.lm.init.trans) # p: 0.842 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(smmo.t.lm.init.trans)) # p: 0.9399 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(smmo.t.lm.init.trans))
qqline(residuals(smmo.t.lm.init.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(smmo.t.lm.init.trans, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(smmo.t.lm.init.trans)

# Initial (trans) model: ~ sq.lux + sq.temp + sq.pol_abundance + sq.pol_shannon.yj + sq.flo_richness
summ(smmo.t.lm.init.trans, digits=4) # Adj-R2: 0.9262; p: 0.0001


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
smmo.t.lm.inter.trans <- stepAIC(smmo.t.lm.init.trans, ~.^2, trace=F)
summ(smmo.t.lm.inter.trans,digits=4)

# Can't make interaction model as initial was already perfect fit model => STOP


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
smmo.t_init_fitval <- predict(smmo.t.lm.init, CF_smmo, interval="confidence") %>%
  data.frame() 
smmo.t_g1 <- fitted_vs_actual(smmo.t_init_fitval, CF_smmo$seedmass_meanopen, 
                              "seedmass_meanopen - Initial Model")

# Initial (trans) model
smmo.t_init_trans_fitval <- predict(smmo.t.lm.init.trans, CF_smmo, interval="confidence") %>%
  data.frame()
smmo.t_g2 <- fitted_vs_actual(smmo.t_init_trans_fitval, CF_smmo$seedmass_meanopen, 
                              "seedmass_meanopen - Initial (Transformed) Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(smmo.t_g1, smmo.t_g2, ncol=2)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: ~ temp + lux + pol_abundance + pol_shannon + flo_richness + flo_shannon
summ(smmo.t.lm.init, digits=4) # Adj-R2: 0.88; p: 0.0019

# Initial (trans) model: ~ sq.lux + sq.imperv100 + sq.pol_abundance + sq.flo_richness + sq.flo_shannon
summ(smmo.t.lm.init.trans, digits=4) # Adj-R2: 0.7145; p: 0.0232


# NO possible Anova tests


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Initial model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(smmo.t.lm.init, confint=T, digits=4, ci.width=.95, center=T, pvals=T)
# Call: ~ temp + lux + pol_abundance + pol_shannon/yj + flo_richness
# Adj-R2: 0.8845; p: 0.0006

# Plot how predictor 'lux' is related to response var
plot_model(smmo.t.lm.init, type="pred", terms='lux', show.data=T, line.size=1.3,
           title="Capsicum Frutescens | Seed Mass of Open Flowers vs Light Intensity",
           axis.title=c("light intensity [lx]", "fitted values | seed mass of open flowers [g]"))

# Plot how predictor 'temp' is related to response var
plot_model(smmo.t.lm.init, type="pred", terms='temp', show.data=T, line.size=1.3,
           title="Capsicum Frutescens | Seed Mass of Open Flowers vs Temperature",
           axis.title=c("temperature [°C]", "fitted values | seed mass of open flowers [g]"))

# Plot how predictor 'pol_shannon.yj' is related to response var
plot_model(smmo.t.lm.init, type="pred", terms='pol_shannon.yj', show.data=T, line.size=1.3,
           title="Capsicum Frutescens | Seed Mass of Open Flowers vs Pollinator Shannon Index (yj)",
           axis.title=c("pollinator shannon index", "fitted values | seed mass of open flowers [g]"))

# Plot how predictor 'flo_richness' is related to response var
plot_model(smmo.t.lm.init, type="pred", terms='flo_richness', show.data=T, line.size=1.3,
           title="Capsicum Frutescens | Seed Mass of Open Flowers vs Floral Richness",
           axis.title=c("floral richness", "fitted values | seed mass of open flowers [g]"))


# ---- Initial (trans) model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(smmo.t.lm.init.trans, confint=T, digits=4, ci.width=.95, center=T, pvals=T)
# Call: ~ sq.lux + sq.temp + sq.pol_abundance + sq.pol_shannon.yj + sq.flo_richness
# Adj-R2: 0.9262; p: 0.0001

# Plot how predictor 'lux' is related to response var
plot_model(smmo.t.lm.init, type="pred", terms='lux', show.data=T, line.size=1.3,
           title="Capsicum Frutescens | Seed Mass of Open Flowers vs Light Intensity (^2)",
           axis.title=c("light intensity [lx] (^2)", "fitted values | seed mass of open flowers [g]"))

# Plot how predictor 'temp' is related to response var
plot_model(smmo.t.lm.init, type="pred", terms='temp', show.data=T, line.size=1.3,
           title="Capsicum Frutescens | Seed Mass of Open Flowers vs Temperature (^2)",
           axis.title=c("temperature [°C] (^2)", "fitted values | seed mass of open flowers [g]"))

# Plot how predictor 'pol_abundance' is related to response var
plot_model(smmo.t.lm.init, type="pred", terms='pol_abundance', show.data=T, line.size=1.3,
           title="Capsicum Frutescens | Seed Mass of Open Flowers vs Pollinator Abundance (^2)",
           axis.title=c("pollinator abundance (^2)", "fitted values | seed mass of open flowers [g]"))

# Plot how predictor 'pol_shannon.yj' is related to response var
plot_model(smmo.t.lm.init, type="pred", terms='pol_shannon.yj', show.data=T, line.size=1.3,
           title="Capsicum Frutescens | Seed Mass of Open Flowers vs Pollinator Shannon Index (yj & ^2)",
           axis.title=c("pollinator shannon index (^2)", "fitted values | seed mass of open flowers [g]"))

# Plot how predictor 'flo_richness' is related to response var
plot_model(smmo.t.lm.init, type="pred", terms='flo_richness', show.data=T, line.size=1.3,
           title="Capsicum Frutescens | Seed Mass of Open Flowers vs Floral Richness (^2)",
           axis.title=c("floral richness (^2)", "fitted values | seed mass of open flowers [g]"))

# ------------------------------------------------------------------------------

# Plot correlation between 'imperv100' and 'temp'
ggscatter(CF_fmmo, x="imperv100", y="temp", 
          add="reg.line", conf.int=T, 
          cor.coef=T, cor.method="pearson",
          title="Capsicum Frutescens | Temperature vs Impervious Surface of 100m Buffer
                \n Significant Positive Correlation",
          xlab ="impervious surface of 100m buffer [%]", 
          ylab="temperature [°C]")

# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------


# ---- Clean-up environment for the next script ----
rm(list=ls())

