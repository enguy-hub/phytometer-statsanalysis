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
ra_path = "./analysis_data/RA/RA_Data_2021_4analysis_garden.xlsx"
RA_data <- read_excel(ra_path, sheet = 1)


# Check structure and summaries of the data
str(RA_data)
summary(RA_data)


# Remove "Unnecessary" variables
RA_data <- RA_data %>%
  dplyr::select(-c("urbanclass100", "urbanclass200", "urbanclass500", "urbanclass1000")) # 


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


# -- Best lm() model for: avgdrymass_ferseed ----


# -- Create new dataframe, which remove "non-related" vars ----
RA_admpsmd <- RA_data %>%
  dplyr::select(-c("fremass_meandiff", "drymass_meandiff", 
                   "fremass_meanopen", "drymass_meanopen",
                   "avgfremass_pseed_meandiff",
                   "avgfremass_pseed_meanopen", "avgdrymass_pseed_meanopen",
                   "avgfremass_ferseed", "avgdrymass_ferseed"))


# -- Check correlation of dependent and independent vars again ----
admpsmd_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14)
admpsmd_corr <- RA_admpsmd[,admpsmd_vars]
chart.Correlation(admpsmd_corr, histogram=TRUE)


# -- Create multiple regression lm() model ----
admpsmd.lm0 <- lm(avgdrymass_pseed_meandiff ~ lux + imperv100 + # temp + 
                  pol_abundance + pol_richness + pol_shannon + 
                  flo_abundance + flo_richness + flo_shannon, data=RA_admpsmd)
summ(admpsmd.lm0, digits=4) # Adj-R2: -0.35; p: 0.745


# ---- Create initial model with stepAIC() ----
admpsmd.lm.init <- MASS::stepAIC(admpsmd.lm0, direction = "both", trace = FALSE)
summ(admpsmd.lm.init, digits= 4) # Adj-R2: 0.34; p: 0.049

# Check model$call
admpsmd.lm.init$call # avgdrymass_pseed_meandiff ~ imperv100 + flo_abundance

# Check for multi-collinerity: For all vars, less than 3 is good
vif(admpsmd.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(admpsmd.lm.init) # p: 0.2 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(admpsmd.lm.init) # p: 0.472 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(admpsmd.lm.init)) # p: 0.91 --> Residuals are norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(admpsmd.lm.init))
qqline(residuals(admpsmd.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(admpsmd.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(admpsmd.lm.init)

# Initial model: avgdrymass_pseed_meandiff ~ imperv100 + flo_abundance
summ(admpsmd.lm.init, digits= 4) # Adj-R2: 0.3425; p: 0.049


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
admpsmd.lm.inter <- stepAIC(admpsmd.lm.init, ~.^2, trace=F)
summ(admpsmd.lm.inter,digits=4) # Adj-R2: 0.3425 ; p: 0.049

# Same as initial => No significant interaction terms found


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
RA_admpsmd <- RA_admpsmd %>%
  mutate(sq.imperv100 = imperv100^2,
         sq.flo_abundance = flo_abundance^2)

# Create transformed model
admpsmd.lm.init.trans <- lm(avgdrymass_pseed_meandiff ~ sq.imperv100 + sq.flo_abundance, data=RA_admpsmd)
summ(admpsmd.lm.init.trans, digits=4) # Adj-R2: 0.3544; p: 0.0451

# Check model$call
admpsmd.lm.init.trans$call # avgdrymass_pseed_meandiff ~ sq.imperv100+sq.flo_abundance

# Check for multi-collinerity: For all vars, less than 3 is good
vif(admpsmd.lm.init.trans) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(admpsmd.lm.init) # p: 0.2 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(admpsmd.lm.init) # p: 0.472 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(admpsmd.lm.init.trans)) # p: 0.94 --> Residuals is norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(admpsmd.lm.init.trans))
qqline(residuals(admpsmd.lm.init.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(admpsmd.lm.init.trans, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(admpsmd.lm.init.trans)

# Initial model: avgdrymass_pseed_meandiff ~ sq.imperv1000 + sq.flo_abundance
summ(admpsmd.lm.init.trans, digits= 4) # Adj-R2: 0.3544; p: 0.0451


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
admpsmd.lm.inter.trans <- stepAIC(admpsmd.lm.init.trans, ~.^2, trace=F)
summ(admpsmd.lm.inter.trans,digits=4) # Adj-R2: 0.354; p: 0.0451

# Same as initial (transformed) model => No significant interaction terms found 


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
admpsmd_init_fitval <- predict(admpsmd.lm.init, RA_admpsmd, interval="confidence") %>%
  data.frame() 
admpsmd_g1 <- fitted_vs_actual(admpsmd_init_fitval, RA_admpsmd$avgdrymass_pseed_meandiff, 
                               "avgdrymass_pseed_meandiff - Initial Model")

# Initial (trans) model
admpsmd_init_trans_fitval <- predict(admpsmd.lm.init.trans, RA_admpsmd, interval="confidence") %>%
  data.frame()
admpsmd_g2 <- fitted_vs_actual(admpsmd_init_trans_fitval, RA_admpsmd$avgdrymass_pseed_meandiff, 
                           "avgdrymass_pseed_meandiff - Initial (Transformed) Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(admpsmd_g1,admpsmd_g2, ncol=2)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: avgdrymass_pseed_meandiff ~ imperv1000 + flo_abundance
summ(admpsmd.lm.init, digits= 4) # Adj-R2: 0.342; p: 0.049

# Initial (trans) model: avgdrymass_pseed_meandiff ~ sq.imperv1000 + sq.flo_abundance
summ(admpsmd.lm.init.trans, digits= 4) # Adj-R2: 0.354; p: 0.045

# No ANOVA tests: The best two models have different predictors


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Initial model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(admpsmd.lm.init, confint = TRUE, digits=4, ci.width=.95, center=T, pvals=T)
# Call: avgdrymass_pseed_meandiff ~ imperv1000 + flo_abundance
# Adj-R2: 0.3425; p: 0.0494

# Plot how predictor 'imperv100' is related to response var
plot_model(admpsmd.lm.init, type="pred", terms='imperv100', show.data=T, line.size=1.3,
           title="Ranunculus Dry Mass Per Seed vs Impervious Surface",
           axis.title=c("impervious surface (%) of 1000m buffer", "fitted values | dry mass per seed of `open - bagged` flowers"))


# ---- Initial (transformed) model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(admpsmd.lm.init.trans, confint = TRUE, digits=4, ci.width=.95, center=T, pvals=T)
# Call: avgdrymass_pseed_meandiff ~ sq.imperv1000 + sq.flo_abundance
# Adj-R2: 0.354; p: 0.0451

# Plot how predictor 'sq.imperv100' is related to response var
plot_model(admpsmd.lm.init.trans, type="pred", terms='sq.imperv100', show.data=T, line.size=1.3,
           title="Ranunculus Dry Mass Per Seed vs Impervious Surface (Squared Transformed)",
           axis.title=c("impervious surface (%) of 1000m buffer (squared transformed)", "fitted values | dry mass per seed of `open - bagged` flowers"))

# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------

# -- Best lm() model for: avgdrymass_ferseed ----


# -- Create new dataframe, which remove "non-related" vars ----
RA_admfs <- RA_data %>%
  dplyr::select(-c("fremass_meandiff", "drymass_meandiff", 
                   "fremass_meanopen", "drymass_meanopen",
                   "avgfremass_pseed_meandiff", "avgdrymass_pseed_meandiff",
                   "avgfremass_pseed_meanopen", "avgdrymass_pseed_meanopen",
                   "avgfremass_ferseed"))


# -- Check correlation of dependent and independent vars again ----
admfs_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14)
admfs_corr <- RA_admfs[,admfs_vars]
chart.Correlation(admfs_corr, histogram=TRUE)


# -- Create initial multiple regression lm() model ----
admfs.lm0 <- lm(avgdrymass_ferseed ~ lux + imperv100 + # temp + 
                pol_abundance + pol_shannon + # pol_richness + 
                flo_abundance + flo_richness + flo_shannon, data=RA_admfs)
summ(admfs.lm0, digits=4) # Adj-R2: 0.386; p: 0.22


# ---- Create initial model with stepAIC() ----
admfs.lm.init <- MASS::stepAIC(admfs.lm0, direction = "both", trace = FALSE)
summ(admfs.lm.init, digits= 4) # Adj-R2: 0.534; p: 0.035

# Check model$call
admfs.lm.init$call # avgdrymass_ferseed ~ imperv100 + pol_abundance + flo_abundance + flo_richness

# Check for multi-collinerity: For all vars, less than 3 is good
vif(admfs.lm.init) %>% 
  knitr::kable() # Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(admfs.lm.init) # p: 0.093 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(admfs.lm.init) # p: 0.334 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(admfs.lm.init)) # p: 0.672 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(admfs.lm.init))
qqline(residuals(admfs.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(admfs.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(admfs.lm.init)

# Initial model: avgdrymass_ferseed ~ imperv100 + pol_abundance + flo_abundance + flo_richness
summ(admfs.lm.init, digits= 4) # Adj-R2: 0.534; p: 0.035


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
admfs.lm.inter <- stepAIC(admfs.lm.init, ~.^2, trace=F)
summ(admfs.lm.inter,digits=4) # Adj-R2: 0.772; p: 0.0255

# Check model$call
admfs.lm.inter$call # avgdrymass_ferseed ~ imperv100 + pol_abundance + flo_abundance + flo_richness

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(admfs.lm.inter) # p: 0.31 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(admfs.lm.inter) # p: 0.56 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(admfs.lm.inter)) # p: 0.472 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(admfs.lm.inter))
qqline(residuals(admfs.lm.inter))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(admfs.lm.inter, type = "rstandard") # curve --> slight non-linearity

# Initial model: avgdrymass_ferseed ~ imperv100 + pol_abundance + flo_abundance + flo_richness + pol_abundance:flo_richness + imperv100:flo_richness + imperv100:pol_abundance
summ(admfs.lm.inter, digits= 4) # Adj-R2: 0.772; p: 0.0255


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
RA_admfs <- RA_admfs %>%
  mutate(sq.imperv100 = imperv100^2,
         sq.pol_abundance = pol_abundance^2,
         sq.flo_abundance = flo_abundance^2,
         sq.flo_richness = flo_richness^2)

# Create transformed model
admfs.lm.init.trans <- lm(avgdrymass_ferseed ~ sq.imperv100 + sq.pol_abundance +
                          sq.flo_abundance + sq.flo_richness, data=RA_admfs)
summ(admfs.lm.init.trans, digits=4) # Adj-R2: 0.41; p: 0.082

# Check model$call
admfs.lm.init.trans$call # avgdrymass_ferseed ~ sq.imperv100 + sq.pol_abundance + sq.flo_abundance + sq.flo_richness

# Check for multi-collinerity: For all vars, less than 3 is good
vif(admfs.lm.init.trans) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(admfs.lm.init.trans) # p: 0.028 --> Not Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(admfs.lm.init.trans) # p: 0.57 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(admfs.lm.init.trans)) # p: 0.688 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(admfs.lm.init.trans))
qqline(residuals(admfs.lm.init.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(admfs.lm.init.trans, type = "rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(admfs.lm.init.trans)

# Initial model: avgdrymass_ferseed ~ sq.imperv100 + sq.pol_richness + sq.flo_abundance
summ(admfs.lm.init.trans, digits= 4) # Adj-R2: 0.41; p: 0.082


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
admfs.lm.inter.trans <- stepAIC(admfs.lm.init.trans, ~.^2, trace=F)
summ(admfs.lm.inter.trans,digits=4) # Adj-R2: 0.73; p: 0.02

# Check model$call
admfs.lm.inter.trans$call # avgdrymass_ferseed ~ sq.imperv100 + sq.pol_abundance + sq.flo_abundance + sq.flo_richness + sq.pol_abundance:sq.flo_richness + sq.imperv100:sq.flo_richness

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(admfs.lm.inter.trans) # p: 0.0077 --> Not Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(admfs.lm.inter.trans) # p: 0.27 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(admfs.lm.inter.trans)) # p: 0.296 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(admfs.lm.inter.trans))
qqline(residuals(admfs.lm.inter.trans))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(admfs.lm.inter.trans, type = "rstandard") # curve --> slight non-linearity

# Initial model: avgdrymass_ferseed ~ sq.imperv100 + sq.pol_abundance + sq.flo_abundance + sq.flo_richness + sq.pol_abundance:sq.flo_richness + sq.imperv100:sq.flo_richness
summ(admfs.lm.inter.trans, digits= 4) # Adj-R2: 0.73; p: 0.02


# ---- Linear graphs to compare initial models against best model(s) ----

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

# Initial (trans) model
admfs_init_trans_fitval <- predict(admfs.lm.init.trans, RA_admfs, interval="confidence") %>%
  data.frame()
admfs_g3 <- fitted_vs_actual(admfs_init_trans_fitval, RA_admfs$avgdrymass_ferseed, 
                             "avgdrymass_ferseed - Initial (Transformed) Model")

# Interaction (trans) model
admfs_inter_trans_fitval <- predict(admfs.lm.inter.trans, RA_admfs, interval="confidence") %>%
  data.frame()
admfs_g4 <- fitted_vs_actual(admfs_inter_trans_fitval, RA_admfs$avgdrymass_ferseed, 
                             "avgdrymass_ferseed - Interaction (Transformed) Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(admfs_g1,admfs_g2,admfs_g3,admfs_g4, ncol=2)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: avgdrymass_ferseed ~ imperv1000 + pol_abundance + flo_abundance + flo_richness
summ(admfs.lm.init, digits= 4) # Adj-R2: 0.534; p: 0.035

# Interaction model: avgdrymass_ferseed ~ imperv1000 + pol_abundance + flo_abundance + flo_richness + pol_abundance:flo_richness + imperv100:flo_richness + imperv100:pol_abundance
summ(admfs.lm.inter, digits= 4) # Adj-R2: 0.772; p: 0.0255

# Initial (trans) model: avgdrymass_ferseed ~ sq.imperv100 + sq.pol_abundance + sq.flo_abundance + sq.flo_richness
summ(admfs.lm.init.trans, digits= 4) # Adj-R2: 0.41; p: 0.082

# Initial (trans) model: avgdrymass_ferseed ~ sq.imperv100 + sq.pol_abundance + sq.flo_abundance + sq.flo_richness + sq.pol_abundance:sq.flo_richness + sq.imperv100:sq.flo_richness
summ(admfs.lm.inter.trans, digits= 4) # Adj-R2: 0.73; p: 0.02

# Anova testing between models
anova(admfs.lm.init.trans, admfs.lm.inter.trans, test="F") # p: 0.041 => Improved
anova(admfs.lm.init, admfs.lm.inter, test="F") # p: 0.093 => A bit Improved


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Interaction model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(admfs.lm.inter, confint = TRUE, digits=4, ci.width=.95, center=T, pvals=T)
# Call: avgdrymass_ferseed ~ imperv1000 + pol_abundance + flo_abundance + flo_richness + pol_abundance:flo_richness + imperv100:flo_richness + imperv100:pol_abundance
# Adj-R2: 0.772; p: 0.0255

# Plot how predictor 'imperv100' is related to response var
plot_model(admfs.lm.inter, type="pred", terms='imperv100', show.data=T, line.size=1.3,
           title="Dry Mass Per Fertile Seed vs Impervious Surface",
           axis.title=c("impervious surface (%) of 100m buffer", "fitted values | dry mass per fertile seed"))

# Plot how predictor 'flo_abundance' is related to response var
plot_model(admfs.lm.inter, type="pred", terms='flo_abundance', show.data=T, line.size=1.3,
           title="Dry Mass Per Fertile Seed vs Floral Abundance",
           axis.title=c("floral abundance", "fitted values | dry mass per fertile seed"))

# Plot how predictor 'flo_richness' is related to response var
plot_model(admfs.lm.inter, type="pred", terms='flo_richness', show.data=T, line.size=1.3,
           title="Dry Mass Per Fertile Seed vs Floral Richness",
           axis.title=c("floral richness", "fitted values | dry mass per fertile seed"))

# Plot how 'pol_abundance * flo_richness' is related to the fitted values of the response var
p_admfs.inter_polabu.floric <- 
  plot_model(admfs.lm.inter, type="pred", line.size=1.3,
             terms=c("pol_abundance", "flo_richness"), # [1.16, 1.72]
             title="Ranunculus Dry Mass\nPer Fertile Seed\nvs Pollinator Abundance
                    \nInteraction: Floral Richness",
             axis.title=c("pollinator abundance", "fitted values | dry mass per fertile seed"),
             legend.title="floral\nrichness")
p_admfs.inter_polabu.floric

p_admfs.inter_floric.polabu <- 
  plot_model(admfs.lm.inter, type="pred", line.size=1.3,
             terms=c("flo_richness", "pol_abundance"), # [0.52, 0.64]
             title="Ranunculus Dry Mass\nPer Fertile Seed\nvs Floral Richness
                    \nInteraction: Pollinator\nAbundance",
             axis.title=c("floral richness", "fitted values | dry mass per fertile seed"),
             legend.title="pollinator\nabundance")    
p_admfs.inter_floric.polabu

gridExtra::grid.arrange(p_admfs.inter_polabu.floric, p_admfs.inter_floric.polabu, ncol=2)


# ------------------------------------------------------------------------------


# -- Clean-up environment for the next script ----
rm(list=ls())
