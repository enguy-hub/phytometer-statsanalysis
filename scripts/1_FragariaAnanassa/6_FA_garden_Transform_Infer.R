# -- Prerequisites ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", "jtools",
                   "PerformanceAnalytics", "sjPlot")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Users/hienn/Desktop/StatisticalAnalysis"
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

# -- Create new dataframe, without "non-related" vars ----
FA_mmd <- FA_data %>%
  dplyr::select(-c("mass_meanopen", "ratio_meandiff", "ratio_meanopen"))


# -- Check correlation of dependent and independent vars again ----
mmd_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
mmd_corr <- FA_mmd[,mmd_vars]
chart.Correlation(mmd_corr, histogram=TRUE)


# ---- Create multiple regression lm() model ----
mmd.lm0 <- lm(mass_meandiff ~ temp + lux + imperv1000 + 
              pol_shannon.yj + # pol_abundance.yj + pol_richness +
              flo_abundance.yj + flo_shannon, data=FA_mmd)
summ(mmd.lm0, digits=4) # Adj-R2: 0.666; p: 0.0356


# ---- Create initial model with stepAIC() ----
mmd.lm.init <- MASS::stepAIC(mmd.lm0, direction="both", trace=FALSE)
summ(mmd.lm.init, digits= 4) # Adj-R2: 0.738; p: 0.004

# Check model$call
mmd.lm.init$call # ~ temp + lux + imperv1000 + pol_shannon.yj

# Check for multi-collinerity: For all vars, less than 3 is good
vif(mmd.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mmd.lm.init, type = "rstandard") # curve --> slight non-linearity

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmd.lm.init))
qqline(residuals(mmd.lm.init))

# Shapiro test
shapiro.test(residuals(mmd.lm.init)) # p: 0.631 --> Residuals are norm-dist

# Check CERES plot
ceresPlots(mmd.lm.init)

# Initial model: ~ temp + lux imperv1000 + pol_shannon.yj
summ(mmd.lm.init, digits= 4) # Adj-R2: 0.738; p: 0.004



# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
mmd.lm.inter <- stepAIC(mmd.lm.init, ~.^2, trace=F)
summ(mmd.lm.inter,digits=4) # Adj-R2: 0.863; p: 0.0029

# Check model$call
mmd.lm.inter$call # ~ temp+lux+imperv1000+pol_shannon.yj+temp:imperv1000+temp:pol_shannon.yj

# Check residual plot
residualPlots(mmd.lm.inter, type = "rstandard") # => straight line

# Check if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmd.lm.inter))
qqline(residuals(mmd.lm.inter))

# Shapiro test
shapiro.test(residuals(mmd.lm.inter)) # p: 0.2544 --> Residuals are norm-dist

# Interaction model: ~ temp+lux+imperv1000+pol_shannon.yj+temp:imperv1000+temp:pol_shannon.yj
summ(mmd.lm.inter, digits= 4) # Adj-R2: 0.863; p: 0.0029


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
FA_mmd <- FA_mmd %>%
  mutate(sq.temp = temp^2,
         sq.lux = lux^2,
         sq.imperv1000 = imperv1000^2,
         sq.pol_shannon.yj = pol_shannon.yj^2)

# Create transformed model
mmd.lm.init.trans <- lm(mass_meandiff ~ sq.temp + sq.lux + sq.imperv1000 + sq.pol_shannon.yj, data=FA_mmd)
summ(mmd.lm.init.trans, digits=4) # Adj-R2: 0.7537; p: 0.0032

# Find best model with stepAIC()
mmd.lm.init.trans <- stepAIC(mmd.lm.init.trans, direction="both", trace=F)
summ(mmd.lm.init.trans, digits=4) # Adj-R2: 0.7799; p: 0.0002

# Check model$call
mmd.lm.init.trans$call # ~ sq.imperv1000 + sq.pol_shannon.yj

# Check for multi-collinerity: For all vars, less than 3 is good
vif(mmd.lm.init.trans) %>% 
  knitr::kable() # All < 3: Pass

# Check CERES plot
ceresPlots(mmd.lm.init.trans) # => Not too straight but okay

# Check residual plot
residualPlots(mmd.lm.init.trans, type = "rstandard") # => Blue line is straighter

# Check if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmd.lm.init.trans))
qqline(residuals(mmd.lm.init.trans))

# Shapiro test
shapiro.test(residuals(mmd.lm.init.trans)) # p: 0.5118 --> Residuals are norm-dist

# Initial (trans) model: ~ sq.imperv1000 + sq.pol_shannon.yj
summ(mmd.lm.init.trans, digits= 4) # Adj-R2: 0.7799; p: 0.0002


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
mmd.lm.inter.trans <- stepAIC(mmd.lm.init.trans, ~.^2, trace=F)
summ(mmd.lm.inter.trans, digits=4) # Adj-R2: 0.8735; p: 0.0063

# Check model$call
mmd.lm.inter.trans$call # ~ sq.temp+sq.lux+sq.imperv1000+sq.pol_shannon.yj+sq.temp:sq.lux+sq.temp:sq.imperv1000+sq.lux:sq.imperv1000

# Check residual plot
residualPlots(mmd.lm.inter.trans, type = "rstandard") # => straighter

# Check if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmd.lm.inter.trans))
qqline(residuals(mmd.lm.inter.trans))

# Shapiro test
shapiro.test(residuals(mmd.lm.inter.trans)) # p: 0.324 --> Residuals are norm-dist

# Interaction (trans) model: 
# ~ sq.temp+sq.lux+sq.imperv1000+sq.pol_shannon.yj+sq.temp:sq.lux+sq.temp:sq.imperv1000+sq.lux:sq.imperv1000
summ(mmd.lm.inter.trans, digits= 4) # Adj-R2: 0.8735; p: 0.0063


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: mass_meandiff ~ temp + lux + imperv1000 + pol_shannon.yj
summ(mmd.lm.init, digits= 4) # Adj-R2: 0.738; p: 0.004

# Interaction model: mass_meandiff ~ temp + lux + imperv1000 + pol_shannon.yj + temp:imperv1000 + temp:pol_shannon.yj
summ(mmd.lm.inter, digits= 4) # Adj-R2: 0.863; p: 0.0029

# Initial (trans) model: mass_meandiff ~ sq.temp + sq.lux + sq.imperv1000 + sq.pol_shannon.yj
summ(mmd.lm.init.trans, digits= 4) # Adj-R2: 0.7537; p: 0.0032

# Interaction (trans) model: mass_meandiff ~ sq.temp + sq.lux + sq.imperv1000 + sq.pol_shannon.yj + sq.temp:sq.lux + sq.temp:sq.imperv1000 + sq.lux:sq.imperv1000
summ(mmd.lm.inter.trans, digits= 4) # Adj-R2: 0.8735 ; p: 0.0063

# Anova testing between models
anova(mmd.lm.init.trans, mmd.lm.inter.trans, test="F") # p: 0.1 => Not much improvement
anova(mmd.lm.init, mmd.lm.inter, test = "F") # p: 0.06 => A bit improvement


# ---- Testing/Checking linear assumptions for best model(s) ----

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mmd.lm.inter) # p: 0.69 --> Pass
ncvTest(mmd.lm.inter.trans) # p: 0.83 --> Pass

# Auto correlated Errors test - H0: consecutive errors are not correlated 
# => p-value more than 0.05 is good
set.seed(1)
durbinWatsonTest(mmd.lm.inter) # p: 0.274 --> Consecutive errors are independent of each other
durbinWatsonTest(mmd.lm.inter.trans) # p: 0.656 --> Consecutive errors are independent of each other


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
mmd_init_fitval <- predict(mmd.lm.init, FA_mmd, interval="confidence") %>%
  data.frame() 
mmd_g1 <- fitted_vs_actual(mmd_init_fitval, FA_mmd$mass_meandiff, 
                           "mass_meandiff - Initial Model")

# Interaction model
mmd_inter_fitval <- predict(mmd.lm.inter, FA_mmd, interval="confidence") %>%
  data.frame()
mmd_g2 <- fitted_vs_actual(mmd_inter_fitval, FA_mmd$mass_meandiff, 
                           "mass_meandiff - Interaction Model")

# Initial (trans) model
mmd_init_trans_fitval <- predict(mmd.lm.init.trans, FA_mmd, interval="confidence") %>%
  data.frame()
mmd_g3 <- fitted_vs_actual(mmd_init_trans_fitval, FA_mmd$mass_meandiff, 
                           "mass_meandiff - Initial (Transformed) Model")

# Interaction (trans) model
mmd_inter_trans_fitval <- predict(mmd.lm.inter.trans, FA_mmd, interval="confidence") %>%
  data.frame()
mmd_g4 <- fitted_vs_actual(mmd_inter_trans_fitval, FA_mmd$mass_meandiff, 
                           "mass_meandiff - Interaction (Transformed) Model")

# Plot grid of the models
gridExtra::grid.arrange(mmd_g1,mmd_g2,mmd_g3,mmd_g4, ncol=4)


# ---- Plotting the relationship of vars in the best model(s) ----

# ---- Interaction model ----

# Estimated coefficients of predictors and their confidence intervals
summ(mmd.lm.inter, confint = TRUE, digits=4, ci.width = .95, center=T, pvals=T)
# Call: mass_meandiff~temp+lux+imperv1000+pol_shannon.yj+temp:imperv1000+temp:pol_shannon.yj
# Adj-R2: 0.863; p: 0.0029

# Plot how predictor 'lux' is related to response var
plot_model(mmd.lm.inter, type="pred", terms='lux', show.data=T, line.size=1.3)

# Plot how predictor 'imperv1000' is related to response var
plot_model(mmd.lm.inter, type="pred", terms='imperv1000', show.data=T, line.size=1.3)

# Plot how predictor 'pol_shannon.yj' isrelated to response var
plot_model(mmd.lm.inter, type="pred", terms='pol_shannon.yj', show.data=T, line.size=1.3)


# Plot how 'temp * imperv1000' is related to the fitted values of the response var
p_mmd.inter_temp.im1000 <- plot_model(mmd.lm.inter, type="pred", line.size=1.3,
                                      terms = c("temp", "imperv1000")) # [1.58, 14.04]
p_mmd.inter_im1000.temp <- plot_model(mmd.lm.inter, type="pred", line.size=1.3,
                                      terms=c("imperv1000", "temp")) # [0.31, 0.6]

gridExtra::grid.arrange(p_mmd.inter_temp.im1000, p_mmd.inter_im1000.temp, ncol=2)


# Plot how 'temp * pol_shannon.yj' is related to the fitted values of the response var
p_mmd.inter_temp.pol <- plot_model(mmd.lm.inter, type="pred", line.size=1.3,
                                   terms = c("temp", "pol_shannon.yj")) #  [1.58, 14.04]
p_mmd.inter_pol.temp <- plot_model(mmd.lm.inter, type="pred", line.size=1.3,
                                   terms=c("pol_shannon.yj", "temp")) #  [0.31, 0.6]

gridExtra::grid.arrange(p_mmd.inter_temp.pol, p_mmd.inter_pol.temp, ncol=2)


# ---- Interaction (transformed) model ----

# Estimated coefficients of predictors and their confidence intervals
summ(mmd.lm.inter.trans, confint = TRUE, digits=4, ci.width = .95, center=T, pvals=T)
# Call: mass_meandiff~sq.temp+sq.lux+sq.imperv1000+sq.pol_shannon.yj+sq.temp:sq.lux+sq.temp:sq.imperv1000+sq.lux:sq.imperv1000
# Adj-R2: 0.8735; p: 0.0063

# Plot how predictor 'sq.imperv1000' is related to response var
plot_model(mmd.lm.inter.trans, type="pred", terms='sq.imperv1000', show.data=T, line.size=1.3)

# Plot how predictor 'sq.pol_shannon.yj' isrelated to response var
plot_model(mmd.lm.inter.trans, type="pred", terms='sq.pol_shannon.yj', show.data=T, line.size=1.3)


# Plot how 'temp * imperv1000' is related to the fitted values of the response var
p_mmd.inter.trans_temp.im1000 <- plot_model(mmd.lm.inter.trans, type="pred", line.size=1.3,
                                 terms = c("sq.temp", "sq.imperv1000")) #  [1.58, 14.04]
p_mmd.inter.trans_im1000.temp <- plot_model(mmd.lm.inter.trans, type="pred", line.size=1.3,
                                 terms=c("sq.imperv1000", "sq.temp")) #  [0.31, 0.6]

gridExtra::grid.arrange(p_mmd.inter.trans_temp.im1000, p_mmd.inter.trans_im1000.temp, ncol=2)


# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------


# ------------- Best lm() model for: mass_meanopen -----------------------------

# ---- Create new dataframe, without "non-related" vars ----
FA_mmo <- FA_data %>%
  dplyr::select(-c("mass_meandiff", "ratio_meandiff", "ratio_meanopen"))


# ---- Check correlation of dependent and independent vars again ----
mmo_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
mmo_corr <- FA_mmo[,mmo_vars]
chart.Correlation(mmo_corr, histogram=TRUE)


# ---- Create initial multiple regression lm() model ----
mmo.lm0 <- lm(mass_meanopen ~ temp + lux + imperv1000 + 
                pol_shannon.yj + # pol_abundance.yj + pol_richness +    
                flo_abundance.yj + flo_shannon, data=FA_mmo)
summ(mmo.lm0) # Adj-R2: 0.37; p: 0.19


# ---- Create initial model with stepAIC() ----
mmo.lm.init <- MASS::stepAIC(mmo.lm0, direction="both", trace=F)
summ(mmo.lm.init) # Adj-R2: 0.48 ; p: 0.031

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


# ---- Create initial (transformed) model ----

# Add transformed variables (squared)
FA_mmo <- FA_mmo %>%
  mutate(sq.temp = temp^2, 
         sq.imperv1000 = imperv1000^2,
         sq.pol_shannon.yj = pol_shannon.yj^2)

# Create transformed model
mmo.lm.init.trans <- lm(mass_meanopen ~ sq.temp + sq.imperv1000 + 
                        sq.pol_shannon.yj, data=FA_mmo)
summ(mmo.lm.init.trans, digits=4) # Adj-R2: 0.562; p: 0.0147

# Find best model with stepAIC()
mmo.lm.init.trans <- stepAIC(mmo.lm.init.trans, direction="both", trace=F)
summ(mmo.lm.init.trans, digits=4) # Adj-R2: 0.562; p: 0.0147

# => The same model

# Check model$call
mmo.lm.init.trans$call # mass_meandiff ~ sq.temp + sq.imperv1000 + sq.pol_shannon.yj

# Check for multi-collinerity: For all vars, less than 3 is good
vif(mmo.lm.init.trans) %>% 
  knitr::kable() # All < 3: Pass

# Check CERES plot
ceresPlots(mmo.lm.init.trans) # => Not too straight but okay

# Check residual plot
residualPlots(mmo.lm.init.trans, type = "rstandard") # => Not too straight

# Check if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmo.lm.init.trans))
qqline(residuals(mmo.lm.init.trans))

# Shapiro test
shapiro.test(residuals(mmo.lm.init.trans)) # p: 0.2533 --> Residuals are norm-dist

# Transformed model: mass_meandiff ~ sq.temp + sq.imperv1000 + sq.pol_shannon.yj
summ(mmo.lm.init.trans, digits= 4) # Adj-R2: 0.562; p: 0.0147


# ---- Create interaction (transformed) model ----

# Add interaction to transformed model
mmo.lm.inter.trans <- stepAIC(mmo.lm.init.trans, ~.^2, trace=F)
summ(mmo.lm.inter.trans, digits=4) # Adj-R2: 0.8582; p: 0.0032

# Check residual plot
residualPlots(mmo.lm.inter.trans, type = "rstandard") # => curve line

# Check if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmo.lm.inter.trans))
qqline(residuals(mmo.lm.inter.trans))

# Shapiro test
shapiro.test(residuals(mmo.lm.inter.trans)) # p: 0.7445 --> Residuals are norm-dist

# Interaction model: mass_meanopen ~ sq.temp + sq.imperv1000 + sq.pol_shannon.yj + sq.imperv1000:sq.pol_shannon.yj + sq.temp:sq.pol_shannon.yj + sq.temp:sq.imperv1000
summ(mmo.lm.inter.trans, digits= 4) # Adj-R2: 0.8582; p: 0.0032


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: mass_meanopen ~ temp + imperv1000 + pol_shannon.yj
summ(mmo.lm.init, digits= 4) # Adj-R2: 0.479 ; p: 0.031

# Initial (Transformed) model: mass_meanopen ~ sq.temp + sq.imperv1000 + sq.pol_shannon.yj
summ(mmo.lm.init.trans, digits= 4) # Adj-R2: 0.56; p: 0.0147

# Interaction model: mass_meanopen ~ temp + imperv1000 * pol_shannon.yj
summ(mmo.lm.inter, digits= 4) # Adj-R2: 0.7785; p: 0.0021

# Interaction (Transformed) model: mass_meanopen ~ sq.temp + sq.imperv1000 + sq.pol_shannon.yj + sq.imperv1000:sq.pol_shannon.yj + sq.temp:sq.pol_shannon.yj + sq.temp:sq.imperv1000
summ(mmo.lm.inter.trans, digits= 4) # Adj-R2: 0.8582; p: 0.0032

# Anova testing between models
anova(mmo.lm.init.trans, mmo.lm.inter.trans, test="F") # p: 0.02 => Improved
anova(mmo.lm.init, mmo.lm.inter, test="F") # p: 0.00067 => Much improved


# ---- Testing/Checking linear assumptions for best model(s) ----

# Constant variance (homoscedasticity) of errors testing: > 0.05 is pass --> Constant variance of errors
ncvTest(mmo.lm.inter) # p: 0.45 --> Pass 
ncvTest(mmo.lm.inter.trans) # p: 0.96 --> Pass 

# Auto correlated Errors test: p-value more than 0.05 is good
# Null: no correlation between the residuals --> the residuals are independent.
set.seed(1)
durbinWatsonTest(mmo.lm.inter) # p: 0.7 --> Pass
durbinWatsonTest(mmo.lm.inter.trans) # p: 0.77 --> Pass


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

# Initial (Transformed) model
mmo_init.trans_fitval <- predict(mmo.lm.init.trans, FA_mmo, interval = "confidence") %>%
  data.frame()
mmo_g3 <- fitted_vs_actual(mmo_init.trans_fitval, FA_mmo$mass_meanopen,  
                           "mass_meanopen - Initial (Transformed) Model")

# Interaction (Transformed) model
mmo_inter.trans_fitval <- predict(mmo.lm.inter.trans, FA_mmo, interval = "confidence") %>%
  data.frame()
mmo_g4 <- fitted_vs_actual(mmo_inter.trans_fitval, FA_mmo$mass_meanopen,  
                           "mass_meanopen - Interaction (Transformed) Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(mmo_g1, mmo_g2, mmo_g3, mmo_g4, ncol = 4)


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


# ---- Interaction (Transformed) model ----

# Estimated coefficients of predictors and their confidence intervals
summ(mmo.lm.inter.trans, confint=T, digits=4, ci.width = .95, center=T, pvals=T)
# Call: mass_meanopen~sq.temp+sq.imperv1000+sq.pol_shannon.yj+sq.imperv1000:sq.pol_shannon.yj+sq.temp:sq.pol_shannon.yj+sq.temp:sq.imperv1000
# Adj-R2: 0.8582; p: 0.0032

# Plot how predictor 'sq.temp' is related to response var
plot_model(mmo.lm.inter.trans, type="pred", terms='sq.temp', show.data=T, line.size=1.3)

# Plot how predictor 'sq.imperv1000' is related to response var
plot_model(mmo.lm.inter.trans, type="pred", terms='sq.imperv1000', show.data=T, line.size=1.3)

# Plot how predictor 'sq.pol_shannon.yj' isrelated to response var
plot_model(mmo.lm.inter.trans, type="pred", terms='sq.pol_shannon.yj', show.data=T, line.size=1.3)


# Plot how 'imperv1000 * pol_shannon.yj' is related to the fitted values of the response var
p_mmo.inter.trans_imp.pol <- plot_model(mmo.lm.inter.trans, type="pred", line.size=1.3,
                             terms = c("sq.imperv1000", "sq.pol_shannon.yj")) # [1.58, 14.04]
p_mmo.inter.trans_pol.imp <- plot_model(mmo.lm.inter.trans, type="pred", line.size=1.3,
                             terms=c("sq.pol_shannon.yj", "sq.imperv1000")) # [0.31, 0.6]

gridExtra::grid.arrange(p_mmo.inter.trans_imp.pol, p_mmo.inter.trans_pol.imp, ncol=2)


# ------------------------------------------------------------------------------


# -- Clean-up environment for the next script ----
rm(list=ls())

