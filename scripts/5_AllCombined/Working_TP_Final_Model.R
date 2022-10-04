# ---- Prerequisite procedures ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", 
                   "jtools", "PerformanceAnalytics", "sjPlot", "ggpubr")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Hien/Garden/MyGithub/phytometer-statsanalysis"
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
# 3/ If possible, perform anova() test the new model against the old model, 
#    with the following syntax:
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

# -- RespVar: mass_pseed_meanopen ----

# -- Create new dataframe, which remove "non-related" vars ----
TP_mpsmo <- TP_data %>%
  dplyr::select(-c("flowmass_meandiff", "flowmass_meanopen", 
                   "seedmass_meandiff", "seedmass_meanopen",
                   "mass_pseed_meandiff"))


# ------------- mass_pseed_meanopen & transformed predictor vars ---------------

# -- Check correlation of dependent and independent vars again ----
mpsmo.t_vars <- c(2,3,4,5,6,7,8,10,11,12,14,15,16)
mpsmo.t_corr <- TP_mpsmo[,mpsmo.t_vars]
chart.Correlation(mpsmo.t_corr, histogram=T)


# -- Create initial multiple regression lm() model ----
mpsmo.t.lm0 <- lm(mass_pseed_meanopen ~ imperv1000 + temp + # lux +    
                  flo_richness + pol_richness + flo_abundance.yj + pol_shannon + flo_shannon + 
                  pol_abundance.yj, data=TP_mpsmo)
summ(mpsmo.t.lm0, digits=4) # Adj-R2: 0.38; p: 0.2765


# ---- Create initial model with stepAIC() ---- 
mpsmo.t.lm.init <- MASS::stepAIC(mpsmo.t.lm0, direction="both", trace=F)
summ(mpsmo.t.lm.init, digits= 4) # Adj-R2: 0.5548; p: 0.0495
pred_r_squared(mpsmo.t.lm.init) # Pr-R2: 0.150

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


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
mpsmo.t_init_fitval <- predict(mpsmo.t.lm.init, TP_mpsmo, interval="confidence") %>%
  data.frame() 
mpsmo.t_g1 <- fitted_vs_actual(mpsmo.t_init_fitval, TP_mpsmo$mass_pseed_meanopen, 
                               "mass_pseed_meanopen - Initial Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(mpsmo.t_g1, ncol=1)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: ~ lux + flo_richness + pol_richness + flo_abundance.yj + flo_shannon
summ(mpsmo.t.lm.init, digits=4, center=T) # Adj-R2: 0.5548; p: 0.0495


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Initial model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(mpsmo.t.lm.init, confint=T, digits=4, ci.width=.95, center=T)
# Call: ~ lux + flo_richness + pol_richness + flo_abundance.yj + flo_shannon
# Adj-R2: 0.5548; p: 0.0495

# Plot how predictor 'pol_richness' is related to response var
plot_polrich <-
  plot_model(mpsmo.t.lm.init, type="pred", terms='pol_richness', 
             show.data=T, line.size=1.2, title="Pollinator richness",
  axis.title=c("pollinator richness", "predicted values | mass per seed [g]")) + 
  theme(text=element_text(size=9))  

# Plot how predictor 'flo_abundance.yj' is related to response var
plot_floabun.yj <-
  plot_model(mpsmo.t.lm.init, type="pred", terms='flo_abundance.yj', 
             show.data=T, line.size=1.2, title="Floral abundance (yj)",
  axis.title=c("floral abundance (yj)", "predicted values | mass per seed [g]")) + 
  theme(text=element_text(size=9))  

# Plot how predictor 'flo_shannon' is related to response var
plot_flosha <-
  plot_model(mpsmo.t.lm.init, type="pred", terms='flo_shannon', 
             show.data=T, line.size=1.3, title="Floral shannon index",
  axis.title=c("floral shannon index", "predicted values | mass per seed [g]")) + 
  theme(text=element_text(size=9))  


# ------------------------------------------------------------------------------
# Create a combined plot of the best predictor variables
combined_plot1 <-
  ggarrange(plot_polrich, plot_flosha, plot_floabun.yj
            + rremove("x.text"), ncol = 3,
            labels = c("A", "B", "C"), font.label=list(size=12))

annotate_figure(combined_plot1, 
  top = text_grob("Trifolium pratense | Multiple regression model | Mass per seed of open flowers\n",
  color="#D55E00", face="bold", size=12, lineheight=1))


# ------------------------------------------------------------------------------


# ---- Clean-up environment for the next script ----
rm(list=ls())

