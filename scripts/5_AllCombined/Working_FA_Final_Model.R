# ---- Prerequisites ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", 
                   "jtools", "PerformanceAnalytics", "sjPlot", "ggpubr")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
fa_path = "./analysis_data/FA/FA_Data_2021_4analysis_garden_transformed.xlsx"
FA_data <- read_excel(fa_path, sheet = 1)


# Check structure and summaries of the data
str(FA_data)
summary(FA_data)


# Remove "Non-normal distributed" variables
FA_data <- FA_data %>%
  dplyr::select(-c("urbanclass100", "urbanclass200", "urbanclass500", "urbanclass1000"))


# ------------------------------------------------------------------------------


# ---- Function: fitted_vs_actual ----
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

# -- RespVar: mass_meanopen ----

# ---- Create "FA_mmo" dataframe, remove "non-related" vars ----
FA_mmo <- FA_data %>%
  dplyr::select(-c("mass_meandiff", "ratio_meandiff", "ratio_meanopen"))


# -------------- mass_meanopen & transformed predictor vars --------------------

# -- Check correlation of dependent and independent vars again ----
mmo.t._vars <- c(2,3,4,5,6,7,8,10,11,13,15,17,18)
mmo.t._corr <- FA_mmo[,mmo.t._vars]
chart.Correlation(mmo.t._corr, histogram=TRUE)


# -- Create inital multiple regression lm() model ----
mmo.t.lm0 <- lm(mass_meanopen ~ imperv1000 + # imperv100 +
                temp + # lux +  
                pol_richness + flo_richness.yj +
                pol_shannon.yj + flo_shannon + 
                pol_abundance.yj  + flo_abundance.yj, 
                data=FA_mmo)
summ(mmo.t.lm0) # Adj-R2: -0.05; p: 0.59

# -----------------------------------

# ---- Create initial model with stepAIC() ----
mmo.t.lm.init <- MASS::stepAIC(mmo.t.lm0, direction="both", trace=F)
summ(mmo.t.lm.init, digits= 4) # Adj-R2: 0.4792; p: 0.031
pred_r_squared(mmo.t.lm.init)

# Check model$call
mmo.t.lm.init$call # ~ temp + imp1000 + pol_shannon.yj

# Check for multi-collinerity: For all vars, less than 3 is good
vif(mmo.t.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mmo.t.lm.init) # p: 0.983 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mmo.t.lm.init) # p: 0.212 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mmo.t.lm.init)) # p: 0.773 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmo.t.lm.init))
qqline(residuals(mmo.t.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mmo.t.lm.init, type = "rstandard") # => slight non-linearity

# Check CERES plot
ceresPlots(mmo.t.lm.init)

# Initial model: ~ temp + imperv1000 + pol_shannon.yj
summ(mmo.t.lm.init, digits= 4) # Adj-R2: 0.479; p: 0.031


# -----------------------------------

# ---- Create interaction model (from initial model) using stepAIC()
mmo.t.lm.inter <- stepAIC(mmo.t.lm.init, ~.^2, trace=F)
summ(mmo.t.lm.inter,digits=4) # Adj-R2: 0.7785; p: 0.0021
pred_r_squared(mmo.t.lm.inter) # Pr-R2: 0.4768561

# Check model$call
mmo.t.lm.inter$call # ~ temp + imp1000 * pol_shannon.yj

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mmo.t.lm.inter) # p: 0.448 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mmo.t.lm.inter) # p: 0.73 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mmo.t.lm.inter)) # p: 0.086 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmo.t.lm.inter))
qqline(residuals(mmo.t.lm.inter))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mmo.t.lm.inter, type = "rstandard") # => slight non-linearity

# Interaction model: ~ temp + imp1000 * pol_shannon.yj
summ(mmo.t.lm.inter, digits= 4) # Adj-R2: 0.7785; p: 0.0021


# -----------------------------------

# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
mmo.t_init_fitval <- predict(mmo.t.lm.init, FA_mmo, interval = "confidence") %>%
  data.frame() 
mmo.t_g1 <- fitted_vs_actual(mmo.t_init_fitval, FA_mmo$mass_meanopen,
                             "mass_meanopen - Initial Model")

# Interaction model
mmo.t_inter_fitval <- predict(mmo.t.lm.inter, FA_mmo, interval = "confidence") %>%
  data.frame()
mmo.t_g2 <- fitted_vs_actual(mmo.t_inter_fitval, FA_mmo$mass_meanopen,
                             "mass_meanopen - Interaction Model")

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(mmo.t_g1, mmo.t_g2, ncol=2)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: ~ temp + imperv1000 + pol_shannon.yj
summ(mmo.t.lm.init, center=T, digits= 4) # Adj-R2: 0.479; p: 0.031

# Interaction model: ~ temp + imperv1000 * pol_shannon.yj
summ(mmo.t.lm.inter, center=T, digits= 4) # Adj-R2: 0.7785; p: 0.0021

# Anova testing between models
anova(mmo.t.lm.init, mmo.t.lm.inter, test="F") # p: 0.0067 => Improved


# ---- Plotting the relationship of vars in the best model(s) ----

# ---- Interaction model ----

# Estimated coefficients of the predictors and their confidence intervals
summ(mmo.t.lm.inter, confint=T, digits=4, ci.width=.95, center=T, pvals=T)
# Call: mass_meanopen ~ temp + imperv1000 * pol_shannon.yj
# Adj-R2: 0.7785; p: 0.0021

# Plot how predictor 'temp' is related to response var
plot_temp <-
  plot_model(mmo.t.lm.inter, type="pred", terms='temp', 
             show.data=T, line.size=1.2, title="Temperature", 
             axis.title=c("temperature [°C]", 
                          "predicted values | fruit mass [g]")) + 
  theme(text=element_text(size=9))  

# Plot how predictor 'imperv1000' is related to response var
plot_imp1000 <- 
  plot_model(mmo.t.lm.inter, type="pred", terms='imperv1000', 
             show.data=T, line.size=1.2, title="Imperviousness 1000m buffer",
             axis.title=c("1000m buffer imperviousness [%]", 
                          "predicted values | fruit mass [g]")) + 
  theme(text=element_text(size=9))  

# Plot how predictor 'pol_shannon.yj' is related to response var
plot_polsha.yj <-
  plot_model(mmo.t.lm.inter, type="pred", terms='pol_shannon.yj', 
             show.data=T, line.size=1.2, title="yj (pollinator diversity) [shannon]",
             axis.title=c("yj (pollinator diversity) [shannon]", 
                          "predicted values | fruit mass [g]")) + 
  theme(text=element_text(size=9)) 


# ------------------------------------------------------------------------------
# Create a combined plot of the best predictor variables
combined_plot1 <-
  ggarrange(plot_temp, plot_polsha.yj, plot_imp1000 + rremove("x.text"), 
            labels=c("A", "B", "C"), ncol=3, font.label=list(size=12))

annotate_figure(combined_plot1, 
  top=text_grob("Fragaria x ananassa\n", color="#D55E00", 
                face="italic", size=12, lineheight=1))


# ------------------------------------------------------------------------------

# Plot relationship of 'imperv1000 * pol_shannon.yj' predicted values
p_mmo.t.inter_imp.pol <- 
  plot_model(mmo.t.lm.inter, type="pred", line.size=1.2, value.size=3, 
             title="Interaction: yj (pollinator diversity) [shannon]",
             terms=c("imperv1000", "pol_shannon.yj [1.2, 4.4]"), # 
             legend.title="yj (pollinator diversity) [shannon]",
             axis.title=c("1000m buffer imperviousness [%]", 
                          "predicted values | fruit mass [g]")) +
  theme(legend.position="top",
        text=element_text(size=9))

p_mmo.t.inter_pol.imp <- 
  plot_model(mmo.t.lm.inter, type="pred", line.size=1.2, value.size=1, 
             title="Interaction: 1000m buffer imperviousness",
             terms=c("pol_shannon.yj", "imperv1000 [0.52, 0.75]"), #  
             legend.title="1000m buffer imperviousness [%]", 
             axis.title=c("yj (pollinator diversity) [shannon]", 
                          "predicted values | fruit mass [g]")) +
  theme(legend.position="top",
        text=element_text(size=9))  


# ------------------------------------------------------------------------------
# Create a combined plot of the best predictor variables
combined_plot2 <-
  ggarrange(p_mmo.t.inter_imp.pol, p_mmo.t.inter_pol.imp + rremove("x.text"), 
            labels=c("D", "E"), ncol=2, font.label=list(size=12))

annotate_figure(combined_plot2, 
  top = text_grob("Fragaria x ananassa\n", color="#D55E00", 
                  face="italic", size=12, lineheight=1))


# ------------------------------------------------------------------------------


# -- Clean-up environment for the next script ----
rm(list=ls())
