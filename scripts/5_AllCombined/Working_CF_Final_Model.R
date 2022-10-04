# ---- Prerequisite procedures ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car",
                   "jtools", "PerformanceAnalytics", "sjPlot", "ggpubr")
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

# -- RespVar: seedmass_meanopen ----

# -- Create new dataframe, which remove "non-related" vars ----
CF_smmo <- CF_data %>%
  dplyr::select(-c("fruimass_meanopen.yj"))


# --------------- seedmass_meanopen & transformed predictor vars ---------------

# -- Check correlation of dependent and independent vars again ----
smmo.t_vars <- c(2,3,4,5,6,7,8,9,10,12,14,15,16)
smmo.t_corr <- CF_smmo[,smmo.t_vars]
chart.Correlation(smmo.t_corr, histogram=TRUE)


# -- Create initial multiple regression lm() model ----
smmo.t.lm0 <- lm(seedmass_meanopen ~ lux + imperv100 + temp + 
                 pol_abundance + pol_shannon.yj + flo_richness + # pol_richness + 
                 flo_abundance.yj + flo_shannon, data=CF_data)
summ(smmo.t.lm0, digits=4) # Adj-R2: 0.8315; p: 0.0282


# ---- Create initial model with stepAIC() ----
smmo.t.lm.init <- MASS::stepAIC(smmo.t.lm0, direction="both", trace=F)
summ(smmo.t.lm.init, digits= 4) # Adj-R2: 0.8845; p: 0.0006
pred_r_squared(smmo.t.lm.init) # Pr-R2: 0.7104604

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


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
smmo.t_init_fitval <- predict(smmo.t.lm.init, CF_smmo, interval="confidence") %>%
  data.frame() 
smmo.t_g1 <- fitted_vs_actual(smmo.t_init_fitval, CF_smmo$seedmass_meanopen, 
                              "seedmass_meanopen - Initial Model")


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: ~ temp + lux + pol_abundance + pol_shannon + flo_richness + flo_shannon
summ(smmo.t.lm.init, digits=4) # Adj-R2: 0.88; p: 0.0019


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Initial model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(smmo.t.lm.init, confint=T, digits=4, ci.width=.95, center=T, pvals=T)
# Call: ~ temp + lux + pol_abundance + pol_shannon.yj + flo_richness
# Adj-R2: 0.8845; p: 0.0006


# Plot how predictor 'lux' is related to response var
plot_lux <-
  plot_model(smmo.t.lm.init, type="pred", terms='lux', 
             show.data=T, line.size=1.2, title="Light intensity",
  axis.title=c("light intensity [lx]", "predicted values | seed mass [g]")) + 
  theme(text=element_text(size=8))  

# Plot how predictor 'temp' is related to response var
plot_temp <-
  plot_model(smmo.t.lm.init, type="pred", terms='temp', 
             show.data=T, line.size=1.2, title="Temperature",
  axis.title=c("temperature [°C]", "predicted values | seed mass [g]")) + 
  theme(text=element_text(size=8))  

# Plot how predictor 'pol_shannon.yj' is related to response var
plot_polsha.yj <-
  plot_model(smmo.t.lm.init, type="pred", terms='pol_shannon.yj', 
             show.data=T, line.size=1.2, title="Pollinator shannon index",
  axis.title=c("pollinator shannon index", "predicted values | seed mass [g]")) + 
  theme(text=element_text(size=8))  

# Plot how predictor 'flo_richness' is related to response var
plot_floric <-
  plot_model(smmo.t.lm.init, type="pred", terms='flo_richness', 
             show.data=T, line.size=1.2, title="Floral richness",
  axis.title=c("floral richness", "predicted values | seed mass [g]")) + 
  theme(text=element_text(size=8))  


# ------------------------------------------------------------------------------
# Create a combined plot of the best predictor variables
combined_plot1 <-
  ggarrange(plot_temp, plot_lux, plot_polsha.yj, plot_floric 
            + rremove("x.text"), ncol = 4,
            labels = c("A", "B", "C", "D"), font.label=list(size=12))

annotate_figure(combined_plot1, 
  top = text_grob("Capsicum frutescens | Multiple regression model | Seed mass of open flowers\n",
  color="#D55E00", face="bold", size=12, lineheight=1))


# ------------------------------------------------------------------------------
# Plot correlation between 'imperv100' and 'temp'
plot_imp100vstemp <-
  ggscatter(CF_smmo, x="imperv100", y="temp", 
    add="reg.line", conf.int=T,
    cor.coef=T, cor.method="pearson",
    #="Capsicum frutescens | Temperature vs Imperviousness 100m buffer",
    xlab ="imperviousness 100m buffer [%]", 
    ylab="temperature [°C]")

annotate_figure(plot_imp100vstemp, 
    top = text_grob("Capsicum frutescens | Temperature vs Imperviousness 100m buffer",
                    color="#D55E00", face="bold", size=12, lineheight=5))


# ------------------------------------------------------------------------------


# ---- Clean-up environment for the next script ----
rm(list=ls())

