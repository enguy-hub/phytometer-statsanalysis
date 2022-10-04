# -- Prerequisites ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", 
                   "jtools", "PerformanceAnalytics", "sjPlot", "ggpubr")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
ra_path = "./analysis_data/RA/RA_Data_2021_4analysis_garden_transformed.xlsx"
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

# -- RespVar: avgdrymass_ferseed ----

# -- Create new dataframe, which remove "non-related" vars ----
RA_admfs <- RA_data %>%
  dplyr::select(-c("fremass_meandiff", "drymass_meandiff", 
                   "fremass_meanopen", "drymass_meanopen",
                   "avgfremass_pseed_meandiff", "avgdrymass_pseed_meandiff",
                   "avgfremass_pseed_meanopen", "avgdrymass_pseed_meanopen",
                   "avgfremass_ferseed"))


# ----------- avgdrymass_ferseed & non-transformed predictor vars --------------

# -- Check correlation of dependent and independent vars again ----
admfs_vars <- c(2,3,4,5,6,7,8,9,11,12,13,15,16)
admfs_corr <- RA_admfs[,admfs_vars]
chart.Correlation(admfs_corr, histogram=T)


# -- Create initial multiple regression lm() model ----
admfs.lm0 <- lm(avgdrymass_ferseed ~ imperv100 + #imperv1000 +
                lux + # temp + 
                pol_shannon + # flo_shannon +
                pol_richness + # flo_richness + 
                pol_abundance + flo_abundance, 
                data=RA_admfs)
summ(admfs.lm0, digits=4) # Adj-R2: 0.3937; p: 0.1264


# ---- Create initial model with stepAIC() ----
admfs.lm.init <- MASS::stepAIC(admfs.lm0, direction="both", trace=F)
summ(admfs.lm.init, digits=4, center=T) # Adj-R2: 0.5341; p: 0.035
pred_r_squared(admfs.lm.init)

# Check model$call
admfs.lm.init$call # ~ imperv100 + pol_abun + flo_ric + flo_abundance 

# Check for multi-collinerity: For all vars, less than 3 is good
vif(admfs.lm.init) %>% 
  knitr::kable() # Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(admfs.lm.init) # p: 0.0092 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(admfs.lm.init) # p: 0.874 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(admfs.lm.init)) # p: 0.03495 --> Residuals ARE NOT norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(admfs.lm.init))
qqline(residuals(admfs.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(admfs.lm.init, type="rstandard") # curve --> slight non-linearity

# Check CERES plot
ceresPlots(admfs.lm.init)

# Initial model: ~ imperv100 + pol_abundance + flo_abundance + flo_richness
summ(admfs.lm.init, digits=4) # Adj-R2: 0.46; p: 0.0361


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
admfs.lm.inter <- stepAIC(admfs.lm.init, ~.^2, trace=F)
summ(admfs.lm.inter,digits=4) # Adj-R2: 0.772; p: 0.0255
pred_r_squared(admfs.lm.init)

# Check model$call
admfs.lm.inter$call # ~ imperv100 * pol_abundance * flo_richness + flo_abundance

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(admfs.lm.inter) # p: 0.267 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(admfs.lm.inter) # p: 0.852 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(admfs.lm.inter)) # p: 0.57 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(admfs.lm.inter))
qqline(residuals(admfs.lm.inter))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(admfs.lm.inter, type = "rstandard") # curve --> slight non-linearity

# Initial model: avgdrymass_ferseed ~ imperv100 * pol_abundance * flo_richness + flo_abundance
summ(admfs.lm.inter, digits=4, center=T) # Adj-R2: 0.772; p: 0.0255


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

# Plot grid: old model vs new 'non-linear transformed' model
gridExtra::grid.arrange(admfs_g1,admfs_g2, ncol=2)


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: ~ imperv100 + pol_abundance + flo_abundance + flo_richness
summ(admfs.lm.init, digits=4, center=T) # Adj-R2: 0.5341; p: 0.0350

# Interaction model: ~ imperv100 * pol_abundance * flo_richness + flo_abundance
summ(admfs.lm.inter, digits=4, center=T) # Adj-R2: 0.7717; p: 0.0255

# Anova testing between models
anova(admfs.lm.init, admfs.lm.inter, test="F") # p: 2.2e-16 => Improved


# ---- Plotting the effect of 'significant' vars in the best model(s) ----

# ---- Interaction model ----

# Table with estimated coefficients of predictors and their confidence intervals
summ(admfs.lm.inter, confint=T, digits=4, ci.width=.95, center=T)
# Call: ~ imperv100 * pol_abundance * flo_richness + flo_abundance
# Adj-R2: 0.7717; p: 0.0255

# Plot how predictor 'imperv100' is related to response var
plot_imp100 <-
  plot_model(admfs.lm.inter, type="pred", terms='imperv100', 
             show.data=T, line.size=1.3, title="Imperviousness 100m buffer", 
    axis.title=c("imperviousness 100m buffer [%]", 
                 "predicted values | mass per fertile seed [g]")) +
    theme(text=element_text(size=9))

# Plot how predictor 'flo_abundance' is related to response var
plot_floabun <-
  plot_model(admfs.lm.inter, type="pred", terms='flo_abundance', 
             show.data=T, line.size=1.3, title="Floral abundance",
    axis.title=c("floral abundance", 
                 "predicted values | mass per fertile seed [g]")) +
    theme(text=element_text(size=9))


# ------------------------------------------------------------------------------
# Create a combined plot of the best predictor variables
combined_plot1 <-
  ggarrange(plot_imp100, plot_floabun + rremove("x.text"), 
            labels=c("A", "B"), ncol=2, font.label=list(size=12))

annotate_figure(combined_plot1, 
  top=text_grob("Ranunculus acris | Multiple regression model | Mass per fertile seed of open flowers\n",
  color="#D55E00", face="bold", size=12, lineheight=1))


# ------------------------------------------------------------------------------

# Plot how 'pol_abundance * flo_richness' is related to the fitted values of the response var
p_admfs.inter_polabu.floric <- 
  plot_model(admfs.lm.inter, type="pred", line.size=1.2, value.size=3, 
             title="Interaction term: Floral richness",
             terms=c("pol_abundance", "flo_richness"), # [1.16, 1.72]
             legend.title="floral richness",
             axis.title=c("pollinator abundance", 
                          "fitted values | mass per fertile seed [g]")) +
  theme(legend.position="top",
        text=element_text(size=9))  

p_admfs.inter_floric.polabu <- 
  plot_model(admfs.lm.inter, type="pred", line.size=1.2, value.size=3, 
             title="Interaction term: Pollinator abundance",
             terms=c("flo_richness", "pol_abundance"), # [0.52, 0.64]
             legend.title="pollinator abundance",
             axis.title=c("floral richness", 
                          "fitted values | mass per fertile seed [g]")) +
  theme(legend.position="top",
        text=element_text(size=9))    


# ------------------------------------------------------------------------------
# Create a combined plot of the best predictor variables
combined_plot2 <-
  ggarrange(p_admfs.inter_polabu.floric, p_admfs.inter_floric.polabu + rremove("x.text"), 
            labels=c("C", "D"), ncol=2, font.label=list(size=12))

annotate_figure(combined_plot2, 
  top = text_grob("Ranunculus acris | Multiple regression model | Mass per fertile seed of open flowers\n",
  color="#D55E00", face="bold", size=12, lineheight=1))


# ------------------------------------------------------------------------------

# -- Clean-up environment for the next script ----
rm(list=ls())
