# -- Prerequisites ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", "jtools",
                   "PerformanceAnalytics", "sjPlot")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
fa_path = "./analysis_data/FA/FA_Data_2021_4analysis_garden.xlsx"
FA_data <- read_excel(fa_path, sheet = 1)


# Check structure and summaries of the data
str(FA_data)
summary(FA_data)


# Remove "Non-normal distributed" variables
FA_data <- FA_data %>%
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

# ------------------ Best model for: mass_meanopen -----------------------------

# -- Create new dataframe, which remove "non-related" vars ----
FA_mmo <- FA_data %>%
  dplyr::select(-c("mass_meandiff", "ratio_meandiff", "ratio_meanopen"))


# -- Check correlation of dependent and independent vars again ----
mmo_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14)
mmo_corr <- FA_mmo[,mmo_vars]
chart.Correlation(mmo_corr, histogram=TRUE)


# -- Create multiple regression lm() model ----
mmo.lm0 <- lm(mass_meanopen ~ temp + # lux + 
              imperv1000 + # imperv100 
              pol_richness + # pol_shannon + pol_abundance +
              flo_richness # + flo_shannon + flo_abundance
              , data=FA_mmo)
summ(mmo.lm0) # Adj-R2: 0.10; p: 0.44


# ---- Create initial model with stepAIC() ----
mmo.lm.init <- MASS::stepAIC(mmo.lm0, direction="both", trace=F)
summ(mmo.lm.init, digits= 4) # Adj-R2: 0.379; p: 0.098
pred_r_squared(mmo.lm.init)

# Check model$call
mmo.lm.init$call # ~ temp + imp1000 + pol_shannon + flo_shannon

# Check for multi-collinerity: For all vars, less than 3 is good
vif(mmo.lm.init) %>% 
  knitr::kable() # All < 3: Pass

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mmo.lm.init) # p: 0.32 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mmo.lm.init) # p: 0.16 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mmo.lm.init)) # p: 0.421 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmo.lm.init))
qqline(residuals(mmo.lm.init))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mmo.lm.init, type = "rstandard") # => slight non-linearity

# Check CERES plot
ceresPlots(mmo.lm.init)

# Initial model: ~ temp + imperv1000 + pol_shannon + flo_shannon
summ(mmo.lm.init, digits= 4) # Adj-R2: 0.379 ; p: 0.0981


# ---- Create interaction model ----

# Create interaction model (from initial model) using stepAIC()
mmo.lm.inter <- stepAIC(mmo.lm.init, ~.^2, trace=F)
summ(mmo.lm.inter,digits=4) # Adj-R2: 0.682; p: 0.0168 
pred_r_squared(mmo.lm.inter)

# Check model$call
mmo.lm.inter$call # ~ temp + imp1000 * pol_shannon + flo_shannon

# Test constant variance (homoscedasticity) of errors (> 0.05 = pass):
ncvTest(mmo.lm.inter) # p: 0.93 --> Pass

# Auto-correlated Errors test - H0: consecutive errors are not correlated (p > 0.05 = pass)
durbinWatsonTest(mmo.lm.inter) # p: 0.7 --> Consecutive errors are independent of each other

# Shapiro test
shapiro.test(residuals(mmo.lm.inter)) # p: 0.157 --> Residuals ARE norm-dist

# Check qqplot to see if residuals of fitted values of the model is normally distributed
qqnorm(residuals(mmo.lm.inter))
qqline(residuals(mmo.lm.inter))

# Check residual plot: Fitted values vs Residual (actual - fitted values)
residualPlots(mmo.lm.inter, type = "rstandard") # => slight non-linearity

# Interaction model: ~ temp + imp1000 * pol_shannon + flo_shannon
summ(mmo.lm.inter, digits= 4) # Adj-R2: 0.6824 ; p: 0.0168


# ---- Linear graphs to compare initial models against best model(s) ----

# Initial model
mmo_init_fitval <- predict(mmo.lm.init, FA_mmo, interval = "confidence") %>%
  data.frame() 
mmo_g1 <- fitted_vs_actual(mmo_init_fitval, FA_mmo$mass_meanopen,
                           "mass_meanopen - Initial Model")

# Interaction model
mmo_inter_fitval <- predict(mmo.lm.inter, FA_mmo, interval = "confidence") %>%
  data.frame()
mmo_g2 <- fitted_vs_actual(mmo_inter_fitval, FA_mmo$mass_meanopen,
                           "mass_meanopen - Interaction Model")


# ---- Compare Adj-R2, p-value, and ANOVA test of the models ----

# Initial model: ~ temp + imperv1000 + pol_shannon + flo_shannon
summ(mmo.lm.init, digits= 4) # Adj-R2: 0.379; p: 0.098

# Interaction model: ~ temp + imperv1000 * pol_shannon + flo_shannon
summ(mmo.lm.inter, digits= 4) # Adj-R2: 0.682; p: 0.0168

# Anova testing between models
anova(mmo.lm.init, mmo.lm.inter, test="F") # p: 0.0217 => Improved


# ---- Plotting the relationship of vars in the best model(s) ----

# ---- Interaction model ----

# Estimated coefficients of the predictors and their confidence intervals
summ(mmo.lm.inter, confint=T, digits=4, ci.width=.95, center=T, pvals=T)
# Call: mass_meanopen ~ temp + imperv1000 * pol_shannon + flo_shannon
# Adj-R2: 0.68; p: 0.0168

# Plot how predictor 'temp' is related to response var
plot_model(mmo.lm.inter, type="pred", terms='temp', show.data=T, line.size=1.3,
           title="Fragaria Fruit Mass vs Temperature",
           axis.title=c("temperature celsius", "fitted values | fruit mass of `open` flowers"))

# Plot how predictor 'imperv1000' is related to response var
plot_model(mmo.lm.inter, type="pred", terms='imperv1000', show.data=T, line.size=1.3,
           title="Fragaria Fruit Mass vs Impervious Surface",
           axis.title=c("impervious surface (%) of 1000m buffer", "fitted values | fruit mass of `open` flowers"))

# Plot how predictor 'pol_shannon' is related to response var
plot_model(mmo.lm.inter, type="pred", terms='pol_shannon', show.data=T, line.size=1.3,
           title="Fragaria Fruit Mass vs Pollinator Diversity Shannon Index",
           axis.title=c("pollinator diversity shannon index", "fitted values | fruit mass of `open` flowers"))


# Plot how 'imperv1000 * pol_shannon' is related to the fitted values of the response var
p_mmo.inter_imp.pol <- 
  plot_model(mmo.lm.inter, type="pred", line.size=1.3,
             terms=c("imperv1000", "pol_shannon"), # [1.16, 1.72]
             title="Fragaria Fruit Mass vs\nImpervious Surface\n(%) of 1000m Buffer
                    \nInteraction: Pollinator Diversity\nShannon Index",
             axis.title=c("impervious surface (%) of 1000m\nbuffer", "fitted values | fruit mass of `open` flowers"),
             legend.title="pollinator\ndiversity\nshannon\nindex")
p_mmo.inter_imp.pol

p_mmo.inter_pol.imp <- 
  plot_model(mmo.lm.inter, type="pred", line.size=1.3,
             terms=c("pol_shannon", "imperv1000"), # [0.52, 0.64]
             title="Fragaria Fruit Mass vs\nPollinator Diversity\nShannon Index
                    \nInteraction: Impervious\nSurface (%) of\n1000m Buffer",
             axis.title=c("pollinator diversity shannon index", "fitted values | fruit mass of `open` flowers"),
             legend.title="impervious\nsurface\n(%) of\n1000m buffer")    
p_mmo.inter_pol.imp

gridExtra::grid.arrange(p_mmo.inter_imp.pol, p_mmo.inter_pol.imp, ncol=2)


# ------------------------------------------------------------------------------


# -- Clean-up environment for the next script ----
rm(list=ls())
