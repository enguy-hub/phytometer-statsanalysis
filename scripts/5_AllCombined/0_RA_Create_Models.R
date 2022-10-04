# -- Prerequisites ----

# Set path and read the data
ra_norm_path = "./analysis_data/RA/RA_Data_2021_4analysis_garden.xlsx"
RA_norm_data <- read_excel(ra_norm_path, sheet = 1)

ra_trans_path = "./analysis_data/RA/RA_Data_2021_4analysis_garden_transformed.xlsx"
RA_trans_data <- read_excel(ra_trans_path, sheet = 1)


# Remove "Non-normal distributed" variables
RA_norm_data <- RA_norm_data %>%
  dplyr::select(-c("urbanclass100", "urbanclass200", "urbanclass500", "urbanclass1000"))

RA_trans_data <- RA_trans_data %>%
  dplyr::select(-c("pol_abundance", "flo_abundance", "urbanclass100", 
                   "urbanclass200", "urbanclass500", "urbanclass1000")) # 

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
# ----------------- Best NORM model for: avgdrymass_ferseed --------------------
# ------------------------------------------------------------------------------

# Create new dataframe, which remove "non-related" vars
RA_norm_admfs <- RA_norm_data %>%
  dplyr::select(-c("fremass_meandiff", "drymass_meandiff", 
                   "fremass_meanopen", "drymass_meanopen",
                   "avgfremass_pseed_meandiff", "avgdrymass_pseed_meandiff",
                   "avgfremass_pseed_meanopen", "avgdrymass_pseed_meanopen",
                   "avgfremass_ferseed"))

# Check correlation of dependent and independent vars again
admfs_norm_vars <- c(2,3,4,5,6,7,8,9,10,11,12,13,14)
admfs_norm_corr <- RA_norm_admfs[,admfs_norm_vars]
chart.Correlation(admfs_norm_corr, histogram=TRUE)

# ------------------------------------------------

# Model-0: Temp + Imperv1000 + pol_ric + flo_ric
admfs_norm.lm0 <- lm(avgdrymass_ferseed ~ temp + imperv1000 +
                       pol_richness + flo_richness
                     , data=RA_norm_admfs)
summ(admfs_norm.lm0, digits=4) # Adj-R2: 0.0015; p: 0.4589
pred_r_squared(admfs_norm.lm0) # -1.015

# Initial model
admfs_norm.lm0.init <- MASS::stepAIC(admfs_norm.lm0, direction = "both", trace = FALSE)
summ(admfs_norm.lm0.init, digits= 4) # Adj-R2: 0.1874; p: 0.0783
pred_r_squared(admfs_norm.lm0.init) # 0.0038

# !!! Interaction model
admfs_norm.lm0.inter <- stepAIC(admfs_norm.lm0, ~.^2, trace=F)
summ(admfs_norm.lm0.inter,digits=4) # Adj-R2: 0.3419; p: 0.12
pred_r_squared(admfs_norm.lm0.inter) # 0.1938

# ------------------------------------------------

# Model-1: Temp + Imperv1000 + pol_sha + flo_sha
admfs_norm.lm1 <- lm(avgdrymass_ferseed ~ temp + imperv1000 + 
                       pol_shannon + flo_shannon 
                     , data=RA_norm_admfs)
summ(admfs_norm.lm1, digits=4) # Adj-R2: -0.1307; p: 0.6408
pred_r_squared(admfs_norm.lm1) # -0.828

# Initial model
admfs_norm.lm1.init <- MASS::stepAIC(admfs_norm.lm1, direction = "both", trace = FALSE)
summ(admfs_norm.lm1.init, digits= 4) # Adj-R2: 0.1134; p: 0.1396
pred_r_squared(admfs_norm.lm1.init) # -0.138

# Interaction model
admfs_norm.lm1.inter <- stepAIC(admfs_norm.lm1, ~.^2, trace=F)
summ(admfs_norm.lm1.inter,digits=4) # Adj-R2: 0.1844; p: 0.1994
pred_r_squared(admfs_norm.lm1.inter) # 0.0432

# ------------------------------------------------

# Model-2: Lux + Imperv100 + pol_ric
admfs_norm.lm2 <- lm(avgdrymass_ferseed ~ lux + imperv100 + pol_richness
                     , data=RA_norm_admfs)
summ(admfs_norm.lm2, digits=4) # Adj-R2: 0.2588; p: 0.1356
pred_r_squared(admfs_norm.lm2) # -0.094

# Initial model
admfs_norm.lm2.init <- MASS::stepAIC(admfs_norm.lm2, direction = "both", trace = FALSE)
summ(admfs_norm.lm2.init, digits= 4) # Adj-R2: 0.3324; p: 0.0533
pred_r_squared(admfs_norm.lm2.init) # 0.087

# Interaction model
admfs_norm.lm2.inter <- stepAIC(admfs_norm.lm2, ~.^2, trace=F)
summ(admfs_norm.lm2.inter,digits=4) # Same as initial

# ------------------------------------------------

# Model-3: Lux + Imperv100 + pol_sha
admfs_norm.lm3 <- lm(avgdrymass_ferseed ~ lux + imperv100 + pol_shannon
                     , data=RA_norm_admfs)
summ(admfs_norm.lm3, digits=4) # Adj-R2: 0.2493; p: 0.1428
pred_r_squared(admfs_norm.lm3) # -0.176

# Initial model
admfs_norm.lm3.init <- MASS::stepAIC(admfs_norm.lm3, direction = "both", trace = FALSE)
summ(admfs_norm.lm3.init, digits= 4) # Adj-R2: 0.3185; p: 0.0591
pred_r_squared(admfs_norm.lm3.init) # 0.1107

# Interaction model
admfs_norm.lm3.inter <- stepAIC(admfs_norm.lm3, ~.^2, trace=F)
summ(admfs_norm.lm3.inter,digits=4) # Same as initial


# ------------------------------------------------------------------------------


# -- Clean-up environment for the next script ----
rm(list=ls())
