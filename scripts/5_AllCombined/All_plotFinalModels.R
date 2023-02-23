# ------------------------------------------------------------------------------
# -------------------------- Prerequisites -------------------------------------
# ------------------------------------------------------------------------------

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "MASS", "car", "jtools", 
                   "PerformanceAnalytics", "sjPlot", "ggpubr")
lapply(list_packages, library, character.only = TRUE)

# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
setwd(pdir)



# ------------------------------------------------------------------------------
# ------------------------- Functions corner -----------------------------------
# ------------------------------------------------------------------------------

# ----- Function: PRESS - predicted residual sums of squares -----
PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}

# ----- Function: pred_r_squared -----
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
# --------------------- F. ananassa | Plots & figures --------------------------
# ------------------------------------------------------------------------------

# Set path and read the data
fa_path = "./analysis_data/FA/FA_Data_2021_4analysis_garden_transformed.xlsx"
FA_data <- read_excel(fa_path, sheet = 1)

# Check structure and summaries of the data
str(FA_data)
summary(FA_data)

# Remove "Non-normal distributed" variables
FA_data <- FA_data %>%
  dplyr::select(-c("urbanclass100", "urbanclass200", 
                   "urbanclass500", "urbanclass1000"))

# Create "FA_mmo" dataframe, remove "non-related" vars
FA_mmo <- FA_data %>%
  dplyr::select(-c("mass_meandiff", "ratio_meandiff", "ratio_meanopen",
                   "imperv200", "imperv500"))

# Check correlation of dependent and independent vars again
mmo.t._vars <- c(2,3,4,5,6,8,9,11,13,15,16)
mmo.t._corr <- FA_mmo[,mmo.t._vars]
chart.Correlation(mmo.t._corr, histogram=TRUE)


# ----- Create models -----

# Create initial "full" multiple regression lm() model
mmo.t.lm0 <- lm(mass_meanopen ~ imperv1000 + # imperv100 +
                  temp + # lux +  
                  pol_richness + flo_richness.yj +
                  pol_shannon.yj + flo_shannon + 
                  pol_abundance.yj  + flo_abundance.yj, 
                data=FA_mmo)
summ(mmo.t.lm0) # Adj-R2: 0.03; p: 0.52

# Create step-wise with stepAIC() 
mmo.t.lm.init <- MASS::stepAIC(mmo.t.lm0, direction="both", trace=F)
summ(mmo.t.lm.init, digits= 4) # Adj-R2: 0.4792; p: 0.031
pred_r_squared(mmo.t.lm.init) # Pr-R2: -0.01666

# Check model$call
mmo.t.lm.init$call # ~ temp + imp1000 + pol_shannon.yj

# Create interaction step-wise model using stepAIC()
mmo.t.lm.inter <- stepAIC(mmo.t.lm.init, ~.^2, trace=F)
summ(mmo.t.lm.inter,digits=4) # Adj-R2: 0.7785; p: 0.0021
pred_r_squared(mmo.t.lm.inter) # Pr-R2: 0.4768561

# Check model$call
mmo.t.lm.inter$call # ~ temp + imp1000 * pol_shannon.yj


# ----- Plot predictor vars' relationship -----

# Estimated coefficients of the predictors and their confidence intervals
summ(mmo.t.lm.inter, digits=4, center=T, pvals=T, vifs=T) # , confint=T, ci.width=.95
# Call: mass_meanopen ~ temp + imperv1000 * pol_shannon.yj
# Adj-R2: 0.7785; p: 0.0021

# Plot how predictor 'temp' is related to response var
plot_temp <-
  plot_model(mmo.t.lm.inter, type="pred", terms='temp', 
             show.data=T, line.size=1.2, title="", 
             axis.title=c("temperature [°C]", 
                          "predicted values | fruit mass [g]")) + 
  theme_classic() +
  theme(plot.margin = margin(1, 0, 1, 0, "cm"),
        text=element_text(size=18))

# Plot how predictor 'imperv1000' is related to response var
plot_imp1000 <- 
  plot_model(mmo.t.lm.inter, type="pred", terms='imperv1000', 
             show.data=T, line.size=1.2, title="",
             axis.title=c("landscape imperviousness 1000m [%]", 
                          "predicted values | fruit mass [g]")) + 
  theme_classic() +
  theme(plot.margin = margin(1, 0, 1, 0, "cm"),
        text=element_text(size=18))

# Plot how predictor 'pol_shannon.yj' is related to response var
plot_polsha.yj <-
  plot_model(mmo.t.lm.inter, type="pred", terms='pol_shannon.yj', 
             show.data=T, line.size=1.2, title="",
             axis.title=c("yj (pollinator diversity) [shannon]", 
                          "predicted values | fruit mass [g]")) + 
  theme_classic() +
  theme(plot.margin = margin(1, 0, 1, 0, "cm"),
        text=element_text(size=18))

# Create a combined plot
combined_plot1 <-
  ggarrange(plot_temp, plot_polsha.yj, plot_imp1000, ncol=3, 
            labels=list("A", "B", "C"), font.label=list(size=16), vjust=4, hjust=-2)

annotate_figure(combined_plot1, 
                top=text_grob("Fragaria x ananassa", color="#D55E00", 
                              face="italic", size=24, vjust=1))



# ------------------------------------------------------------------------------
# -------------------- C. frutescens | Plots & figures -------------------------
# ------------------------------------------------------------------------------

# Set path and read the data
cf_path = "./analysis_data/CF/CF_Data_2021_4analysis_garden_transformed.xlsx"
CF_data <- read_excel(cf_path, sheet = 1)

# Check structure and summaries of the data sets
str(CF_data)
summary(CF_data)

# Remove "Non-normal distributed" variables
CF_data <- CF_data %>%
  dplyr::select(-c("urbanclass100", "urbanclass200", 
                   "urbanclass500", "urbanclass1000",
                   "fruimass_meandiff", "fruimass_meandiff.yj", 
                   "fruimass_meanopen", "seedmass_meandiff", 
                   "ratio_meandiff", "ratio_meanopen", 
                   "ratio_meanopen.yj", "mass_pseed_meanopen"))

# Create new dataframe, which remove "non-related" vars
CF_smmo <- CF_data %>%
  dplyr::select(-c("fruimass_meanopen.yj", "imperv200", "imperv500"))

# Check correlation of dependent and independent vars again
smmo.t_vars <- c(2,3,4,5,6,7,8,10,12,13,14)
smmo.t_corr <- CF_smmo[,smmo.t_vars]
chart.Correlation(smmo.t_corr, histogram=TRUE)


# ----- Create models -----

# Create initial "full" multiple regression lm() model
smmo.t.lm0 <- lm(seedmass_meanopen ~ imperv100 + 
                   lux + temp + 
                   pol_abundance + flo_abundance.yj + 
                   pol_shannon.yj + flo_shannon + 
                   flo_richness # + pol_richness
                 ,data=CF_data)
summ(smmo.t.lm0, digits=4) # Adj-R2: 0.8315; p: 0.0282

# Create step-wise with stepAIC() 
smmo.t.lm.init <- MASS::stepAIC(smmo.t.lm0, direction="both", trace=F)
summ(smmo.t.lm.init, digits= 4) # Adj-R2: 0.8845; p: 0.0006
pred_r_squared(smmo.t.lm.init) # Pr-R2: 0.7104604

# Check model$call
smmo.t.lm.init$call # ~ lux + temp + pol_abundance + pol_shannon.yj + flo_richness

# Create interaction step-wise model using stepAIC()
smmo.t.lm.inter <- stepAIC(smmo.t.lm.init, ~.^2, trace=F)
summ(smmo.t.lm.inter,digits=4)

# Can't make interaction model as initial was already perfect fit model => STOP


# ----- Plot predictor vars' relationship -----

# Estimated coefficients of predictors and their confidence intervals
summ(smmo.t.lm.init, digits=4, center=T, pvals=T, vifs=T) # , confint=T, ci.width=.95
# Call: ~ temp + lux + pol_abundance + pol_shannon.yj + flo_richness
# Adj-R2: 0.8845; p: 0.0006

# Plot how predictor 'lux' is related to response var
plot_lux <-
  plot_model(smmo.t.lm.init, type="pred", terms='lux', 
             show.data=T, line.size=1.2, title="",
             axis.title=c("light intensity [lux]", 
                          "predicted values | seed mass [g]")) + 
  theme_classic() +
  theme(plot.margin = margin(2, 0, 2, 0, "cm"),
        text=element_text(size=18))

# Plot how predictor 'temp' is related to response var
plot_temp <-
  plot_model(smmo.t.lm.init, type="pred", terms='temp', 
             show.data=T, line.size=1.2, title="",
             axis.title=c("temperature [°C]", 
                          "predicted values | seed mass [g]")) + 
  theme_classic() +
  theme(plot.margin = margin(2, 0, 2, 0, "cm"),
        text=element_text(size=18))  

# Plot how predictor 'pol_shannon.yj' is related to response var
plot_polsha.yj <-
  plot_model(smmo.t.lm.init, type="pred", terms='pol_shannon.yj', 
             show.data=T, line.size=1.2, title="",
             axis.title=c("pollinator diversity [shannon]", 
                          "predicted values | seed mass [g]")) + 
  theme_classic() +
  theme(plot.margin = margin(2, 0, 2, 0, "cm"),
        text=element_text(size=18))  

# Plot how predictor 'flo_richness' is related to response var
plot_floric <-
  plot_model(smmo.t.lm.init, type="pred", terms='flo_richness', 
             show.data=T, line.size=1.2, title="",
             axis.title=c("floral richness [no. species]", 
                          "predicted values | seed mass [g]")) + 
  theme_classic() +
  theme(plot.margin = margin(2, 0, 2, 0, "cm"),
        text=element_text(size=18))  

# Create a combined plot
combined_plot1 <-
  ggarrange(plot_temp, plot_lux, plot_polsha.yj, plot_floric, 
            ncol = 4, labels=list("A", "B", "C", "D"), 
            font.label=list(size=16), vjust=6, hjust=-3)


annotate_figure(combined_plot1, 
                top=text_grob("Capsicum frutescens", color="#D55E00", 
                              face="italic", size=24, vjust=2))



# ------------------------------------------------------------------------------
# ----------------------- R. acris | Plots & figures ---------------------------
# ------------------------------------------------------------------------------

# Set path and read the data
ra_path = "./analysis_data/RA/RA_Data_2021_4analysis_garden_transformed.xlsx"
RA_data <- read_excel(ra_path, sheet = 1)

# Check structure and summaries of the data
str(RA_data)
summary(RA_data)

# Remove "Unnecessary" variables
RA_data <- RA_data %>%
  dplyr::select(-c("urbanclass100", "urbanclass200", "urbanclass500", "urbanclass1000"))

# Create new dataframe, which remove "non-related" vars
RA_admfs <- RA_data %>%
  dplyr::select(-c("imperv200", "imperv500",
                   "fremass_meandiff", "drymass_meandiff", 
                   "fremass_meanopen", "drymass_meanopen",
                   "avgfremass_pseed_meandiff", "avgdrymass_pseed_meandiff",
                   "avgfremass_pseed_meanopen", "avgdrymass_pseed_meanopen",
                   "avgfremass_ferseed"))

# Check correlation of dependent and independent vars again
admfs_vars <- c(2,3,4,5,6,7,9,10,11,13,14)
admfs_corr <- RA_admfs[,admfs_vars]
chart.Correlation(admfs_corr, histogram=T)


# ----- Create models -----

# Create initial "full" multiple regression lm() model
admfs.lm0 <- lm(avgdrymass_ferseed ~ imperv100 + # imperv1000 +
                  lux + # temp + 
                  pol_shannon + # flo_shannon +
                  flo_richness + # pol_richness + 
                  flo_abundance + pol_abundance
                ,data=RA_admfs)
summ(admfs.lm0, digits=4) # Adj-R2: 0.4375; p: 0.1392

# Create step-wise with stepAIC()
admfs.lm.init <- MASS::stepAIC(admfs.lm0, direction="both", trace=F)
summ(admfs.lm.init, digits=4, center=T) # Adj-R2: 0.5341; p: 0.035
pred_r_squared(admfs.lm.init) # Pre-R2: -2.60

# Check model$call
admfs.lm.init$call # ~ imperv100 + flo_ric + flo_abundance + pol_abun 

# Create interaction model (from initial model) using stepAIC()
admfs.lm.inter <- stepAIC(admfs.lm.init, ~.^2, trace=F)
summ(admfs.lm.inter,digits=4) # Adj-R2: 0.772; p: 0.0255
pred_r_squared(admfs.lm.inter) #Pre-R2: -1.90

# Check model$call
admfs.lm.inter$call # ~ imperv100 * pol_abundance * flo_richness + flo_abundance


# ----- Plot predictor vars' relationship -----

# Estimated coefficients of predictors and their confidence intervals
summ(admfs.lm.inter, digits=4, center=T, pvals=T, vifs=T) # , confint=T, ci.width=.95
# Call: ~ imperv100 * pol_abundance * flo_richness + flo_abundance
# Adj-R2: 0.7717; p: 0.0255

# Plot how predictor 'imperv100' is related to response var
plot_imp100 <-
  plot_model(admfs.lm.inter, type="pred", terms='imperv100', 
             show.data=T, line.size=1.3, title="", 
             axis.title=c("landscape imperviousness 100m [%]", 
                          "predicted values | mass per fertile seed [g]")) +
  theme_classic() +
  theme(plot.margin = margin(2, 1, 1, 2, "cm"),
        text=element_text(size=20))  

# Plot how predictor 'flo_abundance' is related to response var
plot_floabun <-
  plot_model(admfs.lm.inter, type="pred", terms='flo_abundance', 
             show.data=T, line.size=1.3, title="",
             axis.title=c("floral abundance [no. flowers]", 
                          "predicted values | mass per fertile seed [g]")) +
  theme_classic() +
  theme(plot.margin = margin(2, 1, 1, 2, "cm"),
        text=element_text(size=20))  

# Create a combined plot
combined_plot1 <-
  ggarrange(plot_imp100, plot_floabun, ncol=2, labels=list("A", "B"), 
            font.label=list(size=16), vjust=6, hjust=-8)

annotate_figure(combined_plot1, 
                top=text_grob("Ranunculus acris", color="#D55E00", 
                              face="italic", size=24, vjust=2))



# ------------------------------------------------------------------------------
# --------------------- T. pratense | Plots & figures --------------------------
# ------------------------------------------------------------------------------

# Set path and read the data
tp_path = "./analysis_data/TP/TP_Data_2021_4analysis_garden_transformed.xlsx"
TP_data <- read_excel(tp_path, sheet = 1)

# Check structure and summaries of the data sets
str(TP_data)
summary(TP_data)

# Remove "Non-normal distributed" variables
TP_data <- TP_data %>%
  dplyr::select(-c("urbanclass100", "urbanclass200", "urbanclass500", "urbanclass1000"))

# Create new dataframe, which remove "non-related" vars
TP_mpsmo <- TP_data %>%
  dplyr::select(-c("flowmass_meandiff", "flowmass_meanopen", 
                   "seedmass_meandiff", "seedmass_meanopen",
                   "mass_pseed_meandiff", "imperv200", "imperv500"))

# Check correlation of dependent and independent vars again
mpsmo.t_vars <- c(2,3,4,5,6,8,10,12,13,14)
mpsmo.t_corr <- TP_mpsmo[,mpsmo.t_vars]
chart.Correlation(mpsmo.t_corr, histogram=T)


# ----- Create models -----

# Create initial "full" multiple regression lm() model
mpsmo.t.lm0 <- lm(mass_pseed_meanopen ~ imperv1000 + # imperv100 +
                    lux + # temp +
                    flo_richness + pol_richness + 
                    pol_shannon + flo_shannon +
                    flo_abundance.yj + pol_abundance.yj
                  ,data=TP_mpsmo)
summ(mpsmo.t.lm0, digits=4) # Adj-R2: 0.38; p: 0.2765

# Create step-wise with stepAIC() 
mpsmo.t.lm.init <- MASS::stepAIC(mpsmo.t.lm0, direction="both", trace=F)
summ(mpsmo.t.lm.init, digits= 4) # Adj-R2: 0.5548; p: 0.0495
pred_r_squared(mpsmo.t.lm.init) # Pr-R2: 0.0489

# Check model$call
mpsmo.t.lm.init$call # ~ lux + flo_richness + pol_richness + flo_shannon + flo_abundance.yj

# Create interaction step-wise model using stepAIC()
mpsmo.t.lm.inter <- stepAIC(mpsmo.t.lm.init, ~.^2, trace=F)
summ(mpsmo.t.lm.inter,digits=4)

# Can't make interaction model as initial was already perfect fit model => STOP


# ----- Plot predictor vars' relationship -----

# Table with estimated coefficients of predictors and their confidence intervals
summ(mpsmo.t.lm.init, digits=4, center=T, pvals=T, vifs=T) # , confint=T, ci.width=.95
# Call: ~ lux + flo_richness + pol_richness + flo_abundance.yj + flo_shannon
# Adj-R2: 0.5548; p: 0.0495

# Plot how predictor 'pol_richness' is related to response var
plot_polrich <-
  plot_model(mpsmo.t.lm.init, type="pred", terms='pol_richness', 
             show.data=T, line.size=1.2, title="",
             axis.title=c("pollinator richness [no. species]", 
                          "predicted values | mass per seed [g]")) + 
  theme_classic() +
  theme(plot.margin = margin(2, 0, 2, 0, "cm"),
        text=element_text(size=18))  

# Plot how predictor 'flo_abundance.yj' is related to response var
plot_floabun.yj <-
  plot_model(mpsmo.t.lm.init, type="pred", terms='flo_abundance.yj', 
             show.data=T, line.size=1.2, title="",
             axis.title=c("yj (floral abundance) [no. flowers]", 
                          "predicted values | mass per seed [g]")) + 
  theme_classic() +
  theme(plot.margin = margin(2, 0, 2, 0, "cm"),
        text=element_text(size=18))  

# Plot how predictor 'flo_shannon' is related to response var
plot_flosha <-
  plot_model(mpsmo.t.lm.init, type="pred", terms='flo_shannon', 
             show.data=T, line.size=1.3, title="",
             axis.title=c("floral diversity [shannon]", 
                          "predicted values | mass per seed [g]")) + 
  theme_classic() +
  theme(plot.margin = margin(2, 0, 2, 0, "cm"),
        text=element_text(size=18)) 

# Create a combined plot
combined_plot1 <-
  ggarrange(plot_polrich, plot_flosha, plot_floabun.yj, ncol=3, 
            labels=list("A", "B", "C"), font.label=list(size=16), 
            vjust=6, hjust=-2)

annotate_figure(combined_plot1, 
                top=text_grob("Trifolium pratense", color="#D55E00", 
                              face="italic", size=24, vjust=2))



# ------------------------------------------------------------------------------
# --------------------- Temp vs 100m Imperv Plots ------------------------------
# ------------------------------------------------------------------------------

# FA | 'imperv100' and 'temp'
plotFA_imp100vstemp <-
  ggscatter(FA_mmo, x="imperv100", y="temp",
            xlab="landscape imperviousness 100m [%]", ylab="temperature [°C]",
            cor.method="pearson", cor.coef=T, cor.coef.size=5, 
            add="reg.line", conf.int=T, font.label=c(16, "plain")) +
  ggtitle("F. ananassa \n") +
  theme_classic() +
  theme(plot.margin = margin(0, 1, 0, 1, "cm"),
        plot.title = element_text(size=20, face="bold.italic", lineheight=1, vjust=-3),
        axis.title = element_text(size=18, lineheight=1))

# CF | 'imperv100' and 'temp'
plotCF_imp100vstemp <-
  ggscatter(CF_smmo, x="imperv100", y="temp",
            xlab="landscape imperviousness 100m [%]", ylab="temperature [°C]",
            cor.method="pearson", cor.coef=T, cor.coef.size=5, 
            add="reg.line", conf.int=T, font.label=c(16, "plain")) +
  ggtitle("C. frutescens \n") +
  theme_classic() +
  theme(plot.margin = margin(0, 1, 0, 1, "cm"),
        plot.title = element_text(size=20, face="bold.italic", lineheight=1, vjust=-3),
        axis.title = element_text(size=18, lineheight=1))

# RA | 'imperv100' and 'temp'
plotRA_imp100vstemp <-
  ggscatter(RA_admfs, x="imperv100", y="temp",
            xlab="landscape imperviousness 100m [%]", ylab="temperature [°C]",
            cor.method="pearson", cor.coef=T, cor.coef.size=5, 
            add="reg.line", conf.int=T, font.label=c(16, "plain")) +
  ggtitle("R. acris \n") +
  theme_classic() +
  theme(plot.margin = margin(0, 1, 0, 1, "cm"),
        plot.title = element_text(size=20, face="bold.italic", lineheight=1, vjust=-3),
        axis.title = element_text(size=18, lineheight=1))

# TP | 'imperv100' and 'temp'
plotTP_imp100vstemp <-
  ggscatter(TP_mpsmo, x="imperv100", y="temp",
            xlab="landscape imperviousness 100m [%]", ylab="temperature [°C]",
            cor.method="pearson", cor.coef=T, cor.coef.size=5, 
            add="reg.line", conf.int=T, font.label=c(16, "plain")) + 
  ggtitle("T. pratense \n") +
  theme_classic() +
  theme(plot.margin = margin(0, 1, 0, 1, "cm"),
        plot.title = element_text(size=20, face="bold.italic", lineheight=1, vjust=-3),
        axis.title = element_text(size=18, lineheight=1))


# Combined plot
tempvsimp100_plots <-
  ggarrange(plotFA_imp100vstemp, plotCF_imp100vstemp,
            plotRA_imp100vstemp, plotTP_imp100vstemp,
            nrow=2, ncol=2)


# Display combined plot
annotate_figure(tempvsimp100_plots,
                top=text_grob("Temperature vs Landscape Imperviousness 100m \n",
                color="#D55E00", face="bold", size=24, vjust=1))



# ------------------------------------------------------------------------------
# --------------------- Interaction Plots | FA & RA ----------------------------
# ------------------------------------------------------------------------------

# ----- FA | interaction relationship -----

# Plot 'imperv1000 * pol_shannon.yj' relation predicted values
p_mmo.t.inter_imp.pol <- 
  plot_model(mmo.t.lm.inter, type="pred", line.size=1.2, title="F. ananassa",
             terms=c("imperv1000", "pol_shannon.yj [1, 4]"),
             colors = c("orange", "blue"),
             legend.title="yj (pollinator diversity) [shannon index]",
             axis.title=c("landscape imperviousness 1000m [%]", 
                          "predicted values | fruit mass [g]")) +
  theme_classic() +
  theme(legend.position="top",
        plot.title = element_text(size=20, face="bold.italic", lineheight=1),
        text=element_text(size=18))


# ----- RA | Interaction relationship -----

# Plot 'pol_abundance * flo_richness' relation to response var's fitted values
p_admfs.inter_floric.polabu <- 
  plot_model(admfs.lm.inter, type="pred", line.size=1.2, title="R. acris",
             terms=c("flo_richness", "pol_abundance [20, 98]"),
             colors = c("orange", "blue"),
             legend.title="pollinator abundance [no. pollinators]",
             axis.title=c("floral richness [no. species]", 
                          "fitted values | mass per fertile seed [g]")) +
  theme_classic() +
  theme(legend.position="top",
        plot.title = element_text(size=20, face="bold.italic", lineheight=1),
        text=element_text(size=18))    


# Create a combined plot
combined_plot_inter <-
  ggarrange(p_mmo.t.inter_imp.pol, p_admfs.inter_floric.polabu, 
            ncol=2, labels=list("A", "B"), font.label=list(size=18))

annotate_figure(combined_plot_inter)


# ------------------------------------------------------------------------------

# ---- Clean-up environment for the next script ----
rm(list=ls())

