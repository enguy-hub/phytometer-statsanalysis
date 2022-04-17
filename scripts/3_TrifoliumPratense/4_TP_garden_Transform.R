# -- Prerequisite ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "openxlsx", "car", "VGAM")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Hien/Garden/MyGithub/Phytometer_StatisticalAnalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
tp_path = "./analysis_data/TP/TP_Data_2021_4analysis_garden.xlsx"
TP_data <- read_excel(tp_path, sheet = 1)


# Check structure and summaries of the datasets
str(TP_data)
summary(TP_data)


# ------------------------------------------------------------------------------


# Preliminary normality tests
shapiro.test(TP_data$flowmass_meandiff) # p = 0.7869
shapiro.test(TP_data$flowmass_meanopen) # p = 0.3643
shapiro.test(TP_data$seedmass_meandiff) # p = 0.787
shapiro.test(TP_data$seedmass_meanopen) # p = 0.8173
shapiro.test(TP_data$mass_pseed_meandiff) # p = 0.1763
shapiro.test(TP_data$mass_pseed_meanopen) # p = 0.3094

shapiro.test(TP_data$temp) # p = 0.2837
shapiro.test(TP_data$lux) # p = 0.7345
shapiro.test(TP_data$imperv100) # p: 0.2312
shapiro.test(TP_data$imperv200) # p: 0.559
shapiro.test(TP_data$imperv500) # p: 0.3629
shapiro.test(TP_data$imperv1000) # p: 0.6755

shapiro.test(TP_data$pol_abundance) # p = 0.07697 => Try transform
shapiro.test(TP_data$pol_richness) # p = 0.1642
shapiro.test(TP_data$pol_shannon) # p = 0.9729

shapiro.test(TP_data$flo_abundance) # p = 6.747e-06 => Try transform
shapiro.test(TP_data$flo_richness) # p = 0.7406
shapiro.test(TP_data$flo_shannon) # p = 0.009473 => Try transform


# ------------------------------------------------------------------------------


# -- Yeo-Johnson Transform - pol_abundance ----
head(TP_data$pol_abundance, 20) 

shapiro.test(TP_data$pol_abundance) # old p: 0.07697
hist(TP_data$pol_abundance)

pa.lm <- lm(TP_data$pol_abundance ~ 1)
pa.yj = boxCox(pa.lm,family = "yjPower",plotit = T, lambda = seq(-2, 2))

pa.lamb <- pa.yj$x[which.max(pa.yj$y)] # -0.02 => use -0.02

shapiro.test(yeo.johnson(TP_data$pol_abundance, -0.02)) # new p: 0.7197

TP_data <- TP_data %>%
  mutate(pol_abundance.yj = yeo.johnson(TP_data$pol_abundance, -0.02), .after = "pol_abundance")

hist(TP_data$pol_abundance.yj) # Looks better
shapiro.test(TP_data$pol_abundance.yj) # p: 0.7197


# -- Yeo-Johnson Transform - flo_abundance ----
head(TP_data$flo_abundance, 20)

shapiro.test(TP_data$flo_abundance) # old p: 6.747e-06
hist(TP_data$flo_abundance)

va.lm <- lm(TP_data$flo_abundance ~ 1)
va.yj = boxCox(va.lm,family = "yjPower",plotit = T, lambda = seq(-2, 2))

va.lamb <- va.yj$x[which.max(va.yj$y)] # -0.22 => use -0.2

shapiro.test(yeo.johnson(TP_data$flo_abundance, -0.22)) # new p: 0.9927

TP_data <- TP_data %>%
  mutate(flo_abundance.yj = yeo.johnson(TP_data$flo_abundance, -0.22), .after = "flo_abundance")

hist(TP_data$flo_abundance.yj) # Looks better
shapiro.test(TP_data$flo_abundance.yj) # p: 0.9927


# -- Yeo-Johnson Transform - flo_shannon ----
# --> Can't transform for some reason 

head(TP_data$flo_shannon, 20)

hist(TP_data$flo_shannon)
shapiro.test(TP_data$flo_shannon) # old p: 0.0094


# ---------------------------------------------------------------------------- #


# -- Export new TP_data dataframe ----
tp_data_transformed_export <- paste("./analysis_data/TP/TP_Data_2021_4analysis_garden_transformed.xlsx", sep = "")
write.xlsx(TP_data, tp_data_transformed_export, append = TRUE)


# -- Clean-up environment for the next script ----
rm(list=ls())
