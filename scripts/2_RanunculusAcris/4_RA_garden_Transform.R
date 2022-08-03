# -- Prerequisite ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "openxlsx", "car", "VGAM")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
ra_path = "./analysis_data/RA/RA_Data_2021_4analysis_garden.xlsx"
RA_data <- read_excel(ra_path, sheet = 1)


# Check structure and summaries of the datasets
str(RA_data)
summary(RA_data)


# ------------------------------------------------------------------------------


# Preliminary normality tests
shapiro.test(RA_data$fremass_meandiff) # p = 0.4627
shapiro.test(RA_data$fremass_meanopen) # p = 0.3056
shapiro.test(RA_data$drymass_meandiff) # p = 0.6053
shapiro.test(RA_data$drymass_meanopen) # p = 0.7632

shapiro.test(RA_data$avgfremass_pseed_meandiff) # p = 0.3
shapiro.test(RA_data$avgfremass_pseed_meanopen) # p = 0.81
shapiro.test(RA_data$avgdrymass_pseed_meandiff) # p = 0.53
shapiro.test(RA_data$avgdrymass_pseed_meanopen) # p = 0.69

shapiro.test(RA_data$avgfremass_ferseed) # p = 0.99
shapiro.test(RA_data$avgdrymass_ferseed) # p = 0.93

shapiro.test(RA_data$temp) # p = 0.19
shapiro.test(RA_data$lux) # p = 0.44
shapiro.test(RA_data$imperv100) # p: 0.23
shapiro.test(RA_data$imperv200) # p: 0.56
shapiro.test(RA_data$imperv500) # p: 0.36
shapiro.test(RA_data$imperv1000) # p: 0.67

shapiro.test(RA_data$pol_abundance) # p = 0.077 => Try transform
shapiro.test(RA_data$pol_richness) # p = 0.16
shapiro.test(RA_data$pol_shannon) # p = 0.97

shapiro.test(RA_data$flo_abundance) # p = 6.747e-06 => Try transform
shapiro.test(RA_data$flo_richness) # p = 0.7406
shapiro.test(RA_data$flo_shannon) # p = 0.009473 => Try transform


# ------------------------------------------------------------------------------


# -- Yeo-Johnson Transform - pol_abundance ----
head(RA_data$pol_abundance, 20)

shapiro.test(RA_data$pol_abundance) # old p: 0.07697
hist(RA_data$pol_abundance)

pa.lm <- lm(RA_data$pol_abundance ~ 1)
pa.yj = boxCox(pa.lm,family = "yjPower",plotit = T, lambda = seq(-2, 2))

pa.lamb <- pa.yj$x[which.max(pa.yj$y)] # -0.02 => use -0.02

shapiro.test(yeo.johnson(RA_data$pol_abundance, -0.02)) # new p: 0.7197

RA_data <- RA_data %>%
  mutate(pol_abundance.yj = yeo.johnson(RA_data$pol_abundance, -0.02), .after = "pol_abundance")

hist(RA_data$pol_abundance.yj) # Looks better
shapiro.test(RA_data$pol_abundance.yj) # p: 0.7197


# -- Yeo-Johnson Transform - flo_abundance ----
head(RA_data$flo_abundance, 20)

shapiro.test(RA_data$flo_abundance) # old p: 6.747e-06
hist(RA_data$flo_abundance)

va.lm <- lm(RA_data$flo_abundance ~ 1)
va.yj = boxCox(va.lm,family = "yjPower",plotit = T, lambda = seq(-2, 2))

va.lamb <- va.yj$x[which.max(va.yj$y)] # -0.22 => use -0.22

shapiro.test(yeo.johnson(RA_data$flo_abundance, -0.22)) # new p: 0.992

RA_data <- RA_data %>%
  mutate(flo_abundance.yj = yeo.johnson(RA_data$flo_abundance, -0.22), .after = "flo_abundance")

hist(RA_data$flo_abundance.yj) # Looks better
shapiro.test(RA_data$flo_abundance.yj) # p: 0.9


# -- Transform - flo_shannon ----
# --> Can't transform for some reason 

head(RA_data$flo_shannon, 20)

hist(RA_data$flo_shannon)
shapiro.test(RA_data$flo_shannon) # old p: 0.0094


# ------------------------------------------------------------------------------


# -- Export new RA_data dataframe ----
ra_data_transformed_export <- paste("./analysis_data/RA/RA_Data_2021_4analysis_garden_transformed.xlsx", sep = "")
write.xlsx(RA_data, ra_data_transformed_export, append = TRUE)


# -- Clean-up environment for the next script ----
rm(list=ls())

