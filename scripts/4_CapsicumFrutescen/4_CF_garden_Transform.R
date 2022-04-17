# -- Prerequisite ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "openxlsx", "car", "VGAM")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Hien/Garden/MyGithub/Phytometer_StatisticalAnalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
cf_path = "./analysis_data/CF/CF_Data_2021_4analysis_garden.xlsx"
CF_data <- read_excel(cf_path, sheet = 1)


# Check structure and summaries of the datasets
str(CF_data)
summary(CF_data)


# ---------------------------------------------------------------------------- #


# Preliminary normality tests
shapiro.test(CF_data$fruimass_meandiff) # p = 0.0004 => Try transform
shapiro.test(CF_data$fruimass_meanopen) # p = 0.0012 => Try transform
shapiro.test(CF_data$seedmass_meandiff) # p = 0.075
shapiro.test(CF_data$seedmass_meanopen) # p = 0.3328
shapiro.test(CF_data$mass_pseed_meanopen) # p = 0.5389
shapiro.test(CF_data$ratio_meandiff) # p = 0.53
shapiro.test(CF_data$ratio_meanopen) # p = 0.006081 => Try transform

shapiro.test(CF_data$temp) # p = 0.7455
shapiro.test(CF_data$lux) # p = 0.7607
shapiro.test(CF_data$imperv100) # p: 0.2312
shapiro.test(CF_data$imperv200) # p: 0.559
shapiro.test(CF_data$imperv500) # p: 0.3629
shapiro.test(CF_data$imperv1000) # p: 0.6755

shapiro.test(CF_data$pol_abundance) # p = 0.3658
shapiro.test(CF_data$pol_richness) # p = 0.39
shapiro.test(CF_data$pol_shannon) # p = 0.081 => Try transform

shapiro.test(CF_data$flo_abundance) # p = 0.00014 => Try transform
shapiro.test(CF_data$flo_richness) # p = 0.8176
shapiro.test(CF_data$flo_shannon) # p = 0.2163


# ------------------------------------------------------------------------------


# ---- Yeo-Johnson Transform - fruimass_meandiff ----
head(CF_data$fruimass_meandiff, 20) 

shapiro.test(CF_data$fruimass_meandiff) # old p: 0.0004
hist(CF_data$fruimass_meandiff)

fmmd.lm <- lm(CF_data$fruimass_meandiff ~ 1)
fmmd.yj = boxCox(fmmd.lm,family = "yjPower",plotit = T, lambda = seq(-10, 10))

fmmd.lamb <- fmmd.yj$x[which.max(fmmd.yj$y)] # -3.33 => use -3.3

shapiro.test(yjPower(CF_data$fruimass_meandiff, -3.3)) # new p: 0.3215

CF_data <- CF_data %>%
  mutate(fruimass_meandiff.yj = yjPower(CF_data$fruimass_meandiff, -3.3), .after = "fruimass_meandiff")

hist(CF_data$fruimass_meandiff.yj) # Looks better
shapiro.test(CF_data$fruimass_meandiff.yj) # p: 0.321


# ---- Yeo-Johnson Transform - fruimass_meanopen ----
head(CF_data$fruimass_meanopen, 20)

shapiro.test(CF_data$fruimass_meanopen) # old p: 0.0012
hist(CF_data$fruimass_meanopen)

fmmo.lm <- lm(CF_data$fruimass_meanopen ~ 1)
fmmo.yj = boxCox(fmmo.lm,family = "yjPower",plotit = T, lambda = seq(-20, 20))

fmmo.lamb <- fmmo.yj$x[which.max(fmmo.yj$y)] # -3.43 => use -3.43

shapiro.test(yeo.johnson(CF_data$fruimass_meanopen, -3.43)) # new p: 0.4584

CF_data <- CF_data %>%
  mutate(fruimass_meanopen.yj = yeo.johnson(CF_data$fruimass_meanopen, -3.43), .after = "fruimass_meanopen")

hist(CF_data$fruimass_meanopen.yj) # Looks better
shapiro.test(CF_data$fruimass_meanopen.yj) # p: 0.4584


# ---- Yeo-Johnson Transform - ratio_meanopen ----
head(CF_data$ratio_meanopen, 20)

shapiro.test(CF_data$ratio_meanopen) # old p: 0.006
hist(CF_data$ratio_meanopen)

rmo.lm <- lm(CF_data$ratio_meanopen ~ 1)
rmo.yj = boxCox(rmo.lm,family = "yjPower",plotit = T, lambda = seq(-1, 15))

rmo.lamb <- rmo.yj$x[which.max(rmo.yj$y)] # 3.52 => use 3.52

shapiro.test(bcPower(CF_data$ratio_meanopen, 3.52)) # new p: 0.97

CF_data <- CF_data %>%
  mutate(ratio_meanopen.yj = bcPower(CF_data$ratio_meanopen, 3.52), .after = "ratio_meanopen")

hist(CF_data$ratio_meanopen.yj) # Looks better
shapiro.test(CF_data$ratio_meanopen.yj) # p: 0.97


# ---- Yeo-Johnson Transform - pol_shannon ----
head(CF_data$pol_shannon, 20)

shapiro.test(CF_data$pol_shannon) # old p: 0.08123
hist(CF_data$pol_shannon)

ps.lm <- lm(CF_data$pol_shannon ~ 1)
ps.yj = boxCox(ps.lm,family = "yjPower",plotit = T, lambda = seq(-2, 10))

ps.lamb <- ps.yj$x[which.max(ps.yj$y)] # 2.96 => use 2.96

shapiro.test(yeo.johnson(CF_data$pol_shannon, 2.96)) # new p: 0.9996

CF_data <- CF_data %>%
  mutate(pol_shannon.yj = yeo.johnson(CF_data$pol_shannon, 2.96), .after = "pol_shannon")

hist(CF_data$pol_shannon.yj) # Looks better
shapiro.test(CF_data$pol_shannon.yj) # p: 0.9996


# ---- Yeo-Johnson Transform - flo_abundance ----
head(CF_data$flo_abundance, 20)

shapiro.test(CF_data$flo_abundance) # old p: 0.00014
hist(CF_data$flo_abundance)

va.lm <- lm(CF_data$flo_abundance ~ 1)
va.yj = boxCox(va.lm,family = "yjPower",plotit = T, lambda = seq(-2, 2))

va.lamb <- va.yj$x[which.max(va.yj$y)] # 0.02 => use 0.02

shapiro.test(yeo.johnson(CF_data$flo_abundance, 0.02)) # new p: 0.3461

CF_data <- CF_data %>%
  mutate(flo_abundance.yj = yeo.johnson(CF_data$flo_abundance, 0.02), .after = "flo_abundance")

hist(CF_data$flo_abundance.yj) # Looks better
shapiro.test(CF_data$flo_abundance.yj) # p: 0.3461


# ---------------------------------------------------------------------------- #


# -- Export new CF_data dataframe ----
cf_data_export <- paste("./analysis_data/CF/CF_Data_2021_4analysis_garden_transformed.xlsx", sep = "")
write.xlsx(CF_data, cf_data_export, append = TRUE)


# -- Clean-up environment for the next script ----
rm(list=ls())
