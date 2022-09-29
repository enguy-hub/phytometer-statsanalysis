# -- Prerequisite ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "openxlsx", "car", "VGAM")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
# pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
fa_path = "./analysis_data/FA/FA_Data_2021_4analysis_garden.xlsx"
FA_data <- read_excel(fa_path, sheet = 1)


# Check structure and summaries of the data
str(FA_data)
summary(FA_data)


# Preliminary normality tests
shapiro.test(FA_data$mass_meandiff) # p: 0.1979
shapiro.test(FA_data$mass_meanopen) # p: 0.101
shapiro.test(FA_data$ratio_meandiff) # p: 0.3334
shapiro.test(FA_data$ratio_meanopen) # p: 0.07748

shapiro.test(FA_data$temp) # p: 0.9495
shapiro.test(FA_data$lux) # p: 0.5441
shapiro.test(FA_data$imperv100) # p: 0.2312
shapiro.test(FA_data$imperv200) # p: 0.559
shapiro.test(FA_data$imperv500) # p: 0.3629
shapiro.test(FA_data$imperv1000) # p: 0.6755

shapiro.test(FA_data$pol_abundance) # p: 1.692e-05 => Try transform
shapiro.test(FA_data$pol_richness) # p: 0.084
shapiro.test(FA_data$pol_shannon) # p: 0.014 => Try transform

shapiro.test(FA_data$flo_abundance) # p: 0.00142 => Try transform
shapiro.test(FA_data$flo_richness) # p: 0.04623 => Try transform
shapiro.test(FA_data$flo_shannon) # p: 0.068


# ------------------------------------------------------------------------------


# -- Yeo-Johnson Transform - pol_abundance ----
head(FA_data$pol_abundance, 15)

shapiro.test(FA_data$pol_abundance) # old p: 1.692e-05
hist(FA_data$pol_abundance)

pa.lm <- lm(FA_data$pol_abundance ~ 1)

pa.yj = boxCox(pa.lm,family = "yjPower",plotit = T, lambda = seq(-2, 2))

pa.lamb <- pa.yj$x[which.max(pa.yj$y)] # -0.26 => use -0.26

shapiro.test(yeo.johnson(FA_data$pol_abundance, -0.26)) # new p: 0.99

FA_data <- FA_data %>%
  mutate(pol_abundance.yj = yeo.johnson(FA_data$pol_abundance, -0.26),
         .after = "pol_abundance")
  
hist(FA_data$pol_abundance.yj) # Looks better
shapiro.test(FA_data$pol_abundance.yj) # p: 0.99


# -- Yeo-Johnson Transform - pol_shannon ----
head(FA_data$pol_shannon, 20) 

shapiro.test(FA_data$pol_shannon) # old p: 0.014
hist(FA_data$pol_shannon)

ps.lm <- lm(FA_data$pol_shannon ~ 1)
ps.yj = boxCox(ps.lm,family = "yjPower",plotit = T, lambda = seq(0, 6))

ps.lamb <- ps.yj$x[which.max(ps.yj$y)] # 2.6 => use 2.6

shapiro.test(yeo.johnson(FA_data$pol_shannon, 2.6)) # new p: 0.3517

FA_data <- FA_data %>%
  mutate(pol_shannon.yj = yeo.johnson(FA_data$pol_shannon, 2.6), 
         .after = "pol_shannon")

hist(FA_data$pol_shannon.yj) # Looks better
shapiro.test(FA_data$pol_shannon.yj) # p: 0.3517


# -- Yeo-Johnson Transform - flo_abundance ----
head(FA_data$flo_abundance, 20)

shapiro.test(FA_data$flo_abundance) # old p: 0.0014
hist(FA_data$flo_abundance)

fa.lm <- lm(FA_data$flo_abundance ~ 1)
fa.yj = boxCox(fa.lm,family = "yjPower",plotit = T, lambda = seq(-2, 2))

fa.lamb <- fa.yj$x[which.max(fa.yj$y)] # -0.02 => use -0.02

shapiro.test(yeo.johnson(FA_data$flo_abundance, -0.02)) # new p: 0.54

FA_data <- FA_data %>%
  mutate(flo_abundance.yj = yeo.johnson(FA_data$flo_abundance, -0.02), 
         .after = "flo_abundance")

hist(FA_data$flo_abundance.yj) # Looks better
shapiro.test(FA_data$flo_abundance.yj) # p: 0.54


# -- Yeo-Johnson Transform - flo_richness ----
head(FA_data$flo_richness, 20) 

shapiro.test(FA_data$flo_richness) # old p: 0.046
hist(FA_data$flo_richness)

fr.lm <- lm(FA_data$flo_richness ~ 1)
fr.yj = boxCox(fr.lm,family = "yjPower",plotit = T, lambda = seq(-3, 3))

fr.lamb <- fr.yj$x[which.max(fr.yj$y)] # -0.69 => Use -0.69

shapiro.test(yeo.johnson(FA_data$flo_richness, -0.69)) # new p: 0.32

FA_data <- FA_data %>%
  mutate(flo_richness.yj = yeo.johnson(FA_data$flo_richness, -0.69),
         .after = "flo_richness")

hist(FA_data$flo_richness.yj) # Looks better
shapiro.test(FA_data$flo_richness.yj) # p: 0.32


# ------------------------------------------------------------------------------


# -- Export new FA_data dataframe ----
FA_data_transformed_export <- paste("./analysis_data/FA/FA_Data_2021_4analysis_garden_transformed.xlsx", sep = "")
write.xlsx(FA_data, FA_data_transformed_export, append = TRUE)


# -- Clean-up environment for the next script ----
rm(list=ls())

