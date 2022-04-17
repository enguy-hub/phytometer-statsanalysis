# -- Prerequisite ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "openxlsx", 
                   "ggplot2", "ggpubr", "rstatix")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Hien/Garden/MyGithub/Phytometer_StatisticalAnalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
fa_path = "./analysis_data/FA/FA_Data_2021_4analysis.xlsx"
FA_data <- read_excel(fa_path, sheet = 1)


# Check structure and summaries of the datasets
str(FA_data)
summary(FA_data)


# ------------------------------------------------------------------------------


# ---- Working with response variable: "mass" ----

# Shapiro-test 
shapiro.test(FA_data$mass) # p: 0.0005253 => Not normal --> Try remove outliers


# Density plot and QQ-plot
ggqqplot(FA_data$mass)
ggdensity(FA_data$mass, 
          main = "Density plot of Fragaria fruit mass, n: 84, shapiro-p: 0.0005",
          xlab = "Fruit Mass")


# -------------------- #


# -- !! Using FA_data and Wilcox test !! ----

# Wilcox Rank-Sum Test - `Bagged` vs `Open` treatment
treatment.wilcox <- FA_data %>%
  rstatix::wilcox_test(mass ~ treatment, alternative = "less") %>%
  add_significance()
treatment.wilcox


# Boxplot
ggplot(FA_data, aes(x = treatment, y = mass, fill = treatment)) +
  geom_boxplot(width = 0.2, position = position_dodge(0.8), alpha = 0.6) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "Treatment", y = "Fruit Mass") +
  ggtitle("Fragaria | Fruit Mass: Pollination excluded (Bagged) < Open",
          subtitle = get_test_label(treatment.wilcox,
          description = 'Wilcox Rank-Sum Test, n: 84, bagged: 30, open: 54'))


# -------------------- #


# -- Create data-frames for/and removing outliers ----
FAF_data <- FA_data
FAF_open <- FA_data %>%
  filter(treatment == "Open")
FAF_bagged <- FA_data %>%
  filter(treatment == "Bagged")


# Removing outliers
bagged_outliers <- boxplot(FAF_bagged$mass, plot = TRUE)$out

open_outliers <- boxplot(FAF_open$mass, plot = TRUE)$out
FAF_open <- FAF_open[-which(FAF_open$mass %in% open_outliers),]

open_outliers1 <- boxplot(FAF_open$mass, plot = TRUE)$out

FAF_data <- FAF_data[-which(FAF_data$mass %in% open_outliers),]

f_outliers <- boxplot(FAF_data$mass, plot = TRUE)$out
FAF_data <- FAF_data[-which(FAF_data$mass %in% f_outliers),]

f_outliers1 <- boxplot(FAF_data$mass, plot = TRUE)$out
FAF_data<- FAF_data[-which(FAF_data$mass %in% f_outliers1),]

f_outliers2 <- boxplot(FAF_data$mass, plot = TRUE)$out


# -------------------- #


# Shapiro-test
shapiro.test(FAF_data$mass) # p = 0.46 => Normal distribution


# Density plot and QQ-plot
ggqqplot(FAF_data$mass)
ggdensity(FAF_data$mass, 
          main = "Density plot of Fragaria fruit mass ('outliers-removed'), n: 75, shapiro-p: 0.46",
          xlab = "Fruit Mass")


# -------------------- #


# -- !! Using FAF_data and t-test !! ----

# F-test of `mass` variable - `Bagged` vs `Open` treatment
treatment.ftest <- var.test(mass ~ treatment, data = FAF_data)
treatment.ftest # p = 0.88 => var.equal=TRUE


# T-test of `mass` variable - `Bagged` vs `Open` treatment
treatment.ttest <- FAF_data %>%
  t_test(mass ~ treatment, var.equal=T, alternative="less") %>%
  add_significance()
treatment.ttest


# Boxplot
ggplot(FAF_data, aes(x = treatment, y = mass, fill = treatment)) +
  geom_boxplot(width = 0.2, position = position_dodge(0.2), alpha = 0.6) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "Treatment", y = "Fruit Mass") +
  ggtitle("Fragaria | Fruit Mass ('outliers-removed'): Pollination excluded (Bagged) < Open",
          subtitle = get_test_label(treatment.ttest, 
          description = "T-Test, n: 75, bagged: 30, open: 45"))


# ------------------------------------------------------------------------------


# -- Clean-up environment for the next script ----
rm(list=ls())

