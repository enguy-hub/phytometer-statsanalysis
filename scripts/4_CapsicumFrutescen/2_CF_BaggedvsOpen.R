# -- Prerequisite ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "openxlsx", 
                   "ggplot2", "ggpubr", "rstatix")
lapply(list_packages, library, character.only = TRUE)

# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)

# Set path and read the data
cf_path = "./analysis_data/CF/CF_Data_2021_4analysis.xlsx"
CF_data <- read_excel(cf_path, sheet = 1)

# Check structure and summaries of the datasets
str(CF_data)
summary(CF_data)


# ------------------------------------------------------------------------------

# ---- Working with response variable: "fruit_mass" ----

# Shapiro-test 
shapiro.test(CF_data$fruit_mass) # p = 1.686e-14 => NOT normal distribution

# Density plot and QQ-plot
ggqqplot(CF_data$fruit_mass)
ggdensity(CF_data$fruit_mass, 
          main = "Density plot of Capsicum fruit mass, n: 135, shapiro-p: < 0.0001",
          xlab = "Fruit Mass")


# -- !! Using CF_data and Wilcox test !! ----

# Wilcox Rank-Sum Test - `Bagged` vs `Open` treatment
f.treatment.wilcox <- CF_data %>%
  rstatix::wilcox_test(fruit_mass ~ treatment, alternative="less") %>%
  add_significance()
f.treatment.wilcox # p: <0.001

# Boxplot
ggplot(CF_data, aes(x = treatment, y = fruit_mass, fill = treatment)) +
  geom_boxplot(width = 0.2, position = position_dodge(0.8), alpha = 0.6) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "Treatment", y = "Fruit Mass (g)") +
  ggtitle("Capsicum Frutescens | Fruit Mass: `Bagged` plants < `Open` plants",
          subtitle = get_test_label(f.treatment.wilcox,
          description = "Wilcox Rank-Sum Test, n: 135, bagged: 27, open: 108"))


# -------------------- #

# ---- Working with response variable: "seed_mass" ----

# Shapiro-test
shapiro.test(CF_data$seed_mass) # p = 8.191e-05 => NOT normal distribution

# Density plot and QQ-plot
ggqqplot(CF_data$seed_mass)
ggdensity(CF_data$seed_mass, 
          main = "Density plot of Capsicum seed mass, n: 135, shapiro-p: < 0.0001",
          xlab = "Seed Mass")


# -- !! Using CF_data and Wilcox test !! ----

# Wilcox Rank-Sum Test - `Bagged` vs `Open`
s.treatment.wilcox <- CF_data %>%
  rstatix::wilcox_test(seed_mass ~ treatment, alternative = "less") %>%
  add_significance()
s.treatment.wilcox # p: <0.001

# Boxplot
ggplot(CF_data, aes(x = treatment, y = seed_mass, fill = treatment)) +
  geom_boxplot(width = 0.1, position = position_dodge(0.8), alpha = 0.6) +  
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "Treatment", y = "Seed Mass (g)") +
  ggtitle("Capsicum Frutescens | Seed Mass: `Bagged` plants < `Open` plants",
          subtitle = get_test_label(s.treatment.wilcox,
          description = "Wilcoxon Rank-Sum Test, n: 135, bagged: 27, open: 108"))


# -------------------- #

# ---- Working with response variable: "num_nutlets" ----

# Shapiro-test
shapiro.test(CF_data$num_nutlets) # p = 0.0004 => NOT normal distribution

# Density plot and QQ-plot
ggqqplot(CF_data$num_nutlets)
ggdensity(CF_data$num_nutlets, 
          main = "Density plot of Capsicum num nutlets, n: 135, shapiro-p: < 0.0001",
          xlab = "Number of nutlets")


# -- !! Using CF_data and Wilcox test !! ----

# Wilcox Rank-Sum Test - `Bagged` vs `Open`
n.treatment.wilcox <- CF_data %>%
  rstatix::wilcox_test(num_nutlets ~ treatment, alternative = "less") %>%
  add_significance()
n.treatment.wilcox # p: < 0.001

# Boxplot
ggplot(CF_data, aes(x = treatment, y = num_nutlets, fill = treatment)) +
  geom_boxplot(width = 0.1, position = position_dodge(0.8), alpha = 0.6) +  
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "Treatment", y = "Number of Nutlets") +
  ggtitle("Capsicum Frutescens | Number of Nutlets: `Bagged` plants < `Open` plants",
          subtitle = get_test_label(n.treatment.wilcox,
          description = "Wilcoxon Rank-Sum Test, n: 135, bagged: 27, open: 108"))


# ------------------------------------------------------------------------------

# ---- "fruit_mass": Create data-frames for removing outliers ----
CFF_data <- CF_data
CFF_open <- CF_data %>%
  filter(treatment == "Open")
CFF_bagged <- CF_data %>%
  filter(treatment == "Bagged")

# Removing outliers
fbagged_outliers <- boxplot(CFF_bagged$fruit_mass, plot = TRUE)$out

fopen_outliers <- boxplot(CFF_open$fruit_mass, plot = TRUE)$out
CFF_open <- CFF_open[-which(CFF_open$fruit_mass %in% fopen_outliers),]

fopen_outliers1 <- boxplot(CFF_open$fruit_mass, plot = TRUE)$out
CFF_open <- CFF_open[-which(CFF_open$fruit_mass %in% fopen_outliers1),]

fopen_outliers2 <- boxplot(CFF_open$fruit_mass, plot = TRUE)$out

CFF_data <- CFF_data[-which(CFF_data$fruit_mass %in% fopen_outliers),]
CFF_data <- CFF_data[-which(CFF_data$fruit_mass %in% fopen_outliers1),]

f_outliers <- boxplot(CFF_data$fruit_mass, plot = TRUE)$out

# Shapiro-test
shapiro.test(CFF_data$fruit_mass) # p = 0.1733 => Norm-dist, use t-test

# Density plot and QQ-plot
ggqqplot(CFF_data$fruit_mass)
ggdensity(CFF_data$fruit_mass, 
          main = "Density plot of Capsicum fruit mass ('outliers-removed'), n: 127, shapiro-p: < 0.0001",
          xlab = "Fruit Mass")


# -- !! Using CFF_data and t-test !! ----

# F-test - `Bagged` vs `Open`
f.treatment.ftest <- var.test(fruit_mass ~ treatment, data = CFF_data)
f.treatment.ftest # p = 0.0042 => var.equal=FALSE

# T-test - `Bagged` vs `Open`
f.treatment.ttest <- CFF_data %>%
  t_test(fruit_mass ~ treatment, var.equal=F, alternative="less") %>%
  add_significance()
f.treatment.ttest # p: <0.001

# Boxplot
ggplot(CFF_data, aes(x = treatment, y = fruit_mass, fill = treatment)) +
  geom_boxplot(width = 0.2, position = position_dodge(0.8), alpha = 0.6) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "treatment", y = "Fruit Mass (g)") +
  ggtitle("Capsicum Frutescens | Fruit Mass ('outliers-removed'): `Bagged` plants < `Open` plants",
          subtitle = get_test_label(f.treatment.ttest,
          description = "T-Test, n: 127, bagged: 27, open: 100"))


# -------------------- #

# ---- "seed_mass": Create data-frames for removing outliers ----
CFS_data <- CF_data
CFS_open <- CF_data %>%
  filter(treatment == "Open")
CFS_bagged <- CF_data %>%
  filter(treatment == "Bagged")

# Removing outliers
sbagged_outliers <- boxplot(CFS_bagged$seed_mass, plot = TRUE)$out
CFS_bagged <- CFS_bagged[-which(CFS_bagged$seed_mass %in% sbagged_outliers),]

sbagged_outliers1 <- boxplot(CFS_bagged$seed_mass, plot = TRUE)$out

sopen_outliers <- boxplot(CFS_open$seed_mass, plot = TRUE)$out
CFS_open <- CFS_open[-which(CFS_open$seed_mass %in% sopen_outliers),]

sopen_outliers1 <- boxplot(CFS_open$seed_mass, plot = TRUE)$out

CFS_data <- CFS_data[-which(CFS_data$seed_mass %in% sbagged_outliers),]
CFS_data <- CFS_data[-which(CFS_data$seed_mass %in% sopen_outliers),]

# Shapiro-test
shapiro.test(CFS_data$seed_mass) # p = 0.0001578 => Still not normal, use wilcox

# Density plot and QQ-plot
ggqqplot(CFS_data$seed_mass)
ggdensity(CFS_data$seed_mass, 
          main = "Density plot of 'cleaned-up' Capsicum seed mass, n: 129, shapiro-p: 0.00016",
          xlab = "Capsicum seed mass")

# --> Still NOT norm-dist: No t-test nor wilcox test needed anymore


# ---------------------------------------------------------------------------- #

# Clean-up environment for the next script
rm(list=ls())

