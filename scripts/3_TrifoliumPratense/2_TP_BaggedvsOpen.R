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
tp_path = "./analysis_data/TP/TP_Data_2021_4analysis.xlsx"
TP_data <- read_excel(tp_path, sheet = 1)

# Check structure and summaries of the data sets
str(TP_data)
summary(TP_data)


# ------------------------------------------------------------------------------

# ---- Working with response variable: "flower_mass" ----

# Shapiro-test 
shapiro.test(TP_data$flower_mass) # p = 4.046e-11 => Not normal distribution

# Density plot and QQ-plot
ggqqplot(TP_data$flower_mass)
ggdensity(TP_data$flower_mass, 
          main = "Density plot of Trifolium flower mass, n: 359, shapiro-p: < 0.0001",
          xlab = "Flowerhead Mass")


# -- !! Using TP_data and Wilcox test !! ----

# Wilcox Rank-Sum Test - `Bagged` vs `Open` treatment
f.treatment.wilcox <- TP_data %>%
  rstatix::wilcox_test(flower_mass ~ treatment, alternative="less") %>%
  add_significance()
f.treatment.wilcox # p: <0.001

# Boxplot
ggplot(TP_data, aes(x = treatment, y = flower_mass, fill = treatment)) +
  geom_boxplot(width = 0.2, position = position_dodge(0.8), alpha = 0.6) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "Treatment", y = "Flowerhead Mass (g)") +
  ggtitle("Trifolium Pratense | Flowerhead Mass: `Bagged` plants < `Open` plants",
          subtitle = get_test_label(f.treatment.wilcox,
          description = "Wilcox Rank-Sum Test, n: 359, bagged: 74, open: 285"))


# -------------------- #

# ---- Working with "seed_mass" response variable ----

# Shapiro-test
shapiro.test(TP_data$seed_mass) # p = 2.2e-16 => Not normal distribution

# Density plot and QQ-plot
ggqqplot(TP_data$seed_mass)
ggdensity(TP_data$seed_mass, 
          main = "Density plot of Trifolium seed mass, n: 359, shapiro-p: < 0.0001",
          xlab = "Seed Mass")


# -- !! Using TP_data and Wilcox test !! ----

# Wilcox Rank-Sum Test - `Bagged` vs `Open` treatment
s.treatment.wilcox <- TP_data %>%
  rstatix::wilcox_test(seed_mass ~ treatment, alternative = "less") %>%
  add_significance()
s.treatment.wilcox # p: <0.001

# Boxplot
ggplot(TP_data, aes(x = treatment, y = seed_mass, fill = treatment)) +
  geom_boxplot(width = 0.1, position = position_dodge(0.8), alpha = 0.6) +  
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "Treatment", y = "Seed Mass (g)") +
  ggtitle("Trifolium Pratense | Seed Mass: `Bagged` plants < `Open` plants",
          subtitle = get_test_label(s.treatment.wilcox,
          description = "Wilcox Rank-Sum Test, n: 359, bagged: 74, open: 285"))


# -------------------- #

# ---- Working with "num_nutlets" response variable ----

# Shapiro-test
shapiro.test(TP_data$num_nutlets) # p = 2.2e-16 => Not normal distribution

# Density plot and QQ-plot
ggqqplot(TP_data$num_nutlets)
ggdensity(TP_data$num_nutlets, 
          main = "Density plot of Trifolium nutlets, n: 359, shapiro-p: < 0.0001",
          xlab = "Number of nutlets")


# -- !! Using TP_data and Wilcox test !! ----

# Wilcox Rank-Sum Test - `Bagged` vs `Open` treatment
n.treatment.wilcox <- TP_data %>%
  rstatix::wilcox_test(num_nutlets ~ treatment, alternative = "less") %>%
  add_significance()
n.treatment.wilcox # p: <0.001

# Boxplot
ggplot(TP_data, aes(x = treatment, y = num_nutlets, fill = treatment)) +
  geom_boxplot(width = 0.1, position = position_dodge(0.8), alpha = 0.6) +  
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "Treatment", y = "Number of Nutlets") +
  ggtitle("Trifolium Pratense | Number of Nutlets: `Bagged` plants < `Open` plants",
          subtitle = get_test_label(n.treatment.wilcox,
          description = "Wilcox Rank-Sum Test, n: 359, bagged: 74, open: 285"))


# ------------------------------------------------------------------------------

# ---- "flower_mass": Create data-frames for removing outliers ----
TPF_data <- TP_data
TPF_open <- TP_data %>%
  filter(treatment == "Open")
TPF_bagged <- TP_data %>%
  filter(treatment == "Bagged")

# Removing outliers 
fopen_outliers <- boxplot(TPF_open$flower_mass, plot = TRUE)$out
TPF_open <- TPF_open[-which(TPF_open$flower_mass %in% fopen_outliers),]

fopen_outliers1 <- boxplot(TPF_open$flower_mass, plot = TRUE)$out
TPF_open <- TPF_open[-which(TPF_open$flower_mass %in% fopen_outliers1),]

fopen_outliers2 <- boxplot(TPF_open$flower_mass, plot = TRUE)$out

fbagged_outliers <- boxplot(TPF_bagged$flower_mass, plot = TRUE)$out

TPF_data <- TPF_data[-which(TPF_data$flower_mass %in% fopen_outliers),]
TPF_data <- TPF_data[-which(TPF_data$flower_mass %in% fopen_outliers1),]

f_outliers <- boxplot(TPF_data$flower_mass, plot = TRUE)$out
TPF_data <- TPF_data[-which(TPF_data$flower_mass %in% f_outliers),]

f_outliers1 <- boxplot(TPF_data$flower_mass, plot = TRUE)$out
TPF_data <- TPF_data[-which(TPF_data$flower_mass %in% f_outliers1),]

f_outliers2 <- boxplot(TPF_data$flower_mass, plot = TRUE)$out

# Shapiro-test
shapiro.test(TPF_data$flower_mass) # p = 7.619e-07 => Still NOT norm-dist

# Density plot and QQ-plot
ggqqplot(TPF_data$flower_mass)
ggdensity(TPF_data$flower_mass, 
          main = "Density plot of Trifolium flower mass ('outliers-removed'), n: 343, shapiro-p: < 0.0001",
          xlab = "Flowerhead Mass")

# --> Still NOT norm-dist: No t-test nor wilcox test needed anymore


# -------------------- #

# ---- "seed_mass": Create TPS_data, TPS_open and TPS_bagged dataframes for removing outliers
TPS_data <- TP_data
TPS_open <- TP_data %>%
  filter(treatment == "Open")
TPS_bagged <- TP_data %>%
  filter(treatment == "Bagged")

# Removing outliers
sbagged_outliers <- boxplot(TPS_bagged$seed_mass, plot = TRUE)$out
TPS_bagged <- TPS_bagged[-which(TPS_bagged$seed_mass %in% sbagged_outliers),]

sbagged_outliers1 <- boxplot(TPS_bagged$seed_mass, plot = TRUE)$out

sopen_outliers <- boxplot(TPS_open$seed_mass, plot = TRUE)$out
TPS_open <- TPS_open[-which(TPS_open$seed_mass %in% sopen_outliers),]

sopen_outliers1 <- boxplot(TPS_open$seed_mass, plot = TRUE)$out
TPS_open <- TPS_open[-which(TPS_open$seed_mass %in% sopen_outliers1),]

sopen_outliers2 <- boxplot(TPS_open$seed_mass, plot = TRUE)$out
TPS_open <- TPS_open[-which(TPS_open$seed_mass %in% sopen_outliers2),]

sopen_outliers3 <- boxplot(TPS_open$seed_mass, plot = TRUE)$out
TPS_open <- TPS_open[-which(TPS_open$seed_mass %in% sopen_outliers3),]

sopen_outliers4 <- boxplot(TPS_open$seed_mass, plot = TRUE)$out

TPS_data <- TPS_data[-which(TPS_data$seed_mass %in% sbagged_outliers),]
TPS_data <- TPS_data[-which(TPS_data$seed_mass %in% sopen_outliers),]
TPS_data <- TPS_data[-which(TPS_data$seed_mass %in% sopen_outliers1),]
TPS_data <- TPS_data[-which(TPS_data$seed_mass %in% sopen_outliers2),]
TPS_data <- TPS_data[-which(TPS_data$seed_mass %in% sopen_outliers3),]

# Shapiro-test
shapiro.test(TPS_data$seed_mass) # p = 2.2e-16 => Still NOT norm-dist

# Density plot and QQ-plot
ggqqplot(TPS_data$seed_mass)
ggdensity(TPS_data$seed_mass, 
          main = "Density plot of Trifolium seed mass 'outliers-removed', n: 320, shapiro-p: < 0.0001",
          xlab = "Seed mMass")

# --> Still NOT norm-dist: No t-test nor wilcox test needed anymore


# ------------------------------------------------------------------------------

# -- Clean-up environment for the next script ----
rm(list=ls())

