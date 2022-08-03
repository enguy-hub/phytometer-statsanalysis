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
ra_path = "./analysis_data/RA/RA_Data_2021_4analysis.xlsx"
RA_data <- read_excel(ra_path, sheet = 1)

# Check structure and summaries of the data sets
str(RA_data)
summary(RA_data)


# ------------------------------------------------------------------------------

# ---- Working with response variable: "dry_mass" ----

# Shapiro-test
shapiro.test(RA_data$dry_mass) # p = 8.625e-12 => Not norm-dist --> Try remove outliers

# Density plot and QQ-plot
ggqqplot(RA_data$dry_mass)
ggdensity(RA_data$dry_mass, 
          main = "Density plot of Ranunculus seed mass, n: 383, shapiro-p: < 0.0001",
          xlab = "Dry Seed Mass")


# -- !! Using RA_data and Wilcox test !! ----

# Wilcox Rank-Sum Test - `Bagged` vs `Open` treatment
d.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(dry_mass ~ treatment, alternative="less") %>%
  add_significance()
d.treatment.wilcox # p: 1.07e-20

# Boxplot
ggplot(RA_data, aes(x = treatment, y = dry_mass, fill = treatment)) +
  geom_boxplot(width = 0.2, position = position_dodge(0.8), alpha = 0.6) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "treatment", y = "seed mass (g)") +
  ggtitle("Ranunculus acris | Seed mass: `Bagged` plants < `Open` plants",
          subtitle = get_test_label(d.treatment.wilcox,
          description = "Wilcox rank-sum test, n: 383, bagged: 52, open: 331"))


# -------------------- #

# ---- Working with response variable: "num_nutlets" ----

# Shapiro-test 
shapiro.test(RA_data$num_nutlets) # p: 1.647e-11 => Not norm-dist --> Try remove outliers

# Density plot and QQ-plot
ggqqplot(RA_data$num_nutlets)
ggdensity(RA_data$num_nutlets, 
          main = "Density plot of Ranunculus number of nutlets, n: 383, shapiro-p: < 0.0001",
          xlab = "Number of Nutlets")


# -- !! Using RA_data and Wilcox test !! ----

# Wilcox Rank-Sum Test - `Bagged` vs `Open` treatment
n.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(num_nutlets ~ treatment, alternative="less") %>%
  add_significance()
n.treatment.wilcox # p: < 0.001

# Boxplot
ggplot(RA_data, aes(x = treatment, y = num_nutlets, fill = treatment)) +
  geom_boxplot(width = 0.2, position = position_dodge(0.8), alpha = 0.6) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "treatment", y = "number of seed") +
  ggtitle("Ranunculus acris | Number of seed: `Bagged` plants < `Open` plants",
          subtitle = get_test_label(n.treatment.wilcox,
          description = "Wilcox rank-sum test, n: 383, bagged: 52, open: 331"))


# -------------------- #

# ---- Working with response variable: "fertile_nutlets" ----

# Shapiro-test 
shapiro.test(RA_data$fertile_nutlets) # p:  1.986e-07 => Not norm-dist --> Try remove outliers

# Density plot and QQ-plot
ggqqplot(RA_data$fertile_nutlets)
ggdensity(RA_data$fertile_nutlets, 
          main = "Density plot of Ranunculus fertile seed, n: 383, shapiro-p: < 0.0001",
          xlab = "Fertile Seeds")


# -- !! Using RA_data and Wilcox test !! ----

# Wilcox Rank-Sum Test - `Bagged` vs `Open` treatment
fn.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(fertile_nutlets ~ treatment, alternative="less") %>%
  add_significance()
fn.treatment.wilcox # p: < 0.001

# Boxplot
ggplot(RA_data, aes(x = treatment, y = fertile_nutlets, fill = treatment)) +
  geom_boxplot(width = 0.2, position = position_dodge(0.8), alpha = 0.6) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "treatment", y = "fertile seed") +
  ggtitle("Ranunculus acris | Fertile seed: `Bagged` plants < `Open` plants",
          subtitle = get_test_label(fn.treatment.wilcox,
          description = "Wilcox rank-sum test, n: 383, bagged: 52, open: 331"))


# -------------------- #

# ---- Working with response variable: "infertile_nutlets" ----

# Shapiro-test 
shapiro.test(RA_data$infertile_nutlets) # p: 2.2e-16 => Not norm-dist --> Try remove outliers

# Density plot and QQ-plot
ggqqplot(RA_data$infertile_nutlets)
ggdensity(RA_data$infertile_nutlets, 
          main = "Density plot of Ranunculus infertile seed, n: 383, shapiro-p: < 0.0001",
          xlab = "Infertile Seeds")


# -- !! Using RA_data and Wilcox test !! ----

# Wilcox Rank-Sum Test - `Bagged` vs `Open` treatment
in.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(infertile_nutlets ~ treatment, alternative="greater") %>%
  add_significance()
in.treatment.wilcox # p: < 0.001

# Boxplot
ggplot(RA_data, aes(x = treatment, y = infertile_nutlets, fill = treatment)) +
  geom_boxplot(width = 0.2, position = position_dodge(0.8), alpha = 0.6) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "treatment", y = "infertile seed") +
  ggtitle("Ranunculus acris | Infertile seed: `Bagged` plants > `Open` plants",
          subtitle = get_test_label(in.treatment.wilcox,
          description = "Wilcox rank-sum test, n: 383, bagged: 52, open: 331"))


# ------------------------------------------------------------------------------

# -- Clean-up environment for the next script ----
rm(list=ls())

