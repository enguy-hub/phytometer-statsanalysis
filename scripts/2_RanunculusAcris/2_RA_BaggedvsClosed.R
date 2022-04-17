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
ra_path = "./analysis_data/RA/RA_Data_2021_4analysis.xlsx"
RA_data <- read_excel(ra_path, sheet = 1)


# Check structure and summaries of the data sets
str(RA_data)
summary(RA_data)


# ------------------------------------------------------------------------------


# ---- Working with response variable: "fresh_mass" ----

# Shapiro-test 
shapiro.test(RA_data$fresh_mass) # p: 1.695e-07 => Not norm-dist --> Try remove outliers


# Density plot and QQ-plot
ggqqplot(RA_data$fresh_mass)
ggdensity(RA_data$fresh_mass, 
          main = "Density plot of Ranunculus fresh seed mass, n: 383, shapiro-p: < 0.0001",
          xlab = "Fresh Seed Mass")


# -------------------- #


# -- Create data-frames for/and removing outliers ----
RAF_data <- RA_data
RAF_open <- RA_data %>%
  filter(treatment == "Open")
RAF_bagged <- RA_data %>%
  filter(treatment == "Bagged")


# Removing outliers
fopen_outliers <- boxplot(RAF_open$fresh_mass, plot = TRUE)$out

fbagged_outliers <- boxplot(RAF_bagged$fresh_mass, plot = TRUE)$out
RAF_bagged <- RAF_bagged[-which(RAF_bagged$fresh_mass %in% fbagged_outliers),]

fbagged_outliers1 <- boxplot(RAF_bagged$fresh_mass, plot = TRUE)$out
RAF_bagged <- RAF_bagged[-which(RAF_bagged$fresh_mass %in% fbagged_outliers1),]

fbagged_outliers2 <- boxplot(RAF_bagged$fresh_mass, plot = TRUE)$out

RAF_data <- RAF_data[-which(RAF_data$fresh_mass %in% fbagged_outliers),]
RAF_data <- RAF_data[-which(RAF_data$fresh_mass %in% fbagged_outliers1),]


# -------------------- #


# Shapiro-test
shapiro.test(RAF_data$fresh_mass) # p: 5.956e-08 => Still not norm-dist 


# Density plot and QQ-plot
ggqqplot(RAF_data$fresh_mass)
ggdensity(RAF_data$fresh_mass, 
          main = "Density plot of Ranunculus fresh seed mass ('outliers-removed'), n: 356, shapiro-p: < 0.0001",
          xlab = "Fresh Seed Mass")


# -------------------- #


# -- !! Using RA_data and Wilcox test !! ----

# Wilcox Rank-Sum Test - `Bagged` vs `Open` treatment
f.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(fresh_mass ~ treatment, alternative="less") %>%
  add_significance()
f.treatment.wilcox


# Boxplot
ggplot(RA_data, aes(x = treatment, y = fresh_mass, fill = treatment)) +
  geom_boxplot(width = 0.2, position = position_dodge(0.8), alpha = 0.6) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "Treatment", y = "Fresh Seed Mass") +
  ggtitle("Ranunculus | Fresh Seed Mass: Pollination excluded (Bagged) < Open",
          subtitle = get_test_label(f.treatment.wilcox,
          description = "Wilcox Rank-Sum Test, n: 383, bagged: 52, open: 331"))


# ------------------------------------------------------------------------------


# ---- Working with response variable: "dry_mass" ----


# Shapiro-test
shapiro.test(RA_data$dry_mass) # p = 8.625e-12 => Not norm-dist --> Try remove outliers


# Density plot and QQ-plot
ggqqplot(RA_data$dry_mass)
ggdensity(RA_data$dry_mass, 
          main = "Density plot of Ranunculus dry seed mass, n: 383, shapiro-p: < 0.0001",
          xlab = "Dry Seed Mass")


# -------------------- #


# -- Create data-frames for/and removing outliers ----
RAD_data <- RA_data
RAD_open <- RA_data %>%
  filter(treatment == "Open")
RAD_bagged <- RA_data %>%
  filter(treatment == "Bagged")


# Removing outliers
dbagged_outliers <- boxplot(RAD_bagged$dry_mass, plot = TRUE)$out
RAD_bagged <- RAD_bagged[-which(RAD_bagged$dry_mass %in% dbagged_outliers),]

dbagged_outliers1 <- boxplot(RAD_bagged$dry_mass, plot = TRUE)$out

dopen_outliers <- boxplot(RAD_open$dry_mass, plot = TRUE)$out
RAD_open <- RAD_open[-which(RAD_open$dry_mass %in% dopen_outliers),]

dopen_outliers1 <- boxplot(RAD_open$dry_mass, plot = TRUE)$out

RAD_data <- RAD_data[-which(RAD_data$dry_mass %in% dbagged_outliers),]
RAD_data <- RAD_data[-which(RAD_data$dry_mass %in% dopen_outliers),]


# -------------------- #


# Shapiro-test
shapiro.test(RAD_data$dry_mass) # p = 2.929e-11 => Still not norm-dist


# Density plot and QQ-plot
ggqqplot(RAD_data$dry_mass)
ggdensity(RAD_data$dry_mass, 
          main = "Density plot of Ranunculus dry seed mass ('outliers-removed'), n: 350, shapiro-p: < 0.0001",
          xlab = "Dry Seed Mass")


# -------------------- #


# -- !! Using RA_data and Wilcox test !! ----

# Wilcox Rank-Sum Test - `Bagged` vs `Open` treatment
d.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(dry_mass ~ treatment, alternative="less") %>%
  add_significance()
d.treatment.wilcox


# Boxplot
ggplot(RA_data, aes(x = treatment, y = dry_mass, fill = treatment)) +
  geom_boxplot(width = 0.2, position = position_dodge(0.8), alpha = 0.6) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "Treatment", y = "Dry Seed Mass") +
  ggtitle("Ranunculus | Dry Seed Mass: Pollination excluded (Bagged) < Open",
          subtitle = get_test_label(d.treatment.wilcox,
          description = "Wilcox Rank-Sum Test, n: 383, bagged: 52, open: 331"))


# ------------------------------------------------------------------------------


# -- Clean-up environment for the next script ----
rm(list=ls())

