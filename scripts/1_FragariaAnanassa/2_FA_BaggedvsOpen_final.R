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
          xlab = "Fruit mass")

# Wilcox Rank-Sum Test of "mass" - `Bagged` vs `Open` treatment
treatment.wilcox <- FA_data %>%
  rstatix::wilcox_test(mass ~ treatment, alternative="less") %>%
  add_significance()
treatment.wilcox # p: 0.0733


# ------------------------------------------------------------------------------

# Boxplot of "mass"
ggplot(FA_data, aes(x=treatment, y=mass, fill=treatment)) +
  geom_boxplot(width=0.2, position=position_dodge(0.8), 
               alpha=0.6, show.legend=F) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "treatment", y = "fruit mass [g]") +
  ggtitle("Fragaria x ananassa | Fruit mass | Wilcox rank-sum test",
          subtitle = get_test_label(treatment.wilcox,
          description = 'H1: Bagged < Open, n-bagged: 30, n-open: 54')) + 
  theme(text=element_text(size=11),
        legend.position = "none")



# ------------------------------------------------------------------------------


# -- Clean-up environment for the next script ----
rm(list=ls())

