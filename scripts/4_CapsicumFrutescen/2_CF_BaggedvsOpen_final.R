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

# Wilcox Rank-Sum Test of "fruit_mass" - `Bagged` vs `Open` treatment
f.treatment.wilcox <- CF_data %>%
  rstatix::wilcox_test(fruit_mass ~ treatment, alternative="less") %>%
  add_significance()
f.treatment.wilcox # p: <0.001


# ---- Working with response variable: "seed_mass" ----

# Shapiro-test
shapiro.test(CF_data$seed_mass) # p = 8.191e-05 => NOT normal distribution

# Density plot and QQ-plot
ggqqplot(CF_data$seed_mass)
ggdensity(CF_data$seed_mass, 
          main = "Density plot of Capsicum seed mass, n: 135, shapiro-p: < 0.0001",
          xlab = "Seed Mass")

# Wilcox Rank-Sum Test of "seed_mass" - `Bagged` vs `Open`
s.treatment.wilcox <- CF_data %>%
  rstatix::wilcox_test(seed_mass ~ treatment, alternative="less") %>%
  add_significance()
s.treatment.wilcox # p: <0.001


# ---- Working with response variable: "num_nutlets" ----

# Shapiro-test
shapiro.test(CF_data$num_nutlets) # p = 0.0004 => NOT normal distribution

# Density plot and QQ-plot
ggqqplot(CF_data$num_nutlets)
ggdensity(CF_data$num_nutlets, 
          main = "Density plot of Capsicum num seed, n: 135, shapiro-p: < 0.0001",
          xlab = "Number of nutlets")

# Wilcox Rank-Sum Test of "num_nutlets"- `Bagged` vs `Open`
n.treatment.wilcox <- CF_data %>%
  rstatix::wilcox_test(num_nutlets ~ treatment, alternative="less") %>%
  add_significance()
n.treatment.wilcox # p: < 0.001


# ------------------------------------------------------------------------------

# Boxplot for "fruit_mass" Wilcox test
plot_fruitmass <-
  ggplot(CF_data, aes(x =treatment, y=fruit_mass, fill=treatment)) +
    geom_boxplot(width=0.2, position=position_dodge(0.8), alpha=0.6, show.legend=F) +
    geom_jitter(color="black", size=0.6, alpha=0.9) +
    labs(x = "treatment", y = "fruit mass [g]") +
    ggtitle("Fruit mass",
            subtitle = get_test_label(f.treatment.wilcox,
            description = 'H1: Bagged < Open')) + 
    theme(text=element_text(size=10),
          legend.position = "none") 
plot_fruitmass

# Boxplot for "seed_mass" Wilcox test
plot_seedmass <-
  ggplot(CF_data, aes(x=treatment, y=seed_mass, fill=treatment)) +
    geom_boxplot(width=0.1, position=position_dodge(0.8), alpha=0.6, show.legend=F) +  
    geom_jitter(color="black", size=0.6, alpha=0.9) +
    labs(x = "treatment", y = "seed mass [g]") +
    ggtitle("Seed mass",
            subtitle = get_test_label(s.treatment.wilcox,
            description = 'H1: Bagged < Open')) + 
    theme(text=element_text(size=10),
          legend.position = "none") 
plot_seedmass

# Boxplot for "num_nutlets" Wilcox test
plot_numseed <-
  ggplot(CF_data, aes(x=treatment, y=num_nutlets, fill=treatment)) +
    geom_boxplot(width=0.1, position=position_dodge(0.8), alpha=0.6, show.legend=F) +  
    geom_jitter(color="black", size=0.6, alpha=0.9) +
    labs(x = "treatment", y = "number of seed") +
    ggtitle("Number of seed",
            subtitle = get_test_label(n.treatment.wilcox,
            description = 'H1: Bagged < Open')) + 
    theme(text=element_text(size=10),
          legend.position = "none") 
plot_numseed


# ---------------------------------------------------------------------------- #

# Create a combined plot of the best predictor variables
#png("./results_plots/4_CF/1_CF_smmo_complot.png")

combined_plot1 <-
  ggarrange(plot_fruitmass, plot_seedmass, plot_numseed 
            + rremove("x.text"), ncol = 3,
            labels = c("A", "B", "C"), font.label=list(size=12))

annotate_figure(combined_plot1, 
  top=text_grob("Capsicum frutescens | Wilcoxon rank-sum tests | n-bagged: 27 | n-open: 108\n",
  color="#D55E00", face="bold", size=12, lineheight=1))


# ---------------------------------------------------------------------------- #

# Clean-up environment for the next script
rm(list=ls())

