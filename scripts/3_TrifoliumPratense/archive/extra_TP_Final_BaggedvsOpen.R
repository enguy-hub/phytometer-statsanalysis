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

# ---- Working with "seed_mass" response variable ----

# Shapiro-test
shapiro.test(TP_data$seed_mass) # p = 2.2e-16 => Not normal distribution

# Density plot and QQ-plot
ggqqplot(TP_data$seed_mass)
ggdensity(TP_data$seed_mass, 
          main = "Density plot of Trifolium seed mass, n: 359, shapiro-p: < 0.0001",
          xlab = "Seed Mass")

# Wilcox Rank-Sum Test of "seed_mass" - `Bagged` vs `Open` treatment
s.treatment.wilcox <- TP_data %>%
  rstatix::wilcox_test(seed_mass ~ treatment, alternative = "less") %>%
  add_significance()
s.treatment.wilcox # p: <0.001


# ---- Working with "num_nutlets" response variable ----

# Shapiro-test
shapiro.test(TP_data$num_nutlets) # p = 2.2e-16 => Not normal distribution

# Density plot and QQ-plot
ggqqplot(TP_data$num_nutlets)
ggdensity(TP_data$num_nutlets, 
          main = "Density plot of Trifolium seed, n: 359, shapiro-p: < 0.0001",
          xlab = "Number of nutlets")

# Wilcox Rank-Sum Test of "num_nutlets" - `Bagged` vs `Open` treatment
n.treatment.wilcox <- TP_data %>%
  rstatix::wilcox_test(num_nutlets ~ treatment, alternative = "less") %>%
  add_significance()
n.treatment.wilcox # p: <0.001


# ------------------------------------------------------------------------------

# Boxplot for "seed_mass" Wilcox test
plot_seedmass <-
  ggplot(TP_data, aes(x=treatment, y=seed_mass, fill=treatment)) +
    geom_boxplot(width = 0.1, position = position_dodge(0.8), 
                 alpha = 0.6, show.legend=F) +  
    geom_jitter(color="black", size=0.6, alpha=0.9) +
    labs(x = "treatment", y = "seed mass [g]") +
    ggtitle("Seed mass",
            subtitle = get_test_label(s.treatment.wilcox,
            description = "H1: Bagged < Open")) + 
    theme(text=element_text(size=10, face="italic"),
          legend.position = "none") 

# Boxplot for "num_nutlets" Wilcox test
plot_numseed <-
  ggplot(TP_data, aes(x=treatment, y=num_nutlets, fill=treatment)) +
    geom_boxplot(width=0.1, position=position_dodge(0.8), 
                 alpha=0.6, show.legend=F) +  
    geom_jitter(color="black", size=0.6, alpha=0.9) +
    labs(x = "treatment", y = "number of seed") +
    ggtitle("Number of seed",
            subtitle = get_test_label(n.treatment.wilcox,
            description = "H1: Bagged < Open")) + 
    theme(text=element_text(size=10, face="italic"),
          legend.position = "none") 


# ---------------------------------------------------------------------------- #
# Create a combined plot of the best predictor variables
combined_plot1 <-
  ggarrange(plot_seedmass, plot_numseed + 
            rremove("x.text"), ncol = 2,
            labels = c("A", "B"), 
            font.label=list(size=12))

annotate_figure(combined_plot1, 
  top=text_grob("Trifolium pratense | Wilcoxon rank-sum tests | n-bagged: 74 | n-open: 285\n",
  color="#D55E00", face="bold", size=12, lineheight=1))

# ------------------------------------------------------------------------------

# -- Clean-up environment for the next script ----
rm(list=ls())

