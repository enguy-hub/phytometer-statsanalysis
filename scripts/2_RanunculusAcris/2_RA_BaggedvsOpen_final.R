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

# Wilcox Rank-Sum Test of "dry_mass" - `Bagged` vs `Open` treatment
d.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(dry_mass ~ treatment, alternative="less") %>%
  add_significance()
d.treatment.wilcox # p: 1.07e-20


# ---- Working with response variable: "num_nutlets" ----

# Shapiro-test 
shapiro.test(RA_data$num_nutlets) # p: 1.647e-11 => Not norm-dist --> Try remove outliers

# Density plot and QQ-plot
ggqqplot(RA_data$num_nutlets)
ggdensity(RA_data$num_nutlets, 
          main = "Density plot of Ranunculus number of nutlets, n: 383, shapiro-p: < 0.0001",
          xlab = "Number of Nutlets")

# Wilcox Rank-Sum Test of "num_nutlets" - `Bagged` vs `Open` treatment
n.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(num_nutlets ~ treatment, alternative="less") %>%
  add_significance()
n.treatment.wilcox # p: < 0.001


# ---- Working with response variable: "fertile_nutlets" ----

# Shapiro-test 
shapiro.test(RA_data$fertile_nutlets) # p:  1.986e-07 => Not norm-dist --> Try remove outliers

# Density plot and QQ-plot
ggqqplot(RA_data$fertile_nutlets)
ggdensity(RA_data$fertile_nutlets, 
          main = "Density plot of Ranunculus fertile seed, n: 383, shapiro-p: < 0.0001",
          xlab = "Fertile Seeds")

# Wilcox Rank-Sum Test of "fertile_nutlets" - `Bagged` vs `Open` treatment
fn.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(fertile_nutlets ~ treatment, alternative="less") %>%
  add_significance()
fn.treatment.wilcox # p: < 0.001


# ---- Working with response variable: "infertile_nutlets" ----

# Shapiro-test 
shapiro.test(RA_data$infertile_nutlets) # p: 2.2e-16 => Not norm-dist --> Try remove outliers

# Density plot and QQ-plot
ggqqplot(RA_data$infertile_nutlets)
ggdensity(RA_data$infertile_nutlets, 
          main = "Density plot of Ranunculus infertile seed, n: 383, shapiro-p: < 0.0001",
          xlab = "Infertile Seeds")

# Wilcox Rank-Sum Test of "infertile_nutlets" - `Bagged` vs `Open` treatment
in.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(infertile_nutlets ~ treatment, alternative="greater") %>%
  add_significance()
in.treatment.wilcox # p: < 0.001


# ------------------------------------------------------------------------------

# Boxplot for "dry_mass" Wilcox test
plot_drymass <-
  ggplot(RA_data, aes(x=treatment, y=dry_mass, fill=treatment)) +
    geom_boxplot(width=0.2, position=position_dodge(0.8), 
                 alpha=0.6, show.legend=F) +
    geom_jitter(color="black", size=0.6, alpha=0.9) +
    labs(x = "treatment", y = "seed mass [g]") +
    ggtitle("Seed mass",
            subtitle = get_test_label(d.treatment.wilcox,
            description = "H1: Bagged < Open")) + 
    theme(text=element_text(size=10),
          legend.position = "none") 

# Boxplot for "num_nutlets" Wilcox test
plot_numseed <-
  ggplot(RA_data, aes(x=treatment, y=num_nutlets, fill=treatment)) +
    geom_boxplot(width=0.2, position=position_dodge(0.8), 
                 alpha=0.6, show.legend=F) +
    geom_jitter(color="black", size=0.6, alpha=0.9) +
    labs(x = "treatment", y = "number of seed") +
    ggtitle("Number of seed",
            subtitle = get_test_label(n.treatment.wilcox,
            description = "H1: Bagged < Open")) + 
    theme(text=element_text(size=10),
          legend.position = "none") 

# Boxplot for "fertile_nutlets" Wilcox test
plot_fertseed <-
  ggplot(RA_data, aes(x=treatment, y=fertile_nutlets,fill=treatment)) +
    geom_boxplot(width=0.2, position=position_dodge(0.8), 
                 alpha=0.6, show.legend=F) +
    geom_jitter(color="black", size=0.6, alpha=0.9) +
    labs(x = "treatment", y = "fertile seed") +
    ggtitle("Number of fertile seed",
            subtitle = get_test_label(fn.treatment.wilcox,
            description = "H1: Bagged < Open")) + 
    theme(text=element_text(size=10),
          legend.position = "none") 

# Boxplot for "infertile_nutlets" Wilcox test
plot_infertseed <-
  ggplot(RA_data, aes(x= treatment, y = infertile_nutlets, fill = treatment)) +
    geom_boxplot(width = 0.2, position = position_dodge(0.8), alpha = 0.6, show.legend=F) +
    geom_jitter(color="black", size=0.6, alpha=0.9) +
    labs(x = "treatment", y = "infertile seed") +
    ggtitle("Number of infertile seed",
            subtitle = get_test_label(in.treatment.wilcox,
            description = "H1: Bagged > Open")) + 
    theme(text=element_text(size=10),
          legend.position = "none") 


# ---------------------------------------------------------------------------- #
# Create a combined plot of the best predictor variables
#png("./results_plots/2_RA/0_RA_wilcox_complot.png")

combined_plot1 <-
  ggarrange(plot_drymass, plot_numseed, plot_fertseed, plot_infertseed 
            + rremove("x.text"), ncol = 2, nrow=2,
            labels = c("A", "B", "C", "D"), font.label=list(size=12))

annotate_figure(combined_plot1, 
  top=text_grob("Ranunculus acris | Wilcoxon rank-sum tests | n-bagged: 52 | n-open: 331\n",
  color="#D55E00", face="bold", size=12, lineheight=1))


# ------------------------------------------------------------------------------

# -- Clean-up environment for the next script ----
rm(list=ls())

