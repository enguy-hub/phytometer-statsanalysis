# -- Prerequisite ----

# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
setwd(pdir)

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "openxlsx", "ggplot2", "ggpubr", "rstatix")
lapply(list_packages, library, character.only = TRUE)

# Set path and read the data
fa_path = "./analysis_data/FA/FA_Data_2021_4analysis.xlsx"
cf_path = "./analysis_data/CF/CF_Data_2021_4analysis.xlsx"
ra_path = "./analysis_data/RA/RA_Data_2021_4analysis.xlsx"
tp_path = "./analysis_data/TP/TP_Data_2021_4analysis.xlsx"

# Create dataframes
FA_data <- read_excel(fa_path, sheet = 1)
RA_data <- read_excel(ra_path, sheet = 1)
TP_data <- read_excel(tp_path, sheet = 1)
CF_data <- read_excel(cf_path, sheet = 1)


# ------------------------------------------------------------------------------
# -------------------------------- F. ananassa ---------------------------------
# ------------------------------------------------------------------------------

# ----- Response variable: "mass" -----
# Shapiro-test 
shapiro.test(FA_data$mass) # p: 0.0005253 => Not normal --> Try remove outliers

# Density plot and QQ-plot
ggqqplot(FA_data$mass)
ggdensity(FA_data$mass, 
          main = "Density plot of Fragaria fruit mass, n: 84, shapiro-p: 0.0005",
          xlab = "Fruit mass")

# Wilcox Rank-Sum Test of "mass" - `Bagged` vs `Open` treatment
fa.treatment.wilcox <- FA_data %>%
  rstatix::wilcox_test(mass ~ treatment, alternative="less") %>%
  add_significance()
fa.treatment.wilcox # p: 0.0733


# ------------------------------------------------------------------------------
# ------------------------------ C. frutescens ---------------------------------
# ------------------------------------------------------------------------------

# ----- Response variable: "fruit_mass" -----
# Shapiro-test 
shapiro.test(CF_data$fruit_mass) # p = 1.686e-14 => NOT normal distribution

# Density plot and QQ-plot
ggqqplot(CF_data$fruit_mass)
ggdensity(CF_data$fruit_mass, 
          main = "Density plot of Capsicum fruit mass, n: 135, shapiro-p: < 0.0001",
          xlab = "Fruit Mass")

# Wilcox Rank-Sum Test of "fruit_mass" - `Bagged` vs `Open` treatment
cf.f.treatment.wilcox <- CF_data %>%
  rstatix::wilcox_test(fruit_mass ~ treatment, alternative="less") %>%
  add_significance()
cf.f.treatment.wilcox # p: <0.001


# ---- Response variable: "seed_mass" ----
# Shapiro-test
shapiro.test(CF_data$seed_mass) # p = 8.191e-05 => NOT normal distribution

# Density plot and QQ-plot
ggqqplot(CF_data$seed_mass)
ggdensity(CF_data$seed_mass, 
          main = "Density plot of Capsicum seed mass, n: 135, shapiro-p: < 0.0001",
          xlab = "Seed Mass")

# Wilcox Rank-Sum Test of "seed_mass" - `Bagged` vs `Open`
cf.s.treatment.wilcox <- CF_data %>%
  rstatix::wilcox_test(seed_mass ~ treatment, alternative="less") %>%
  add_significance()
cf.s.treatment.wilcox # p: <0.001


# ----- Response variable: "num_nutlets" -----
# Shapiro-test
shapiro.test(CF_data$num_nutlets) # p = 0.0004 => NOT normal distribution

# Density plot and QQ-plot
ggqqplot(CF_data$num_nutlets)
ggdensity(CF_data$num_nutlets, 
          main = "Density plot of Capsicum num seed, n: 135, shapiro-p: < 0.0001",
          xlab = "Number of nutlets")

# Wilcox Rank-Sum Test of "num_nutlets"- `Bagged` vs `Open`
cf.n.treatment.wilcox <- CF_data %>%
  rstatix::wilcox_test(num_nutlets ~ treatment, alternative="less") %>%
  add_significance()
cf.n.treatment.wilcox # p: < 0.001


# ------------------------------------------------------------------------------
# -------------------------------- R. acris ------------------------------------
# ------------------------------------------------------------------------------

# ----- Response variable: "dry_mass" -----
# Shapiro-test
shapiro.test(RA_data$dry_mass) # p = 8.625e-12 => Not norm-dist --> Try remove outliers

# Density plot and QQ-plot
ggqqplot(RA_data$dry_mass)
ggdensity(RA_data$dry_mass, 
          main = "Density plot of Ranunculus seed mass, n: 383, shapiro-p: < 0.0001",
          xlab = "Dry Seed Mass")

# Wilcox Rank-Sum Test of "dry_mass" - `Bagged` vs `Open` treatment
ra.d.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(dry_mass ~ treatment, alternative="less") %>%
  add_significance()
ra.d.treatment.wilcox # p: 1.07e-20


# ----- Response variable: "num_nutlets" -----
# Shapiro-test 
shapiro.test(RA_data$num_nutlets) # p: 1.647e-11 => Not norm-dist --> Try remove outliers

# Density plot and QQ-plot
ggqqplot(RA_data$num_nutlets)
ggdensity(RA_data$num_nutlets, 
          main = "Density plot of Ranunculus number of nutlets, n: 383, shapiro-p: < 0.0001",
          xlab = "Number of Nutlets")

# Wilcox Rank-Sum Test of "num_nutlets" - `Bagged` vs `Open` treatment
ra.n.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(num_nutlets ~ treatment, alternative="less") %>%
  add_significance()
ra.n.treatment.wilcox # p: < 0.001


# ----- Response variable: "fertile_nutlets" -----
# Shapiro-test 
shapiro.test(RA_data$fertile_nutlets) # p:  1.986e-07 => Not norm-dist --> Try remove outliers

# Density plot and QQ-plot
ggqqplot(RA_data$fertile_nutlets)
ggdensity(RA_data$fertile_nutlets, 
          main = "Density plot of Ranunculus fertile seed, n: 383, shapiro-p: < 0.0001",
          xlab = "Fertile Seeds")

# Wilcox Rank-Sum Test of "fertile_nutlets" - `Bagged` vs `Open` treatment
ra.fn.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(fertile_nutlets ~ treatment, alternative="less") %>%
  add_significance()
ra.fn.treatment.wilcox # p: < 0.001


# ----- Response variable: "infertile_nutlets" -----
# Shapiro-test 
shapiro.test(RA_data$infertile_nutlets) # p: 2.2e-16 => Not norm-dist --> Try remove outliers

# Density plot and QQ-plot
ggqqplot(RA_data$infertile_nutlets)
ggdensity(RA_data$infertile_nutlets, 
          main = "Density plot of Ranunculus infertile seed, n: 383, shapiro-p: < 0.0001",
          xlab = "Infertile Seeds")

# Wilcox Rank-Sum Test of "infertile_nutlets" - `Bagged` vs `Open` treatment
ra.in.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(infertile_nutlets ~ treatment, alternative="greater") %>%
  add_significance()
ra.in.treatment.wilcox # p: < 0.001


# ------------------------------------------------------------------------------
# ------------------------------- T. pratense ----------------------------------
# ------------------------------------------------------------------------------

# ----- Response variable: "seed_mass" -----
# Shapiro-test
shapiro.test(TP_data$seed_mass) # p = 2.2e-16 => Not normal distribution

# Density plot and QQ-plot
ggqqplot(TP_data$seed_mass)
ggdensity(TP_data$seed_mass, 
          main = "Density plot of Trifolium seed mass, n: 359, shapiro-p: < 0.0001",
          xlab = "Seed Mass")

# Wilcox Rank-Sum Test of "seed_mass" - `Bagged` vs `Open` treatment
tp.s.treatment.wilcox <- TP_data %>%
  rstatix::wilcox_test(seed_mass ~ treatment, alternative = "less") %>%
  add_significance()
tp.s.treatment.wilcox # p: <0.001


# ----- Response variable: "num_nutlets" -----
# Shapiro-test
shapiro.test(TP_data$num_nutlets) # p = 2.2e-16 => Not normal distribution

# Density plot and QQ-plot
ggqqplot(TP_data$num_nutlets)
ggdensity(TP_data$num_nutlets, 
          main = "Density plot of Trifolium seed, n: 359, shapiro-p: < 0.0001",
          xlab = "Number of nutlets")

# Wilcox Rank-Sum Test of "num_nutlets" - `Bagged` vs `Open` treatment
tp.n.treatment.wilcox <- TP_data %>%
  rstatix::wilcox_test(num_nutlets ~ treatment, alternative = "less") %>%
  add_significance()
tp.n.treatment.wilcox # p: <0.001


# ------------------------------------------------------------------------------
# ------------------------- Creating single plot -------------------------------
# ------------------------------------------------------------------------------


# --------------- Plot for F. ananassa ---------------

# Boxplot of "mass"
fa.plot_fruitmass <-
  ggplot(FA_data, aes(x=treatment, y=mass, fill=treatment)) +
  geom_boxplot(width=0.2, position=position_dodge(0.8), 
               alpha=0.6, show.legend=F) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "treatment", y = "fruit mass [g]") +
  ggtitle("F. ananassa | Wilcoxon test", 
          subtitle = get_test_label(fa.treatment.wilcox,
          description = "H1: bagged < open, n-bagged: 30, n-open: 54")) + 
  theme(plot.title = element_text(size=10, face="italic", lineheight=1),
        plot.subtitle = element_text(size=8, lineheight=1),
        axis.title = element_text(size=8, lineheight=1),
        legend.position="none")
fa.plot_fruitmass


# --------------- Plots for C. frutescens ---------------

# Boxplot for "fruit_mass" Wilcox test
cf.plot_fruitmass <-
  ggplot(CF_data, aes(x =treatment, y=fruit_mass, fill=treatment)) +
  geom_boxplot(width=0.2, position=position_dodge(0.8), alpha=0.6, show.legend=F) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "treatment", y = "fruit mass [g]") +
  ggtitle("C. frutescens | Wilcoxon test", subtitle = get_test_label(cf.f.treatment.wilcox,
          description = "H1: bagged < open, n-bagged: 27, n-open: 108")) + 
  theme(plot.title = element_text(size=10, face="italic", lineheight=1),
        plot.subtitle = element_text(size=8, lineheight=1),
        axis.title = element_text(size=8, lineheight=1),
        legend.position = "none") 
cf.plot_fruitmass

# Boxplot for "seed_mass" Wilcox test
cf.plot_seedmass <-
  ggplot(CF_data, aes(x=treatment, y=seed_mass, fill=treatment)) +
  geom_boxplot(width=0.1, position=position_dodge(0.8), alpha=0.6, show.legend=F) +  
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "treatment", y = "seeds mass [g]") +
  ggtitle("C. frutescens | Wilcoxon test", subtitle = get_test_label(cf.s.treatment.wilcox,
         description = "H1: bagged < open, n-bagged: 27, n-open: 108")) + 
  theme(plot.title = element_text(size=10, face="italic", lineheight=1),
        plot.subtitle = element_text(size=8, lineheight=1),
        axis.title = element_text(size=8, lineheight=1),
        legend.position = "none") 
cf.plot_seedmass

# Boxplot for "num_nutlets" Wilcox test
cf.plot_numseed <-
  ggplot(CF_data, aes(x=treatment, y=num_nutlets, fill=treatment)) +
  geom_boxplot(width=0.1, position=position_dodge(0.8), alpha=0.6, show.legend=F) +  
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "treatment", y = "no. of seeds") +
  ggtitle("C. frutescens | Wilcoxon test",subtitle = get_test_label(cf.n.treatment.wilcox,
         description = "H1: bagged < open, n-bagged: 27, n-open: 108")) + 
  theme(plot.title = element_text(size=10, face="italic", lineheight=1),
        plot.subtitle = element_text(size=8, lineheight=1),
        axis.title = element_text(size=8, lineheight=1),
        legend.position = "none") 
cf.plot_numseed


# -------------- Plots for R. acris ---------------

# Boxplot for "dry_mass" Wilcox test
ra.plot_drymass <-
  ggplot(RA_data, aes(x=treatment, y=dry_mass, fill=treatment)) +
  geom_boxplot(width=0.2, position=position_dodge(0.8), 
               alpha=0.6, show.legend=F) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "treatment", y = "seeds mass [g]") +
  ggtitle("R. acris | Wilcoxon test", subtitle = get_test_label(ra.d.treatment.wilcox,
          description = "H1: bagged < open, n-bagged: 52, n-open: 331")) + 
  theme(plot.title = element_text(size=10, face="italic", lineheight=1),
        plot.subtitle = element_text(size=8, lineheight=1),
        axis.title = element_text(size=8, lineheight=1),
        legend.position = "none") 
ra.plot_drymass

# Boxplot for "num_nutlets" Wilcox test
ra.plot_numseed <-
  ggplot(RA_data, aes(x=treatment, y=num_nutlets, fill=treatment)) +
  geom_boxplot(width=0.2, position=position_dodge(0.8), 
               alpha=0.6, show.legend=F) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "treatment", y = "no. of seeds") +
  ggtitle("R. acris | Wilcoxon test", subtitle = get_test_label(ra.n.treatment.wilcox,
          description = "H1: bagged < open, n-bagged: 52, n-open: 331")) + 
  theme(plot.title = element_text(size=10, face="italic", lineheight=1),
        plot.subtitle = element_text(size=8, lineheight=1),
        axis.title = element_text(size=8, lineheight=1),
        legend.position = "none") 
ra.plot_numseed

# Boxplot for "fertile_nutlets" Wilcox test
ra.plot_fertseed <-
  ggplot(RA_data, aes(x=treatment, y=fertile_nutlets,fill=treatment)) +
  geom_boxplot(width=0.2, position=position_dodge(0.8), 
               alpha=0.6, show.legend=F) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "treatment", y = "no. of fertile seeds") +
  ggtitle("R. acris | Wilcoxon test", subtitle = get_test_label(ra.fn.treatment.wilcox,
          description = "H1: bagged < open, n-bagged: 52, n-open: 331")) + 
  theme(plot.title = element_text(size=10, face="italic", lineheight=1),
        plot.subtitle = element_text(size=8, lineheight=1),
        axis.title = element_text(size=8, lineheight=1),
        legend.position = "none") 
ra.plot_fertseed

# Boxplot for "infertile_nutlets" Wilcox test
ra.plot_infertseed <-
  ggplot(RA_data, aes(x= treatment, y = infertile_nutlets, fill = treatment)) +
  geom_boxplot(width = 0.2, position = position_dodge(0.8), alpha = 0.6, show.legend=F) +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "treatment", y = "no. of infertile seeds") +
  ggtitle("R. acris | Wilcoxon test",
          subtitle = get_test_label(ra.in.treatment.wilcox,
          description = "H1: bagged < open, n-bagged: 52, n-open: 331")) + 
  theme(plot.title = element_text(size=10, face="italic", lineheight=1),
        plot.subtitle = element_text(size=8, lineheight=1),
        axis.title = element_text(size=8, lineheight=1),
        legend.position = "none") 
ra.plot_infertseed


# --------------- Plots for T. pratense --------------

# Boxplot for "seed_mass" Wilcox test
tp.plot_seedmass <-
  ggplot(TP_data, aes(x=treatment, y=seed_mass, fill=treatment)) +
  geom_boxplot(width = 0.1, position = position_dodge(0.8), 
               alpha = 0.6, show.legend=F) +  
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "treatment", y = "seeds mass [g]") +
  ggtitle("T. pratense | Wilcoxon test", subtitle = get_test_label(tp.s.treatment.wilcox,
          description = "H1: bagged < open, n-bagged: 74, n-open: 285")) + 
  theme(plot.title = element_text(size=10, face="italic", lineheight=1),
        plot.subtitle = element_text(size=8, lineheight=1),
        axis.title = element_text(size=8, lineheight=1),
        legend.position = "none") 
tp.plot_seedmass

# Boxplot for "num_nutlets" Wilcox test
tp.plot_numseed <-
  ggplot(TP_data, aes(x=treatment, y=num_nutlets, fill=treatment)) +
  geom_boxplot(width=0.1, position=position_dodge(0.8), 
               alpha=0.6, show.legend=F) +  
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  labs(x = "treatment", y = "no. of seeds") +
  ggtitle("T. pratense | Wilcoxon test", subtitle = get_test_label(tp.n.treatment.wilcox,
          description = "H1: bagged < open, n-bagged: 74, n-open: 285")) + 
  theme(plot.title = element_text(size=10, face="italic", lineheight=1),
        plot.subtitle = element_text(size=8, lineheight=1),
        axis.title = element_text(size=8, lineheight=1),
        legend.position = "none") 
tp.plot_numseed


# ------------------------------------------------------------------------------
# --------------------------- Creating combined plots --------------------------
# ------------------------------------------------------------------------------

# Combined plot for cultivate species
cultivate_plot <-
  ggarrange(fa.plot_fruitmass, cf.plot_fruitmass, 
            cf.plot_seedmass, cf.plot_numseed
            + rremove("x.text"), nrow = 2, ncol = 2,
            labels = c("A", "B", "C", "D"), 
            font.label=list(size=10))

annotate_figure(cultivate_plot) #, 
                #top=text_grob("Cultivated species | Wilcoxon test\n",
                #color="#D55E00", face="italic", size=10, lineheight=1))

# Combined plot for wild species
wild_plot <-
  ggarrange(ra.plot_drymass, ra.plot_numseed,
            ra.plot_fertseed, ra.plot_infertseed,
            tp.plot_seedmass, tp.plot_numseed
            + rremove("x.text"), nrow = 3, ncol = 2,
            labels = c("A", "B", "C", "D", "E", "F"), 
            font.label=list(size=10))

annotate_figure(wild_plot) #, 
                #top = text_grob("Wild species | Wilcoxon test\n",
                #color="#D55E00", face="italic", size=10, lineheight=1))


# ------------------------------------------------------------------------------

# -- Clean-up environment for the next script ----
rm(list=ls())

