# ------------------------------------------------------------------------------
# -------------------------- Prerequisites -------------------------------------
# ------------------------------------------------------------------------------

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "openxlsx", 
                   "ggplot2", "ggpubr", "rstatix")
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
          main = "Density plot of Fragaria fruit mass, n=84, shapiro-p=0.0005",
          xlab = "Fruit mass")

# Wilcox Rank-Sum Test of "mass" - `Bagged` vs `Open` treatment
fa.treatment.wilcox <- FA_data %>%
  rstatix::wilcox_test(mass ~ treatment, alternative="less", detailed=T) %>%
  add_significance()
fa.treatment.wilcox # p: 0.0733

# Effect size of Wilcox test
fa.treatment.wilcox.effsize <- FA_data %>%
  rstatix::wilcox_effsize(mass ~ treatment, alternative="less")
fa.treatment.wilcox.effsize # p: 0.159 (small)



# ------------------------------------------------------------------------------
# ------------------------------ C. frutescens ---------------------------------
# ------------------------------------------------------------------------------

# ----- Response variable: "fruit_mass" -----
# Shapiro-test 
shapiro.test(CF_data$fruit_mass) # p = 1.686e-14 => NOT normal distribution

# Density plot and QQ-plot
ggqqplot(CF_data$fruit_mass)
ggdensity(CF_data$fruit_mass, 
          main = "Density plot of Capsicum fruit mass, n=135, shapiro-p < 0.0001",
          xlab = "Fruit Mass")

# Wilcox Rank-Sum Test of "fruit_mass" - `Bagged` vs `Open` treatment
cf.f.treatment.wilcox <- CF_data %>%
  rstatix::wilcox_test(fruit_mass ~ treatment, alternative="less", detailed=T) %>%
  add_significance()
cf.f.treatment.wilcox # p: <0.001

# Effect size of Wilcox test
cf.f.treatment.wilcox.effsize <- CF_data %>%
  rstatix::wilcox_effsize(fruit_mass ~ treatment, alternative="less")
cf.f.treatment.wilcox.effsize # effsize: 0.515 (large)



# ---- Response variable: "seed_mass" ----
# Shapiro-test
shapiro.test(CF_data$seed_mass) # p = 8.191e-05 => NOT normal distribution

# Density plot and QQ-plot
ggqqplot(CF_data$seed_mass)
ggdensity(CF_data$seed_mass, 
          main = "Density plot of Capsicum seed mass, n=135, shapiro-p < 0.0001",
          xlab = "Seed Mass")

# Wilcox Rank-Sum Test of "seed_mass" - `Bagged` vs `Open`
cf.s.treatment.wilcox <- CF_data %>%
  rstatix::wilcox_test(seed_mass ~ treatment, alternative="less", detailed=T) %>%
  add_significance()
cf.s.treatment.wilcox # p: <0.001

# Effect size of Wilcox test
cf.s.treatment.wilcox.effsize <- CF_data %>%
  rstatix::wilcox_effsize(seed_mass ~ treatment, alternative="less")
cf.s.treatment.wilcox.effsize # effsize: 0.571 (large)



# ----- Response variable: "num_nutlets" -----
# Shapiro-test
shapiro.test(CF_data$num_nutlets) # p = 0.0004 => NOT normal distribution

# Density plot and QQ-plot
ggqqplot(CF_data$num_nutlets)
ggdensity(CF_data$num_nutlets, 
          main = "Density plot of Capsicum num seed, n=135, shapiro-p < 0.0001",
          xlab = "Number of nutlets")

# Wilcox Rank-Sum Test of "num_nutlets"- `Bagged` vs `Open`
cf.n.treatment.wilcox <- CF_data %>%
  rstatix::wilcox_test(num_nutlets ~ treatment, alternative="less", detailed=T) %>%
  add_significance()
cf.n.treatment.wilcox # p: < 0.001

# Effect size of Wilcox test
cf.n.treatment.wilcox.effsize <- CF_data %>%
  rstatix::wilcox_effsize(num_nutlets ~ treatment, alternative="less")
cf.n.treatment.wilcox.effsize # effsize: 0.557 (large)



# ------------------------------------------------------------------------------
# -------------------------------- R. acris ------------------------------------
# ------------------------------------------------------------------------------

# ----- Response variable: "fresh_mass" -----
# Shapiro-test
shapiro.test(RA_data$fresh_mass) # p = 8.625e-12 => Not norm-dist --> Try remove outliers

# Density plot and QQ-plot
ggqqplot(RA_data$fresh_mass)
ggdensity(RA_data$fresh_mass, 
          main = "Density plot of Ranunculus fruit mass, n=383, shapiro-p < 0.0001",
          xlab = "Fruit Mass")

# Wilcox Rank-Sum Test of "fresh_mass" - `Bagged` vs `Open` treatment
ra.f.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(fresh_mass ~ treatment, alternative="less", detailed=T) %>%
  add_significance()
ra.f.treatment.wilcox # p: 1.07e-20

# Effect size of Wilcox test
ra.f.treatment.wilcox.effsize <- RA_data %>%
  rstatix::wilcox_effsize(fresh_mass ~ treatment, alternative="less")
ra.f.treatment.wilcox.effsize # effsize: 0.502 (large)



# ----- Response variable: "dry_mass" -----
# Shapiro-test
shapiro.test(RA_data$dry_mass) # p = 8.625e-12 => Not norm-dist --> Try remove outliers

# Density plot and QQ-plot
ggqqplot(RA_data$dry_mass)
ggdensity(RA_data$dry_mass, 
          main = "Density plot of Ranunculus seed mass, n=383, shapiro-p < 0.0001",
          xlab = "Dry Seed Mass")

# Wilcox Rank-Sum Test of "dry_mass" - `Bagged` vs `Open` treatment
ra.d.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(dry_mass ~ treatment, alternative="less", detailed=T) %>%
  add_significance()
ra.d.treatment.wilcox # p: 1.07e-20

# Effect size of Wilcox test
ra.d.treatment.wilcox.effsize <- RA_data %>%
  rstatix::wilcox_effsize(dry_mass ~ treatment, alternative="less")
ra.d.treatment.wilcox.effsize # effsize: 0.473 (medium)



# ----- Response variable: "num_nutlets" -----
# Shapiro-test 
shapiro.test(RA_data$num_nutlets) # p: 1.647e-11 => Not norm-dist --> Try remove outliers

# Density plot and QQ-plot
ggqqplot(RA_data$num_nutlets)
ggdensity(RA_data$num_nutlets, 
          main = "Density plot of Ranunculus number of nutlets, n=383, shapiro-p < 0.0001",
          xlab = "Number of Nutlets")

# Wilcox Rank-Sum Test of "num_nutlets" - `Bagged` vs `Open` treatment
ra.n.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(num_nutlets ~ treatment, alternative="less", detailed=T) %>%
  add_significance()
ra.n.treatment.wilcox # p: < 0.001

# Effect size of Wilcox test
ra.n.treatment.wilcox.effsize <- RA_data %>%
  rstatix::wilcox_effsize(num_nutlets ~ treatment, alternative="less")
ra.n.treatment.wilcox.effsize # effsize: 0.124 (small)



# ----- Response variable: "fertile_nutlets" -----
# Shapiro-test 
shapiro.test(RA_data$fertile_nutlets) # p:  1.986e-07 => Not norm-dist --> Try remove outliers

# Density plot and QQ-plot
ggqqplot(RA_data$fertile_nutlets)
ggdensity(RA_data$fertile_nutlets, 
          main = "Density plot of Ranunculus fertile seed, n=383, shapiro-p < 0.0001",
          xlab = "Fertile Seeds")

# Wilcox Rank-Sum Test of "fertile_nutlets" - `Bagged` vs `Open` treatment
ra.fn.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(fertile_nutlets ~ treatment, alternative="less", detailed=T) %>%
  add_significance()
ra.fn.treatment.wilcox # p: < 0.001

# Effect size of Wilcox test
ra.fn.treatment.wilcox.effsize <- RA_data %>%
  rstatix::wilcox_effsize(fertile_nutlets ~ treatment, alternative="less")
ra.fn.treatment.wilcox.effsize # effsize: 0.519 (large)



# ----- Response variable: "infertile_nutlets" -----
# Shapiro-test 
shapiro.test(RA_data$infertile_nutlets) # p: 2.2e-16 => Not norm-dist --> Try remove outliers

# Density plot and QQ-plot
ggqqplot(RA_data$infertile_nutlets)
ggdensity(RA_data$infertile_nutlets, 
          main = "Density plot of Ranunculus infertile seed, n=383, shapiro-p < 0.0001",
          xlab = "Infertile Seeds")

# Wilcox Rank-Sum Test of "infertile_nutlets" - `Bagged` vs `Open` treatment
ra.in.treatment.wilcox <- RA_data %>%
  rstatix::wilcox_test(infertile_nutlets ~ treatment, alternative="greater", detailed=T) %>%
  add_significance()
ra.in.treatment.wilcox # p: < 0.001

# Effect size of Wilcox test
ra.in.treatment.wilcox.effsize <- RA_data %>%
  rstatix::wilcox_effsize(infertile_nutlets ~ treatment, alternative="less")
ra.in.treatment.wilcox.effsize # effsize: 0.368 (mod)



# ------------------------------------------------------------------------------
# ------------------------------- T. pratense ----------------------------------
# ------------------------------------------------------------------------------

# ----- Response variable: "flower_mass" -----
# Shapiro-test
shapiro.test(TP_data$flower_mass) # p = 4.046e-11 => Not normal distribution

# Density plot and QQ-plot
ggqqplot(TP_data$flower_mass)
ggdensity(TP_data$flower_mass, 
          main = "Density plot of Trifolium fruit mass, n=359, shapiro-p < 0.0001",
          xlab = "Fruit Mass")

# Wilcox Rank-Sum Test of "flower_mass" - `Bagged` vs `Open` treatment
tp.f.treatment.wilcox <- TP_data %>%
  rstatix::wilcox_test(flower_mass ~ treatment, alternative = "less", detailed=T) %>%
  add_significance()
tp.f.treatment.wilcox # p: <0.001

# Effect size of Wilcox test
tp.f.treatment.wilcox.effsize <- TP_data %>%
  rstatix::wilcox_effsize(flower_mass ~ treatment, alternative="less")
tp.f.treatment.wilcox.effsize # effsize: 0.272 (small)



# ----- Response variable: "seed_mass" -----
# Shapiro-test
shapiro.test(TP_data$seed_mass) # p = 2.2e-16 => Not normal distribution

# Density plot and QQ-plot
ggqqplot(TP_data$seed_mass)
ggdensity(TP_data$seed_mass, 
          main = "Density plot of Trifolium seed mass, n=359, shapiro-p < 0.0001",
          xlab = "Seed Mass")

# Wilcox Rank-Sum Test of "seed_mass" - `Bagged` vs `Open` treatment
tp.s.treatment.wilcox <- TP_data %>%
  rstatix::wilcox_test(seed_mass ~ treatment, alternative = "less", detailed=T) %>%
  add_significance()
tp.s.treatment.wilcox # p: <0.001

# Effect size of Wilcox test
tp.s.treatment.wilcox.effsize <- TP_data %>%
  rstatix::wilcox_effsize(seed_mass ~ treatment, alternative="less")
tp.s.treatment.wilcox.effsize # effsize: 0.546 (large)



# ----- Response variable: "num_nutlets" -----
# Shapiro-test
shapiro.test(TP_data$num_nutlets) # p = 2.2e-16 => Not normal distribution

# Density plot and QQ-plot
ggqqplot(TP_data$num_nutlets)
ggdensity(TP_data$num_nutlets, 
          main = "Density plot of Trifolium seed, n=359, shapiro-p < 0.0001",
          xlab = "Number of nutlets")

# Wilcox Rank-Sum Test of "num_nutlets" - `Bagged` vs `Open` treatment
tp.n.treatment.wilcox <- TP_data %>%
  rstatix::wilcox_test(num_nutlets ~ treatment, alternative="less", detailed=T) %>%
  add_significance()
tp.n.treatment.wilcox # p: <0.001

# Effect size of Wilcox test
tp.n.treatment.wilcox.effsize <- TP_data %>%
  rstatix::wilcox_effsize(num_nutlets ~ treatment, alternative="less", detailed=T)
tp.n.treatment.wilcox.effsize # effsize: 0.577 (large)



# ------------------------------------------------------------------------------
# --------------------- Creating plots main RespVars ---------------------------
# ------------------------------------------------------------------------------

# -------------------------- Plots for publication -----------------------------

# Boxplot of F. ananassa - "fruit mass"
fa.plot_fruitmass <-
  ggplot(FA_data, aes(x=treatment, y=mass, fill=treatment)) +
  geom_boxplot(width=0.3, alpha=0.5, show.legend=F) +
  labs(x = "Treatment", y = "Fruit mass [g]") +
  ggtitle("F. ananassa \n \n",
          subtitle = get_test_label(fa.treatment.wilcox, p.col="p",
          description = "Wilcox | H1: bagged < open,\n \n n-bagged: 30, n-open: 54")) +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold.italic"),
        plot.subtitle = element_text(size=15),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=18),
        legend.position="none")
fa.plot_fruitmass 


# Boxplot for C. frutescens - "seed_mass" Wilcox test
cf.plot_seedmass <-
  ggplot(CF_data, aes(x=treatment, y=seed_mass, fill=treatment)) +
  geom_boxplot(width=0.3, alpha=0.5, show.legend=F) +
  labs(x = "Treatment", y = "Seeds mass [g]") +
  ggtitle("C. frutescens \n \n", 
          subtitle = get_test_label(cf.s.treatment.wilcox, p.col="p",
          description = "Wilcox | H1: bagged < open, \n \n n-bagged: 27, n-open: 108")) + 
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold.italic"),
        plot.subtitle = element_text(size=15),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=18),
        legend.position = "none")
cf.plot_seedmass


# Boxplot for R. acris - "dry_mass" Wilcox test
ra.plot_drymass <-
  ggplot(RA_data, aes(x=treatment, y=dry_mass, fill=treatment)) +
  geom_boxplot(width=0.3, alpha=0.5, show.legend=F) +
  labs(x = "Treatment", y = "Seeds mass [g]") +
  ggtitle("R. acris \n \n", 
          subtitle = get_test_label(ra.d.treatment.wilcox, p.col="p",
          description = "Wilcox | H1: bagged < open, \n \n n-bagged: 52, n-open: 331")) + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold.italic"),
        plot.subtitle = element_text(size=15),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=18),
        legend.position = "none")
ra.plot_drymass


# Boxplot for T. pratense - "seed_mass" Wilcox test
tp.plot_seedmass <-
  ggplot(TP_data, aes(x=treatment, y=seed_mass, fill=treatment)) +
  geom_boxplot(width = 0.3, alpha = 0.5, show.legend=F) +
  labs(x = "Treatment", y = "Seeds mass [g]") +
  ggtitle("T. pratense \n \n", 
          subtitle = get_test_label(tp.s.treatment.wilcox, p.col="p",
          description = "Wilcox | H1: bagged < open, \n \n n-bagged: 74, n-open: 285")) +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold.italic"),
        plot.subtitle = element_text(size=15),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=18),
        legend.position = "none")
tp.plot_seedmass


# Combined plot for cultivate species
publication_plots <-
  ggarrange(fa.plot_fruitmass, cf.plot_seedmass, 
            tp.plot_seedmass, ra.plot_drymass, 
            nrow=1, ncol=4, labels=list("(A)", "(B)", "(C)", "(D)"),
            font.label=list(size=16), vjust=3.2, hjust=-1)
annotate_figure(publication_plots)



# ------------------------- Plots for supplement -------------------------------

# Boxplot for C. frutescens - "fruit_mass" Wilcox test
cf.plot_fruitmass <-
  ggplot(CF_data, aes(x =treatment, y=fruit_mass, fill=treatment)) +
  geom_boxplot(width=0.3, alpha=0.5, show.legend=F) +
  labs(x = "treatment", y = "fruit mass [g]") +
  ggtitle("C. frutescens \n \n", 
          subtitle = get_test_label(cf.f.treatment.wilcox,
          description = "Wilcox | H1: bagged < open, \n \n n-bagged:27, n-open: 108")) + 
  theme_classic() +
  theme(plot.title = element_text(size=18, face="bold.italic"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.subtitle = element_text(size=15),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.position = "none")
cf.plot_fruitmass


# Boxplot for C. frutescens - "num_nutlets" Wilcox test
cf.plot_numseed <-
  ggplot(CF_data, aes(x=treatment, y=num_nutlets, fill=treatment)) +
  geom_boxplot(width=0.3, alpha=0.5, show.legend=F) +
  labs(x = "Treatment", y = "Seeds") +
  ggtitle("C. frutescens \n \n", 
          subtitle = get_test_label(cf.n.treatment.wilcox,
          description = "Wilcox | H1: bagged < open, \n \n n-bagged: 27, n-open: 108")) + 
  theme_classic() + 
  theme(plot.title = element_text(size=18, face="bold.italic"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.subtitle = element_text(size=15),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.position = "none")
cf.plot_numseed


# Boxplot for R. acris - "fresh_mass" Wilcox test
ra.plot_fruitmass <-
  ggplot(RA_data, aes(x=treatment, y=fresh_mass, fill=treatment)) +
  geom_boxplot(width=0.3, alpha=0.5, show.legend=F) +
  labs(x = "Treatment", y = "Fruit mass") +
  ggtitle("R. acris \n \n", 
          subtitle = get_test_label(ra.n.treatment.wilcox,
          description = "\n Wilcox | H1: bagged < open, \n \n n-bagged: 52, n-open: 331")) + 
  theme_classic() +
  theme(plot.title = element_text(size=18, face="bold.italic"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.subtitle = element_text(size=15),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.position = "none")
ra.plot_fruitmass


# Boxplot for R. acris - "num_nutlets" Wilcox test
ra.plot_numseed <-
  ggplot(RA_data, aes(x=treatment, y=num_nutlets, fill=treatment)) +
  geom_boxplot(width=0.3, alpha=0.5, show.legend=F) +
  labs(x = "Treatment", y = "Seeds") +
  ggtitle("R. acris \n \n", 
          subtitle = get_test_label(ra.n.treatment.wilcox,
          description = "\n Wilcox | H1: bagged < open, \n \n n-bagged: 52, n-open: 331")) + 
  theme_classic() +
  theme(plot.title = element_text(size=18, face="bold.italic"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.subtitle = element_text(size=15),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.position = "none")
ra.plot_numseed


# Boxplot for R. acris - "fertile_nutlets" Wilcox test
ra.plot_fertseed <-
  ggplot(RA_data, aes(x=treatment, y=fertile_nutlets,fill=treatment)) +
  geom_boxplot(width=0.3, alpha=0.5, show.legend=F) +
  labs(x = "Treatment", y = "Fertile seeds") +
  ggtitle("R. acris \n \n", 
          subtitle = get_test_label(ra.fn.treatment.wilcox,
          description = "Wilcox | H1: bagged < open, \n \n n-bagged: 52, n-open: 331")) + 
  theme_classic() +
  theme(plot.title = element_text(size=18, face="bold.italic"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.subtitle = element_text(size=15),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.position = "none")
ra.plot_fertseed


# Boxplot for R. acris - "infertile_nutlets" Wilcox test
ra.plot_infertseed <-
  ggplot(RA_data, aes(x= treatment, y = infertile_nutlets, fill = treatment)) +
  geom_boxplot(width = 0.3, alpha = 0.5, show.legend=F) +
  # ylim(0, 30) +
  labs(x = "Treatment", y = "Infertile seeds") +
  ggtitle("R. acris \n \n",
          subtitle = get_test_label(ra.in.treatment.wilcox,
          description = "Wilcox | H1: bagged < open, \n \n n-bagged: 52, n-open: 331")) +
  theme_classic() +
  theme(plot.title = element_text(size=18, face="bold.italic"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.subtitle = element_text(size=15),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.position = "none")
ra.plot_infertseed


# Boxplot for T. pratense - "flower_mass" Wilcox test
tp.plot_fruitmass <-
  ggplot(TP_data, aes(x=treatment, y=flower_mass, fill=treatment)) +
  geom_boxplot(width=0.3, alpha=0.5, show.legend=F) +
  # ylim(0, 75) +
  labs(x = "Treatment", y = "Fruit mass") +
  ggtitle("T. pratense \n \n", 
          subtitle = get_test_label(tp.n.treatment.wilcox,
          description = "Wilcox | H1: bagged < open, \n \n n-bagged: 74, n-open: 285")) +
  theme_classic() +
  theme(plot.title = element_text(size=18, face="bold.italic"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.subtitle = element_text(size=15),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.position = "none")
tp.plot_fruitmass


# Boxplot for T. pratense - "num_nutlets" Wilcox test
tp.plot_numseed <-
  ggplot(TP_data, aes(x=treatment, y=num_nutlets, fill=treatment)) +
  geom_boxplot(width=0.3, alpha=0.5, show.legend=F) +
  # ylim(0, 75) +
  labs(x = "Treatment", y = "Seeds") +
  ggtitle("T. pratense \n \n", 
          subtitle = get_test_label(tp.n.treatment.wilcox,
          description = "Wilcox | H1: bagged < open, \n \n n-bagged: 74, n-open: 285")) +
  theme_classic() +
  theme(plot.title = element_text(size=18, face="bold.italic"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.subtitle = element_text(size=15),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.position = "none")
tp.plot_numseed


# Combined plot for wild species
supple_plots <-
  ggarrange(cf.plot_fruitmass, cf.plot_numseed, tp.plot_fruitmass, tp.plot_numseed,
            ra.plot_fruitmass, ra.plot_numseed, ra.plot_fertseed, ra.plot_infertseed,
            nrow=2, ncol=4, labels=list("(A)", "(B)", "(C)", "(D)", "(E)", "(F)", "(G)", "(H)"),
            font.label=list(size=16), vjust=3.2, hjust=-1)
annotate_figure(supple_plots)


# ------------------------------------------------------------------------------

# -- Clean-up environment for the next script ----
rm(list=ls())

