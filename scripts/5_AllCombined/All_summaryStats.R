# ------------------------------------------------------------------------------
# -------------------------- Prerequisites -------------------------------------
# ------------------------------------------------------------------------------

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "openxlsx", 
                   "ggplot2", "ggpubr", "rstatix", "jtools", "coin")
lapply(list_packages, library, character.only=T)

# Set path and read the data
veg_path = "./unprocessed_data/Vegetation_Data_2021_raw.xlsx"
fa_path = "./analysis_data/FA/FA_Data_2021_4analysis.xlsx"
cf_path = "./analysis_data/CF/CF_Data_2021_4analysis.xlsx"
ra_path = "./analysis_data/RA/RA_Data_2021_4analysis.xlsx"
tp_path = "./analysis_data/TP/TP_Data_2021_4analysis.xlsx"


# Create dataframes
Veg_raw <- read_excel(veg_path, sheet=1)
FA_data <- read_excel(fa_path, sheet=1)
RA_data <- read_excel(ra_path, sheet=1)
TP_data <- read_excel(tp_path, sheet=1)
CF_data <- read_excel(cf_path, sheet=1)


# ------------------------------------------------------------------------------
# -------------------------- Summary Stats - All -------------------------------
# ------------------------------------------------------------------------------

# ----- Summary stats: All -----
get_summary_stats(Veg_raw)
get_summary_stats(FA_data)
get_summary_stats(RA_data)
get_summary_stats(TP_data)
get_summary_stats(CF_data)


# ------------------------------------------------------------------------------
# ------------------- Summary Stats - Floral resources -------------------------
# ------------------- Mean, median, Interquartile range ------------------------ 
# ------------------------------------------------------------------------------

#------------------------------------------------------
#-------------- All gardens - campaign ----------------
#------------------------------------------------------
Veg_camp_sum <- Veg_raw %>%
  group_by(campaign) %>%
  summarise_at(c("flo_richness", "flo_abundance"), sum, na.rm = TRUE)
Veg_camp_sum


#------------------------------------------------------
#--------------- All gardens - gardens ----------------
#------------------------------------------------------
Veg_gard_sum <- Veg_raw %>%
  group_by(garden_ID, campaign) %>%
  summarise_at(c("flo_richness", "flo_abundance"), sum, na.rm = TRUE)
print(Veg_gard_sum, n=52)



# ------------------------------------------------------------------------------
# ---------------------- Summary Stats - Reprod Vars ---------------------------
# ------------------- Mean, median, Interquartile range ------------------------ 
# ------------------------------------------------------------------------------

#------------------------------------------------------
#------------- F. ananassa - fruit mass ---------------
#------------------------------------------------------
FA_f_meanSD <- FA_data %>%
  group_by(treatment) %>%
  get_summary_stats(mass, type="mean_sd")
FA_f_meanSD

FA_f_meanSE <- FA_data %>%
  group_by(treatment) %>%
  get_summary_stats(mass, type="mean_se")
FA_f_meanSE

FA_f_medianIQR <- FA_data %>%
  group_by(treatment) %>%
  get_summary_stats(mass, type="median_iqr")
FA_f_medianIQR


#------------------------------------------------------
# ----------- C. frutescens - fruit mass --------------
#------------------------------------------------------
CF_f_meanSD <- CF_data %>%
  group_by(treatment) %>%
  get_summary_stats(fruit_mass, type="mean_sd")
CF_f_meanSD

CF_f_meanSE <- CF_data %>%
  group_by(treatment) %>%
  get_summary_stats(fruit_mass, type="mean_se")
CF_f_meanSE

CF_f_medianIQR <- CF_data %>%
  group_by(treatment) %>%
  get_summary_stats(fruit_mass, type="median_iqr")
CF_f_medianIQR


#------------------------------------------------------
# ------------- C. frutescens - seed mass -------------
#------------------------------------------------------
CF_s_meanSD <- CF_data %>%
  group_by(treatment) %>%
  get_summary_stats(seed_mass, type="mean_sd")
CF_s_meanSD

CF_s_meanSE <- CF_data %>%
  group_by(treatment) %>%
  get_summary_stats(seed_mass, type="mean_se")
CF_s_meanSE

CF_s_medianIQR <- CF_data %>%
  group_by(treatment) %>%
  get_summary_stats(seed_mass, type="median_iqr")
CF_s_medianIQR


#------------------------------------------------------
# ----------- C. frutescens - num nutlets -------------
#------------------------------------------------------
CF_n_meanSD <- CF_data %>%
  group_by(treatment) %>%
  get_summary_stats(num_nutlets, type="mean_sd")
CF_n_meanSD

CF_n_meanSE <- CF_data %>%
  group_by(treatment) %>%
  get_summary_stats(num_nutlets, type="mean_se")
CF_n_meanSE

CF_n_medianIQR <- CF_data %>%
  group_by(treatment) %>%
  get_summary_stats(num_nutlets, type="median_iqr")
CF_n_medianIQR


#------------------------------------------------------
# -------------- R. acris - fruit mass ----------------
#------------------------------------------------------
RA_f_meanSD <- RA_data %>%
  group_by(treatment) %>%
  get_summary_stats(fresh_mass, type="mean_sd")
RA_f_meanSD

RA_f_meanSE <- RA_data %>%
  group_by(treatment) %>%
  get_summary_stats(fresh_mass, type="mean_se")
RA_f_meanSE

RA_f_medianIQR <- RA_data %>%
  group_by(treatment) %>%
  get_summary_stats(fresh_mass, type="median_iqr")
RA_f_medianIQR


#------------------------------------------------------
# -------------- R. acris - seed mass -----------------
#------------------------------------------------------
RA_s_meanSD <- RA_data %>%
  group_by(treatment) %>%
  get_summary_stats(dry_mass, type="mean_sd")
RA_s_meanSD

RA_s_meanSE <- RA_data %>%
  group_by(treatment) %>%
  get_summary_stats(dry_mass, type="mean_se")
RA_s_meanSE

RA_s_medianIQR <- RA_data %>%
  group_by(treatment) %>%
  get_summary_stats(dry_mass, type="median_iqr")
RA_s_medianIQR


#------------------------------------------------------
# ------------- R. acris - num nutlets ----------------
#------------------------------------------------------
RA_n_meanSD <- RA_data %>%
  group_by(treatment) %>%
  get_summary_stats(num_nutlets, type="mean_sd")
RA_n_meanSD

RA_n_meanSE <- RA_data %>%
  group_by(treatment) %>%
  get_summary_stats(num_nutlets, type="mean_se")
RA_n_meanSE

RA_n_medianIQR <- RA_data %>%
  group_by(treatment) %>%
  get_summary_stats(num_nutlets, type="median_iqr")
RA_n_medianIQR


#------------------------------------------------------
#------------ R. acris - fertile nutlets --------------
#------------------------------------------------------
RA_fn_meanSD <- RA_data %>%
  group_by(treatment) %>%
  get_summary_stats(fertile_nutlets, type="mean_sd")
RA_fn_meanSD

RA_fn_meanSE <- RA_data %>%
  group_by(treatment) %>%
  get_summary_stats(fertile_nutlets, type="mean_se")
RA_fn_meanSE

RA_fn_medianIQR <- RA_data %>%
  group_by(treatment) %>%
  get_summary_stats(fertile_nutlets, type="median_iqr")
RA_fn_medianIQR


#------------------------------------------------------
#----------- R. acris - infertile nutlets -------------
#------------------------------------------------------
RA_in_meanSD <- RA_data %>%
  group_by(treatment) %>%
  get_summary_stats(infertile_nutlets, type="mean_sd")
RA_in_meanSD

RA_in_meanSE <- RA_data %>%
  group_by(treatment) %>%
  get_summary_stats(infertile_nutlets, type="mean_se")
RA_in_meanSE

RA_in_medianIQR <- RA_data %>%
  group_by(treatment) %>%
  get_summary_stats(infertile_nutlets, type="median_iqr")
RA_in_medianIQR


#------------------------------------------------------
# ------------ T. pratense - fruit mass ---------------
#------------------------------------------------------
TP_f_meanSD <- TP_data %>%
  group_by(treatment) %>%
  get_summary_stats(flower_mass, type="mean_sd")
TP_f_meanSD

TP_f_meanSE <- TP_data %>%
  group_by(treatment) %>%
  get_summary_stats(flower_mass, type="mean_se")
TP_f_meanSE

TP_f_medianIQR <- TP_data %>%
  group_by(treatment) %>%
  get_summary_stats(flower_mass, type="median_iqr")
TP_f_medianIQR


#------------------------------------------------------
# ------------ T. pratense - seed mass ----------------
#------------------------------------------------------
TP_s_meanSD <- TP_data %>%
  group_by(treatment) %>%
  get_summary_stats(seed_mass, type="mean_sd")
TP_s_meanSD

TP_s_meanSE <- TP_data %>%
  group_by(treatment) %>%
  get_summary_stats(seed_mass, type="mean_se")
TP_s_meanSE

TP_s_medianIQR <- TP_data %>%
  group_by(treatment) %>%
  get_summary_stats(seed_mass, type="median_iqr")
TP_s_medianIQR


#------------------------------------------------------
# ---------- T. pratense - num nutlets ----------------
#------------------------------------------------------
TP_n_meanSD <- TP_data %>%
  group_by(treatment) %>%
  get_summary_stats(num_nutlets, type="mean_sd")
TP_n_meanSD

TP_n_meanSE <- TP_data %>%
  group_by(treatment) %>%
  get_summary_stats(num_nutlets, type="mean_se")
TP_n_meanSE

TP_n_medianIQR <- TP_data %>%
  group_by(treatment) %>%
  get_summary_stats(num_nutlets, type="median_iqr")
TP_n_medianIQR



# ------------------------------------------------------------------------------

# -- Clean-up environment for the next script ----
rm(list=ls())

