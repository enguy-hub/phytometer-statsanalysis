# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "openxlsx")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir and file paths
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set paths for the data
phytom_path = "./processed_data/Phytometers_Data_2021_processed.xlsx"
env_imperv_path = "./Processed_data/Env_Imperv_Data_2021_processed.xlsx"
polobs_path = "./Processed_data/PolObser_Data_2021_processed.xlsx"
veg_path = "./Processed_data/Veg_Data_2021_processed.xlsx"


# Reading the excel data files as dataframes
CF_raw <- read_excel(phytom_path, sheet = 4)
Env_Imperv_raw <- lapply(excel_sheets(env_imperv_path), read_excel, path = env_imperv_path)
PolObser_raw <- read_excel(polobs_path, sheet = 1)
Veg_raw <- read_excel(veg_path, sheet = 1)


# ---------------------------------------------------------------------------- #


# Create dataframes for PolObser and Veg June
PolObser_Aug <- PolObser_raw %>%
  filter(campaign == "August")
Veg_Aug <- Veg_raw %>%
  filter(campaign == "August")


# Preprocessing CF data
CF_data <- CF_raw %>%
  na.omit() %>%
  dplyr::select(-garden_ID) %>%
  filter(garden_Name != "duernast")


# Joining CF_data with Env_Imperv_data
str(CF_data)

CF_EnvImperv_raw <- lapply(Env_Imperv_raw, function(x) {
  x %>%
    filter(Date >= as.Date("2021-08-09") & Date <= as.Date("2021-08-31")) %>%
    mutate(temp = mean(temp), lux = mean(lux)) %>%
    dplyr::select(-Date) %>%
    distinct() %>%
    mutate(DateStart = "2021-08-09", DateEnd = "2021-08-31", .before = "garden_Name") %>%
    inner_join(CF_data, by = c("garden_Name")) %>%
    relocate(c("temp", "lux", "imperv100", 'imperv200', 'imperv500', "imperv1000"), .after = "num_nutlets")
})


# Combining all tibbles into one dataframe
CF_EnvImperv_data <- bind_rows(CF_EnvImperv_raw)


# Create treatment and garden variables
CF_EnvImperv_data <- CF_EnvImperv_data %>%
  mutate(treatment = case_when(grepl("_C", fruit_ID) ~ "Bagged", 
                               grepl("_O", fruit_ID) ~ "Open"), .after = "urbanclass1000") %>%
  dplyr::select(-c("height", "width"))


# Joining Pollination Observation data
CF_EnvImperv_data <- CF_EnvImperv_data %>%
  inner_join(PolObser_Aug, by = c("garden_Name")) %>%
  dplyr::select(-campaign) %>%
  inner_join(Veg_Aug, by = c("garden_Name")) %>%
  dplyr::select(-campaign) %>%
  relocate(c("flo_abundance", "flo_richness", "flo_shannon"), .after = "pol_shannon")


# ---------------------------------------------------------------------------- #


# -- Exporting analysis ready excel file ----
cf_binded_export <- paste("./analysis_data/CF/CF_Data_2021_4analysis.xlsx", sep = "")
write.xlsx(CF_EnvImperv_data, cf_binded_export, append = TRUE)


# -- Clean-up environment for the next script ---- 
rm(list=ls())
