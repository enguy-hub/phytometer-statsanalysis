# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "openxlsx")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir and file paths
pdir = "C:/Garden/MyGithub/Phytometer_StatisticalAnalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set paths for the data
phytom_path = "./processed_data/Phytometers_Data_2021_processed.xlsx"
env_imperv_path = "./processed_data/Env_Imperv_Data_2021_processed.xlsx"
polobs_path = "./processed_data/PolObser_Data_2021_processed.xlsx"
veg_path = "./processed_data/Veg_Data_2021_processed.xlsx"


# Reading the excel data files as dataframes
RA_raw <- read_excel(phytom_path, sheet = 2)
Env_Imperv_raw <- lapply(excel_sheets(env_imperv_path), read_excel, path = env_imperv_path)
PolObser_raw <- read_excel(polobs_path, sheet = 1)
Veg_raw <- read_excel(veg_path, sheet = 1)


# ---------------------------------------------------------------------------- #


# Dataframes for PolObser and Veg June
PolObser_Jul <- PolObser_raw %>%
  filter(campaign == "July")
Veg_Jul <- Veg_raw %>%
  filter(campaign == "July")


# Preprocessing RA_data
RA_data <- RA_raw %>%
  na.omit() %>%
  dplyr::select(-garden_ID) %>%
  filter(garden_Name != "duernast")


# Joining RA_data with Env_Imperv_data
RA_EnvImperv_raw <- lapply(Env_Imperv_raw, function(x) {
  x %>%
    filter(Date >= as.Date("2021-06-30") & Date < as.Date("2021-07-24")) %>%
    mutate(temp = mean(temp), lux = mean(lux)) %>%
    dplyr::select(-Date) %>%
    distinct() %>%
    mutate(DateStart = "2021-06-30", DateEnd = "2021-07-23", .before = "garden_Name") %>%
    inner_join(RA_data, by = c("garden_Name")) %>%
    relocate(c("temp", "lux", "imperv100", "imperv200", "imperv500", "imperv1000"), .after = "infertile_nutlets")
})


# Combining all tibbles into one dataframe
RA_EnvImperv_data <- bind_rows(RA_EnvImperv_raw)

# Create treatment and sum variables
RA_EnvImperv_data <- RA_EnvImperv_data %>%
  mutate(treatment = case_when(grepl("_C", seed_ID) ~ "Bagged", 
                               grepl("_O", seed_ID) ~ "Open"), .after = "urbanclass1000") 
  
# Joining Pollination Observation data
RA_EnvImperv_data <- RA_EnvImperv_data %>%
  inner_join(PolObser_Jul, by = c("garden_Name")) %>%
  dplyr::select(-campaign) %>%
  inner_join(Veg_Jul, by = c("garden_Name")) %>%
  dplyr::select(-campaign) %>%
  relocate(c("flo_abundance", "flo_richness", "flo_shannon"), .after = "pol_shannon")


# ---------------------------------------------------------------------------- #


# -- Exporting analysis ready excel file ----
ra_binded_export <- paste("./analysis_data/RA/RA_Data_2021_4analysis.xlsx", sep = "")
write.xlsx(RA_EnvImperv_data, ra_binded_export, append = TRUE)


# -- Clean-up environment for the next script ----
rm(list=ls())
