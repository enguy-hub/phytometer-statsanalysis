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
FA_raw <- read_excel(phytom_path, sheet = 1)
Env_Imperv_raw <- lapply(excel_sheets(env_imperv_path), read_excel, path = env_imperv_path)
PolObser_raw <- read_excel(polobs_path, sheet = 1)
Veg_raw <- read_excel(veg_path, sheet = 1)


# Dataframes of June, July, and August
PolObser_Jun <- PolObser_raw %>%
  filter(campaign == "June")
Veg_Jun <- Veg_raw %>%
  filter(campaign == "June")


# Pre-processing FA_data
FA_data <- FA_raw %>%
  na.omit() %>%
  select(-c(garden_ID, note)) %>%
  filter(garden_Name != "duernast")


# Joining FA_data with Env_Imperv_raw
FA_EnvImperv_raw <- lapply(Env_Imperv_raw, function(x) {
  x %>%
    filter(Date >= as.Date("2021-06-07") & Date < as.Date("2021-07-01")) %>%
    mutate(temp = mean(temp), lux = mean(lux)) %>%
    select(-Date) %>%
    distinct() %>%
    mutate(DateStart = "2021-06-07", DateEnd = "2021-06-30", .before = "garden_Name") %>%
    inner_join(FA_data, by = c("garden_Name")) %>%
    relocate(c("temp", "lux", "imperv100", "imperv200", "imperv500", "imperv1000"), .after = "deform")
})


# Combining all tibbles into one dataframe and more processing
FA_EnvImperv_data <- bind_rows(FA_EnvImperv_raw)

FA_EnvImperv_data <- FA_EnvImperv_data %>%
  mutate(treatment = case_when(grepl("_C", fruit_ID) ~ "Bagged", 
                               grepl("_O", fruit_ID) ~ "Open"), .after = "urbanclass1000") %>%
  relocate(deform, .after = "fruit_ID")


# Joining Pollination Observation and Vegetation data
FA_EnvImperv_data <- FA_EnvImperv_data %>%
  inner_join(PolObser_Jun, by = c("garden_Name")) %>%
  select(-campaign) %>%
  inner_join(Veg_Jun, by = c("garden_Name")) %>%
  select(-campaign) %>%
  relocate(c("flo_abundance", "flo_richness", "flo_shannon"), .after = "pol_shannon")


# ---------------------------------------------------------------------------- #


# -- Exporting analysis ready excel file ----
FA_binded_export <- paste("./analysis_data/FA/FA_Data_2021_4analysis.xlsx", sep = "")
write.xlsx(FA_EnvImperv_data, FA_binded_export, append = TRUE)


# -- Clean-up environment for the next script ----
rm(list=ls())

