# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "openxlsx", "vegan", "lubridate")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir and file paths
# pdir = "C:/Users/hienn/Desktop/StatisticalAnalysis"
pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set paths for the data
veg_path = "./unprocessed_data/Vegetation_Data_2021_raw.xlsx"
imperv_path = "./unprocessed_data/Imperviousness_Data_2021_raw.xlsx"
polobs_path = "./unprocessed_data/PolObser_Data_2021_raw.xlsx"
env_path = "./unprocessed_data/Env_Data_2021_raw.xlsx"


# Reading the excel data files as dataframes
Veg_raw <- read_excel(veg_path, sheet = 1)
Imperv_raw <- read_excel(imperv_path, sheet = 1)
PolObser_raw <- read_excel(polobs_path, sheet = 1)
Env_raw <- lapply(excel_sheets(env_path), read_excel, path = env_path)


# List of sheet names for saving later
list_of_sheetnames <- list("ES", "FG", "GG", "GP", "IGF", "KG", "OB", 
                           "OBZ", "OPI", "PAS", "SA", "SG", "SS")


# Work with Env_data - cleaning & exporting the data
str(Env_raw)

Env_raw <- lapply(Env_raw, function(x) {
  x %>%
    mutate(Datetime = ymd_hms(Datetime)) %>%
    group_by(Date) %>%
    mutate(temp = mean(Temp_Celsius), lux = mean(Intensity_Lux)) %>%
    ungroup() %>%
    select(garden_Name, Date, temp, lux) %>%
    distinct()
})


# Combing Env_data & Imperv_data then export the data
Env_Imperv_data <- lapply(Env_raw, function(x) {
  x %>%
    inner_join(Imperv_raw, by = c("garden_Name"))
})

Env_Imperv_data <- lapply(Env_Imperv_data, function(x) {
  x %>%
    mutate(urbanclass100 = case_when(imperv100 <= 0.6 ~ "Low", 
                                imperv100 > 0.6 ~ "High"),
           urbanclass200 = case_when(imperv200 <= 0.6 ~ "Low", 
                                     imperv200 > 0.6 ~ "High"),
           urbanclass500 = case_when(imperv500 <= 0.6 ~ "Low", 
                                     imperv500 > 0.6 ~ "High"),
           urbanclass1000 = case_when(imperv1000 <= 0.6 ~ "Low", 
                                      imperv1000 > 0.6 ~ "High")) %>%
    select(-garden_ID)
})

Env_Imperv_export <- paste("./Processed_data/Env_Imperv_Data_2021_processed.xlsx", sep = "")
write.xlsx(Env_Imperv_data, Env_Imperv_export, sheetName = list_of_sheetnames, append = TRUE)


# Work with PolObsers
PolObser_data <- PolObser_raw %>%
  select(-garden_ID) %>%
  mutate(pol_abundance = rowSums(.[6:(ncol(.)-1)])) %>%
  group_by(campaign, garden_Name) %>%
  summarise(across(bumblebee:pol_abundance, ~ sum(.x, na.rm = TRUE))) %>%
  ungroup %>%
  mutate(pol_richness = apply(.[3:(ncol(.)-1)] > 0, 1, sum))

PolObser_data <- PolObser_data %>%
  mutate(pol_shannon = diversity(PolObser_data[,c(3:(ncol(.)-2))])) %>%
  select(campaign, garden_Name, pol_abundance, pol_richness, pol_shannon)

PolObser_export <- paste("./Processed_data/PolObser_Data_2021_processed.xlsx", sep = "")
write.xlsx(PolObser_data, PolObser_export, append = TRUE)


# Work with Vegetation
Veg_data <- Veg_raw %>%
  select(-c(garden_ID)) %>%
  na.omit() %>%
  group_by(garden_Name, campaign) %>%
  summarise(across(white_flowers:flo_abundance, ~ sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  relocate(c("flo_richness", "flo_abundance"), .after = "campaign")

Veg_data <- Veg_data %>%
  mutate(flo_shannon = diversity(Veg_data[,c(3:(ncol(.)-2))])) %>%
  select(campaign, garden_Name, flo_abundance, flo_richness, flo_shannon)

Veg_export <- paste("./Processed_data/Veg_Data_2021_processed.xlsx", sep = "")
write.xlsx(Veg_data, Veg_export, append = TRUE)

# ---------------------------------------------------------------------------- #

# Clean-up environment for the next script
rm(list = c('list_packages', 'list_of_sheetnames', 'pdir',
            'Veg_raw', 'PolObser_raw', 'Env_raw', 'Imperv_raw',
            'Env_Imperv_data', 'PolObser_data', 'Veg_data'))
rm(list = c('env_path', 'imperv_path', 'polobs_path', 'veg_path',
            'Env_Imperv_export', 'Veg_export', 'PolObser_export'))
