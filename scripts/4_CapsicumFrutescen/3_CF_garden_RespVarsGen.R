# -- Prerequisite ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "openxlsx", "car")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Hien/Garden/MyGithub/phytometer-statsanalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
cf_path = "./analysis_data/CF/CF_Data_2021_4analysis.xlsx"
CF_data <- read_excel(cf_path, sheet = 1)


# Check structure and summaries of the datasets
str(CF_data)
summary(CF_data)


# ---------------------------------------------------------------------------- #


# Generate new response vars
CF_garden <- CF_data %>%
  group_by(garden_Name, treatment) %>%
  summarise(fruimass_t = mean(fruit_mass),
            seedmass_t = mean(seed_mass),
            ratio_t = mean(ratio),
            mass_pseed_t = mean(avg_mass_pseed)) %>%
  mutate(fruimass_meandiff = fruimass_t[2] - fruimass_t[1],
         fruimass_meanopen = fruimass_t[2],
         seedmass_meandiff = seedmass_t[2] - seedmass_t[1],
         seedmass_meanopen = seedmass_t[2],
         ratio_meandiff = ratio_t[2] - ratio_t[1],
         ratio_meanopen = ratio_t[2],
         mass_pseed_meanopen = mass_pseed_t[2]) %>%
  ungroup() %>%
  dplyr::select(-c("fruimass_t", "seedmass_t", 'ratio_t', 'mass_pseed_t', "treatment")) %>%
  distinct()

CF_garden <- CF_garden %>%
  inner_join(CF_data, by = c("garden_Name")) 

CF_garden <- CF_garden %>%
  dplyr::select(-c("DateStart", "DateEnd", "treatment", "plant_ID", "fruit_ID", 
            "fruit_mass", "seed_mass", "ratio", "avg_mass_pseed")) %>%
  distinct()

CF_garden <- CF_garden %>%
  relocate(c("fruimass_meandiff", "fruimass_meanopen", "seedmass_meandiff", "seedmass_meanopen", 
             "ratio_meandiff", "ratio_meanopen", "mass_pseed_meanopen"), .after = "urbanclass1000")


# ---------------------------------------------------------------------------- #


# -- Export new CF_garden dataframe ----
cf_garden_export <- paste("./analysis_data/CF/CF_Data_2021_4analysis_garden.xlsx", sep = "")
write.xlsx(CF_garden, cf_garden_export, append = TRUE)


# -- Clean-up environment for the next script ----
rm(list=ls())

