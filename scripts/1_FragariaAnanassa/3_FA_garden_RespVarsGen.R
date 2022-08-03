# -- Prerequisite ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "openxlsx", "car")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
fa_path = "./analysis_data/FA/FA_Data_2021_4analysis.xlsx"
FA_data <- read_excel(fa_path, sheet = 1)


# Check structure and summaries of the datasets
str(FA_data)
summary(FA_data)


# ---------------------------------------------------------------------------- #


# Generate new response vars
FA_garden <- FA_data %>%
  group_by(garden_Name, treatment) %>%
  summarise(mass_treatment = mean(mass),
            ratio_treatment = mean(ratio)) %>%
  mutate(mass_meanopen = mass_treatment[2],
         mass_meandiff = mass_treatment[2] - mass_treatment[1],
         ratio_meanopen = ratio_treatment[2],
         ratio_meandiff = ratio_treatment[2] - ratio_treatment[1]) %>%
  ungroup() %>%
  select(-c("mass_treatment", "ratio_treatment", "treatment")) %>%
  distinct()

FA_garden <- FA_garden %>%
  inner_join(FA_data, by = c("garden_Name")) 

FA_garden <- FA_garden %>%
  select(-c("DateStart", "DateEnd", "plant_ID", "fruit_ID", 
            "treatment", "deform", "mass", "ratio","height", "width")) %>%
  distinct()

FA_garden <- FA_garden %>%
  relocate(c("mass_meandiff", "mass_meanopen","ratio_meandiff", "ratio_meanopen"), 
             .after = "urbanclass1000")


# ---------------------------------------------------------------------------- #


# -- Export new FA_garden dataframe ----
fa_garden_export <- paste("./analysis_data/FA/FA_Data_2021_4analysis_garden.xlsx", sep = "")
write.xlsx(FA_garden, fa_garden_export, append = TRUE)


# -- Clean-up environment for the next script ----
rm(list=ls())
