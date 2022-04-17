# ---- Prerequisite ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "openxlsx", "car")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Hien/Garden/MyGithub/Phytometer_StatisticalAnalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
tp_path = "./analysis_data/TP/TP_Data_2021_4analysis.xlsx"
TP_data <- read_excel(tp_path, sheet = 1)


# Check structure and summaries of the datasets
str(TP_data)
summary(TP_data)


# ---------------------------------------------------------------------------- #


# Generate new response vars
TP_garden <- TP_data %>%
  group_by(garden_Name, treatment) %>%
  summarise(flowmass_t = mean(flower_mass),
            seedmass_t = mean(seed_mass),
            mass_pseed_t = mean(avg_mass_pseed),
            nutlets_t = mean(num_nutlets)) %>%
  mutate(flowmass_meandiff = flowmass_t[2] - flowmass_t[1],
         flowmass_meanopen = flowmass_t[2],
         seedmass_meandiff = seedmass_t[2] - seedmass_t[1],
         seedmass_meanopen = seedmass_t[2],
         mass_pseed_meandiff = mass_pseed_t[2] - mass_pseed_t[1],
         mass_pseed_meanopen = seedmass_t[2]/nutlets_t[2]) %>%
  ungroup() %>%
  dplyr::select(-c("flowmass_t", "seedmass_t", "nutlets_t", "mass_pseed_t", "treatment")) %>%
  distinct()

TP_garden <- TP_garden %>%
  inner_join(TP_data, by = c("garden_Name")) 

TP_garden <- TP_garden %>%
  dplyr::select(-c("DateStart", "DateEnd", "treatment", "plant_ID", "flower_ID", 
            "bag_type", "flower_mass", "seed_mass", "avg_mass_pseed", "num_nutlets")) %>%
  distinct()

TP_garden <- TP_garden %>%
  relocate(c("flowmass_meandiff", "flowmass_meanopen", "seedmass_meandiff", "seedmass_meanopen", 
             "mass_pseed_meandiff", "mass_pseed_meanopen"), .after = "urbanclass1000")


# ---------------------------------------------------------------------------- #


# -- Export new TP_garden dataframe ----
tp_garden_export <- paste("./analysis_data/TP/TP_Data_2021_4analysis_garden.xlsx", sep = "")
write.xlsx(TP_garden, tp_garden_export, append = TRUE)


# -- Clean-up environment for the next script ----
rm(list=ls())
