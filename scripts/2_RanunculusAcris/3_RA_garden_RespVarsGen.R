# ---- Prerequisite ----

# All packages needed for this script
list_packages <- c("tidyverse", "dplyr", "readxl", "openxlsx", "car")
lapply(list_packages, library, character.only = TRUE)


# Set paths to project dir
pdir = "C:/Garden/MyGithub/phytometer-statsanalysis"
#pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)


# Set path and read the data
ra_path = "./analysis_data/RA/RA_Data_2021_4analysis.xlsx"
RA_data <- read_excel(ra_path, sheet = 1)


# Check structure and summaries of the datasets
str(RA_data)
summary(RA_data)


# ---------------------------------------------------------------------------- #


# Generate new response vars
RA_meandiff <- RA_data %>%
  group_by(garden_Name, treatment) %>%
  summarise(fremass_t = mean(fresh_mass),
            drymass_t = mean(dry_mass),
            avgfremass_pseed_t = mean(avg_fremass_pseed),
            avgdrymass_pseed_t = mean(avg_drymass_pseed)) %>%
  mutate(fremass_meandiff = fremass_t[2] - fremass_t[1],
         fremass_meanopen = fremass_t[2],
         drymass_meandiff = drymass_t[2] - drymass_t[1],
         drymass_meanopen = drymass_t[2],
         avgfremass_pseed_meandiff = avgfremass_pseed_t[2] - avgfremass_pseed_t[1],
         avgfremass_pseed_meanopen = avgfremass_pseed_t[2],
         avgdrymass_pseed_meandiff = avgdrymass_pseed_t[2] - avgdrymass_pseed_t[1],
         avgdrymass_pseed_meanopen = avgdrymass_pseed_t[2]) %>%
  ungroup() %>%
  dplyr::select(-c("treatment", "fremass_t", "drymass_t", 
                   "avgfremass_pseed_t", "avgdrymass_pseed_t")) %>%
  distinct()

RA_meandiff <- RA_meandiff %>%
  inner_join(RA_data, by = c("garden_Name")) 

RA_meandiff <- RA_meandiff %>%
  dplyr::select(-c("DateStart", "DateEnd", "treatment","plant_ID", "seed_ID", 
                   "fresh_mass", "dry_mass", "avg_fremass_pseed", "avg_drymass_pseed",
                   "num_nutlets", "fertile_nutlets", "infertile_nutlets")) %>%
  distinct()


# Creating RA_avgmass_ferseed: Average mass per fertile seed
RA_avgmass_ferseed <- RA_data %>%
  group_by(garden_Name) %>%
  filter(fertile_nutlets == 0) %>%
  summarise(avgfremass_inferseed = mean(fresh_mass/infertile_nutlets),
            avgdrymass_inferseed = mean(dry_mass/infertile_nutlets)) %>%
  ungroup()

RA_avgmass_ferseed <- RA_avgmass_ferseed %>%
  inner_join(RA_data, by = c("garden_Name")) 

RA_avgmass_ferseed <- RA_avgmass_ferseed %>%
  group_by(garden_Name) %>%
  filter(fertile_nutlets > 0) %>%
  summarise(fremass_ferseed = (fresh_mass - (avgfremass_inferseed * infertile_nutlets))/fertile_nutlets,
            drymass_ferseed = (dry_mass - (avgdrymass_inferseed * infertile_nutlets))/fertile_nutlets) %>%
  ungroup() 

RA_avgmass_ferseed <- RA_avgmass_ferseed %>%
  inner_join(RA_data, by = c("garden_Name")) 

RA_avgmass_ferseed <- RA_avgmass_ferseed %>%
  group_by(garden_Name) %>%
  summarise(avgfremass_ferseed = mean(fremass_ferseed),
            avgdrymass_ferseed = mean(drymass_ferseed)) %>%
  ungroup() 


# Merge RA_meandiff and RA_avgmass_ferseed into RA_garden
RA_garden <- RA_meandiff %>%
  inner_join(RA_avgmass_ferseed, by = c("garden_Name")) 

RA_garden <- RA_garden %>%
  relocate(c("fremass_meandiff", "fremass_meanopen", "drymass_meandiff", "drymass_meanopen",
             "avgfremass_pseed_meandiff", "avgfremass_pseed_meanopen",
             "avgdrymass_pseed_meandiff", "avgdrymass_pseed_meanopen",
             "avgfremass_ferseed", "avgdrymass_ferseed"), .after = "urbanclass1000")


# ---------------------------------------------------------------------------- #


# -- Export new FA_garden dataframe ----
ra_garden_export <- paste("./analysis_data/RA/RA_Data_2021_4analysis_garden.xlsx", sep = "")
write.xlsx(RA_garden, ra_garden_export, append = TRUE)


# -- Clean-up environment for the next script ----
rm(list=ls())
