# Set paths to project dir
#pdir = "C:/Users/hienn/Desktop/StatisticalAnalysis"
pdir = "~/Hien/StatisticalAnalysis"
setwd(pdir)

# Prerequisites - installing packages and load libraries
list_packages <- c("tidyverse", "dplyr", "readxl", "openxlsx", "ggplot2", "nlme",
                   "vegan", "lubridate", "PerformanceAnalytics", "lme4", "car",
                   "rstatix", "ggpubr", "lmerTest", "jtools", "MASS", "knitr",
                   "magrittr","broom", "effects", "VGAM", "interactions", 'sjPlot')
install.packages(list_packages)

# Un-command the line below and run it to activate the libraries nows
lapply(list_packages, library, character.only = TRUE)
