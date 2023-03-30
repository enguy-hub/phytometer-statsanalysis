## Urban Productive Ecosystem | Phytometer Experiment in 2021

Github repository for the statistical analysis done for the pollination service evaluation experiment conducted by 
the Urban Productive Ecosystem lab from the Technical University of Munich in the summer of 2021.

### Prerequisites

Please have R software and R Studio installed in your computer and perform step 4 
to step 7 from the `Installation` section below the first time you ever use this repo

### Installation

1. Clone or download (as zip) this repo
2. Navigate to the project folder 
3. Open the `Phytometer_StatsAnalysis.Rproj` file in R Studio
4. Open the `packageInstaller.R` file, located in the `scripts/` folder
5. Set the `phytometer-statsanalysis` folder is as working directory in R Studio
![setWD](src/images/setWD.png =100x100)
6. Run all the code in the `packageInstaller.R` file (try `Ctrl + Enter` combination)
7. Wait until all packages are installed and loaded
8. Close the `packageInstaller.R` file if you want

## Running the models

1. Navigate into the `script/5_AllCombined/` folder
2. Open the `All_plotFinalModels.R` or `All_plotWilcox.R` file
3. Change the path of the `pdir` object to the absolute path where the 
`phytometer-statsanalysis` folder is located
4. Run the codes in these files to see the models and result plots
5. Repeat the process with the other R files in this folder


