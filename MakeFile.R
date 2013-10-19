## clean all output from previous runs of scripts
outputs <- c("globalterrorismdb_clean.csv",          ## script 1
             "countriesMostAttackedPerRegion.txt",   ## script 2
             "citiesMostAttacked.txt",               ## script 3 
             "countriesMostAttacksPerPop.txt",       ## script 4
             "deadliestAttacksOutliers.txt",         ## script 4
             list.files(pattern = "*.png$")          ## all figures
            )
file.remove(outputs)

# script 0 only has to be run once on a machine, it simply installs all required packages
#source("00_installPackages.R")

## run all scripts
source("01_preprocessData.R")   ## can take up to a minute to run because of reading a big dataset
source("02_analysisPlots.R")    ## analyse global terrorism using plots
source("03_analysisMaps.R")     ## show terrorism on a world map
source("04_integrateGapMinder.R")  ## analyse terrorism vs GDP
source("05_israel.R")           ## terrorism in Israel