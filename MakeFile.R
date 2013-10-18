## TODO readme, Rmd

## clean out any previous work
outputs <- c("globalterrorismdb_clean.csv",            
             "globalterrorismdb_israel.csv",
             "citiesMostAttacked.txt",
             list.files(pattern = "*.png$"))
file.remove(outputs)

#source("00_installPackages.R")

## run my scripts
source("01_preprocessData.R")
source("02_analysisPlots.R")