# read the raw input
dat <- read.csv("globalterrorismdb.csv", header = TRUE, na.strings = c("", "."))

# sanity check that import was ok (there should be ~105k rows and 123 columns)
dim(dat)

# only keep the relevant columns that we are going to use
columnsKeep <- c("iyear", "imonth", "iday", "country_txt", "region_txt", "city",
                 "attacktype1_txt", "nkill", "nwound")
dat <- dat[columnsKeep]

# rename some columns
library(plyr)
dat <- rename(dat, c("iyear" = "year", "imonth" = "month", "iday" = "day",
                     "country_txt" = "country", "region_txt" = "region", "attacktype1_txt" = "attacktype"))

# replace NA values for number of killed/wounded with 0
# this isn't necessary always a smart thing to do, but it makes sense for the purposes of this analysis
dat$nkill[is.na(dat$nkill)] <- 0
dat$nwound[is.na(dat$nwound)] <- 0

# the world map data used later knows about the world as it was in 1980, so all the post-USSR countries
# did not exist. For consistency, rename the two regions coming from the USSR as simply 'USSR'.
# For most of the analysis we will keep the separate countries, but before mapping them we will need
# to rename all individual countries to 'USSR'. For now, simply combine their regions into one. 
levels(dat$region) <- c(levels(dat$region), 'USSR')
dat$region[dat$region == 'Central Asia'] <- 'USSR'
dat$region[dat$region == 'Russia & the Newly Independent States (NIS)'] <- 'USSR'
dat <- droplevels(dat)

# helper function for saving a dataframe (if you want to change the parameters,
# only need to change them once, assuming all dataframes should be saved similarly)
# note that we omit quotes to save file space, but keep them for cities because some
# city names contain commas
writeTable <- function(x, name) {
  write.table(x, file=name, sep=",", col.names=TRUE, row.names=FALSE, quote=which(colnames(dat) == 'city'))
}

# save the processed data frame, which happens to be exactly 10% of the original size
writeTable(dat, "globalterrorismdb_clean.csv")

# also save a subset of the data that only happens in Israel, for analysis in a separate script
israelDat <- subset(dat, country == 'Israel')
writeTable(israelDat, "globalterrorismdb_israel.csv")