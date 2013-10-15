# make sure we have the necessary libraries loaded
library(plyr)
library(ggplot2)
library(RColorBrewer)

# read the data
dat <- read.table("globalterrorismdb_clean.csv", header = TRUE, sep = ',')

# in many plyr functions I will add a column for a simple count or sum,
# so instead of repeating that little piece of code every time, just make them functions
plyrFxCount <- function(x, name="count") {
  df <- data.frame( nrow(x) )
  colnames(df)[1] <- name
  return(df)
}
plyrFxSum <- function(x, toSum, name="sum") {
  df <- data.frame( sum(x$toSum) )
  colnames(df)[1] <- name
  return(df)
}

## --- First we will look at region-level stats --- ##

# reorder region levels by total number of attacks in each region
regionAttackOrder = order(table(dat$region), decreasing=TRUE)
regionAttackLevels = names(table(dat$region))[regionAttackOrder]
dat$region <- factor(dat$region, levels = regionAttackLevels)

regions = levels(dat$region)

# create a color palette for the 12 regions. Sequential Brewer palettes only have 9 colours,
# so add 3 manually (also remove their yellow because it's hard to see, replace with gold)
regionCol <- c(brewer.pal(9, name="Set1")[-6], '#EEC900', '#00EEEE','#7FFF00','#E9967A')

# calculate the number of attacks in each country per year
regionYear <- ddply(dat, region ~ year, plyrFxCount, "nattacks")

# fix a little "problem" (well, a good problem), where some regions have years with 0 attacks
# this will cause some missing points in the plots which doesn't look nice, so we will
# just add a value of 0 for every region/year pair that doesn't exist
regionYearPossibilities <- merge(regions, unique(dat$year))
regionYear <- merge(regionYear, regionYearPossibilities,
                    by.x = c('region','year'), by.y = c("x","y"), all.y = TRUE)
regionYear$nattacks[is.na(regionYear$nattacks)] <- 0

# let's look at the number of attacks per year in each world region
ggplot(regionYear, aes(x = year, y = nattacks, color = region)) +
  geom_line(show_guide=FALSE) +
  geom_point(show_guide=FALSE) +
  xlab("Year") + 
  ggtitle("Number of terrorist attacks in world regions") + 
  ylab("# of Attacks") +
  facet_wrap(~region) +
  scale_color_manual(values = regionCol) + 
  theme(strip.text = element_text(face="bold"))

# This already reveals some interesting data.
# Central America seemed to be very unstable starting at the late 1970's and slowly getting better with time,
# until almost eliminating terrorist attacks before the new millenium.
# The Middle East and South Asia both had a surge in terrorist attacks since circa 2005, after both having relative
# quiet since the mid 90's.
# South America was was consistently pretty dangerous througout the 80's and 90's, and has calmed since.
# Western Europe is another region worth mentioning, that had many attacks up until the new millenium.
# The rest of the regions are worth glancing at, but are not as interesting.

# now let's look at the same plot, but with all the regions superposed
ggplot(regionYear, aes(x = year, y = nattacks, color = region)) +
  geom_line() +
  geom_point() +
  xlab("Year") + 
  ggtitle("Number of terrorist attacks in world regions") + 
  ylab("# of Attacks") +
  scale_color_manual(values = regionCol) + 
  theme(legend.justification = c(0,1), legend.position = c(0,1), legend.title = element_blank()) +
  guides(col = guide_legend(ncol = 2))

# While this looks messy and a little harder to read, it is interesting to see global patterns.
# We can see that from the late 70's til the late 90's, many regions experienced higher terror attacks,
# and by 2000 most have achieved relative peace. Interestingly, the Middle East and South Asia (both of
# which also seemed to have much lower terrorist activity around the turn of the millenium) have both
# seen a sharp increase in the past decade.

#bbbb<-arrange(ddply(dat, .(country,region), plyrFxCount, "ss"), region, ss)




breaks <- seq(from = min(regionsyears$year), to = max(regionsyears$year), by = 5)
a <- cut(regionsyears$year, breaks = breaks, include.lowest=TRUE, right=FALSE)
regionsyears$bin <- a
regionsyears <- regionsyears[complete.cases(regionsyears),]
regionbinyears<-ddply(regionsyears, region~bin, function(x){data.frame(tot=sum(x$totAttacks))})

regionsOrder<-order(table(dat$region),decreasing=TRUE)
regionsLevels<-names(table(dat$region))[regionsOrder]
regionbinyears$region<-factor(regionbinyears$region,levels=regionsLevels)

aa<-merge(regions, levels(a))
regionbinyears <- merge(regionbinyears, aa, by.x=c('region','bin'), by.y=c("x","y"), all.y=TRUE)
regionbinyears$tot[is.na(regionbinyears$tot)] <- 0

d<-ggplot(regionbinyears, aes(x=tot, y=region, cex=2))
d+geom_point(show_guide=FALSE)+facet_wrap(~bin)+aes(col=region)