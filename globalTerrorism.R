dat <- read.csv("globalterrorismdb.csv", header = TRUE, na.strings = c("", "."))
dim(dat)
israel <- subset(dat, country_txt == 'Israel')
write.csv(israel, "globalterrorismdbIsrael.csv", quote = FALSE, row.names = FALSE)

ddd <- read.csv("globalterrorismdbIsrael.csv", header = TRUE)

# only keep columns we need. rename region_txt to just region, etc

# install.packages("maps")
# install.packages("mapdata")
library(maps)
library(mapdata)
library(plyr)
data(world.cities)

map('worldHires',c('Israel','Bulgaria'),xlim=c(21,37),ylim=c(28,45))
map.cities(country='Israel',minpop=80000,label=FALSE)
points( x=c(34.95, 34.77), y=c(29.56,32.07),col=c('blue','green'),pch=18,cex=c(1.9,1.3))
idx<-which(world.cities$name %in% c('Haifa', 'Jerusalem' , 'Ashqelon', 'Sederot'))
points(x=world.cities$long[idx], y=world.cities$lat[idx])

ddply(subset(dat, country_txt == 'United States'), ~iyear, function(x){return(data.frame(tot=nrow(x)))})

countriesAttacks <- ddply(dat, ~country_txt, function(x) { return(data.frame(totAttacks = nrow(x))) })
head(arrange(countriesAttacks, totAttacks,decreasing=TRUE),n=15)


arrange(ddply(dat, ~region_txt, function(x) { return(data.frame(totAttacks = nrow(x))) }), totAttacks)




gDat <- read.delim("gapminderDataFiveYear.txt")
str(gDat)
countriesAttacks2<-merge(countriesAttacks, subset(gDat, year==max(year), select=c('country','pop')), by.x = 'country_txt', by.y='country')
countriesAttacks2$attacksNorm <- countriesAttacks2$pop / countriesAttacks2$totAttacks



regionsyears<-(ddply(dat, region_txt~iyear, function(x) { return(data.frame(totAttacks = nrow(x))) }))
breaks <- seq(from = min(regionsyears$iyear), to = max(regionsyears$iyear), by = 5)
a <- cut(regionsyears$iyear, breaks = breaks, include.lowest=TRUE, right=FALSE)
regionsyears$bin <- a
regionsyears <- regionsyears[complete.cases(regionsyears),]
regionbinyears<-ddply(regionsyears, region_txt~bin, function(x){data.frame(sum(x$totAttacks))})

regionsOrder<-order(table(dat$region_txt),decreasing=TRUE)
regionsLevels<-names(table(dat$region_txt))[regionsOrder]
regionbinyears$region_txt<-factor(regionbinyears$region_txt,levels=regionsLevels)

d<-ggplot(regionbinyears, aes(x=sum.x.totAttacks., y=region_txt))
d+geom_point(show_guide=FALSE)+facet_wrap(~bin)+aes(col=region_txt)