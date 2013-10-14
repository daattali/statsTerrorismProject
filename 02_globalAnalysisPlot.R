# make sure we have the necessary libraries loaded
library(plyr)
library(ggplot2)

regions = levels(dat$region)

regionsyears<-(ddply(dat, region ~ year, function(x) { return(data.frame(totAttacks = nrow(x))) }))
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