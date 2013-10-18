source('common.R')

library(lattice)

# 
countriesAttacks <- ddply(dat, ~country+region, function(x) { return(data.frame(totAttacks = nrow(x))) })
head(arrange(countriesAttacks, totAttacks,decreasing=TRUE),n=5)

gDat <- read.delim("gapminderDataFiveYear.txt")
str(gDat)
countriesAttacks2<-merge(countriesAttacks, subset(gDat, year==max(year), select=c('country','pop')), by.x = 'country', by.y='country')
countriesAttacks2$attacksNorm <- countriesAttacks2$pop / countriesAttacks2$totAttacks




#gdp
nn<-merge(countriesAttacks, subset(gDat, year==max(year)), by.x=c('country'),by.y=c('country'))
xyplot(gdpPercap~totAttacks,nn)
# usually poorer countries get terrorist activities


countriesAttacktype <-ddply(dat, .(country, attacktype), function(x){data.frame(sum(x$nkill,na.rm=TRUE))})
names(countriesAttacktype)[1]<-'country'
names(countriesAttacktype)[3]<-'nkill'
vvv<-merge(countriesAttacktype,subset(gDat, year==2007,select=c('country','gdpPercap')))
xyplot(nkill~gdpPercap, vvv, group=attacktype,auto.key=TRUE,pch=18,cex=1)
# can see USA cuz of 9/11

deadliest <- tail(arrange(subset(dat, !is.na(nkill)),nkill), n=250)
deadliest2<-subset(deadliest,select=c('year','month','day','attacktype','nkill','nwound','country'))
names(deadliest2)[7]<-'country'
bb<-merge(deadliest2,subset(gDat, year==2007,select=c('country','gdpPercap')))
xyplot(nkill~gdpPercap, bb, group=attacktype,auto.key=TRUE,pch=18,cex=1)
# can see attacks in the US are outliers (1995 oklahoma bombing and 9/11), can also see canada 1985 and rwanda assault

# MAKE COPY OF DAT FIRST, do following opeartions on the copy
woundest <- tail(arrange(dat, nwound+nkill), n=250)
woundest$tot<-woundest$nkill+woundest$nwound
woundest<-subset(woundest,select=c('year','month','day','attacktype','nkill','nwound','country','tot'))
woundestG<-merge(woundest,subset(gDat, year==2007,select=c('country','gdpPercap','continent')), by.x='country',by.y='country')
xyplot(tot~gdpPercap, woundestG, group=attacktype,auto.key=TRUE,pch=18,cex=1)
# or arrange by continent too
# i think the 10000 wounded in peru in 1983 is an error. i tried looking it up, couldn't find any info about it, but did happen in sri lanka.
