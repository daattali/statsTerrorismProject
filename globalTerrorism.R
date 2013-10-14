
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




#gdp
nn<-merge(countriesAttacks, subset(gDat, year==max(year)), by.x=c('country_txt'),by.y=c('country'))
xyplot(gdpPercap~totAttacks,nn)
# usually poorer countries get terrorist activities

nn<-merge(countriesAttacks3, gDat, by.x=c('country_txt','iyear'),by.y=c('country','year'))
nn<-nn[-which(nn$gdpPercap==max(nn$gdpPercap)),]
xyplot(gdpPercap~totAttacks|iyear,nn,group=continent,pch=18,auto.key=TRUE)


countriesAttacktype <-ddply(dat, .(country_txt, attacktype1_txt), function(x){data.frame(sum(x$nkill,na.rm=TRUE))})
names(countriesAttacktype)[1]<-'country'
names(countriesAttacktype)[3]<-'nkill'
vvv<-merge(countriesAttacktype,subset(gDat, year==2007,select=c('country','gdpPercap')))
xyplot(nkill~gdpPercap, vvv, group=attacktype1_txt,auto.key=TRUE,pch=18,cex=1)
# can see USA cuz of 9/11

deadliest <- tail(arrange(subset(dat, !is.na(nkill)),nkill), n=250)
deadliest2<-subset(deadliest,select=c('iyear','imonth','iday','attacktype1_txt','nkill','nwound','country_txt'))
names(deadliest2)[7]<-'country'
bb<-merge(deadliest2,subset(gDat, year==2007,select=c('country','gdpPercap')))
xyplot(nkill~gdpPercap, bb, group=attacktype1_txt,auto.key=TRUE,pch=18,cex=1)
# can see attacks in the US are outliers (1995 oklahoma bombing and 9/11), can also see canada 1985 and rwanda assault

# MAKE COPY OF DAT FIRST, do following opeartions on the copy
dat[is.na(dat$nkill),]$nkill = 0               
dat[is.na(dat$nwound),]$nwound = 0
woundest <- tail(arrange(dat, nwound+nkill), n=250)
woundest$tot<-woundest$nkill+woundest$nwound
woundest<-subset(woundest,select=c('iyear','imonth','iday','attacktype1_txt','nkill','nwound','country_txt','tot'))
woundestG<-merge(woundest,subset(gDat, year==2007,select=c('country','gdpPercap','continent')), by.x='country_txt',by.y='country')
xyplot(tot~gdpPercap, woundestG, group=attacktype1_txt,auto.key=TRUE,pch=18,cex=1)
# or arrange by continent too
# i think the 10000 wounded in peru in 1983 is an error. i tried looking it up, couldn't find any info about it, but did happen in sri lanka.



#cities, hostages


regionsyears<-(ddply(dat, region_txt~iyear, function(x) { return(data.frame(totAttacks = nrow(x))) }))
breaks <- seq(from = min(regionsyears$iyear), to = max(regionsyears$iyear), by = 5)
a <- cut(regionsyears$iyear, breaks = breaks, include.lowest=TRUE, right=FALSE)
regionsyears$bin <- a
regionsyears <- regionsyears[complete.cases(regionsyears),]
regionbinyears<-ddply(regionsyears, region_txt~bin, function(x){data.frame(sum(x$totAttacks))})

regionsOrder<-order(table(dat$region_txt),decreasing=TRUE)
regionsLevels<-names(table(dat$region_txt))[regionsOrder]
regionbinyears$region_txt<-factor(regionbinyears$region_txt,levels=regionsLevels)

d<-ggplot(regionbinyears, aes(x=sum.x.totAttacks., y=region_txt, cex=2))
d+geom_point(show_guide=FALSE)+facet_wrap(~bin)+aes(col=region_txt)




cities <- table(dat$city)
cities <- sort(cities[cities > 100], decreasing=TRUE)
cities<-cities[-1]
cityNames <- names(cities)
datcities<-subset(dat,city %in% cityNames)
bycity<-ddply(datcities, country_txt~city, function(x) {data.frame(totAttacks=nrow(x))})
bycity <- head(arrange(bycity, totAttacks, decreasing=TRUE), n=20)
# do this again but give bycity n=100, see how many of them are capitals (MANY)
world.cities <- within(world.cities, name <- revalue(name, c(Bayrut = "Beirut", "Guatemala" = "Guatemala City", "al-Mawsil" = "Mosul")))
citiesfull<-merge(bycity, world.cities, by.x=c('country_txt','city'), by.y=c('country.etc','name'))
citiesfull<-rbind(citiesfull, subset(cbind(bycity[bycity$city=='Belfast',], world.cities[world.cities$name=='Belfast' & world.cities$country.etc == 'UK',]), select = -c(country.etc, name)))
arrange(citiesfull,totAttacks)
map('worldHires')
points(x=citiesfull$long,y=citiesfull$lat,col=palette(),pch=15, cex=2)





csea<-droplevels(unique(subset(dat, region_txt=='Southeast Asia')$country_txt))
map('worldHires')
map('worldHires',regions=csea,add=TRUE,col='yellow',fill=TRUE)
library(fBasics)
regionDanger<-arrange(ddply(subset(dat,year>=2000), ~region, function(x){data.frame(tot=nrow(x))}),desc(tot))
heatColors <- seqPalette(max(regionDanger$tot),name="Reds")
regionDanger$col <- heatColors[regionDanger$tot]
heatColorsRank <- seqPalette(nrow(regionDanger),name="Reds")
regionDanger$colRank <- rev(heatColorsRank)

map('worldHires')
for(i in 1:nrow(regionDanger)){
  cc<-droplevels(unique(subset(dat, region==regionDanger[i,'region'])$country))
  map('worldHires',regions=cc,add=TRUE,col=regionDanger[i,'col'],fill=TRUE)
}
