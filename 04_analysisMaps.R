
library(maps)
library(mapdata)
library(plyr)
data(world.cities)

map('worldHires',c('Israel','Bulgaria'),xlim=c(21,37),ylim=c(28,45))
map.cities(country='Israel',minpop=80000,label=FALSE)
points( x=c(34.95, 34.77), y=c(29.56,32.07),col=c('blue','green'),pch=18,cex=c(1.9,1.3))
idx<-which(world.cities$name %in% c('Haifa', 'Jerusalem' , 'Ashqelon', 'Sederot'))
points(x=world.cities$long[idx], y=world.cities$lat[idx])









cities <- table(dat$city)
cities <- sort(cities[cities > 100], decreasing=TRUE)
cities<-cities[-1]
cityNames <- names(cities)
datcities<-subset(dat,city %in% cityNames)
bycity<-ddply(datcities, country~city, function(x) {data.frame(totAttacks=nrow(x))})
bycity <- head(arrange(bycity, totAttacks, decreasing=TRUE), n=20)
# do this again but give bycity n=100, see how many of them are capitals (MANY)
world.cities <- within(world.cities, name <- revalue(name, c(Bayrut = "Beirut", "Guatemala" = "Guatemala City", "al-Mawsil" = "Mosul")))
citiesfull<-merge(bycity, world.cities, by.x=c('country','city'), by.y=c('country.etc','name'))
citiesfull<-rbind(citiesfull, subset(cbind(bycity[bycity$city=='Belfast',], world.cities[world.cities$name=='Belfast' & world.cities$country.etc == 'UK',]), select = -c(country.etc, name)))
arrange(citiesfull,totAttacks)
map('worldHires')
points(x=citiesfull$long,y=citiesfull$lat,col=palette(),pch=15, cex=2)





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