rm(list=ls())

library(adehabitat)
library(ggplot2)
library(ggmap)
library(grid)
library(gridExtra)
library(maps)       # Provides functions that let us plot the maps
library(mapdata)    # Contains the hi-resolution points that mark out the countries.
library(mapproj)
library(maptools)
library(plyr)
library(rworldmap)
library(rgdal)
require(rgeos)
library(raster)
library(sp)
#library(SDMTools)
gpclibPermit()
library(utils)

# Data from http://thematicmapping.org/downloads/world_borders.php.
# Direct link: http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip
# Unpack and put the files in a dir 'data'

getwd()
#setwd("~/data")
#unzip("TM_WORLD_BORDERS_SIMPL-0.3.zip")
# world.map <- readOGR(dsn="C:/Users/David Hayman/Documents/data", layer="TM_WORLD_BORDERS_SIMPL-0.3")

#setwd("C:/Users/David Hayman/Documents/GitHub/measles/maps")

incidence<-read.csv("incidence_series.csv",header=T)
head(incidence)

cover<-read.csv("coverage_series.csv",header=T)
head(cover)

immigration<-read.csv("immigration.csv",header=T)
head(immigration)

population<-read.csv("population.csv", header=T)
head(population)

annualtravel<-read.csv("annualtravel.csv",header=T)
head(annualtravel)

monthtravel<-read.csv("monthtravel.csv",header=T)
head(monthtravel)

#######

# world.ggmap <- fortify(world.map, region = "NAME")
# save(world.ggmap,file="world.ggmap.Rda")
load("world.ggmap.Rda")
n <- length(unique(world.ggmap$id))

id = unique(world.ggmap$id)
id<-as.data.frame(id)
colnames(cover)[3]<-"id"
colnames(incidence)[3]<-"id"

cover$id

summary(immigration$Nationality,maxsum=Inf)
## match immigration names to IDs from maps
levels(immigration$Nationality)[match(c("Antigua & Barbuda","Bosnia & Herzegovina","Myanmar","Ivory Coast",
                                        "Faeroe Islands","Guinea - Bissau","Vatican City","Iran","North Korea",
                                        "South Korea","Laos","Libya","Federated States of Micronesia","Moldova",
                                        "St Kitts - Nevis","St Lucia","St Vincent and the Grenadines","Sao Tome & Principe",
                                        "Serbia & Montenegro","South Sudan","Syria","Timor Leste","Turkemenistan","Great Britain",
                                        "United States of America","Virgin Islands, USA","Vietnam"),levels(immigration$Nationality))] <-
  c("Antigua and Barbuda","Bosnia and Herzegovina","Burma","Cote d'Ivoire",
    "Faeroe Islands","Guinea-Bissau","Holy See (Vatican City)","Iran (Islamic Republic of)","Korea, Democratic People's Republic of",
    "Korea, Republic of","Lao People's Democratic Republic","Libyan Arab Jamahiriya","Micronesia, Federated States of","Republic of Moldova",
    "Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines","Sao Tome and Principe","Serbia","Sudan","Syrian Arab Republic",
    "Timor-Leste","Turkmenistan","United Kingdom","United States","United States Virgin Islands","Viet Nam")

## match incidence and cover data names to maps ID
class(cover$id)
levels(cover$id)[match(c("Bahamas (the)",
"Bolivia (Plurinational State of)",
"Central African Republic (the)",
"Comoros (the)",
"Congo (the)",
"Côte d'Ivoire",
"Czech Republic (the)",
"Democratic People's Republic of Korea (the)",
"Democratic Republic of the Congo (the)",
"Dominican Republic (the)",
"Gambia (the)",
"Lao People's Democratic Republic (the)",
"Libya",
"Marshall Islands (the)",
"Micronesia (Federated States of)",
"Netherlands (the)",
"Niger (the)",
"Philippines (the)",
'Republic of Korea (the)',
"Republic of Moldova (the)",
"Russian Federation (the)",
"Sudan (the)",
"Syrian Arab Republic (the)",
"United Arab Emirates (the)",
"United Kingdom of Great Britain and Northern Ireland (the)",
"United States of America (the)",
"Venezuela (Bolivarian Republic of)"),levels(cover$id))] <-
  c("Bahamas",
    "Bolivia",
    "Central African Republic",
    "Comoros",
    "Congo",
    "Cote d'Ivoire",
    "Czech Republic",
    "Korea, Democratic People's Republic of",
    "Democratic Republic of the Congo",
    "Dominican Republic",
    "Gambia",
    "Lao People's Democratic Republic",
    "Libyan Arab Jamahiriya",
    "Marshall Islands",
    "Micronesia, Federated States of",
    "Netherlands",
    "Niger",
    "Philippines",
    'Korea, Republic of Korea',
    "Republic of Moldova",
    "Russia",
    "Sudan",
    "Syrian Arab Republic",
    "United Arab Emirates",
    "United Kingdom",
    "United States",
    "Venezuela")

levels(incidence$id)[match(c("Bahamas (the)",
                             "Bolivia (Plurinational State of)",
                             "Central African Republic (the)",
                             "Comoros (the)",
                             "Congo (the)",
                             "Côte d'Ivoire",
                             "Czech Republic (the)",
                             "Democratic People's Republic of Korea (the)",
                             "Democratic Republic of the Congo (the)",
                             "Dominican Republic (the)",
                             "Gambia (the)",
                             "Lao People's Democratic Republic (the)",
                             "Libya",
                             "Marshall Islands (the)",
                             "Micronesia (Federated States of)",
                             "Netherlands (the)",
                             "Niger (the)",
                             "Philippines (the)",
                             'Republic of Korea (the)',
                             "Republic of Moldova (the)",
                             "Russian Federation (the)",
                             "Sudan (the)",
                             "Syrian Arab Republic (the)",
                             "United Arab Emirates (the)",
                             "United Kingdom of Great Britain and Northern Ireland (the)",
                             "United States of America (the)",
                             "Venezuela (Bolivarian Republic of)"),levels(incidence$id))] <-
  c("Bahamas",
    "Bolivia",
    "Central African Republic",
    "Comoros",
    "Congo",
    "Cote d'Ivoire",
    "Czech Republic",
    "Korea, Democratic People's Republic of",
    "Democratic Republic of the Congo",
    "Dominican Republic",
    "Gambia",
    "Lao People's Democratic Republic",
    "Libyan Arab Jamahiriya",
    "Marshall Islands",
    "Micronesia, Federated States of",
    "Netherlands",
    "Niger",
    "Philippines",
    'Korea, Republic of Korea',
    "Republic of Moldova",
    "Russia",
    "Sudan",
    "Syrian Arab Republic",
    "United Arab Emirates",
    "United Kingdom",
    "United States",
    "Venezuela")


####
colnames(population)[7]<-"id"

levels(population$id)[match(c(
                         "Bolivia (Plurinational State of)",
                         "C?te d'Ivoire",
                         "Democratic People's Republic of Korea",
                         "Libya",
                         "Micronesia (Federated States of)",
                          "Republic of Korea",
                         "Russian Federation",
                         "South Sudan",
                         "United States of America",
                         "Venezuela (Bolivarian Republic of)"),levels(population$id))] <-
  c("Bolivia",
    "Cote d'Ivoire",
    "Korea, Democratic People's Republic of",
    "Libyan Arab Jamahiriya",
    "Micronesia, Federated States of",
    'Korea, Republic of Korea',
     "Russia",
    "Sudan",
    "United States",
    "Venezuela")

####
colnames(annualtravel)[1]<-"id"
levels(annualtravel$id)[match(c(
  'Samoa, American',
  'Virgin Islands, British',
  "Burma (Myanmar)",
  "China, People's Republic of",
  "Congo, the Democratic Republic of the",
  'Falkland Islands',
  'Faeroe Islands',
  'French Southern Territories',
  'Vatican City State',
  'Hong Kong (Special Administrative Region)',
  'Iran',
  'Laos',
  'Libya',
  'Macau (Special Administrative Region)',
  'Gaza Strip/Palestine/West Bank',
  'Moldova',
  'St Helena',
  'St Kitts and Nevis',
  'St Lucia',
  'St Maarten (Dutch Part)',
  'St Pierre and Miquelon',
  'St Vincent and the Grenadines',
  'South Georgia and the South Sandwich Islands',
  'Syria',
  'Former Yugoslav Republic of Macedonia (FYROM)',
  'Tanzania',
  'United States of America',
  'Virgin Islands, United States',
  'Wallis and Futuna'),levels(annualtravel$id))] <-
  c(
    "American Samoa",
    "British Virgin Islands",
    "Burma",
    "China",
      "Democratic Republic of the Congo",
    "Falkland Islands (Malvinas)",
    "Faroe Islands",
    "French Southern and Antarctic Lands",
    "Holy See (Vatican City)",
    "Hong Kong",
    "Iran (Islamic Republic of)",
    "Lao People's Democratic Republic",
    "Libyan Arab Jamahiriya",
    "Macau",
    "Palestine",
      "Republic of Moldova",
    "Saint Helena",
    "Saint Kitts and Nevis",
    'Saint Lucia',
    'Saint Martin',
    'Saint Pierre and Miquelon',
    'Saint Vincent and the Grenadines',
    'South Georgia South Sandwich Islands',
    'Syrian Arab Republic',
    'The former Yugoslav Republic of Macedonia',
    "United Republic of Tanzania",
    "United States",
    "United States Virgin Islands",
    "Wallis and Futuna Islands")

##
####
colnames(monthtravel)[1]<-"id"
levels(monthtravel$id)[match(c(
  'Samoa, American',
  'Virgin Islands, British',
  "Burma (Myanmar)",
  "China, People's Republic of",
  "Congo, the Democratic Republic of the",
  'Falkland Islands',
  'Faeroe Islands',
  'French Southern Territories',
  'Vatican City State',
  'Hong Kong (Special Administrative Region)',
  'Iran',
  'Laos',
  'Libya',
  'Macau (Special Administrative Region)',
  'Gaza Strip/Palestine/West Bank',
  'Moldova',
  'St Helena',
  'St Kitts and Nevis',
  'St Lucia',
  'St Maarten (Dutch Part)',
  'St Pierre and Miquelon',
  'St Vincent and the Grenadines',
  'South Georgia and the South Sandwich Islands',
  'Syria',
  'Former Yugoslav Republic of Macedonia (FYROM)',
  'Tanzania',
  'United States of America',
  'Virgin Islands, United States',
  'Wallis and Futuna'),levels(monthtravel$id))] <-
  c(
    "American Samoa",
    "British Virgin Islands",
    "Burma",
    "China",
    "Democratic Republic of the Congo",
    "Falkland Islands (Malvinas)",
    "Faroe Islands",
    "French Southern and Antarctic Lands",
    "Holy See (Vatican City)",
    "Hong Kong",
    "Iran (Islamic Republic of)",
    "Lao People's Democratic Republic",
    "Libyan Arab Jamahiriya",
    "Macau",
    "Palestine",
    "Republic of Moldova",
    "Saint Helena",
    "Saint Kitts and Nevis",
    'Saint Lucia',
    'Saint Martin',
    'Saint Pierre and Miquelon',
    'Saint Vincent and the Grenadines',
    'South Georgia South Sandwich Islands',
    'Syrian Arab Republic',
    'The former Yugoslav Republic of Macedonia',
    "United Republic of Tanzania",
    "United States",
    "United States Virgin Islands",
    "Wallis and Futuna Islands")



m1 <- merge(incidence, cover, by.x = "id",by.y="id")
m2 <- merge(cover, m1, by.x = "id",by.y="id")
dim(m2)

## to use the travel data...
names(immigration)
head(immigration)
class(immigration)

## silly dataset contains weird comma's that R doesn't detect as the thousands separator.
## This magic fixes it...

# convert to characters
num_clients_char <- as.character(immigration$Number.of.Clients)
# split up by the silly comma
num_clients_split <- strsplit(num_clients_char, ",")
# now combine back
combine_thousands <- function(x)
{
  x <- as.numeric(x)
  thousand_power <- ((length(x):1)-1)*3
  sum(10^thousand_power*x)
}
immigration$Number.of.Clients <- sapply(num_clients_split, combine_thousands)

class(immigration$Month.of.Arrival)
test<-aggregate( cbind(  Number.of.Clients ) ~ Nationality , data = immigration , sum )
dim(test)
head(test)
colnames(test)<-c("id","Immigration")
m3 <- merge(m2,test, by.x = "id",by.y="id",all=T)

dim(m3)
#####################

## match the immigration year to the measles data

head(immigration)
class(immigration$Month.of.Arrival)
summary(immigration$Month.of.Arrival)
newdata<-immigration[which(immigration$Month.of.Arrival%in%c("2012-01",
                                                           "2012-02",
                                                           "2012-03",
                                                           "2012-04",
                                                           "2012-05",
                                                           "2012-06",
                                                           "2012-07",
                                                           "2012-08",
                                                           "2012-09",
                                                           "2012-10",
                                                           "2012-11",
                                                           "2012-12")),]

testn<-aggregate( cbind(Number.of.Clients ) ~ Nationality , data = newdata , sum )
dim(testn)
head(testn)
colnames(testn)<-c("id","Immigration")
m4 <- merge(m2,testn, by.x = "id",by.y="id",all=T)

dim(m4)

## 0-19 years - to see if it changes when only young are considered
#
#newdata2<-newdata[which(newdata$Age.Range%in%c("0-19 Years")),]
#
#testn2<-aggregate( cbind(Number.of.Clients ) ~ Nationality , data = newdata2 , sum )
#dim(testn2)
#head(testn2)
#colnames(testn2)<-c("id","Immigration")
#m5 <- merge(m2,testn2, by.x = "id",by.y="id",all=T)
#
#dim(m5)
#######

names(population)
head(population)
summary(population$Indicator=="Population (in thousands) total")
newpop<-population[which(population$Indicator%in%c("Population (in thousands) total")),]
newpop2<-newpop[which(newpop$Year==2012),]
head(newpop2)

mp <- merge(m4,newpop2, by.x = "id",by.y="id",all=T)

dpdf <- data.frame(id = mp$id,
                  incidence = mp$X2012.x/mp$Numeric.Value*1000, ## note 2012 best data
                  cover = mp$X2012.y,## 2012 best data
                  immigration = mp$Immigration) 

head(dpdf)

np1<-ggplot(dpdf, aes(map_id = id)) +ggtitle("Immigration") +
  geom_map(aes(fill = immigration), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
 # theme(text = element_text(size=30))+
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")
pdf(paste("np1.pdf"), width=10, height=5)
np1
dev.off()

np2<-ggplot(dpdf, aes(map_id = id)) +ggtitle("Incidence (per million)") +
  geom_map(aes(fill = incidence), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
 # theme(text = element_text(size=30))+
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")
pdf(paste("np2.pdf"), width=10, height=5)
np2
dev.off()

np3<-ggplot(dpdf, aes(map_id = id)) +ggtitle("Vaccination cover (%)") +
  geom_map(aes(fill = cover), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
  # theme(text = element_text(size=30))+
  scale_fill_gradient("",low = "red", high = "blue", guide = "colorbar")
pdf(paste("np3.pdf"), width=10, height=5)
np3
dev.off()

grid.arrange(np1, np2, np3, ncol=3, main="Measles")

dpdf$risk <- with(dpdf, mp$X2012.x/mp$Numeric.Value*1000* mp$Immigration)
head(dpdf)
write.csv(dpdf, "mapdata.csv", row.names=FALSE,col.names=T)
write.csv(dpdf, "mapdata_immigration.csv", row.names=FALSE,col.names=T)
dpdf<-read.csv("mapdata.csv",header=T)

np4<-ggplot(dpdf, aes(map_id = id)) +ggtitle("Risk  (Immigrant travel)") +
  geom_map(aes(fill = risk), map =world.ggmap) +
  xlab("longitude") +
  ylab("latitude") +
 # theme(text = element_text(size=30))+
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")

pdf(paste("np4.pdf"), width=10, height=5)
np4
dev.off()

grid.arrange(np4, np1, np2, np3, ncol=2, main="Measles (2012)")

# output top countries

topimmigration <- dpdf[order(-dpdf$immigration),] 
topimmigration[1:10,c(1,4)]

topincidence <- dpdf[order(-dpdf$incidence),] 
topincidence[1:10,c(1,2)]

topvaccine <- dpdf[order(dpdf$cover),] 
topvaccine[1:10,c(1,3)]

toprisk <- dpdf[order(-dpdf$risk),] 
toprisk[1:10,c(1,5)]

##

colnames(annualtravel)[2:7]<-c("nzX2008","nzX2009","nzX2010","nzX2011","nzX2012","nzX2013")

mpNZ <- merge(mp,annualtravel, by.x = "id",by.y="id",all=T)

dim(mpNZ)


dpdfNZ <- data.frame(id = mpNZ$id,
                   incidence = mpNZ$X2012.x/mpNZ$Numeric.Value*1000, ## note 2012 best data
                   cover = mpNZ$X2012.y,## 2012 best data
                   immigration = mpNZ$nzX2012) 

dpdfNZ$risk <- with(dpdfNZ, mpNZ$X2012.x/mpNZ$Numeric.Value*1000* mpNZ$nzX2012)
head(dpdfNZ)

head(dpdfNZ)
plot(dpdfNZ$id,dpdfNZ$immigration)

dpdfNZ <- subset(dpdfNZ,id != "Total")
dpdfNZ <- subset(dpdfNZ,id != "ASIA")

nznp1<-ggplot(dpdfNZ, aes(map_id = id)) +ggtitle("Travel") +
  geom_map(aes(fill = immigration), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
#  theme(text = element_text(size=30))+
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

pdf(paste("nznp1.pdf"), width=10, height=5)
nznp1
dev.off()

nznp2<-ggplot(dpdfNZ, aes(map_id = id)) +ggtitle("Incidence (per million)") +
  geom_map(aes(fill = incidence), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
#  theme(text = element_text(size=30))+
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

pdf(paste("nznp2.pdf"), width=10, height=5)
nznp2
dev.off()

nznp3<-ggplot(dpdfNZ, aes(map_id = id)) +ggtitle("Vaccination cover (%)") +
  geom_map(aes(fill = cover), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
#  theme(text = element_text(size=30))+
  scale_fill_gradient("",low = "red", high = "blue", guide = "colorbar")

pdf(paste("nznp3.pdf"), width=10, height=5)
nznp3
dev.off()

grid.arrange(nznp1, nznp2, nznp3, ncol=3, main="Measles")

write.csv(dpdfNZ, "mapdata_nzers.csv", row.names=FALSE,col.names=T)
#dpdf<-read.csv("mapdata.csv",header=T)

nznp4<-ggplot(dpdfNZ, aes(map_id = id)) +ggtitle("Risk (New Zealander travel)") +
  geom_map(aes(fill = risk), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
#  theme(text = element_text(size=30))+
  scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")

pdf(paste("nznp4.pdf"), width=10, height=5)
nznp4
dev.off()


grid.arrange(nznp4, nznp1, nznp2, nznp3, ncol=2, main="Measles (2012)")

##

dpdfNZTot <- data.frame(id = mpNZ$id,
                     incidence = mpNZ$X2012.x/mpNZ$Numeric.Value*1000, ## note 2012 best data
                     cover = mpNZ$X2012.y,## 2012 best data
                     immigration = mpNZ$nzX2012+mpNZ$Immigration) 

dpdfNZTot$risk <- with(dpdfNZTot, mpNZ$X2012.x/mpNZ$Numeric.Value*1000* (mpNZ$Immigration+mpNZ$nzX2012))
head(dpdfNZTot)

plot(dpdfNZTot$id,dpdfNZTot$immigration)

dpdfNZTot <- subset(dpdfNZTot,id != "Total")
dpdfNZTot <- subset(dpdfNZTot,id != "ASIA")

totnp1<-ggplot(dpdfNZTot, aes(map_id = id)) +ggtitle("Travel") +
  geom_map(aes(fill = immigration), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
 # theme(text = element_text(size=30))+
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

pdf(paste("totnp1.pdf"), width=10, height=5)
totnp1
dev.off()

totnp2<-ggplot(dpdfNZTot, aes(map_id = id)) +ggtitle("Incidence (per million)") +
  geom_map(aes(fill = incidence), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
#  theme(text = element_text(size=30))+
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

pdf(paste("totnp2.pdf"), width=10, height=5)
totnp2
dev.off()

totnp3<-ggplot(dpdfNZTot, aes(map_id = id)) +ggtitle("Vaccination cover (%)") +
  geom_map(aes(fill = cover), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
  #theme(text = element_text(size=30))+
  scale_fill_gradient("",low = "red", high = "blue", guide = "colorbar")

pdf(paste("totnp3.pdf"), width=10, height=5)
totnp3
dev.off()

grid.arrange(totnp1, totnp2, totnp3, ncol=3, main="Measles")

write.csv(dpdfNZTot, "mapdata_tot.csv", row.names=FALSE,col.names=T)
#dpdf<-read.csv("mapdata.csv",header=T)

totnp4<-ggplot(dpdfNZTot, aes(map_id = id)) +ggtitle("Risk (All travel)") +
  geom_map(aes(fill = risk), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  xlab("longitude") +
  ylab("latitude") +
#  theme(text = element_text(size=30))+
  scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")

pdf(paste("totnp4.pdf"), width=10, height=5)
totnp4
dev.off()


grid.arrange(totnp4, totnp1, totnp2, totnp3, ncol=2, main="Measles (2012)")

##

pdf(paste("risk_measles_base.pdf"))
grid.arrange(np2,np3, ncol=1, main="Measles incidence and vaccination cover (2012)")
dev.off()

pdf(paste("risk_measles_foreign.pdf"))
grid.arrange(np1, np4, ncol=1, main="Foreign travellers to New Zealand and risk (Travellers * Country incidence)")
dev.off()

pdf(paste("risk_measles_nzers.pdf"))
grid.arrange(nznp1, nznp4, ncol=1, main="New Zealand travellers and risk (Travellers * Country incidence)")
dev.off()

pdf(paste("risk_measles_tot.pdf"))
grid.arrange(totnp1, totnp4, main="Total travellers and risk (Travellers * Country incidence)")
dev.off()


##
ttimes<-read.csv("times.csv",header=T)
lmres<-lm(res~time,data=ttimes)
summary(lmres)
plot(ttimes$res,xaxt="n",type="l",xlab="Date",ylab="")
axis(1, at=1:71, labels=ttimes$mydate) 
abline(lmres,col="red",
       lty=2)
acf(ttimes$res,lag.max = length(ttimes$res),plot=T,main="")
change<-read.csv("change.csv",header=T)

plot(change[18:25,1],change[18:25,4],type="l",xlab="Year",ylab="",ylim=c(0,max(change[,4])),
     bty="n")
ylim <- range(c(0,max(change[,4])))
xlim <- range(change[,1])
par(fig = c(.6, 1, 0.6, 1), mar=c(0,0,0,0), new=TRUE)
plot(change[,1],change[,4],type="l",xlab="Year",ylab="",
     ylim=c(0,max(change[,4])),bg="grey",add=T,bty="n")

## set wd to get data
nzers<-read.csv("monthtravel.csv",header=T)
head(nzers)
names(nzers)
dim(nzers)

plot(as.numeric(nzers[247,2:79]),type="l",ylim=c(min(as.numeric(nzers[247,2:79])),max(ttimes$res)),
)
points(ttimes$res,add=T,type="l",col="red")

par(mar = c(7, 4, 4, 2) + 0.1)
plot(as.numeric(nzers[247,8:79]),type="l",xaxt="n",xlab="",ylab="Numbers",
     ,ylim=c(min(as.numeric(nzers[247,2:79])),max(ttimes$res)))
points(ttimes$res,type="l",col="red")
# Set up x axis with tick marks alone
# Create arbitrary text
labels <- paste(ttimes$mydate, sep = " ")
axis(side=1, at=1:71, labels=labels,las=2)
legend("topleft",c("New Zealanders","Non-New Zealanders"),
       bty="n",col=c("black","red"),lty=1)
# check same length
length(nzers[247,8:78])
length(ttimes$res)

nz<-nzers[247,8:78]
im<-ttimes$res
newd<-rbind(nz,im)
rownames(newd)<-c("nz","im")
colnames(newd)<-ttimes$mydate
sum<-colSums(newd)
newd<-rbind(sum,newd)
rownames(newd)<-c("total","nz","im")

pdf(paste("nzers.pdf"), width=5, height=5)
plot(as.numeric(newd[1,]),type="l",xaxt="n",xlab="",ylab="Numbers",
     ,ylim=c(min(newd),max(newd)),cex.lab=0.6,cex.axis=0.6)
points(as.numeric(newd[2,]),type="l",col="red")
points(as.numeric(newd[3,]),type="l",col="black",lty=2)
# Set up x axis with tick marks alone
# Create arbitrary text
labels <- paste(colnames(newd), sep = " ")
axis(side=1, at=seq(from=1,to=71,by=2), cex=0.5,labels=labels[seq(from=1,to=71,by=2)],las=2,cex.axis=0.6)
abline(v=seq(from=6,to=71,by=12),lty=3,col="grey")
legend("topleft",bg="white",c("Total","New Zealanders","Non-New Zealanders"),
       col=c("black","red","black"),lty=c(1,1,2),box.col=rgb(0,0,0,alpha=0.5) , cex=0.6)
dev.off()

plot(as.numeric(newd[1,]),type="l",xaxt="n",xlab="",ylab="Numbers",
     ,ylim=c(min(newd),max(newd)))
points(as.numeric(newd[2,]),type="l",col="red")
points(as.numeric(newd[3,]),type="l",col="black",lty=2)
# Set up x axis with tick marks alone
# Create arbitrary text
labels <- paste(colnames(newd), sep = " ")
axis(side=1, at=seq(from=1,to=71,by=2), labels=labels[seq(from=1,to=71,by=2)],las=2)
abline(v=seq(from=6,to=71,by=12),lty=3,col="grey")
legend("topleft",bg="white",c("Total","New Zealanders","Non-New Zealanders"),
       col=c("black","red","black"),lty=c(1,1,2),box.col=rgb(0,0,0,alpha=0.5) )
# check same length
