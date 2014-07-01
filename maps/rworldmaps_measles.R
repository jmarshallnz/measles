library(maps)       # Provides functions that let us plot the maps
library(mapdata)    # Contains the hi-resolution points that mark out the countries.
library(rworldmap)
library(adehabitat)
library(rgdal)
library(raster)
library(maps)
library(maptools)
library(sp)
#library(SDMTools)
library(ggplot2)
require(rgeos)
library(ggmap)
library(mapdata)
gpclibPermit()
library(mapproj)
library(plyr)
library(grid)
library(gridExtra)
##

world_map <- map_data("world")
world.ggmap <- fortify(world_map, region = "NAME")

n <- length(unique(world.ggmap$region))
df <- data.frame(id = unique(world.ggmap$region),
                 growth = 4*runif(n),
                 category = factor(sample(1:5, n, replace=T)))

## noise
df[c(sample(1:100,40)),c("growth", "category")] <- NA


ggplot(df, aes(map_id = id)) +
  geom_map(aes(fill = growth, color = category), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient(low = "red", high = "blue", guide = "colorbar")

map.world <- map_data(map = "world")
# map = name of map provided by the maps package.
# These include county, france, italy, nz, state, usa, world, world2.
str(map.world)
# how many regions
length(unique(map.world$region))
# how many group polygons (some regions have multiple parts)
length(unique(map.world$group))
p1 <- ggplot(map.world, aes(x = long, y = lat, group = group))
p1 <- p1 + geom_polygon() # fill areas
p1 <- p1 + labs(title = "World, plain")
#print(p1)
p2 <- ggplot(map.world, aes(x = long, y = lat, group = group, colour = region))
p2 <- p2 + geom_polygon() # fill areas
p2 <- p2 + theme(legend.position="none") # remove legend with fill colours
p2 <- p2 + labs(title = "World, colour borders")
#print(p2)
p3 <- ggplot(map.world, aes(x = long, y = lat, group = group, fill = region))
p3 <- p3 + geom_polygon() # fill areas
p3 <- p3 + theme(legend.position="none") # remove legend with fill colours
p3 <- p3 + labs(title = "World, filled regions")
#print(p3)
p4 <- ggplot(map.world, aes(x = long, y = lat, group = group, colour = region))
p4 <- p4 + geom_path() # country outline, instead
p4 <- p4 + theme(legend.position="none") # remove legend with fill colours
p4 <- p4 + labs(title = "World, path outlines only")
#print(p4)
grid.arrange(p1, p2, p3, p4, ncol=2, main="ggmap examples")


# Data from http://thematicmapping.org/downloads/world_borders.php.
# Direct link: http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip
# Unpack and put the files in a dir 'data'

gpclibPermit()
library(utils)
library(rgdal)
getwd()
setwd("~/data")
unzip("TM_WORLD_BORDERS_SIMPL-0.3.zip")
world.map <- readOGR(dsn="C:/Users/dtshayma/Documents/data", layer="TM_WORLD_BORDERS_SIMPL-0.3")

world.ggmap <- fortify(world.map, region = "NAME")

n <- length(unique(world.ggmap$id))
df <- data.frame(id = unique(world.ggmap$id),
                 growth = 4*runif(n),
                 category = factor(sample(1:5, n, replace=T)))

## noise
df[c(sample(1:100,40)),c("growth", "category")] <- NA


ggplot(df, aes(map_id = id)) +
  geom_map(aes(fill = growth, color = category), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient(low = "red", high = "blue", guide = "colorbar")

ggplot(df, aes(map_id = id)) +
  geom_map(aes(fill = growth), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient(low = "red", high = "blue", guide = "colorbar")

setwd("C:/Users/dtshayma/Dropbox/measles/data")

incidence<-read.csv("incidence_series.csv",header=T)
head(incidence)

cover<-read.csv("coverage_series.csv",header=T)
head(cover)

immigration<-read.csv("immigration.csv",header=T)
head(immigration)

population<-read.csv("population.csv", header=T)
head(population)

#######

world.ggmap <- fortify(world.map, region = "NAME")

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

m1 <- merge(incidence, cover, by.x = "id",by.y="id")
m2 <- merge(cover, m1, by.x = "id",by.y="id")
dim(m2)
df <- data.frame(id = m2$id,
                 incidence = m2$X2012.x, ## note 2012 best data
                 cover = m2$X2012.y) ## 2012 best data

g1<-ggplot(df, aes(map_id = id)) + ggtitle("Both") +
  geom_map(aes(fill = incidence, color = cover), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient(low = "blue", high = "red", guide = "colorbar")

g2<-ggplot(df, aes(map_id = id)) +ggtitle("Incidence") +
  geom_map(aes(fill = incidence), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient(low = "blue", high = "red", guide = "colorbar")

g3<-ggplot(df, aes(map_id = id)) +ggtitle("Vaccination cover (%)") +
  geom_map(aes(fill = cover), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient(low = "red", high = "blue", guide = "colorbar")

#grid.arrange(g1, g2, g3, ncol=3, main="ggmap examples")
grid.arrange(g2, g3, ncol=2, main="Measles")
#######

## to use the travel data...
names(immigration)
head(immigration)
class(immigration)

##

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
df <- data.frame(id = m3$id,
                 incidence = m3$X2012.x, ## note 2012 best data
                 cover = m3$X2012.y,## 2012 best data
                 immigration = m3$Immigration) 

g4<-ggplot(df, aes(map_id = id)) +ggtitle("Immigration") +
  geom_map(aes(fill = immigration), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

g5<-ggplot(df, aes(map_id = id)) +ggtitle("Incidence") +
  geom_map(aes(fill = incidence), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

g6<-ggplot(df, aes(map_id = id)) +ggtitle("Vaccination cover (%)") +
  geom_map(aes(fill = cover), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "red", high = "blue", guide = "colorbar")

grid.arrange(g4, g5, g6, ncol=3, main="Measles")
#grid.arrange(g2, g3, ncol=2, main="Measles")
#######

df$prod <- with(df, (m3$X2012.x)+(100-m3$X2012.y)+(m3$Immigration))
head(df)

g7<-ggplot(df, aes(map_id = id)) +ggtitle("Risk") +
  geom_map(aes(fill = prod), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")
g7

grid.arrange(g4, g5, g6, g7, ncol=2, main="Measles")
plot1<-grid.arrange(g7, arrangeGrob(g4, g5, g6, ncol=3), 
                    ncol=1,heights=c(1.5,0.6))
#####################

## now tease apart data better..

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
dfn <- data.frame(id = m4$id,
                 incidence = m4$X2012.x, ## note 2012 best data
                 cover = m4$X2012.y,## 2012 best data
                 immigration = m4$Immigration) 

ng4<-ggplot(dfn, aes(map_id = id)) +ggtitle("Immigration") +
  geom_map(aes(fill = immigration), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

ng5<-ggplot(dfn, aes(map_id = id)) +ggtitle("Incidence") +
  geom_map(aes(fill = incidence), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

ng6<-ggplot(dfn, aes(map_id = id)) +ggtitle("Vaccination cover (%)") +
  geom_map(aes(fill = cover), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "red", high = "blue", guide = "colorbar")

grid.arrange(ng4, ng5, ng6, ncol=3, main="Measles")
#grid.arrange(g2, g3, ncol=2, main="Measles")
#######

dfn$prod <- with(dfn, (m4$X2012.x)+(100-m4$X2012.y)+(m4$Immigration))
head(dfn)

ng7<-ggplot(dfn, aes(map_id = id)) +ggtitle("Risk") +
  geom_map(aes(fill = prod), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")
ng7

grid.arrange(ng4, ng5, ng6, ng7, ncol=2, main="Measles")
plot1n<-grid.arrange(ng7, arrangeGrob(ng4, ng5, ng6, ncol=3), 
                    ncol=1,heights=c(1.5,0.6))

## product...
dfn$prod <- with(dfn, (m4$X2012.x)*(100-m4$X2012.y)*(m4$Immigration))
head(dfn)


ng7<-ggplot(dfn, aes(map_id = id)) +ggtitle("Risk") +
  geom_map(aes(fill = prod), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")
ng7

grid.arrange(ng4, ng5, ng6, ng7, ncol=2, main="Measles")
plot1n<-grid.arrange(ng7, arrangeGrob(ng4, ng5, ng6, ncol=3), 
                     ncol=1,heights=c(1.5,0.6))
## log(prod)
#ng7<-ggplot(dfn, aes(map_id = id)) +ggtitle("Risk") +
#  geom_map(aes(fill = log(prod)), map =world.ggmap) +
#  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
#  scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")
#ng7


## 0-19 years

newdata2<-newdata[which(newdata$Age.Range%in%c("0-19 Years")),]

testn2<-aggregate( cbind(Number.of.Clients ) ~ Nationality , data = newdata2 , sum )
dim(testn2)
head(testn2)
colnames(testn2)<-c("id","Immigration")
m5 <- merge(m2,testn2, by.x = "id",by.y="id",all=T)

dim(m5)
dfn2 <- data.frame(id = m5$id,
                  incidence = m5$X2012.x, ## note 2012 best data
                  cover = m5$X2012.y,## 2012 best data
                  immigration = m5$Immigration) 

n2g4<-ggplot(dfn2, aes(map_id = id)) +ggtitle("Immigration") +
  geom_map(aes(fill = immigration), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

n2g5<-ggplot(dfn2, aes(map_id = id)) +ggtitle("Incidence") +
  geom_map(aes(fill = incidence), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

n2g6<-ggplot(dfn2, aes(map_id = id)) +ggtitle("Vaccination cover (%)") +
  geom_map(aes(fill = cover), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "red", high = "blue", guide = "colorbar")

grid.arrange(n2g4, n2g5, n2g6, ncol=3, main="Measles")
#grid.arrange(g2, g3, ncol=2, main="Measles")
#######

dfn2$prod <- with(dfn2, (m5$X2012.x)+(100-m5$X2012.y)+(m5$Immigration))
head(dfn2)

n2g7<-ggplot(dfn2, aes(map_id = id)) +ggtitle("Risk") +
  geom_map(aes(fill = prod), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")
n2g7

grid.arrange(n2g4, n2g5, n2g6, n2g7, ncol=2, main="Measles")
plot1n2<-grid.arrange(n2g7, arrangeGrob(n2g4, n2g5, n2g6, ncol=3), 
                     ncol=1,heights=c(1.5,0.6))

###
names(population)
head(population)
summary(population$Indicator=="Population (in thousands) total")
newpop<-population[which(population$Indicator%in%c("Population (in thousands) total")),]
newpop2<-newpop[which(newpop$Year==2012),]
head(newpop2)

mp <- merge(m4,newpop2, by.x = "id",by.y="id",all=T)

dim(mp)
dpdf <- data.frame(id = mp$id,
                  incidence = mp$X2012.x/mp$Numeric.Value*1000, ## note 2012 best data
                  cover = mp$X2012.y,## 2012 best data
                  immigration = mp$Immigration) 

np1<-ggplot(dpdf, aes(map_id = id)) +ggtitle("Immigration") +
  geom_map(aes(fill = immigration), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

np2<-ggplot(dpdf, aes(map_id = id)) +ggtitle("Incidence") +
  geom_map(aes(fill = incidence), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

np3<-ggplot(dpdf, aes(map_id = id)) +ggtitle("Vaccination cover (%)") +
  geom_map(aes(fill = cover), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "red", high = "blue", guide = "colorbar")

grid.arrange(np1, np2, np3, ncol=3, main="Measles")

dpdf$prod <- with(dpdf, mp$X2012.x/mp$Numeric.Value*1000* (100-mp$X2012.y)* mp$Immigration)
head(dpdf)

np4<-ggplot(dpdf, aes(map_id = id)) +ggtitle("Risk") +
  geom_map(aes(fill = prod), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")
np4

dpdf$PCprod <- with(dpdf, mp$X2012.x/mp$Numeric.Value*1000*  mp$Immigration)
head(dpdf)

np5<-ggplot(dpdf, aes(map_id = id)) +ggtitle("Risk") +
  geom_map(aes(fill = PCprod), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")
np5

dpdf$PCPprod <- with(dpdf, (100-mp$X2012.y)/mp$Numeric.Value*1000*mp$Immigration)
head(dpdf)

np6<-ggplot(dpdf, aes(map_id = id)) +ggtitle("Risk") +
  geom_map(aes(fill = PCPprod), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")
  
np6

np7<-ggplot(dpdf, aes(map_id = id)) +ggtitle("log(Risk)") +
  geom_map(aes(fill = log(PCPprod)), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")

np7
## Samoa

dpdf[which.max(dpdf[,7]),1]

ndpdf<-dpdf[!(dpdf$id=="Samoa"),]

np7<-ggplot(ndpdf, aes(map_id = id)) +ggtitle("Risk") +
  geom_map(aes(fill = log(PCPprod)), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")

np7
