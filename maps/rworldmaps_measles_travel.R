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
setwd("~/data")
#unzip("TM_WORLD_BORDERS_SIMPL-0.3.zip")
world.map <- readOGR(dsn="C:/Users/David Hayman/Documents/data", layer="TM_WORLD_BORDERS_SIMPL-0.3")

setwd("C:/Users/David Hayman/Documents/GitHub/measles/maps")

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

world.ggmap <- fortify(world.map, region = "NAME")
save(world.ggmap,file="world.ggmap.Rda")
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

id
5                              American Samoa
30                              Bouvet Island
33                     British Virgin Islands
37                                      Burma
42                                 Cape Verde
47                                      China
48                           Christmas Island
49                    Cocos (Keeling) Islands
57                                       Cuba
59                             Czech Republic
60           Democratic Republic of the Congo
72                Falkland Islands (Malvinas)
73                              Faroe Islands
79        French Southern and Antarctic Lands
83                                    Germany
92                                   Guernsey
97          Heard Island and McDonald Islands
98                    Holy See (Vatican City)
100                                 Hong Kong
105                Iran (Islamic Republic of)
108                               Isle of Man
113                                    Jersey
117                                  Kiribati
122          Lao People's Democratic Republic
127                    Libyan Arab Jamahiriya
131                                     Macau

145                                    Monaco
146                                  Mongolia
147                                Montenegro
148                                Montserrat
149                                   Morocco
150                                Mozambique
151                                   Namibia
152                                     Nauru
153                                     Nepal
154                               Netherlands
155                      Netherlands Antilles
156                             New Caledonia
157                               New Zealand
158                                 Nicaragua
159                                     Niger
160                                   Nigeria
161                                      Niue
162                            Norfolk Island
163                  Northern Mariana Islands
164                                    Norway
165                                      Oman
166                                  Pakistan
167                                     Palau
168                                 Palestine
169                                    Panama
170                          Papua New Guinea
171                                  Paraguay
172                                      Peru
173                               Philippines
174                          Pitcairn Islands
175                                    Poland
176                                  Portugal
177                               Puerto Rico
178                                     Qatar
179                       Republic of Moldova
180                                   Reunion
181                                   Romania
182                                    Russia
183                                    Rwanda
184                          Saint Barthelemy
185                              Saint Helena
186                     Saint Kitts and Nevis
187                               Saint Lucia
188                              Saint Martin
189                 Saint Pierre and Miquelon
190          Saint Vincent and the Grenadines
191                                     Samoa
192                                San Marino
193                     Sao Tome and Principe
194                              Saudi Arabia
195                                   Senegal
196                                    Serbia
197                                Seychelles
198                              Sierra Leone
199                                 Singapore
200                                  Slovakia
201                                  Slovenia
202                           Solomon Islands
203                                   Somalia
204                              South Africa
205      South Georgia South Sandwich Islands
206                                     Spain
207                                 Sri Lanka
208                                     Sudan
209                                  Suriname
210                                  Svalbard
211                                 Swaziland
212                                    Sweden
213                               Switzerland
214                      Syrian Arab Republic
215                                    Taiwan
216                                Tajikistan
217                                  Thailand
218 The former Yugoslav Republic of Macedonia
219                               Timor-Leste
220                                      Togo
221                                   Tokelau
222                                     Tonga
223                       Trinidad and Tobago
224                                   Tunisia
225                                    Turkey
226                              Turkmenistan
227                  Turks and Caicos Islands
228                                    Tuvalu
229                                    Uganda
230                                   Ukraine
231                      United Arab Emirates
232                            United Kingdom
233               United Republic of Tanzania
234                             United States
235      United States Minor Outlying Islands
236              United States Virgin Islands
237                                   Uruguay
238                                Uzbekistan
239                                   Vanuatu
240                                 Venezuela
241                                  Viet Nam
242                 Wallis and Futuna Islands
243                            Western Sahara
244                                     Yemen
245                                    Zambia
246                                  Zimbabwe
Countries

[33] Burma (Myanmar)
[35] Cabo Verde
[43] China, People's Republic of                   
[45] Cocos (Keeling) Islands                    
[55] Curacao                                       Cyprus                                       
[57] Czech Republic                                Czechoslovakia                               
Faeroe Islands                               
[71] Falkland Islands                              Former Yugoslav Republic of Macedonia (FYROM)
French Southern Territories                  
[81] Gaza Strip/Palestine/West Bank                
[83] Germany                                       Germany, Democratic Republic of              
Hong Kong (Special Administrative Region)    
[103] Iran                                          Israel                                       
Kosovo                                       
[119] Laos                                          
Libya                                        Macau (Special Administrative Region)        
Mexico                                       
Moldova                                      

[143] Monaco                                        Mongolia                                     
[145] Montenegro                                    Montserrat                                   
[147] Morocco                                       Mozambique                                   
[149] Namibia                                       Nauru                                        
[151] Nepal                                         Netherlands                                  
[153] Netherlands Antilles                          New Caledonia                                
[155] New Zealand                                   Nicaragua                                    
[157] Niger                                         Nigeria                                      
[159] Niue                                          Norfolk Island                               
[161] Northern Mariana Islands                      Norway                                       
[163] Oman                                          Pakistan                                     
[165] Palau                                         Panama                                       
[167] Papua New Guinea                              Paraguay                                     
[169] Peru                                          Philippines                                  
[171] Pitcairn                                      Poland                                       
[173] Portugal                                      Puerto Rico                                  
[175] Qatar                                         Reunion                                      
[177] Romania                                       Russia                                       
[179] Rwanda                                        Samoa                                        
[181] Samoa, American                               San Marino                                   
[183] Sao Tome and Principe                         Saudi Arabia                                 
[185] Senegal                                       Serbia                                       
[187] Seychelles                                    Sierra Leone                                 
[189] Singapore                                     Slovakia                                     
[191] Slovenia                                      Solomon Islands                              
[193] Somalia                                       South Africa                                 
[195] South Georgia and the South Sandwich Islands  South Sudan                                  
[197] Spain                                         Sri Lanka                                    
[199] St Helena                                     St Kitts and Nevis                           
[201] St Lucia                                      St Maarten (Dutch Part)                      
[203] St Pierre and Miquelon                        St Vincent and the Grenadines                
[205] Sudan                                         Suriname                                     
[207] Swaziland                                     Sweden                                       
[209] Switzerland                                   Syria                                        
[211] Taiwan                                        Tajikistan                                   
[213] Tanzania                                      Thailand                                     
[215] Timor-Leste                                   Togo                                         
[217] Tokelau                                       Tonga                                        
[219] Trinidad and Tobago                           Tunisia                                      
[221] Turkey                                        Turkmenistan                                 
[223] Turks and Caicos Islands                      Tuvalu                                       
[225] Uganda                                        Ukraine                                      
[227] Union of Soviet Socialist Republics           United Arab Emirates                         
[229] United Kingdom                                United States Minor Outlying Islands         
[231] United States of America                      Uruguay                                      
[233] Uzbekistan                                    Vanuatu                                      
[235] Vatican City State                            Venezuela                                    
[237] Viet Nam                                      Virgin Islands, British                      
[239] Virgin Islands, United States                 Wallis and Futuna                            
[241] Western Sahara                                Yemen                                        
[243] Yemen, Democratic                             Yugoslavia/Serbia and Montenegro             
[245] Zambia                                        Zimbabwe                                     
[247] Total 

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

dim(mp)
head(mp)
dpdf <- data.frame(id = mp$id,
                  incidence = mp$X2012.x/mp$Numeric.Value*1000, ## note 2012 best data
                  cover = mp$X2012.y,## 2012 best data
                  immigration = mp$Immigration) 

head(dpdf)

np1<-ggplot(dpdf, aes(map_id = id)) +ggtitle("Immigration") +
  geom_map(aes(fill = immigration), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

np2<-ggplot(dpdf, aes(map_id = id)) +ggtitle("Incidence (per million)") +
  geom_map(aes(fill = incidence), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

np3<-ggplot(dpdf, aes(map_id = id)) +ggtitle("Vaccination cover (%)") +
  geom_map(aes(fill = cover), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "red", high = "blue", guide = "colorbar")

grid.arrange(np1, np2, np3, ncol=3, main="Measles")

dpdf$risk <- with(dpdf, mp$X2012.x/mp$Numeric.Value*1000* mp$Immigration)
head(dpdf)
write.csv(dpdf, "mapdata.csv", row.names=FALSE,col.names=T)
dpdf<-read.csv("mapdata.csv",header=T)

np4<-ggplot(dpdf, aes(map_id = id)) +ggtitle("Risk 2012") +
  geom_map(aes(fill = risk), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")
np4

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

