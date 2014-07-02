setwd("C:/Users/David Hayman/Dropbox/measles/data")

immigration<-read.csv("immigration.csv",header=T)
head(immigration)

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

## match the immigration year to the measles data

head(immigration)
class(immigration$Month.of.Arrival)
summary(immigration$Month.of.Arrival)

testn<-aggregate( Number.of.Clients ~  Month.of.Arrival, data = immigration , sum )
dim(testn)
head(testn)

plot(testn[,1],testn[,2],type="l")
summary(testn)

plot(testn[,2],type="l")
res<-testn[,2]
time<-1:as.numeric(length(res))
acf(res, lag.max = length(res),plot=T,main="")
mydate<-testn[,1]

resdf<-data.frame(mydate,time,res)
write.csv(resdf,"times.csv",row.names=FALSE)
ttimes<-read.csv("times.csv",header=T)
lmres<-lm(res~time,data=ttimes)
summary(lmres)
plot(ttimes$res,xaxt="n",type="l",xlab="Date",ylab="")
axis(1, at=1:71, labels=ttimes$mydate) 
abline(lmres,col="red",
       lty=2)
acf(ttimes$res,lag.max = length(ttimes$res),plot=T,main="")
