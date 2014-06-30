## regression analyses - measles
## get data
# set wd
setwd("~/Massey_2014/DHayman_20140627")
# read data
data<-read.csv("DHayman_20140627.csv",header=T)
names(data)
data$RptYear<-as.factor(data$RptYear)
data$SurvWeek<-as.factor(data$SurvWeek)
data$NZDep01<-as.factor(data$NZDep01)
data$NZDep06<-as.factor(data$NZDep06)
data$NZDep13<-as.factor(data$NZDep13)
#data$AgeInYears<-as.factor(data$AgeInYears)
data$EthnicityPrioritised<-as.factor(data$EthnicityPrioritised)

time<-aggregate( cbind( DiseaseName ) ~ NZDep01 +NZDep06+NZDep13+AgeInYears+EthnicityPrioritised + SurvWeek + RptYear, 
                 data = data , FUN=sum)

library(reshape)
library(reshape2)
library(plyr)

test<-subset(time, (RptYear %in% c("2007","2008","2009","2010","2011","2012","2013","2014")))
head(test)
dim(test)

test$AgeInYears<-findInterval(test$AgeInYears,c(1,4,9,14,19,29,39,49,59,69,79))
test$AgeInYears<-as.factor(test$AgeInYears)
tage<-revalue(test$AgeInYears, c("0"="<1", "1"="1-3","2"="4-8","3"="9-13","4"="14-18","5"="19-28","6"="29-38","7"="39-48","8"="49-58","9"="59-68"));
tage
test$AgeInYears<-tage
summary(test$EthnicityPrioritised)
teth<-revalue(test$EthnicityPrioritised, c("European or Other"="European", "Middle Eastern/Latin American/African"="MLA",
                                     "Pacific Peoples"="Pacific","Response cannot be classified"="None","Unknown"="None"));

test$EthnicityPrioritised<-teth
head(test)
