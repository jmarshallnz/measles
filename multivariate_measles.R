## regression analyses - measles
## get data
# set wd
setwd("~/Cambridge/Massey 2014/DHayman_20140627")
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
test$AgeInYears<-tage
summary(test$EthnicityPrioritised)
teth<-revalue(test$EthnicityPrioritised, c("European or Other"="European", "Middle Eastern/Latin American/African"="MLA",
                                     "Pacific Peoples"="Pacific","Response cannot be classified"="None","Unknown"="None"));

test$EthnicityPrioritised<-teth
head(test)

# want to set NZ Deprivation to the appropriate year
## why does the following add 1 to all the values?
testt <- within(test, NZDep<- ifelse (test$RptYear == "2007",test$NZDep06,
                              ifelse (test$RptYear == "2008",test$NZDep06,
                              ifelse (test$RptYear == "2009",test$NZDep06,
                              ifelse (test$RptYear == "2010",test$NZDep06,
                              ifelse (test$RptYear == "2011",test$NZDep06,
                              ifelse (test$RptYear == "2012",test$NZDep06,
                              ifelse (test$RptYear == "2013",test$NZDep13,NZDep13))))))))
head(testt)
tail(testt)
str(testt)
testt$NZDep<-as.factor(testt$NZDep-1)

testtable<-aggregate( cbind( DiseaseName + as.numeric(SurvWeek)) ~ NZDep+AgeInYears+EthnicityPrioritised + RptYear, 
                 data = testt , FUN=sum)
dim(testtable)
colnames(testtable)<-c("NZDep","Age","Ethnicity","Year","Cases")
pairs(testtable,panel=panel.smooth)
## appears very little correlation/relationships between them
#
# don't think the next pair of models are valid - 
# because no year (proxy for outbreak) effect accounted for
#
# model1<-aov(Cases~NZDep*Age*Ethnicity,data=testtable)
# summary(model1)
# summary.lm(model1)
#
# model2<-aov(Cases~NZDep+Age+Ethnicity,data=testtable)
# summary(model2)
# summary.lm(model2)
#
# also suggests too many age categories
str(testtable)
levels(testtable$Age)
levels(testtable$Age)[c(5:10)]<-"Old"

model3<-aov(Cases~NZDep*Age*Ethnicity,data=testtable)
summary(model3)
summary.lm(model3)
# looks like no need to keep interaction terms

## account for random year effect (proxy for outbreak)
model4<-aov(Cases~NZDep*Age*Ethnicity+Error(Year),data=testtable)
summary(model4)
# still looks like no need for interaction terms

model5<-aov(Cases~NZDep+Age+Ethnicity+Error(Year),data=testtable)
summary(model5)

model6<-aov(Cases~NZDep+Age+Ethnicity,data=testtable)
summary(model6)
summary.lm(model6)

## results - though many age categries, richer more likely, 1-3 & 9-13 Yrs, Europeans 
## caveats - temporal and spatial autocorrelation, though year random effect
## caveats - no "per capita" analyses - just raw data
