rm(list=ls())
## regression analyses - measles
## get data
# set wd
 setwd("~/Massey 2014/DHayman_20140627")
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
library(pscl)

test<-subset(time, (RptYear %in% c("2007","2008","2009","2010","2011","2012","2013","2014")))
head(test)
dim(test)

test$AgeInYears<-findInterval(test$AgeInYears,c(6,18,25,65))
test$AgeInYears<-as.factor(test$AgeInYears)
tage<-revalue(test$AgeInYears, c("0"="<6", "1"="6-17","2"="18-24","3"="25-64","4"="65+"));
test$AgeInYears<-tage
#summary(test$EthnicityPrioritised)
teth<-revalue(test$EthnicityPrioritised, c("European or Other"="European", "Middle Eastern/Latin American/African"="MLA",
                                     "Pacific Peoples"="Pacific","Response cannot be classified"="None","Unknown"="None"));

test$EthnicityPrioritised<-teth
head(test)
test <- within(test, NZDep06[NZDep06 == "0"]<-NA)
test <- within(test, NZDep13[NZDep13 == "0"]<-NA)

# want to set NZ Deprivation to the appropriate year

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
head(testtable)

tnzd<-revalue(testtable$NZDep, c("1"="1-5","2"="1-5","3"="1-5","4"="1-5",
                                 "5"="1-5","6"="6-10","7"="6-10","8"="6-10",
                                 "9"="6-10","10"="6-10"));

testtable$NZDep<-tnzd
head(testtable)
tt<-aggregate( cbind(Cases) ~ NZDep+Age+Ethnicity + Year, 
                 data = testtable , FUN=sum)

### denominator data
 setwd("~/Massey_2014/measles/data")
denom<-read.csv("NZDep2006Denominators.csv",header=T)
head(denom)
denom<-denom[,-c(9)]
summary(denom)
dim(denom)
denom<-denom[,1:18]
row.has.na <- apply(denom, 1, function(x){any(is.na(x))})
sum(row.has.na)
denomt <- denom[!row.has.na,]
head(denomt)
denomt<-denomt[denomt$Sex_code==99,]
denomt$Eth_Level<-as.factor(denomt$Eth_Level)
denomt<-denomt[denomt$Eth_Level %in% c("24","21","66",'22','77'),]
dm<-cbind(denomt[,c(3,6,9:18)])
head(dm)
dm<-dm[dm$Age_Label %in% c("A: 0- 5 yrs", "A: 6-17 yrs","A:18-24 yrs","A:25-64 yrs","A:65+ yrs"),]
dage<-revalue(dm$Age_Label, c("A: 0- 5 yrs"="<6", "A: 6-17 yrs"="6-17","A:18-24 yrs"="18-24","A:25-64 yrs"="25-64","A:65+ yrs"="65+"));
dm$Age_Label<-dage
colnames(dm)<-c('Age','Ethnicity',"Dep1","Dep2","Dep3","Dep4",'Dep5','Dep6','Dep7','Dep8','Dep9',"Dep10")
head(dm)
dm$Ethnicity <- factor(dm$Ethnicity)
deth<-revalue(dm$Ethnicity, c("Asian (Prioritised)"="Asian","European (NZ European and Other European)"="European",
                              "Maori (Prioritised)"="Maori","MELAA"="MLA","Pacific People (Prioritised)"="Pacific"));
dm$Ethnicity<-deth
head(dm)

popn<-melt(dm,id.vars=c("Age","Ethnicity"),measure.vars=c("Dep1","Dep2","Dep3","Dep4",'Dep5','Dep6','Dep7','Dep8','Dep9',"Dep10"))
colnames(popn)<-c("Age","Ethnicity","NZDep","Popn")
popn
pnzd<-revalue(popn$NZDep, c("Dep1"="1-5","Dep2"="1-5","Dep3"="1-5","Dep4"="1-5",
                                 "Dep5"="1-5","Dep6"="6-10","Dep7"="6-10","Dep8"="6-10",
                                 "Dep9"="6-10","Dep10"="6-10"));

popn$NZDep<-pnzd
head(popn)
tp<-aggregate( cbind(Popn) ~ NZDep+Age+Ethnicity, 
               data = popn , FUN=sum)

# JM's working code
#popn$NZDep <- as.numeric(popn$NZDep)
#popn$merge <- paste(popn$NZDep, popn$Age, popn$Ethnicity)
#testtable$merge <- paste(testtable$NZDep, testtable$Age, testtable$Ethnicity)
#testtable <- testtable[testtable$Ethnicity!="None",]

#popn$cases <- 0
#cases <- matrix(0, length(popn$merge),1)
#rownames(cases) <- popn$merge
#cases[testtable$merge,] <- testtable$Cases
#popn$cases <- cases

## end JM working code

tp$merge <- paste(tp$NZDep, tp$Age, tp$Ethnicity)
testtable$merge <- paste(testtable$NZDep, testtable$Age, testtable$Ethnicity)
testtable <- testtable[testtable$Ethnicity!="None",]

tp$cases <- 0
cases <- matrix(0, length(tp$merge),1)
rownames(cases) <- tp$merge
cases[testtable$merge,] <- testtable$Cases
tp$cases <- cases

#
#model<-glm(cases~Age*Ethnicity*NZDep+offset(log(Popn)),data=popn,family=poisson)
#summary(model)
#
##
#
#modelz<-zeroinfl(cases~Age+Ethnicity+as.factor(NZDep)+offset(log(Popn))|1,data=popn)
#summary(modelz)
#
#modelz<-zeroinfl(cases~Age+Ethnicity+as.factor(NZDep)+offset(log(Popn))|Ethnicity+as.factor(NZDep)+offset(log(Popn)),data=popn)
#
#res<-predict(modelz)
#plot(res,popn$cases)
#cor(res,popn$cases)
#cor.test(res,popn$cases)
#
## reduce NZDep #s
## ZERO INFLATION
hist(tp$cases,xlab="Cases",main='Histogram of cases per category',col='grey')

modelz<-zeroinfl(cases~Age+Ethnicity+NZDep+offset(log(Popn))|1,data=tp)
summary(modelz)

modelz<-zeroinfl(cases~Age+Ethnicity+NZDep+offset(log(Popn))|Ethnicity+NZDep+offset(log(Popn)),data=tp)
summary(modelz)

res<-predict(modelz)
plot(res,tp$cases)
cor(res,tp$cases)
cor.test(res,tp$cases)

modelz<-zeroinfl(cases~Age+Ethnicity+NZDep+offset(log(Popn))|Ethnicity+NZDep+offset(log(Popn)),data=tp,dist="negbin")
summary(modelz)

res<-predict(modelz)
plot(res,tp$cases)
cor(res,tp$cases)
cor.test(res,tp$cases)

str(tp)
head(tp)
pairs(tp[,-c(5)],panel=panel.smooth)

## try dropping the old people

tpminus<-tp[!(tp$Age=="65+"),]
hist(tpminus$cases,xlab="Cases",main='Histogram of cases per category',col='grey')

modelzminus<-zeroinfl(cases~Age*Ethnicity*NZDep+offset(log(Popn))|1+offset(log(Popn)),data=tpminus)
summary(modelzminus)

# perfect fit ;-)

par(mfrow=c(2,2))
hist(tpminus$cases,xlab="Cases",main='Histogram of cases per category',col='grey',breaks=20)
plot(tpminus$cases,main="Cases per category",ylab="Count",pch=16,col="darkgrey")
res<-predict(modelzminus)
plot(res,tpminus$cases,xlab="results",ylab='predictions',main="Fit",pch=16,col="darkgrey")
cor(res,tpminus$cases)
cor.test(res,tpminus$cases)

hist(modelzminus$residuals,main="Histogram of residuals",xlab="residuals",col="grey")

## 
modzm<-zeroinfl(cases~Age*Ethnicity+Age*NZDep+Ethnicity*NZDep+offset(log(Popn))|1+offset(log(Popn)),data=tpminus)
summary(modzm)
res<-predict(modzm)
plot(res,tpminus$cases)
cor(res,tpminus$cases)
cor.test(res,tpminus$cases)

AIC(modzm,modelzminus)

## 
modzm2<-zeroinfl(cases~Age+Ethnicity+NZDep+offset(log(Popn))|1+offset(log(Popn)),data=tpminus)
summary(modzm2)
res<-predict(modzm2)
plot(res,tpminus$cases)
cor(res,tpminus$cases)
cor.test(res,tpminus$cases)

AIC(modzm2,modelzminus)

## to drop MLA due to small #s, but then no zeros!!!!

tpsub<-tpminus[!(tpminus$Ethnicity=="MLA"),]
hist(tpsub$cases,xlab="Cases",main='Histogram of cases per category',col='grey')
plot(tpsub$cases)
modelzsub<-zeroinfl(cases~Age*Ethnicity*NZDep+offset(log(Popn))|1+offset(log(Popn)),data=tpsub)
summary(modelzsub)

res<-predict(modelzsub)
plot(res,tpsub$cases)
cor(res,tpsub$cases)
cor.test(res,tpsub$cases)
