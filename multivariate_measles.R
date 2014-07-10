rm(list=ls())
## regression analyses - measles
## get data
# set wd
# setwd("~/Massey 2014/DHayman_20140627")
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
head(testtable)

tnzd<-revalue(testtable$NZDep, c("1"="1-2","2"="1-2","3"="3-4","4"="3-4",
                                 "5"="5-6","6"="5-6","7"="7-8","8"="7-8",
                                 "9"="9-10","10"="9-10"));

testtable$NZDep<-tnzd
head(testtable)
tt<-aggregate( cbind(Cases) ~ NZDep+Age+Ethnicity + Year, 
                 data = testtable , FUN=sum)

# need to reshape "testtable"
# mdata<-melt(testtable,id=c("NZDep",'Age','Ethnicity','Year','Cases'))
# numerator<-cast(mdata,Age+Ethnicity+Year~NZDep)
#
#numerator[is.na(numerator)] <- 0
#head(numerator)
#colnames(numerator)<-c('Age','Ethnicity','Year',"Dep0","Dep1","Dep2","Dep3","Dep4",'Dep5','Dep6','Dep7','Dep8','Dep9',"Dep10")
#
#
#
#model3<-aov(Cases~NZDep*Age*Ethnicity,data=testtable)
#summary(model3)
#summary.aov(model3)
# looks like no need to keep interaction terms

## account for random year effect (proxy for outbreak)
#model4<-aov(Cases~NZDep*Age*Ethnicity+Error(Year),data=testtable)
#summary(model4)
# still looks like no need for interaction terms

#model5<-aov(Cases~NZDep+Age+Ethnicity+Error(Year),data=testtable)
#summary(model5)
#
#model6<-aov(Cases~NZDep+Age+Ethnicity,data=testtable)
#summary(model6)
#summary.aov(model6)

## results - though many age categries, richer more likely, 1-3 & 9-13 Yrs, Europeans 
## caveats - temporal and spatial autocorrelation, though year random effect
## caveats - no "per capita" analyses - just raw data

## glm
#hist(testtable$Cases,breaks=seq(0,max(testtable$Cases),by=5))
## doesn't appear zero inflated, poisson should be okay - can check

## glm with Poisson errors
# start with full model
#model7<-glm(Cases~NZDep*Age*Ethnicity,data=testtable,family=poisson)
#
#summary(model7)
#anova(model7,test="Chi")
# why difference?

### denominator data
# setwd("~/Massey_2014/measles/data")
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

#head(numerator)
#numerator<-numerator[numerator$Ethnicity!="None",]
#num2008<-numerator[numerator$Year=="2008",-c(3:4)]
#num2009<-numerator[numerator$Year=="2009",-c(3:4)]
#num2010<-numerator[numerator$Year=="2010",-c(3:4)]
#num2011<-numerator[numerator$Year=="2011",-c(3:4)]
#num2012<-numerator[numerator$Year=="2012",-c(3:4)]
#num2013<-numerator[numerator$Year=="2013",-c(3:4)]
#num2014<-numerator[numerator$Year=="2014",-c(3:4)]

## but fill gaps
#nm<-dm[,1:2]
#nm
#
#all2008=merge(nm,num2008, all=T)
#all2008[is.na(all2008)]<-0
#
#all2009=merge(nm,num2009, all=T)
#all2009[is.na(all2009)]<-0
#
#all2010=merge(nm,num2010, all=T)
#all2010[is.na(all2010)]<-0
#
#all2011=merge(nm,num2011, all=T)
#all2011[is.na(all2011)]<-0
#
#all2012=merge(nm,num2012, all=T)
#all2012[is.na(all2012)]<-0
#
#all2013=merge(nm,num2013, all=T)
#all2013[is.na(all2013)]<-0
#
#all2014=merge(nm,num2014, all=T)
#all2014[is.na(all2014)]<-0
#
##
#risk2014<-all2014[,3:12]/dm[,3:12]
#risk2014<-cbind(all2014[,1:2],risk2014)
#
#risktest14<-melt(risk2014,id.vars=c("Age","Ethnicity"),measure.vars=c("Dep1","Dep2","Dep3","Dep4",'Dep5','Dep6','Dep7','Dep8','Dep9',"Dep10"))
#colnames(risktest14)<-c("Age","Ethnicity","NZDep","Cases")
#
#hist(risktest14$Cases)
#
#counts14<-melt(all2014,id.vars=c("Age","Ethnicity"),measure.vars=c("Dep1","Dep2","Dep3","Dep4",'Dep5','Dep6','Dep7','Dep8','Dep9',"Dep10"))
#colnames(counts14)<-c("Age","Ethnicity","NZDep","Cases")
#counts14
#hist(counts14$Cases)

popn<-melt(dm,id.vars=c("Age","Ethnicity"),measure.vars=c("Dep1","Dep2","Dep3","Dep4",'Dep5','Dep6','Dep7','Dep8','Dep9',"Dep10"))
colnames(popn)<-c("Age","Ethnicity","NZDep","Popn")
popn
pnzd<-revalue(popn$NZDep, c("Dep1"="1-2","Dep2"="1-2","Dep3"="3-4","Dep4"="3-4",
                                 "Dep5"="5-6","Dep6"="5-6","Dep7"="7-8","Dep8"="7-8",
                                 "Dep9"="9-10","Dep10"="9-10"));

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

modelz<-zeroinfl(cases~Age+Ethnicity+as.factor(NZDep)+offset(log(Popn))|1,data=popn)
summary(modelz)

modelz<-zeroinfl(cases~Age+Ethnicity+as.factor(NZDep)+offset(log(Popn))|Ethnicity+as.factor(NZDep)+offset(log(Popn)),data=popn)

res<-predict(modelz)
plot(res,popn$cases)
cor(res,popn$cases)
cor.test(res,popn$cases)

## reduce NZDep #s

modelz<-zeroinfl(cases~Age+Ethnicity+NZDep+offset(log(Popn))|1,data=tp)
summary(modelz)

modelz<-zeroinfl(cases~Age+Ethnicity+NZDep+offset(log(Popn))|Ethnicity+as.factor(NZDep)+offset(log(Popn)),data=tp)

res<-predict(modelz)
plot(res,tp$cases)
cor(res,tp$cases)
cor.test(res,tp$cases)

