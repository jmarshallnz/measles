rm(list=ls())

## get data
# set wd
# setwd("~/Massey 2014/DHayman_20140627")
# read data
data<-read.csv("DHayman_20140627.csv",header=T)
vac<-read.csv("DHayman_20140715_Vacc.csv",header=T)
names(data)
data$RptYear<-as.factor(data$RptYear)
data$SurvWeek<-as.factor(data$SurvWeek)
data$NZDep01<-as.factor(data$NZDep01)
data$NZDep06<-as.factor(data$NZDep06)
data$NZDep13<-as.factor(data$NZDep13)
#data$AgeInYears<-as.factor(data$AgeInYears)
data$EthnicityPrioritised<-as.factor(data$EthnicityPrioritised)


time<-aggregate( cbind( DiseaseName ) ~ NZDep01 +NZDep06+NZDep13+AgeInYears+EthnicityPrioritised + SurvWeek + RptYear,# + Dose1Mths + Dose2Mths, 
data = data , FUN=sum)
library(reshape)
library(reshape2)
library(plyr)
library(pscl)
test<-subset(time, (RptYear %in% c("2007","2008","2009","2010","2011","2012","2013","2014")))
head(test)
dim(test)


par(mfrow=c(1,1))
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=2)
hist(time$AgeInYears,col="grey",xlab="Age in years",main="",breaks=90,include.lowest=TRUE,right=F,ylab="Frequency",cex.lab=2)
hist(test$AgeInYears,col="black",breaks=90,include.lowest=TRUE,right=F,add=T)
legend("topright",c("1997-2014","2007-2014"),col=c("grey","black"),pch=15,bty="n",cex=2)

## plot cases / age class
caseyr<-aggregate( DiseaseName ~ AgeInYears, 
data = test , FUN=sum)

require(reshape2)  # this is the library that lets one flatten out data
require(ggplot2)   # plotting library
popimmune<-read.csv("PopnImmunityAll.csv",header=T)
popimmune$Age = factor(popimmune$Age,levels(popimmune$Age)[c(2,3,6,8,10:12,4,5,7,9,1)])
pop<-read.csv("popnsize.csv",header=T)
colnames(pop)<-0:100
pop<-t(pop)
impop<-c(popimmune$Immunity[1:6],rep(popimmune$Immunity[7],8),rep(popimmune$Immunity[8],5),rep(popimmune$Immunity[9],5),
rep(popimmune$Immunity[10],9),rep(popimmune$Immunity[11],20),rep(popimmune$Immunity[12],48))
length(pop)
naive<-round(pop-(pop*impop))

## match cases per age
AgeInYears<-0:100
naive<-cbind(AgeInYears,naive)
colnames(naive)<-c("AgeInYears","Naive")
naive<-merge(naive,caseyr,by="AgeInYears",all=T)
colnames(naive)<-c("AgeInYears","Naive","Cases")

dose1=as.factor(round(vac$Dose1Mths/12,0));
dose1<-summary(dose1)
dose1<-as.data.frame(dose1)
dose1$AgeInYears<-rownames(dose1)
dose2=as.factor(round(na.omit(vac$Dose2Mths/12,0)));
dose2<-summary(dose2)
dose2<-as.data.frame(dose2)
dose2$AgeInYears<-rownames(dose2)

## need to merge vaccination data with data
## to get years the cases were from
datav<-merge(vac,data,by="CaseCode",all=T)
##
datav$Dose1Mths[which(is.na(datav$Dose1Mths))] <- 9999
datav$Dose2Mths[which(is.na(datav$Dose2Mths))] <- 9999
#########################
datav$RptYear<-as.factor(datav$RptYear)
datav$SurvWeek<-as.factor(datav$SurvWeek)
datav$NZDep01<-as.factor(datav$NZDep01)
datav$NZDep06<-as.factor(datav$NZDep06)
datav$NZDep13<-as.factor(datav$NZDep13)
datav$Dose1Mths<-as.factor(datav$Dose1Mths)
datav$Dose2Mths<-as.factor(datav$Dose2Mths)

#data$AgeInYears<-as.factor(data$AgeInYears)
datav$EthnicityPrioritised<-as.factor(datav$EthnicityPrioritised)

timev<-aggregate( cbind( DiseaseName) ~ NZDep01 +NZDep06+NZDep13+AgeInYears+EthnicityPrioritised + SurvWeek + RptYear+ Dose1Mths +Dose2Mths, 
data = datav , FUN=sum,na.rm=F,na.action=na.pass)

testv<-subset(timev, (RptYear %in% c("2007","2008","2009","2010","2011","2012","2013","2014")))
library(plyr)
testv$Dose1Mths<-revalue(testv$Dose1Mths, c("9999"=NA));
testv$Dose2Mths<-revalue(testv$Dose2Mths, c("9999"=NA));
testv$Dose1Mths<-as.numeric(testv$Dose1Mths)
testv$Dose2Mths<-as.numeric(testv$Dose2Mths)
testv$D2vac<-ifelse(testv$Dose1Mths > -1 & testv$Dose2Mths >= testv$Dose1Mths,testv$Dose2Mths,NA)
testv$D1vac<-ifelse(testv$Dose1Mths > -1 & is.na(testv$D2vac) == T,testv$Dose1Mths,NA)
testv$Unvac<-ifelse(is.na(testv$Dose1Mths) == T & is.na(testv$Dose2Mths) == T,testv$AgeInYears,NA)

testv$VC<-ifelse(is.na(testv$Dose1Mths) == T & is.na(testv$Dose2Mths) == T,0,
ifelse(testv$Dose1Mths >= 0 & is.na(testv$Dose2Mths) == T,1,2))
AgeVac<-table(testv$VC,testv$AgeInYears)

row.names(AgeVac)<-c("Unvaccinated","Dose1","Dose2")
AgeVac<-t(AgeVac)
AgeInYears<-(as.numeric(rownames(AgeVac)))
AgeVac<-cbind(AgeVac,AgeInYears)       
AgeInYears<-(as.numeric(rownames(pop)))
pop<-cbind(pop,AgeInYears)
colnames(pop)<-c("Population","AgeInYears")
AgeV<-merge(pop,AgeVac,by="AgeInYears",all=T)

## regression analyses - measles
# setwd("~/Massey 2014/DHayman_20140627")
# read data
data<-read.csv("DHayman_20140627.csv",header=T)
vac<-read.csv("DHayman_20140715_Vacc.csv",header=T)
names(data)
data$RptYear<-as.factor(data$RptYear)
data$SurvWeek<-as.factor(data$SurvWeek)
data$NZDep01<-as.factor(data$NZDep01)
data$NZDep06<-as.factor(data$NZDep06)
data$NZDep13<-as.factor(data$NZDep13)
#data$AgeInYears<-as.factor(data$AgeInYears)
data$EthnicityPrioritised<-as.factor(data$EthnicityPrioritised)

time<-aggregate( cbind( DiseaseName ) ~ NZDep01 +NZDep06+NZDep13+AgeInYears+EthnicityPrioritised + SurvWeek + RptYear,# + Dose1Mths + Dose2Mths, 
data = data , FUN=sum)
library(reshape)
library(reshape2)
library(plyr)
library(pscl)

test<-subset(time, (RptYear %in% c("2007","2008","2009","2010","2011","2012","2013","2014")))
head(test)
dim(test)
caseyr<-aggregate( DiseaseName ~ AgeInYears, 
data = test , FUN=sum)
caseyr
test$AgeInYears<-findInterval(test$AgeInYears,c(3,6,18,25))
test$AgeInYears<-as.factor(test$AgeInYears)
tage<-revalue(test$AgeInYears, c("0"="0-2", "1"="3-5","2"="6-17","3"="18-24","4"="25+"));
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

testtable<-aggregate( cbind( DiseaseName) #+ as.numeric(SurvWeek))
~ NZDep+AgeInYears+EthnicityPrioritised + RptYear, 
data = testt , FUN=sum)
dim(testtable)
colnames(testtable)<-c("NZDep","Age","Ethnicity","Year","Cases")
head(testtable)

#tnzd<-revalue(testtable$NZDep, c("1"="1-5","2"="1-5","3"="1-5","4"="1-5",
#"5"="1-5","6"="6-10","7"="6-10","8"="6-10",
#"9"="6-10","10"="6-10"));

tnzd<-revalue(testtable$NZDep, c("1"="1-3","2"="1-3","3"="1-3","4"="4-7",
                                 "5"="4-7","6"="4-7","7"="4-7","8"="8-10",
                                 "9"="8-10","10"="8-10"));


testtable$NZDep<-tnzd
# head(testtable)
tt<-aggregate( cbind(Cases) ~ NZDep+Age+Ethnicity + Year, 
data = testtable , FUN=sum)

ttyr<-aggregate( cbind(Cases) ~ NZDep+Age+Ethnicity, 
data = testtable , FUN=sum)
ttyr <- ttyr[ttyr$Ethnicity=="None",]

# This chunk uses "results=tex"
library(Hmisc)
latex(ttyr, file="",table.env=FALSE,rowname=NULL)

### denominator data
# setwd("~/Massey_2014/measles/data")
data<-read.csv("DHayman_20140627.csv",header=T)
vac<-read.csv("DHayman_20140715_Vacc.csv",header=T)
names(data)
data$RptYear<-as.factor(data$RptYear)
data$SurvWeek<-as.factor(data$SurvWeek)
data$NZDep01<-as.factor(data$NZDep01)
data$NZDep06<-as.factor(data$NZDep06)
data$NZDep13<-as.factor(data$NZDep13)
#data$AgeInYears<-as.factor(data$AgeInYears)
data$EthnicityPrioritised<-as.factor(data$EthnicityPrioritised)

time<-aggregate( cbind( DiseaseName ) ~ NZDep01 +NZDep06+NZDep13+AgeInYears+EthnicityPrioritised + SurvWeek + RptYear,# + Dose1Mths + Dose2Mths, 
data = data , FUN=sum)
library(reshape)
library(reshape2)
library(plyr)
library(pscl)

test<-subset(time, (RptYear %in% c("2007","2008","2009","2010","2011","2012","2013","2014")))
head(test)
dim(test)
caseyr<-aggregate( DiseaseName ~ AgeInYears, 
data = test , FUN=sum)
caseyr
test$AgeInYears<-findInterval(test$AgeInYears,c(3,6,18,25))
test$AgeInYears<-as.factor(test$AgeInYears)
tage<-revalue(test$AgeInYears, c("0"="0-2", "1"="3-5","2"="6-17","3"="18-24","4"="25+"));
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

testtable<-aggregate( cbind( DiseaseName) #+ as.numeric(SurvWeek))
~ NZDep+AgeInYears+EthnicityPrioritised + RptYear, 
data = testt , FUN=sum)
dim(testtable)
colnames(testtable)<-c("NZDep","Age","Ethnicity","Year","Cases")
head(testtable)

#tnzd<-revalue(testtable$NZDep, c("1"="1-5","2"="1-5","3"="1-5","4"="1-5",
#"5"="1-5","6"="6-10","7"="6-10","8"="6-10",
#"9"="6-10","10"="6-10"));

tnzd<-revalue(testtable$NZDep, c("1"="1-3","2"="1-3","3"="1-3","4"="4-7",
                                 "5"="4-7","6"="4-7","7"="4-7","8"="8-10",
                                 "9"="8-10","10"="8-10"));

testtable$NZDep<-tnzd
# head(testtable)
tt<-aggregate( cbind(Cases) ~ NZDep+Age+Ethnicity + Year, 
data = testtable , FUN=sum)

ttyr<-aggregate( cbind(Cases) ~ NZDep+Age+Ethnicity, 
data = testtable , FUN=sum)
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
dage<-revalue(dm$Age_Label, c("A: 0- 5 yrs"="<6", "A: 6-17 yrs"="6-17","A:18-24 yrs"="18-24","A:25-64 yrs"="25+","A:65+ yrs"="25+"));
dm$Age_Label<-dage
colnames(dm)<-c('Age','Ethnicity',"Dep1","Dep2","Dep3","Dep4",'Dep5','Dep6','Dep7','Dep8','Dep9',"Dep10")
head(dm)
datadm<-dm[dm$Age=="<6",]
dataA<-(rbind(datadm,datadm)/2)
dataA<-round(dataA[,3:12])
dataA<-cbind(c(rep("0-2",5),rep("3-5",5)),rep(datadm$Ethnicity[1:5],2),dataA)
colnames(dataA)<-c('Age','Ethnicity',"Dep1","Dep2","Dep3","Dep4",'Dep5','Dep6','Dep7','Dep8','Dep9',"Dep10")
ddm<-dm[!dm$Age=="<6",]
dm<-rbind(dataA,ddm)
dm$Ethnicity <- factor(dm$Ethnicity)
deth<-revalue(dm$Ethnicity, c("Asian (Prioritised)"="Asian","European (NZ European and Other European)"="European",
"Maori (Prioritised)"="Maori","MELAA"="MLA","Pacific People (Prioritised)"="Pacific"));
dm$Ethnicity<-deth
head(dm)

popn<-melt(dm,id.vars=c("Age","Ethnicity"),measure.vars=c("Dep1","Dep2","Dep3","Dep4",'Dep5','Dep6','Dep7','Dep8','Dep9',"Dep10"))
colnames(popn)<-c("Age","Ethnicity","NZDep","Popn")
popn
#pnzd<-revalue(popn$NZDep, c("Dep1"="1-5","Dep2"="1-5","Dep3"="1-5","Dep4"="1-5",
#"Dep5"="1-5","Dep6"="6-10","Dep7"="6-10","Dep8"="6-10",
#"Dep9"="6-10","Dep10"="6-10"));

pnzd<-revalue(popn$NZDep, c("Dep1"="1-3","Dep2"="1-3","Dep3"="1-3","Dep4"="4-7",
                            "Dep5"="4-7","Dep6"="4-7","Dep7"="4-7","Dep8"="8-10",
                            "Dep9"="8-10","Dep10"="8-10"));


popn$NZDep<-pnzd
head(popn)
tp<-aggregate( cbind(Popn) ~ NZDep+Age+Ethnicity, 
data = popn , FUN=sum)

tp$merge <- paste(tp$NZDep, tp$Age, tp$Ethnicity)
ttyr$merge <- paste(ttyr$NZDep, ttyr$Age, ttyr$Ethnicity)
ttyr <- ttyr[ttyr$Ethnicity!="None",]

tp$cases <- 0
cases <- matrix(0, length(tp$merge),1)
rownames(cases) <- tp$merge
cases[ttyr$merge,] <- ttyr$Cases
tp$cases <- cases
PerCapita<-(tp$cases/tp$Popn)
PerCap<-cbind(tp[,c(1:4,6)],round(PerCapita*10000,4))
colnames(PerCap)<-c("NZDep","Age","Ethnicity","Population","Cases","Per capita")

par(mar=c(12,6,4,2)+0.1)
par(cex.axis=2)
plot(tp$cases/tp$Popn*10000,main="",
        ylab="Per Capita Rate Per 10,000",
        pch=16,cex=2,col=c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(6,10)),
xlab="",cex.lab=2,xaxt="n")
#axis(side = 1, at = 1:75, labels=rep(c("0-2yr, 1-3","0-2yr, 6-10","3-5yr, 1-5","3-5yr, 6-10","6-17yr, 1-5","6-17yr, 6-10","18-24yr, 1-5",'18-24yr, 1-5',"25+yr, 1-5","25+yr, 6-10"),5),
#     las=2,cex.axis=1.5)
mtext(side=1,line=10,text="Age and NZDep",cex=2)
legend("topleft",c("Asian","European","Maori","MLA","Pacific"),
col=c(1:4,6),pch=16,bty="n",cex=1.8)
par(mar = c(5,4,4,2) + 0.1)

tp$perCap<-with(tp,tp$cases/tp$Popn*10000)

ggplot(tp, aes(NZDep, cases, fill=Age)) + 
  geom_bar(stat="identity", position="dodge")+
  ylab("cases")

ggplot(tp, aes(NZDep, cases, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
  ylab("cases")

ggplot(tp, aes(Age, cases, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
  ylab("cases")

ggplot(tp, aes(NZDep, perCap, fill=Age)) + 
  geom_bar(stat="identity", position="dodge")+
  ylab("Per capita per 10000")

ggplot(tp, aes(NZDep, perCap, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
  ylab("Per capita per 10000")

ggplot(tp, aes(Age, perCap, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
  ylab("Per capita per 10000")

tpsub<-tp[!(tp$Ethnicity=="MLA"),]
ggplot(tpsub, aes(NZDep, perCap, fill=Age)) + 
  geom_bar(stat="identity", position="dodge")+
  ylab("Per capita per 10000")

ggplot(tpsub, aes(NZDep, perCap, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
  ylab("Per capita per 10000")

ggplot(tpsub, aes(Age, perCap, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
  ylab("Per capita per 10000")

# This chunk uses "results=tex"
library(Hmisc)
latex(PerCap, file="", table.env=FALSE,rowname=NULL)

## plot this figure?
#hist(tp$cases,xlab="Cases",main='Histogram of cases per category',col='grey',breaks=20)
tp$Ethnicity<- relevel(tp$Ethnicity, "European")
model<-glm(cases~Age*Ethnicity*NZDep+offset(log(Popn)),data=tp,family="quasipoisson")
summary(model)

model1<-update(model,~.-Age:Ethnicity:NZDep)
summary(model1)
anova(model1,test="F") # F test, not Chisq, because dispersion estimated by moments

model2<-update(model1,~.-Age:NZDep)
summary(model2)
anova(model2,test="F")

## use tpsub
#hist(tpsub$cases,xlab="Cases",main='Histogram of cases per category',col="darkgrey",breaks=20)

model<-glm(cases~Age*Ethnicity*NZDep+offset(log(Popn)),data=tpsub,family="quasipoisson")
summary(model)

model1<-update(model,~.-Age:Ethnicity:NZDep)
summary(model1)
anova(model1,test="F")

#model2<-update(model1,~.-Ethnicity:NZDep)
#summary(model2)
#anova(model2,test="F")
model2<-update(model1,~.-Age:NZDep)
summary(model2)
anova(model2,test="F")

#model3<-update(model2,~.-Age:NZDep)
#summary(model3)
#anova(model3,test="F")


par(cex.axis=2)
par(mar=c(5,6,4,2)+0.1)
hist(tpsub$cases,xlab="Number of cases",main='',col='grey',breaks=20,ylab="Number of categories",cex.lab=2)
#res<-predict(model3)
res<-predict(model2)

par(mar=c(5,6,4,2)+0.1)
par(cex.axis=2)
plot(exp(res),tpsub$cases,xlab="cases",ylab='predictions',main="",pch=16,col="darkgrey",cex.lab=2,cex=2)
cor(exp(res),tpsub$cases)
cor.test(exp(res),tpsub$cases)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=2)
#hist(model3$residuals,main="",xlab="residuals",col="grey",cex.lab=2,ylab="")
hist(model2$residuals,main="",xlab="residuals",col="grey",cex.lab=2,ylab="")

library(xtable)
#resan<-xtable(anova(model3,test="F"))
resan<-xtable(anova(model2,test="F"))
print(resan,floating = FALSE)

library(xtable)
xtb<-xtable(summary(model3))
xtb<-xtable(summary(model2))
print(xtb,floating = FALSE)

AgeVac<-table(testv$VC,testv$AgeInYears)
row.names(AgeVac)<-c("Unvaccinated","Dose1","Dose2")
AgeVac<-t(AgeVac)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=2)
barplot(t(AgeVac),xlab="Age",col=c("darkgrey","red","orange"),main="",ylab="Frequency",cex.lab=2)
legend("topright",c("Unvaccinated","Dose 1","Dose 2"),fill=c("darkgrey","red","orange"),cex=1.8,bty="n")
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=2)
hist(vac$Dose1Mths,breaks=100,col=rgb(0,0,1,1/4),xlab="Age in months",main="",cex.lab=2,ylab="Frequency")
abline(v=12,col="blue")
hist(vac$Dose2Mths,breaks=50,add=T,col=rgb(1,0,0,1/4))
abline(v=60,col="red")
legend("topright",c("Dose 1","Dose 2"),col=c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)),pch=15,bty="n",cex=1.8)
legend("top",c("12 months","60 months"),col=c("blue","red"),lty=1,bty="n",cex=1.8)

require(reshape2)  # this is the library that lets one flatten out data
require(ggplot2)   # plotting library
popimmune<-read.csv("PopnImmunityAll.csv",header=T)
popimmune$Age = factor(popimmune$Age,levels(popimmune$Age)[c(2,3,6,8,10:12,4,5,7,9,1)])
pop<-read.csv("popnsize.csv",header=T)
colnames(pop)<-0:100
pop<-t(pop)
impop<-c(popimmune$Immunity[1:6],rep(popimmune$Immunity[7],8),rep(popimmune$Immunity[8],5),rep(popimmune$Immunity[9],5),
rep(popimmune$Immunity[10],9),rep(popimmune$Immunity[11],20),rep(popimmune$Immunity[12],48))
length(pop)
naive<-round(pop-(pop*impop))
par(cex.axis=2)
par(mar=c(5,6,4,2)+0.1)
plot(pop,xlab="Age",ylab="Population",cex.lab=2)
points(naive,pch=16)
legend("topright",c("Population","Naive"),pch=c(1,16),bty="n",cex=1.8)

library(xtable)
totdpdf<-read.csv("mapdata_tot.csv",header=T)
# output top countries
colnames(totdpdf)[1]<-c("country")
toptot <- totdpdf[order(-totdpdf$immigration),] 
immigration<-toptot[1:30,c(1,4)]
topincidence <- totdpdf[order(-totdpdf$incidence),] 
incidence<-topincidence[1:30,c(1,2)]
incidence[,2]<-round(incidence[,2],0)
topvaccine <- totdpdf[order(totdpdf$cover),] 
vaccinecover<-topvaccine[1:30,c(1,3)]
toprisk <- totdpdf[order(-totdpdf$risk),] 
risk<-(toprisk[1:30,c(1,5)])
risk[,2]<-round(risk[,2],0)
toprisk2<-(toprisk[1:10,c(1,2,3,4)])
toprisk2[,2]<-round(toprisk2[,2],0)

library(xtable)
nzdpdf<-read.csv("mapdata_nzers.csv",header=T)
# output top countries
colnames(nzdpdf)[1]<-c("country")
topnz <- nzdpdf[order(-nzdpdf$immigration),] 
nz<-topnz[1:30,c(1,4)]
topnzrisk <- nzdpdf[order(-nzdpdf$risk),] 
nzrisk<-(topnzrisk[1:30,c(1,5)])
nzrisk[,2]<-round(nzrisk[,2],0)

library(xtable)
imdpdf<-read.csv("mapdata_immigration.csv",header=T)
# output top countries
colnames(imdpdf)[1]<-c("country")
topim <- imdpdf[order(-imdpdf$immigration),] 
im<-topim[1:30,c(1,4)]
topimrisk <- imdpdf[order(-imdpdf$risk),] 
imrisk<-(topimrisk[1:30,c(1,5)])
imrisk[,2]<-round(imrisk[,2],0)
# This chunk uses "results=tex"
library(Hmisc)
latex(incidence, file="", table.env=FALSE,rowname=NULL)

library(Hmisc)
latex(vaccinecover, file="", table.env=FALSE,rowname=NULL)


# This chunk uses "results=tex"
library(Hmisc)
latex(im, file="", table.env=FALSE,rowname=NULL)

# This chunk uses "results=tex"
library(Hmisc)
latex(nz, file="", table.env=FALSE,rowname=NULL)

# This chunk uses "results=tex"
library(Hmisc)
latex(immigration, file="", table.env=FALSE,rowname=NULL)

# This chunk uses "results=tex"
library(Hmisc)
latex(risk, file="", table.env=FALSE,rowname=NULL)

# This chunk uses "results=tex"
library(Hmisc)
latex(nzrisk, file="", table.env=FALSE,rowname=NULL)
# This chunk uses "results=tex"
library(Hmisc)
latex(imrisk, file="", table.env=FALSE,rowname=NULL)

# This chunk uses "results=tex"
library(Hmisc)
latex(toprisk2, file="", table.env=FALSE,rowname=NULL)

# haven't had time to sort out which packages I need
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
#gpclibPermit()
library(mapproj)
library(plyr)
library(grid)
library(gridExtra)
#gpclibPermit()
library(utils)
library(rgdal)
#getwd()
#setwd("~/data")
#unzip("TM_WORLD_BORDERS_SIMPL-0.3.zip")
load("world.ggmap.Rda")
n <- length(unique(world.ggmap$id))
id = unique(world.ggmap$id)
id<-as.data.frame(id)
dpdf<-read.csv("mapdata_tot.csv",header=T)
# output top countries

np1<-ggplot(dpdf, aes(map_id = id))  +
geom_map(aes(fill = immigration), map =world.ggmap) +
expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
xlab("longitude") +
ylab("latitude") +
theme(text = element_text(size=30))+
scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

np2<-ggplot(dpdf, aes(map_id = id))  +
geom_map(aes(fill = incidence), map =world.ggmap) +
expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
xlab("longitude") +
ylab("latitude") +
theme(text = element_text(size=30))+
scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

np3<-ggplot(dpdf, aes(map_id = id))  +
geom_map(aes(fill = cover), map =world.ggmap) +
expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
xlab("longitude") +
ylab("latitude") +
theme(text = element_text(size=30))+
scale_fill_gradient("",low = "red", high = "blue", guide = "colorbar")

#grid.arrange(np1, np2, np3, ncol=3, main="")

np4<-ggplot(dpdf, aes(map_id = id)) +
geom_map(aes(fill = risk), map =world.ggmap) +
expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
xlab("longitude") +
ylab("latitude") +
theme(text = element_text(size=30))+
scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")
#np4
# output top countries
dpdf<-read.csv("mapdata_immigration.csv",header=T)
imnp1<-ggplot(dpdf, aes(map_id = id))  +
geom_map(aes(fill = immigration), map =world.ggmap) +
expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
xlab("longitude") +
ylab("latitude") +
theme(text = element_text(size=30))+
scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

imnp4<-ggplot(dpdf, aes(map_id = id)) +
geom_map(aes(fill = risk), map =world.ggmap) +
expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
xlab("longitude") +
ylab("latitude") +
theme(text = element_text(size=30))+
scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")
#np4
dpdf<-read.csv("mapdata_nzers.csv",header=T)
nznp1<-ggplot(dpdf, aes(map_id = id))  +
geom_map(aes(fill = immigration), map =world.ggmap) +
expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
xlab("longitude") +
ylab("latitude") +
theme(text = element_text(size=30))+
scale_fill_gradient("",low = "blue", high = "red", guide = "colorbar")

nznp4<-ggplot(dpdf, aes(map_id = id)) +
geom_map(aes(fill = risk), map =world.ggmap) +
expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
xlab("longitude") +
ylab("latitude") +
theme(text = element_text(size=30))+
scale_fill_gradient("",low = "yellow", high = "red", guide = "colorbar")

print(np2)
print(np3)
print(imnp1)
print(nznp1)
print(np1)
print(np4)
print(nznp4)
print(imnp4)

change<-read.csv("change.csv",header=T)
plot(change[18:25,1],change[18:25,4],type="l",xlab="Year",ylab="",ylim=c(0,max(change[,4])),
bty="n")
ylim <- range(c(0,max(change[,4])))
xlim <- range(change[,1])
par(fig = c(.6, 1, 0.6, 1), mar=c(0,0,0,0), new=TRUE)
plot(change[,1],change[,4],type="l",xlab="Year",ylab="",
ylim=c(0,max(change[,4])),bg="grey",add=T,bty="n")

#setwd("C:\Users\dtshayma\Documents\GitHub\measles")
costtable1<-read.csv("costtable1.csv",header=T)
# This chunk uses "results=tex"
library(Hmisc)
latex(costtable1, file="", table.env=FALSE,rowname=NULL)
costtable2<-read.csv("costtable2v2.csv",header=T)
# This chunk uses "results=tex"
library(Hmisc)
colnames(costtable2) <- c("Year", "Cases", "Days", "Cost","Cost per case","Cost per day")
latex(costtable2, file="", table.env=FALSE,rowname=NULL)
costtable3<-read.csv("costtable3.csv",header=T)
library(Hmisc)
latex(costtable3, file="", table.env=FALSE,rowname=NULL)
hosp<-read.csv("hospital.csv",header=T)
library(ggplot2)
p<-ggplot(data=hosp, aes(x=Days, y=Number)) + geom_bar(stat="identity")+theme(axis.title.x = element_text(size=25),axis.title.y = element_text(size=25),axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16))
print(p)
costtable4<-read.csv("costtable4v2.csv",header=T)
# This chunk uses "results=tex"
library(Hmisc)
colnames(costtable4) <- c("Year", "Gender", "Cost","Cases","Length of stay","Cost per case")
latex(costtable4, file="", table.env=FALSE,rowname=NULL)
costtable5<-read.csv("costtable5v2.csv",header=T)
# This chunk uses "results=tex"
library(Hmisc)
latex(costtable5, file="", table.env=FALSE,rowname=NULL)
vacp<-read.csv("vacc_predictions.csv",header=T)
# This chunk uses "results=tex"
library(Hmisc)
latex(vacp, file="", table.env=FALSE,rowname=NULL)

popimmune<-read.csv("PopnImmunityAll.csv",header=T)
popimmune$Age = factor(popimmune$Age,levels(popimmune$Age)[c(2,3,6,8,10:12,4,5,7,9,1)])
pop<-read.csv("popnsize.csv",header=T)
colnames(pop)<-0:100
pop<-t(pop)
impop<-c(popimmune$Immunity[1:6],rep(popimmune$Immunity[7],8),rep(popimmune$Immunity[8],5),rep(popimmune$Immunity[9],5),
rep(popimmune$Immunity[10],9),rep(popimmune$Immunity[11],20),rep(popimmune$Immunity[12],48))
length(pop)
naive<-round(pop-(pop*impop))
## match cases per age
AgeInYears<-0:100
naive<-cbind(AgeInYears,naive)
colnames(naive)<-c("AgeInYears","Naive")
naive<-merge(naive,caseyr,by="AgeInYears",all=T)
#naive[is.na(naive)] <- 0
colnames(naive)<-c("AgeInYears","Naive","Cases")
##
# par(mar=c(5,6,4,2)+0.1)
#par(cex.axis=2)
#plot(naive$Naive ,col="black",bg="black",pch=1,ylab="Number to vaccinate",xlab="Age in Years",
#main="",cex.lab=2,cex=3)
#points(round(0.28*naive$Naive),col="grey",bg="red",pch=21,cex=2)
#points(round(0.60*naive$Naive),col="grey",bg="orange",pch=21,cex=2)
#points(x=3:18,y=naive$Naive[3:18],col="grey",bg="orange",pch=23,cex=2) # 

#legend("topright",c("Naive","28% naive, all ages","28% naive, all 2-17 yr olds"),
#bty="n",pch=c(1,21,23),col=c("black","grey","grey"),pt.bg=c("black","red","orange"),cex=2)

propsv = NULL
for (i in 1:20 ) {
  propsv[i] = round((sum(naive$Naive[i:20])/sum(naive$Naive)),2)
}
propsvAge = NULL
for (i in 1:20 ) {
  propsvAge[i]=paste(c(min(i:20)-1, max(i:20)-1),collapse="-")
}

table120<-as.data.frame(propsv,propsvAge)
table120<-cbind(Age=rownames(table120),table120)
rownames(table120)<-1:20
colnames(table120)<-c("Age","Proportion")

propsvrev = NULL
for (i in 20:1 ) {
  propsvrev[i] = round((sum(naive$Naive[i:1])/sum(naive$Naive)),2)
}
propsvrevAge = NULL
for (i in 20:1 ) {
  propsvrevAge[i]=paste(c(min(i:1)-1, max(i:1)-1),collapse="-")
}

table201<-as.data.frame(propsvrev,propsvrevAge)
table201<-cbind(Age=rownames(table201),table201)
rownames(table201)<-1:20
colnames(table201)<-c("Age","Proportion")

propsv11 = NULL
for (i in 1:11 ) {
  propsv11[i] = round((sum(naive$Naive[i:11])/sum(naive$Naive)),2)
}
propsv11Age = NULL
for (i in 1:11 ) {
  propsv11Age[i]=paste(c(min(i:11)-1, max(i:11)-1),collapse="-")
}

table11<-as.data.frame(propsv11,propsv11Age)
table11<-cbind(Age=rownames(table11),table11)
rownames(table11)<-1:11
colnames(table11)<-c("Age","Proportion")

require(R0)
#source("estR0.R")

# generation time 
genTime <- generation.time(type="lognormal", val=c(12, 3.5))

jm.epid <- function (epid.nb, GT, R0, epid.length, family, negbin.size = NULL, 
peak.value = 300000, popn = 300000) 
{
if (class(GT) != "R0.GT") {
stop("GT object must be of class R0.GT.")
}
if (family == "negbin" & is.null(negbin.size)) {
negbin.size <- R0/4
}
GT <- GT$GT
epidemics <- matrix(data = 0, nrow = epid.length, ncol = epid.nb)
for (n in 1:epid.nb) {
sim.epid = c(1, rep(0, epid.length - 1))
for (t in 1:epid.length) {
if (family == "poisson") {
susc <- max(0,popn - sum(sim.epid, na.rm=T))
#    peak.value[t]<-susc
new <- rbinom(sim.epid[t], susc, min(1, R0/susc))
if (sum(new) > susc) # arggrggg
{
#      cat("doing the new thing", susc, sim.epid[t], sum(sim.epid[1:t]),"\n")
new <- rep(0, sim.epid[t])
for (i in 1:sim.epid[t]) {
#         cat("taking a random number, susc=", susc, "\n")
new[i] <- rbinom(1, susc, min(1, R0/susc))
susc <- susc - new[i]
if (susc == 0)
{
#             cat("got 'em all", sum(new), "\n")
break;
}
}
}
#        cat("generating", new[1], "individuals from first, susc=", susc, "\n")
}
else if (family == "negbin") {
new <- rnbinom(sim.epid[t], size = negbin.size, 
mu = R0)
}
if (is.na(sum(new)))
break

newd <- rmultinom(1, sum(new), GT)[, 1]
sim.epid[t:(t + length(GT) - 1)] <- sim.epid[t:(t + 
length(GT) - 1)] + newd
if (sim.epid[t + 1] > peak.value & t < (epid.length - 
1)) {
sim.epid[(t + 2):epid.length] <- 0
break
}
}
sim.epid <- sim.epid[!is.na(sim.epid)]
epidemics[, n] <- sim.epid
}
return(epidemics)
}


## if R 0 == 2
set.seed(1) # all infected
res<-jm.epid(epid.nb=1000,GT=genTime,R0=0.99,epid.length=365*5,popn=331385
,family="poisson",peak.value=331385)
sizes<-colSums(res)
sizes
legMd<-as.factor(paste("median =",c(median(sizes))))
legMn<-as.factor(paste("mean =",c(round(mean(sizes)))))
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=2)
hist(sizes,xlim=c(0,200),col="grey",breaks=1000,xlab="Outbreak size",main="",cex.lab=2)
#summary(sizes)
abline(v=median(sizes),col="orange",lty=3,lwd=10)
abline(v=mean(sizes),col="red",lty=3,lwd=10)
#length(which(sizes>5))/1000
legend('topright',c(levels(legMd),levels(legMn)),lty=rep(3,2),col=c("orange","red"),bty="n",lwd=10,cex=2)

par(mar=c(5,6,4,2)+0.1)
par(cex.axis=2)
hist(sizes,breaks=1000,xlab="Outbreak size",main="",cex.lab=2)
abline(v=median(sizes),col="orange",lty=3,lwd=10)
abline(v=mean(sizes),col="red",lty=3,lwd=10)
legend('topright',c(levels(legMd),levels(legMn)),lty=rep(3,2),col=c("orange","red"),bty="n",lwd=10,cex=2)

###
costtable20<-read.csv("cost_benefit_20.csv",header=T)
library(Hmisc)
colnames(costtable20)<-c("DHB", "Vacc", "Vacc costs", "Wages saved", "Manage saved", "Hospitalised", "Hosp saved", "Costs saved", "Outbreak", "OB costs", "B/C")
latex(costtable20, file="", table.env=FALSE,rowname=NULL)
costtable50<-read.csv("cost_benefit_50.csv",header=T)
library(Hmisc)
colnames(costtable50)<-c("DHB", "Vacc", "Vacc costs", "Wages saved", "Manage saved", "Hospitalised", "Hosp saved", "Costs saved", "Outbreak", "OB costs", "B/C")
latex(costtable50, file="", table.env=FALSE,rowname=NULL)
