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

pdf(paste("case_age_dist.pdf"), width=7, height=5)
par(mfrow=c(1,1))
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=2)
hist(time$AgeInYears,col="grey",xlab="Age in years",main="",breaks=90,include.lowest=TRUE,right=F,ylab="Frequency",cex.lab=2)
hist(test$AgeInYears,col="black",breaks=90,include.lowest=TRUE,right=F,add=T)
legend("topright",c("1997-2014","2007-2014"),col=c("grey","black"),pch=15,bty="n",cex=2)
dev.off()
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
# test$AgeInYears<-findInterval(test$AgeInYears,c(3,6,18,25))
test$AgeInYears<-findInterval(test$AgeInYears,c(2,5,18,25))
test$AgeInYears<-as.factor(test$AgeInYears)
#tage<-revalue(test$AgeInYears, c("0"="0-2", "1"="3-5","2"="6-17","3"="18-24","4"="25+"));
tage<-revalue(test$AgeInYears, c("0"="0-1", "1"="2-4","2"="5-17","3"="18-24","4"="25+"));
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
## for MoH desired ages
test$AgeInYears<-findInterval(test$AgeInYears,c(2,5,18,25))
test$AgeInYears<-as.factor(test$AgeInYears)
tage<-revalue(test$AgeInYears, c("0"="0-1", "1"="2-4","2"="5-17","3"="18-24","4"="25+"));
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

## here's the additional fudge the MoH asked for.
## add 5 year olds to next class
dm[11:15,3:12]<-dm[11:15,3:12]+round(dm[6:10,3:12]/3,0)
dm[6:10,3:12]<-round(dm[6:10,3:12]*2/3,0)
## add 2 year olds to next class
dm[6:10,3:12]<-dm[6:10,3:12]+round(dm[1:5,3:12]/3,0)
dm[1:5,3:12]<-round(dm[1:5,3:12]*2/3,0)
## 
# rename
dm$Age<-revalue(dm$Age, c("0-2"="0-1", "3-5"="2-4","6-17"="5-17"));

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

#par(mar=c(12,6,4,2)+0.1)
#par(cex.axis=2)
#plot(tp$cases/tp$Popn*10000,main="",
#        ylab="Per Capita Rate Per 10,000",
#        pch=16,cex=2,col=c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(6,10)),
#xlab="",cex.lab=2,xaxt="n")
#axis(side = 1, at = 1:75, labels=rep(c("0-2yr, 1-3","0-2yr, 6-10","3-5yr, 1-5","3-5yr, 6-10","6-17yr, 1-5","6-17yr, 6-10","18-24yr, 1-5",'18-24yr, 1-5',"25+yr, 1-5","25+yr, 6-10"),5),
#     las=2,cex.axis=1.5)
#mtext(side=1,line=10,text="Age and NZDep",cex=2)
#legend("topleft",c("Asian","European","Maori","MLA","Pacific"),
#col=c(1:4,6),pch=16,bty="n",cex=1.8)
#par(mar = c(5,4,4,2) + 0.1)
#

tp$perCap<-with(tp,tp$cases/tp$Popn*10000)

pdf(paste("case_age_nzdep.pdf"), width=7, height=5)
ggplot(tp, aes(NZDep, cases, fill=Age)) + 
  geom_bar(stat="identity", position="dodge")+
 # theme(text=element_text(size=45))+
  ylab("cases")
dev.off()

pdf(paste("case_eth_nzdep.pdf"), width=7, height=5)
ggplot(tp, aes(NZDep, cases, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
#  theme(text=element_text(size=45))+
  ylab("cases")
dev.off()

pdf(paste("case_age_eth.pdf"), width=7, height=5)
ggplot(tp, aes(Age, cases, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
#  theme(text=element_text(size=45))+
  ylab("cases")
dev.off()

pdf(paste("percap_age_nzdep.pdf"), width=7, height=5)
ggplot(tp, aes(NZDep, perCap, fill=Age)) + 
  geom_bar(stat="identity", position="dodge")+
 # theme(text=element_text(size=45))+
  ylab("Per capita per 10000")
dev.off()

pdf(paste("percap_eth_nzdep.pdf"), width=7, height=5)
ggplot(tp, aes(NZDep, perCap, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
  #theme(text=element_text(size=45))+
  ylab("Per capita per 10000")
dev.off()

pdf(paste("percap_eth_age.pdf"), width=7, height=5)
ggplot(tp, aes(Age, perCap, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
  #theme(text=element_text(size=45))+
  ylab("Per capita per 10000")
dev.off()

tpsub<-tp[!(tp$Ethnicity=="MLA"),]

pdf(paste("percap_nzdep_age.pdf"), width=7, height=5)
ggplot(tpsub, aes(NZDep, perCap, fill=Age)) + 
  geom_bar(stat="identity", position="dodge")+
  #theme(text=element_text(size=45))+
  ylab("Per capita per 10000")
dev.off()

pdf(paste("percap_eth_nzdep.pdf"), width=7, height=5)
ggplot(tpsub, aes(NZDep, perCap, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
  #theme(text=element_text(size=45))+
  ylab("Per capita per 10000")
dev.off()

pdf(paste("percap_eth_age.pdf"), width=7, height=5)
ggplot(tpsub, aes(Age, perCap, fill=Ethnicity)) + 
  geom_bar(stat="identity", position="dodge")+
  #theme(text=element_text(size=45))+
  ylab("Per capita per 10000")
dev.off()
##
#pdf(paste("case_nzdep.pdf"), width=7, height=5)
#ggplot(tp, aes(NZDep, cases, fill=NZDep)) + 
#  geom_bar(stat="identity", position="dodge")+
#  #  theme(text=element_text(size=45))+
#  ylab("cases")
#dev.off()
#
#pdf(paste("percap_nzdep.pdf"), width=7, height=5)
#ggplot(tpsub, aes(NZDep, perCap, fill=NZDep)) + 
#  geom_bar(stat="identity", position="dodge")+
#  #  theme(text=element_text(size=45))+
#  ylab("Per capita per 10000")
#dev.off()


# This chunk uses "results=tex"
library(Hmisc)
latex(PerCap, file="", table.env=FALSE,rowname=NULL)
write.csv(PerCap,"percap.csv")
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

model3<-update(model2,~.-Ethnicity:NZDep)
summary(model3)
anova(model3,test="F")
anovap<-anova(model3,test="F")
write.csv(anovap,"anovap.csv")

pdf(paste("Cases_regmodel.pdf"), width=7, height=5)
par(cex.axis=2)
par(mar=c(5,6,4,2)+0.1)
hist(tpsub$cases,xlab="Number of cases",main='',col='grey',breaks=20,ylab="Number of categories",cex.lab=2)
dev.off()
res<-predict(model3)
#res<-predict(model2)

pdf(paste("Cases_regmodel_prediction.pdf"), width=7, height=5)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=2)
plot(exp(res),tpsub$cases,xlab="cases",ylab='predictions',main="",pch=16,col="darkgrey",cex.lab=2,cex=2)
dev.off()

cor(exp(res),tpsub$cases)
cor.test(exp(res),tpsub$cases)

pdf(paste("Cases_regmodel_resid.pdf"), width=7, height=5)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=2)
#hist(model3$residuals,main="",xlab="residuals",col="grey",cex.lab=2,ylab="")
hist(model2$residuals,main="",xlab="residuals",col="grey",cex.lab=2,ylab="")
dev.off()

library(xtable)
resan<-xtable(anova(model3,test="F"))
#resan<-xtable(anova(model2,test="F"))
print(resan,floating = FALSE)

library(xtable)
#xtb<-xtable(summary(model3))
xtb<-xtable(summary(model2))
print(xtb,floating = FALSE)

AgeVac<-table(testv$VC,testv$AgeInYears)
row.names(AgeVac)<-c("Unvaccinated","Dose1","Dose2")
AgeVac<-t(AgeVac)

pdf(paste("vacc_status.pdf"), width=7, height=5)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=2)
barplot(t(AgeVac),xlab="Age",col=c("darkgrey","red","orange"),main="",ylab="Frequency",cex.lab=2)
legend("topright",c("Unvaccinated","Dose 1","Dose 2"),fill=c("darkgrey","red","orange"),cex=1.8,bty="n")
dev.off()

pdf(paste("vacc_age.pdf"), width=7, height=5)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=2)
hist(vac$Dose1Mths,breaks=100,col=rgb(0,0,1,1/4),xlab="Age in months",main="",cex.lab=2,ylab="Frequency")
abline(v=12,col="blue")
hist(vac$Dose2Mths,breaks=50,add=T,col=rgb(1,0,0,1/4))
abline(v=60,col="red")
legend("topright",c("Dose 1","Dose 2"),col=c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)),pch=15,bty="n",cex=1.8)
legend("top",c("12 months","60 months"),col=c("blue","red"),lty=1,bty="n",cex=1.8)
dev.off()

DOB<-as.numeric(levels(testv$RptYear))[testv$RptYear]-testv$AgeInYears
#hist(DOB,breaks=100,col="grey",main="2007- 2014 measles cases",xlab="Date of Birth")

DOBVac<-table(testv$VC,DOB)
row.names(DOBVac)<-c("Unvaccinated","Dose1","Dose2")
DOBVac<-t(DOBVac)

pdf(paste("dob_vacc_status.pdf"), width=7, height=5)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=1)
barplot(t(DOBVac),xlab="Age",col=c("darkgrey","red","orange"),main="",ylab="Frequency",cex.lab=1)
legend("topleft",c("Unvaccinated","Dose 1","Dose 2"),fill=c("darkgrey","red","orange"),cex=1,bty="n")
dev.off()

pdf(paste("dob_vacc_dose.pdf"), width=7, height=5)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=1)
par(mfrow=c(2,1))
barplot(DOBVac[,2],col=rgb(0,0,1,1/4),xlab="Date of Birth",main="",cex.lab=1,ylab="Frequency")
legend("topleft",c("Dose 1"),col=rgb(0,0,1,1/4),pch=15,bty="n",cex=1)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=1)
barplot(DOBVac[,3],col=rgb(1,0,0,1/4),xlab="Date of Birth",main="",cex.lab=1,ylab="Frequency")
legend("topleft",c("Dose 2"),col=rgb(1,0,0,1/4),pch=15,bty="n",cex=1)
dev.off()

pdf(paste("dob_vacc_unvac.pdf"), width=7, height=5)
par(mar=c(5,6,4,2)+0.1)
par(cex.axis=1.5)
par(mfrow=c(1,1))
barplot(DOBVac[,1],col="grey",xlab="Date of Birth",main="",cex.lab=1.5,ylab="Frequency")
legend("topleft",c("Unvaccinated"),col="grey",pch=15,bty="n",cex=1.5)
dev.off()

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

pdf(paste("naive_allPop.pdf"), width=7, height=5)
par(cex.axis=2)
par(mar=c(5,6,4,2)+0.1)
plot(pop,xlab="Age",ylab="Population",cex.lab=2)
points(naive,pch=16)
legend("topright",c("Population","Naive"),pch=c(1,16),bty="n",cex=1.8)
dev.off()

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

# proportions of the naive population the wuld be covered if vaccinated by age groups now
propvCohort<-c(
round((sum(naive$Naive[3:6])/sum(naive$Naive)),2),
round((sum(naive$Naive[6:12])/sum(naive$Naive)),2),
round((sum(naive$Naive[6:17])/sum(naive$Naive)),2),
round((sum(naive$Naive[6:19])/sum(naive$Naive)),2),
round((sum(naive$Naive[12:17])/sum(naive$Naive)),2),
round((sum(naive$Naive[12:19])/sum(naive$Naive)),2),
round((sum(naive$Naive[1:30])/sum(naive$Naive)),2),
round((sum(naive$Naive[1:45])/sum(naive$Naive)),2),
round((sum(naive$Naive[8:100])/sum(naive$Naive)),2))
names(propvCohort)<-c("2-5","5-11","5-16","5-18","11-16","11-18","0-29","0-44",">8")
write.table(propvCohort,"vaccine_proportions_Cohort.txt",sep=",",row.names=T,col.names=F)

propsv = NULL
for (i in 1:20 ) {
  propsv[i] = round((sum(naive$Naive[i:13])/sum(naive$Naive)),2)
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

write.table(table11,"vaccine_proportions_11yrOlds.txt",sep=",",row.names=F)
write.table(table120,"vaccine_proportions_1_20yrOlds.txt",sep=",",row.names=F)
write.table(table201,"vaccine_proportions_20_1yrOlds.txt",sep=",",row.names=F)

###
