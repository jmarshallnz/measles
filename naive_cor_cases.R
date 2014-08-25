ttage<-aggregate( cbind(Cases) ~ Age, 
               data = testtable , FUN=sum)

ttage<-aggregate( cbind( DiseaseName ) ~ AgeInYears,# + Dose1Mths + Dose2Mths, 
                 data = data , FUN=sum)
ttage
naive
colnames(naive)<-"naive"
aget<-1:100
aget
aget<-as.data.frame(aget)
colnames(aget)<-"AgeInYears"
caset<-merge(aget,ttage,by="AgeInYears",all="T")
caset[is.na(caset)] <- 0
caset<-cbind(caset,naive)
cor(caset$DiseaseName,caset$naive)
cor(caset$DiseaseName[2:100],caset$naive[2:100])
