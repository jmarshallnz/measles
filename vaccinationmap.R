library(maptools)

# read in data from NIR
data <- read.table("NIR_MMR_data_20060101-20140731/NIR_MMR_data_20060101-20140731.txt", header=T, sep="|")

# read in domicile <-> AU map
data_dom <- read.csv("NIR_MMR_data_20060101-20140731/domicile_code.csv")

# merge NIR with domicile/AU so we have AU for each NIR
data_merge <- merge(data, data_dom, by.x = "DOMICILE_CODE", by.y="dom", all=T)

# TODO: figure out how many kids are immunised in each AU
# find number of kids by BIRTH_CALENDAR_YEAR, AU, VACCINE_DOSE

num_kids <- table(data_merge$BIRTH_CALENDAR_YEAR, data_merge$area.unit, data_merge$VACCINE_DOSE)
# drop the 0's from vaccine_dose
dose1 <- t(num_kids[,,2])
dose2 <- t(num_kids[,,3])
dose1 <- as.data.frame(dose1[,-ncol(dose1)])
dose2 <- as.data.frame(dose2[,-ncol(dose2)])
dose1$cau <- rownames(dose1)
dose2$cau <- rownames(dose2)


# read in shapefile for AU's
au <- readShapeSpatial("AU2013_GV_Full")

au_data <- slot(au, "data")

# read in denominator data for AU's
data_denom<-read.csv("2013-mb-dataset-Total-New-Zealand-individual-part-1_AU.csv",header=T)
denom<-data_denom[,1]
for (i in 1:5)
  denom <- cbind(denom, data_denom[,3]/5)
for (i in 1:5)
  denom <- cbind(denom, data_denom[,4]/5)
colnames(denom)[2:11] <- 2013:(2013-9)
denom <- denom[,c(1,9:2)]

dose2_merge <- merge(dose2, denom, by.x="cau", by.y="denom", all=T)
diff <- dose2_merge[,2:8+7] - dose2_merge[,2:8]
image(as.matrix(diff > 0))

dose2_num <- data.frame(cau=dose2_merge$cau, vacc = rowSums(dose2_merge[,2:5]), total = rowSums(dose2_merge[,10:13]))
diff <- dose2_num$total - dose2_num$vacc

dose1_merge <- merge(dose1, denom, by.x="cau", by.y="denom", all=T)
diff <- dose1_merge[,2:8] - dose1_merge[,2:8+7]
image(as.matrix(diff > 0))

au_new <- merge(au_data, dose2_num, by.x="AU2013", by.y="cau", all.x=T)
au_new$prop <- au_new$vacc / au_new$total
library(RColorBrewer)
library(classInt)

fixedBreaks <- c(0,0.5,0.7, 0.9, 1.1, 1.3, 1.5, Inf)
au_new$prop[!is.finite(au_new$prop)] <- NA
breaks <- classIntervals(au_new$prop, n=7, style="fixed", fixedBreaks=fixedBreaks)
pal <- brewer.pal("Spectral", n=7)

cols <- findColours(breaks, pal)
plot(au, col=cols)

## plot to show Ministry the data issues...
fixedBreaks <- c(0,0.6,0.8,1, 1.2,1.4,Inf)
au_new$prop[!is.finite(au_new$prop)] <- NA
breaks <- classIntervals(au_new$prop, n=6, style="fixed", fixedBreaks=fixedBreaks)
pal <- brewer.pal("RdGy", n=6)
pal<-c("red","orange","yellow","grey","blue","violet")
cols <- findColours(breaks, pal)
par(mfrow=c(1,1))
plot(au, col=cols)
legend("topright",c("0-60%","61-80%","81-100%","101-120%","121-140%",">141%","NA")
       ,fill=c("red","orange","yellow","grey","blue","violet","white"),
bty="n",inset=0.1)
## par("usr")
##
par(fig = c(0.05, 0.4, 0.5,0.9), mar=c(5,5,0,0), new=TRUE)
hist(au_new$prop[au_new$prop < 2], breaks=50,col="grey",xlab="Proportion vaccinated",main="")
abline(v=1,col="red",lwd=3)
par(mfrow=c(1,1))

hist(au_new$prop, breaks=100,col="grey",xlab="Proportion vaccination",main="All domiciles")
abline(v=1,col="red")
