hosp<-read.csv("hospital.csv",header=T)
str(hosp)
library(ggplot2)
ggplot(data=hosp, aes(x=Days, y=Number)) + geom_bar(stat="identity")
