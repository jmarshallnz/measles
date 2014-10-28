## simulate measles outbreaks/epidemics using GT and R0 from data.
# http://www.sciencedirect.com/science/article/pii/S0022519311003146
mu    <- 12
sigma <- 3.5
sigma_logn <- sqrt(log(1 + (sigma/mu)^2))
mu_logn    <- log(mu) - log(1 + (sigma/mu)^2) / 2

# then exp(rnorm(n, mu_logn, sigma_logn)) simulates from lognormal with the given mean and sd.

# the R0 library 
require(R0)
#source("estR0.R")

# generation time 
genTime <- generation.time(type="lognormal", val=c(12, 3.5))
class(genTime)

res<-sim.epid(epid.nb=10,GT=genTime,R0=2,epid.length=365,family="poisson",peak.value=100000)
#matplot(res,pch=16,col="grey")
#head(res)
#colSums(res)
sizes<-colSums(res)
#sizes<-apply(res, 2, max)
hist(sizes,breaks=1000)
summary(sizes)

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

new_naive<-as.data.frame(c(34090,
             16094,
             37670,
             22164,
             36664,
             11851,
             10810,
             7672,
             12700,
             10703,
             11850,
             4345,
             23236,
             3410,
             8574,
             28071,
             3212,
             41059,
             2512,
             4697,
             331384),ncol=1)
rownames(new_naive)<-c("Auckland",
                       "Bay of Plenty",
                       "Canterbury",
                       "Capital and Coast",
                       "Counties Manukau",
                       "Hawke's Bay",
"Hutt Valley",
"Lakes",
"MidCentral",
"Nelson Marlborough",
"Northland",
"South Canterbury",
"Southern",
"Tairawhiti",
"Taranaki",
"Waikato",
"Wairarapa",
"Waitemata",
"West Coast",
"Whanganui",
"Total")

set.seed(1) # all infected

results_sim<-matrix(,nrow=nrow(new_naive),ncol=4)
results_sim[,1]<-rownames(new_naive)
colnames(results_sim)<-c("DHB","Median","Mean","Maximum")
for (i in 1:nrow(results_sim)){
res<-jm.epid(epid.nb=1000,GT=genTime,R0=0.99,epid.length=365*5,popn=new_naive[i,1]
            ,family="poisson",peak.value=new_naive[i,1])
sizes<-colSums(res)
results_sim[i,2]<-round(summary(sizes)[3],digits=0)
results_sim[i,3]<-round(summary(sizes)[4],digits=0)
results_sim[i,4]<-round(summary(sizes)[6],digits=0)}

results_sim

hist(sizes,breaks=1000,xlab="Outbreak size",main="",xlim=c(0,250),col="grey")
abline(v=median(sizes),col="orange",lty=3)
abline(v=mean(sizes),col="red",lty=3)
legMd<-as.factor(paste("median =",c(round(median(sizes)))))
legMn<-as.factor(paste("mean =",c(round(mean(sizes)))))
legMx<-as.factor(paste("maximum =",c(round(max(sizes)))))
legend('topright',c(levels(legMd),levels(legMn),levels(legMx)),lty=rep(3,2),col=c("orange","red"),bty="n")


hist(sizes,breaks=1000,xlab="Outbreak size",main="",xlim=c(0,250),col="grey")
abline(v=median(sizes),col="orange",lty=3,lwd=2)
abline(v=mean(sizes),col="red",lty=3,lwd=2)
legMd<-as.factor(paste("median =",c(round(median(sizes)))))
legMn<-as.factor(paste("mean =",c(round(mean(sizes)))))
legend('topright',c(levels(legMd),levels(legMn)),lty=rep(3,2),lwd=2,col=c("orange","red"),bty="n")


