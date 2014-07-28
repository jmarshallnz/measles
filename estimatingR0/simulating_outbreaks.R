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


## use naive and popn size from "multivariate_measles" to scale R0..
scale<-1/(1-0.11) / (1/(1-0.89))

res<-jm.epid(epid.nb=100,GT=genTime,R0=2.13*scale,epid.length=365*5,popn=4.2*1e6 #* 0.1
             ,family="poisson",peak.value=4.2*1e6 * 0.1)
#matplot(res,pch=16,col="grey")
#head(res)
#colSums(res)

sizes<-colSums(res)
sizes

hist(sizes,breaks=1000)
summary(sizes)
## or simulate from R0 without scaling using the same naive population size

res<-jm.epid(epid.nb=100,GT=genTime,R0=1,epid.length=365*5,popn=4.2*1e6 * 0.11
             ,family="poisson",peak.value=4.2*1e6 * 0.11)
#matplot(res,pch=16,col="grey")
#head(res)
#colSums(res)

sizes<-colSums(res)
sizes

hist(sizes,breaks=1000)
summary(sizes)
