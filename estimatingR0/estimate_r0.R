
# compute generation time.  We're wanting a lognormal distribution with mean 12.0 and sd 3.5 from
# http://www.sciencedirect.com/science/article/pii/S0022519311003146
mu    <- 12
sigma <- 3.5
sigma_logn <- sqrt(log(1 + (sigma/mu)^2))
mu_logn    <- log(mu) - log(1 + (sigma/mu)^2) / 2

# then exp(rnorm(n, mu_logn, sigma_logn)) simulates from lognormal with the given mean and sd.

# the R0 library can estimate our distribution from incidence data
incidence <- read.csv("epidemic.csv")

require(R0)

# generation time in weeks
gT <- generation.time(type="lognormal", val=c(12, 3.5)/7)

# convert our incidence data to something we can use
counts <- incidence$Count
names(counts) <- incidence$Date

counts <- counts[1:51]
estR0<-estimate.R(counts, gT, t=1:51, methods=c("EG", "ML", "TD", "AR", "SB"), pop.size=1300000, nsim=100)
estR0<-estimate.R(counts, gT, t=1:51, end=51, methods=c("EG"), pop.size=1300000, nsim=100)
estR0<-estimate.R(counts, gT, t=1:51, end=51, methods=c("ML"), pop.size=1300000, nsim=100)
estR0<-estimate.R(counts, gT, t=1:51, end=51, methods=c("TD"), pop.size=1300000, nsim=100)
estR0<-estimate.R(counts, gT, t=1:51, end=51, methods=c("SB"), pop.size=1300000, nsim=100)
estR0<-estimate.R(counts, gT, t=1:51, end=51, methods=c("TD"), pop.size=1300000, nsim=1000)
pdf("rt.pdf", width=10, height=6)
plot(estR0$estimates$TD, TD.split=T)
dev.off()

