# computes average numbers of contacts per case, and costs per contact in lost wages

library(dplyr)

# read in case data
cases <- read.csv("Measles 2013-2014-20150108-Contacts.csv", skip=6)[,-1]

# eliminate those cases with unknown number of contacts
cases <- cases %>% filter(CaseCont != "Unknown")

# sum up the number of over and under 15 month susceptible contacts (assumed to be contacts that will
# be quarantined)
susceptible_contacts <- cases %>% summarize(Over15months = sum(NoGE15Susc, na.rm=T), Under15Months=sum(NoLT15Susc, na.rm=T))

# NOTE: Do under 15 months need quarantining, or do we assume they're already quarantined?
contacts_per_case <- sum(susceptible_contacts) / nrow(cases) # 2.12

# read in contacts data
contacts <- read.csv("Anonymised Exposed Contact Age Not Immune or Unknown quaratine period.csv")

under_14 <- contacts %>% filter(Under.14.or.14.and.over == "Under 14")
over_14 <- contacts %>% filter(Under.14.or.14.and.over == "14 and Over")

# length of time quarantined
days_u14 <- under_14 %>% summarize(Average_Quarantine = sum(Estimated.days.of.quarantine)/n()) # 8.2
days_o14 <- over_14 %>% summarize(Average_Quarantine = sum(Estimated.days.of.quarantine)/n()) # 6.8

days_all <- contacts %>% summarize(Average_Quarantine = sum(Estimated.days.of.quarantine)/n()) # 7.3

# wages to use...

# read in case data

cases <- read.csv("../DHayman_20140627.csv")

post_2008 <- cases %>% filter(RptYear >= 2008)

# compute the number in each age group
post_2008 <- post_2008 %>% mutate(AgeGroup = ifelse(AgeInYears <= 14, "Under14", ifelse(AgeInYears > 19, "Over19", "Teen")))

num <- table(post_2008$AgeGroup)

# hmm, 721 under 14's, 191 teen's, 254 over 19s...

teens_employed = 0.41
num_cases <- num["Teen"] * teens_employed + num["Over19"]
num_caregivers <- num["Teen"] + num["Under14"]

cost_caregivers <- 102616 / num_caregivers * sum(num) / 247
cost_cases      <- 104539 / num_cases * sum(num) / 247

# TODO: Work out proportion of cases requiring care and that wage etc.



# cases in employment: 191*0.41 + 254

# convergence of N-R to find P (attack proportion)

P  <- seq(0.05, 0.1, by=0.001)
R0 <- 12.8
x0 <- 0.1192

f <- function(P) {
  P / (1 - exp(-R0*P)) - x0
}

fP <- function(P) {
  (1 - (1 + P*R0)*exp(-R0*P))/(1 - exp(-R0*P))^2
}

plot(P, fP(P), type="l")

fp <- function(P) {
}
