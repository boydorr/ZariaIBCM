# IBCM data checks 
library(Hmisc)

# SUMMARY STATISTICS FROM IBCM DATA
abu = 10
miba = 7
kwangila = 5
durumi = 3
pharmacy = 3
total = abu + miba + kwangila + durumi + pharmacy
total

feet = 14
arms = 12
hands = 2
multiple = 2
bite_sites = feet + arms + hands + multiple
bite_sites

# Data 
df = data.frame(
  LGA = c("SG", "Zaria", "KS", "Kubau"),
  pop = c(430500, 601000, 595000, 414700), 
  bites = c(19, 8, 1, 0)
)

months = 21
lga_bi <- df$bites * (12/months) * 100000/ df$pop  
binconf(df$bites, df$pop* (months/12), method = "wilson") * 100000 # annual bite incidence per LGA
binconf(sum(df$bites), sum(df$pop)* (months/12), method = "wilson") * 100000 # overall across the 3 LGAs
binconf(sum(df$bites[1:3]), sum(df$pop[1:3])* (months/12), method = "wilson") * 100000 # overall across the 3 LGAs

# PROBABLE RABIES EXPOSURES AND DEATHS
LR = 16 # low risk bite patients
UR = 1 # unknown risk
prob_exp <- 7 - 2 # probable exposures who attended due to bites, minus those who presented with rabies
conf_exp <- 7 # confirmed exposures who attended due to bites,
HR <- prob_exp + conf_exp; HR # total high risk bite patients
BP = LR + UR + HR;  BP # total bite patients
c(HR, UR, LR)/BP # proportion of bite patients attending clinics according to risk
binconf(c(HR, UR, LR), BP, method = "wilson") # proportion high risk was 0.4 (0.26-0.59)

deaths <- 2
binconf(deaths, sum(df$pop[c(2,4)]) * (months/12), method = "wilson") * 100000 # annual death incidence 

# Back extrapolation of total exposures
exp_PEP <- HR 
n_deaths <- 2
pDeath <- 0.165
exp_no_PEP <- n_deaths / pDeath # pDeath * exp_no_PEP = n_deaths

# calculate: given pDeath and 2 deaths occurred, how many exposures that did not get PEP are likely to have occurred?
# (i.e. how many coin tosses of a biased coin (pDeath) until 2 heads (deaths) are observed?)
set.seed(123)
n_sims <- 100000     # number of simulated experiments

sim_exp <- function(p) {
  deaths <- 0
  exposures_no_PEP <- 0
  while (deaths < 2) {
    exposures_no_PEP <-  exposures_no_PEP + 1
    if (runif(1) < p) deaths <- deaths + 1
  }
  exposures_no_PEP
}
# run simulations
exp_no_PEP <- replicate(n_sims, sim_exp(pDeath)) + 2
quantile(exp_no_PEP, probs = c(0.025, 0.5, 0.975))

# analytically
mean_exp <- 2 / pDeath
# CDF for negative binomial for #failures (exposed survivors) before Y successes (deaths)
# Let X = exposures before 2nd death, then Y = X + 2
alpha <- 0.05
# lower & upper quantiles for X (failures)
q_low_fail  <- qnbinom(alpha/2, size = 2, prob = pDeath)
q_high_fail <- qnbinom(1 - alpha/2, size = 2, prob = pDeath)
# convert to exposures T = X + 2
pi_low  <- q_low_fail  + 2
pi_high <- q_high_fail + 2
exp_non_PEP_quantiles <- c(pi_low, mean_exp, pi_high); exp_non_PEP_quantiles

# Calculate PIs on prob of obtaining PEP if exposed
total_exp <- exp_non_PEP_quantiles + exp_PEP 
exp_PEP/ total_exp # proportion of exposures that got PEP, pPEP!

# Overall incidence of exposures
total_pop <- sum(df$pop) 
total_exp * (12/months) * 100000/total_pop


# INVESTIGATIONS
biters <- 26
suspect <- 3
investigated <- biters + suspect; investigated
biters/investigated #  90% dogs 
suspect/investigated # 10% that were acting suspiciously
referrals <- 11
referrals/investigated # 38% referred to vets
direct_vet <- 10
direct_vet/investigated # 34% came directly to clinics
health_id <- 8
health_id/investigated # 28% were from bite patients going direct to facilities 

rdt <- 8
probable <- 5
probable/investigated # 17% probable
rdt/investigated # 28% tested positive on RDT
(probable + rdt)/ investigated
healthy <- 16
healthy/investigated # 55% healthy
healthy + rdt + probable
