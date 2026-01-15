# IBCM data checks 
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
all = feet + arms + hands + multiple
all

pop = data.frame(
  LGA = c("SG", "Zaria", "KS"),
  pop = c(430000, 550000, 402390)
)

bites = data.frame(
  LGA = c("SG", "Zaria", "KS"),
  bites = c(19, 8, 1)
)

months = 21
lga_bi <- bites$bites * (12/months) * 100000/ pop$pop  # annual bite incidence
lga_bi

bpi <- sum(bites$bites) * (12/months) * 100000/ sum(pop$pop)
bpi

LR = 16
UR = 1
prob_exp <- 7 - 2 # minus the rabies cases
conf_exp <- 7
HR <- prob_exp + conf_exp; HR
BP = LR + UR + HR;  BP
c(HR, UR, LR)/BP

deaths <- 2
denom <- 817801 # Zaria and Kubau LGAs (combined estimated population)
DI <- deaths * (12/ months) * 100000/denom
DI

exp_PEP <- HR - deaths
pDeath <- 0.165
unobs_exp <- deaths / pDeath # pDeath * exp_no_PEP = deaths
total_exp <- unobs_exp + exp_PEP
exp_PEP/ total_exp

Kubau <- denom - pop$pop[pop$LGA %in% c("Zaria")]
total_pop <- sum(pop$pop) + Kubau 
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

intro <- 848
methods <- 1061 + 185; methods
results <- 615 + 968; results
discussion <- 1517
total <- intro + methods + results + discussion
total

