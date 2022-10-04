#### Economic analysis - calculating the basics

# load libraries
library(qs)
# load in saved epi model output file if want to run econ alone
load(file = here::here("UK_output", "total_cases_time.Rdata"))


#### sections in this script are:
# - calculating annual number of deaths, hospitalizations and GP consultations cromer
# - calculating annual number of illness cases (non-febrile (ARI-fev) and fever (fev - death - hosp))
# - calulating annual non-death QALYs
# - calculating annual costs


####### Annual outcomes ######

# sum across years 
total_cases_time[, c("week", "epidemic","total_cases", "scenario_nice", "Date", 
                     "X15", "X16", "X17", "X18", "X19", "X20", "X21") := NULL]
cases_sample_year <- total_cases_time[, lapply(.SD, sum, na.rm=T), by = c("sample", "scenario", "Year", 
                                                                          "Virus")]
# combine age groups 15-25 and 25-44 as they're one in the cromer data
cases_sample_year[,X4 := X4 + X5]
cases_sample_year[,X5 := NULL]
cases_sample_year[,X11 := X11 + X12]
cases_sample_year[,X12 := NULL]

# relabel cromer data to match
cromer <- data.table(qread(here::here("UK_data","cromer_samples_by_age_group.qs")))
cromer[age_group == "[0,0.5)" & risk_group == "LowRisk", variable := "X1"]
cromer[age_group == "[0.5,5)" & risk_group == "LowRisk", variable := "X2"]
cromer[age_group == "[5,15)" & risk_group == "LowRisk", variable := "X3"]
cromer[age_group == "[15,45)" & risk_group == "LowRisk", variable := "X4"]
cromer[age_group == "[45,65)" & risk_group == "LowRisk", variable := "X6"]
cromer[age_group == "[65,+)" & risk_group == "LowRisk", variable := "X7"]

cromer[age_group == "[0,0.5)" & risk_group == "HighRisk", variable := "X8"]
cromer[age_group == "[0.5,5)" & risk_group == "HighRisk", variable := "X9"]
cromer[age_group == "[5,15)" & risk_group == "HighRisk", variable := "X10"]
cromer[age_group == "[15,45)" & risk_group == "HighRisk", variable := "X11"]
cromer[age_group == "[45,65)" & risk_group == "HighRisk", variable := "X13"]
cromer[age_group == "[65,+)" & risk_group == "HighRisk", variable := "X14"]
# change cromer to matching values
colnames(cromer) <- c("sample", "proportion", "ag", "rg", "Virus", "outcome", "variable")
cromer[ Virus == "H3N2", Virus := "AH3N2" ]
cromer[ Virus == "H1N1", Virus := "AH1N1" ]
#rearrange data to generate a replicate for each
cases_sample_year_m <- data.table(melt(cases_sample_year, id.vars = c("sample", "scenario", "Year", "Virus")))

cases_sample_year_m[variable == "X1", age := "[0,0.5)"]
cases_sample_year_m[variable == "X1", risk := "low"]
cases_sample_year_m[variable == "X2", age := "[0.5,5)"]
cases_sample_year_m[variable == "X2", risk := "low"]
cases_sample_year_m[variable == "X3", age := "[5,15)"]
cases_sample_year_m[variable == "X3", risk := "low"]
cases_sample_year_m[variable == "X4", age := "[15,45)"]
cases_sample_year_m[variable == "X4", risk := "low"]
cases_sample_year_m[variable == "X6", age := "[45,65)"]
cases_sample_year_m[variable == "X6", risk := "low"]
cases_sample_year_m[variable == "X7", age := "[65,+)"]
cases_sample_year_m[variable == "X7", risk := "low"]

cases_sample_year_m[variable == "X8", age := "[0,0.5)"]
cases_sample_year_m[variable == "X8", risk := "high"]
cases_sample_year_m[variable == "X9", age := "[0.5,5)"]
cases_sample_year_m[variable == "X9", risk := "high"]
cases_sample_year_m[variable == "X10", age := "[5,15)"]
cases_sample_year_m[variable == "X10", risk := "high"]
cases_sample_year_m[variable == "X11", age := "[15,45)"]
cases_sample_year_m[variable == "X11", risk := "high"]
cases_sample_year_m[variable == "X13", age := "[45,65)"]
cases_sample_year_m[variable == "X13", risk := "high"]
cases_sample_year_m[variable == "X14", age := "[65,+)"]
cases_sample_year_m[variable == "X14", risk := "high"]

cases_sample_year_m$outcome <- "death"
outcomes <- cases_sample_year_m
cases_sample_year_m$outcome <- "hosp"
outcomes <- rbind(outcomes,cases_sample_year_m)
cases_sample_year_m$outcome <- "GP"
outcomes <- rbind(outcomes,cases_sample_year_m)
# dta table
outcomes <- data.table(outcomes)
# match over the sample proportion for each variab
outcomes[cromer, on = c("sample", "Virus", "variable", "outcome"), proportion := i.proportion]
outcomes[, outcome_value := value*proportion]
outcomes[,sum(outcome_value), by = c("outcome", "sample", "scenario")]
  
###### Annual illnessses #######

#From the Carrat paper 
carrat_symptoms <- data.table(
  c("symptoms", "symptoms", "symptoms", "fever", "fever", "fever"),
  c("AH1N1", "AH3N2", "B", "AH1N1", "AH3N2", "B"), 
           c(0.708, 0.645, 0.574, 0.370, 0.406, 0.075),
           c(0.504, 0.546, 0.352, 0.246, 0.309, 0.032),
           c(0.852, 0.733, 0.769, 0.513 ,0.511, 0.169)
           )

colnames(carrat_symptoms) <- c("outcome","Virus", "median", "lower", "upper")

# fit a beta distribution to each of the rows. 
##### Estimating alpha and beta from qunatiles and mean

quantiles_to_fit <- c(0.5,0.025, 0.975)

# output the comparison measures.
f.beta <- function(alpha, beta, x, lower=0, upper=1) {
  p <- pbeta(x[1:3], alpha, beta)
  # return both
  return(c(p))
}

# Sums of squares - for comparing estimated to real. 
delta <- function(fit, actual) sum((fit-actual)^2)

#calculate for the theta and probs what is needed
objective <- function(theta, x, prob, ...) {
  ab <- exp(theta) # Parameters are the *logs* of alpha and beta
  #work out what the logit of the quanitle is for these parameters
  fit <- f.beta(ab[1], ab[2], x=as.numeric(x),...)
  # return the sum of squares. 
  return (delta(fit, prob))
}


start <-log(c(10,10))  
# for each of the samples. 

for(i in 1:nrow(carrat_symptoms)){

  x <- carrat_symptoms[i,c("median", "lower", "upper")]
  
sol <- optim(f=objective,p=start,
             method="L-BFGS-B", 
             x=x,
             prob=c(quantiles_to_fit)
)
parms <- exp(sol$par)           # Estimates of alpha and beta

carrat_symptoms[i,"alpha"] <- parms[1]
carrat_symptoms[i,"beta"] <- parms[2]

}

# check the outputs look reasonable
j <- 6
x <- seq(0,1, 0.01)
y <- dbeta(shape1 = as.numeric(carrat_symptoms[j, "alpha"]), 
      shape2 = as.numeric(carrat_symptoms[j, "beta"]), 
      x = x)
plot(x,y)

# take n samples from each distribution
n_samples <- length(unique(cases_sample_year_m$sample))
H1N1_symptoms <- rbeta(n =n_samples, 
                      shape1 = as.numeric(carrat_symptoms[Virus =="AH1N1" & outcome == "symptoms", "alpha"]), 
                      shape2 = as.numeric(carrat_symptoms[Virus =="AH1N1" & outcome == "symptoms", "beta"]) )
H3N2_symptoms <- rbeta(n =n_samples, 
                      shape1 = as.numeric(carrat_symptoms[Virus =="AH3N2" & outcome == "symptoms", "alpha"]), 
                      shape2 = as.numeric(carrat_symptoms[Virus =="AH3N2" & outcome == "symptoms", "beta"]) )
B_symptoms <- rbeta(n =n_samples, 
                      shape1 = as.numeric(carrat_symptoms[Virus =="B" & outcome == "symptoms", "alpha"]), 
                      shape2 = as.numeric(carrat_symptoms[Virus =="B" & outcome == "symptoms", "beta"]))
H1N1_fever <- rbeta(n =n_samples, 
                       shape1 = as.numeric(carrat_symptoms[Virus =="AH1N1" & outcome == "fever", "alpha"]), 
                       shape2 = as.numeric(carrat_symptoms[Virus =="AH1N1" & outcome == "fever", "beta"]) )
H3N2_fever <- rbeta(n =n_samples, 
                       shape1 = as.numeric(carrat_symptoms[Virus =="AH3N2" & outcome == "fever", "alpha"]), 
                       shape2 = as.numeric(carrat_symptoms[Virus =="AH3N2" & outcome == "fever", "beta"]) )
B_fever <- rbeta(n =n_samples, 
                    shape1 = as.numeric(carrat_symptoms[Virus =="B" & outcome == "fever", "alpha"]), 
                    shape2 = as.numeric(carrat_symptoms[Virus =="B" & outcome == "fever", "beta"]))

# combine into table
illness_samples <- data.table(
  "outcome" = rep(c("symptoms", "fever"), each=(3*n_samples)),
  "sample" =  rep((1:n_samples),6), 
  "Virus" =  rep(c("AH1N1", "AH3N2", "B","AH1N1", "AH3N2", "B"), each = n_samples), 
   "illness" = c(H1N1_symptoms, H3N2_symptoms, B_symptoms, 
                 H1N1_fever, H3N2_fever, B_fever))
# create the data.frame for storing
cases_sample_year_m$outcome <- "symptoms"
outcomes2 <- cases_sample_year_m
cases_sample_year_m$outcome <- "fever"
outcomes2 <- rbind(outcomes2, cases_sample_year_m)
# merge together the model output and the sampling output
outcomes2[illness_samples, on = c("sample", "Virus", "outcome"), proportion := i.illness]
# work out illness numbers
outcomes2[,outcome_value := proportion*value]

# combine all the outputs
outcomes <- data.table(rbind(outcomes, outcomes2))

# cast table so can work on outcomes
outcomes_c <- dcast.data.table(outcomes, sample+scenario+Year+Virus+variable+age +risk+value ~ outcome,
                               value.var = "outcome_value")
outcomes_c[, f_no_symptoms := value - symptoms]
outcomes_c[, f_death := death]
outcomes_c[, f_hosp := hosp - death]
outcomes_c[, f_gp := GP - f_hosp ]
outcomes_c[, f_fever := fever - f_hosp ]
outcomes_c[, f_mild := symptoms - f_fever ]

outcomes_c[, f_death := death]
outcomes_c[, f_hosp := hosp ]
outcomes_c[, f_gp := GP  ]
outcomes_c[, f_fever := fever ]
outcomes_c[, f_mild := symptoms - fever ]

mid_summary <- outcomes_c[, sum(f_mild), by = c("sample", "scenario")]
colnames(mid_summary)[3] <- "mild_symptoms"
mid_summary$fever <- outcomes_c[, sum(f_fever), by = c("sample", "scenario")]$V1
mid_summary$gp <- outcomes_c[, sum(f_gp), by = c("sample", "scenario")]$V1
mid_summary$hosp <- outcomes_c[, sum(f_hosp), by = c("sample", "scenario")]$V1
mid_summary$death <- outcomes_c[, sum(f_death), by = c("sample", "scenario")]$V1

mid_summary_m <- melt(mid_summary, id.vars = c("sample", "scenario"))

ggplot(mid_summary_m, aes(x = scenario, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  theme_linedraw() 

ggplot(mid_summary_m, aes(x = scenario, y = value, fill = scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(variable ~ ., scales = "free_y") + 
  theme_linedraw() 

ggplot(outcomes_c, aes(x = scenario, y = f_mild)) + geom_jitter() + facet_grid(age~.)

outcomes_m <- melt.data.table(outcomes_c, id.vars= c("sample", "scenario", "Year", "Virus",
                                          "variable", "age", "risk"), 
                   measure.vars = c("f_death", "f_hosp", "f_gp", "f_fever", "f_mild"))

###### Annual non-death QALYS #######
#save deaths for later
 death_outcome <- outcomes_m[variable.1 == "f_death"]
# annual non-death summaries
nondeath_outcome <- outcomes_m[variable.1 != "f_death"]
annual_nondeath_outcomes <- nondeath_outcome[, sum(value), by = c("sample", "scenario", "Year", "variable.1")]

# QALYs for hospitalisation. 
# reference from Bageulin paper
QALYS_multiplier_hosp <- rnorm(n_samples, 
                               mean = 0.018, 
                               sd = 0.0018)
QALYS_multiplier_mild <- rnorm(n_samples, 
                               mean = 1.01e-3, 
                               sd = 8.35e-5)

sd_multiplier <- 1.01e-3/8.35e-5
QALYS_multiplier_fever <- rnorm(n_samples, 
                               mean = 7.49e-3, 
                               sd = 7.49e-3/sd_multiplier)

QALY_multipliers <- data.table(sample = rep(1:n_samples), 
                               variable.1 = rep(c("f_hosp", "f_mild", 
                                              "f_fever", "f_gp"), each = n_samples), 
                               multiplier = c(QALYS_multiplier_hosp, 
                                              QALYS_multiplier_mild, 
                                              QALYS_multiplier_fever, 
                                              rep(0,n_samples)))
# combine with outcomes table
annual_nondeath_outcomes[QALY_multipliers, on = c("sample", "variable.1"), 
                         QAlY_multiplier := i.multiplier]
annual_nondeath_outcomes[, QALYS := V1 * QAlY_multiplier]


###### Annual costs #######
c_mean <- 839
c_sd <- 192.1

costs_multiplier_hospital <- rlnorm(n_samples, 
                               mean  = exp(c_mean + (c_sd^2)/2), 
                               sd = sqrt((exp(c_sd^2)-1)*exp(2*c_mean + c_sd^2)))


costs_multiplier_hospital <- rlnorm(n_samples, 
                                    meanlog  = c_mean, 
                                    sdlog = c_sd)
mean(exp(rlnorm(10000, 
       mean  = log(c_mean), 
       sd = log(c_sd))))

## ? do the log normal


# assume for now htey are just normal
# samples from the costs
costs_multiplier_hospital <- rnorm(n_samples, 
                                    mean  = 839, 
                                    sd = 192.1)
costs_multiplier_gp <- rnorm(n_samples, 
                                   mean  = 37, 
                                   sd = 8.4)

# for the vaccination one, change the triangular to a gamma. 


# calculate gamma distribuition
f.gamma <- function(shape, scale, x) {
  p <- pgamma(q=x, shape=shape, scale=scale)
  return(p)#logit_p)
}

#calculate for the theta and probs what is needed
objective <- function(theta, x, prob, ...) {

  ab <- exp(theta) # Parameters are the *logs* of alpha and beta
  #work out what the logit of the quanitle is for these parameters
  fit <- f.gamma(ab[1], ab[2], x, ...)
  # return the sum of squares. 
  return (delta(fit, prob))
}
# Sums of squares.
delta <- function(fit, actual) sum((fit-actual)^2)
# starting values
start <-log(c(10,1))  
# qunatiles needed
quantiles_to_fit<- c(0.025,0.5,0.975)
# for each row, fit a gamma distribution
  
  x <- as.numeric(12,15.85,20)
  
  sol <- optim(f=objective,p=start,
               method="L-BFGS-B", 
               x=x,
               prob=c(quantiles_to_fit))
               
  parms <- exp(sol$par)           # Estimates of alpha and beta
  vacc_cost_shape<- parms[1]
  vacc_cost_scale <- parms[2]
  
  costs_multiplier_vacc <- rgamma(n_samples, 
                               shape  = vacc_cost_shape, 
                               scale = vacc_cost_scale)

  costs_vaccines <- data.table(sample = 1:n_samples, 
             vacc_costs = costs_multiplier_vacc)
  
  cost_multipliers <- data.table(sample = rep(1:n_samples), 
                                 variable.1 = rep(c("f_hosp", "f_mild", 
                                                    "f_fever", "f_gp"), each = n_samples), 
                                 multiplier = c(costs_multiplier_hospital, 
                                                rep(0,n_samples), 
                                                rep(0,n_samples), 
                                                costs_multiplier_gp))
  # combine with outcomes table
  annual_nondeath_outcomes[cost_multipliers, on = c("sample", "variable.1"), 
                           cost_multiplier := i.multiplier]
  annual_nondeath_outcomes[, QALYS := V1 * QAlY_multiplier]
  
  annual_nondeath_outcomes[,costs := V1*cost_multiplier]
# this is the outpcme costs
 annual_costs <-  annual_nondeath_outcomes[,sum(costs), by = c("sample", "scenario", "Year")]
 colnames(annual_costs)[4] <- "outcome_costs"
 # now need the vaccination costs
 total_vaccines[, Year := year(Date)]
 total_vaccines[,Date := NULL]
 annual_vaccines <- total_vaccines[,lapply(.SD, sum), by = c("Scenario", "Year")]
 
 annual_vaccines[,c("Vaccinated21", "Vaccinated20", "Vaccinated19", "Vaccinated18",
                    "Vaccinated17", "Vaccinated16", "Vaccinated15") := NULL]
 colnames(annual_vaccines) <- c("scenario", "Year", rep(paste0("X", 1:14)))
 annual_vaccines$Year <- as.numeric(annual_vaccines$Year)
 annual_costs$Year <- as.numeric(as.character(annual_costs$Year))
 annual_costs$scenario <- as.numeric(as.character(annual_costs$scenario))
 annual_vaccines[, total_vaccines := X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14]
 annual_costs[annual_vaccines, on = c("Year", "scenario"), vaccines_given := total_vaccines ]
 
 annual_costs[costs_vaccines, on = "sample", vacc_costs_dose := i.vacc_costs]
  
 annual_costs[,vacc_costs := vaccines_given*vacc_costs]
 annual_costs[,total_costs := outcome_costs + vacc_costs]
 