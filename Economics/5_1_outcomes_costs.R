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
cases_sample_year_m <- melt(cases_sample_year, id.vars = c("sample", "scenario", "Year", "Virus"))
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
  

outcomes[variable == "X1", age := "[0,0.5)"]
outcomes[variable == "X1", risk := "low"]
outcomes[variable == "X2", age := "[0.5,5)"]
outcomes[variable == "X2", risk := "low"]
outcomes[variable == "X3", age := "[5,15)"]
outcomes[variable == "X3", risk := "low"]
outcomes[variable == "X4", age := "[15,45)"]
outcomes[variable == "X4", risk := "low"]
outcomes[variable == "X6", age := "[45,65)"]
outcomes[variable == "X6", risk := "low"]
outcomes[variable == "X7", age := "[65,+)"]
outcomes[variable == "X7", risk := "low"]

outcomes[variable == "X8", age := "[0,0.5)"]
outcomes[variable == "X8", risk := "high"]
outcomes[variable == "X9", age := "[0.5,5)"]
outcomes[variable == "X9", risk := "high"]
outcomes[variable == "X10", age := "[5,15)"]
outcomes[variable == "X10", risk := "high"]
outcomes[variable == "X11", age := "[15,45)"]
outcomes[variable == "X11", risk := "high"]
outcomes[variable == "X13", age := "[45,65)"]
outcomes[variable == "X13", risk := "high"]
outcomes[variable == "X14", age := "[65,+)"]
outcomes[variable == "X14", risk := "high"]




###### Annual illnessses #######

###### Annual non-death QALYS #######

###### Annual costs #######