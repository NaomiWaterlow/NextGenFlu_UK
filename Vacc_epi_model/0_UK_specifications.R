# UK model specifications
#set the file path to the main folder <- change here and all other should be read in. 
setwd("~/Documents/GitHub/NextGenFlu_UK/")
library(here)
library(fluEvidenceSynthesis)
library(tidyverse)


####### VARIABLES #####
location <- "UK"
posterior_sample_size <- 1000
set.seed(100)
target_scenarios <- c(1,2,3,4,5,6) # change to required
base_scenario_to_use <- 2 # for the economics
vacc_delivery_price <- 15.85#20#12#15.85 # for the economics. 
discount_rate <- 0.035
qaly_discount_rate <- 0.015
use_presampled <- F # this is always false for UK - as only have 1000 samples of each
save_samples <- F # this is always false for UK - as only have 1000 samples of each
change_susceptibility_switch <- "FIXED_REDUCTION" # whether to take account reduction of infection from previous year
name_run <- "FIXED_REDUCTION"
create_plots <- "Yes"

num_parameters_posteriors <- 9
transmisibility_location <- 5 # position in posterior of the transmissibility parameter
infection_delays <- c(0.8,1.8)
initial_infection_location <- 9

age_groups_model <- c(1, 5, 15, 25, 45, 65)
no_risk_groups <- 2
max_age <- 80
num_age_groups <- 7
end_first_year_vaccination <- as.Date("1996-09-01")
#THESE NEED EXTRACTING
high_risk <- c(0.021, 0.055, 0.098, 0.087, 0.092, 0.183, 0.45)
risk_ratios_input <- matrix(c(high_risk,0,0,0,0,0,0,0),
                            ncol = num_age_groups , byrow = T)
susceptibility_pattern <- c(6,6,6,7,7,7,8)
calculate_FOI <- "No" # this should always be No for the UK (as no inter-epidemic period)
num_years <- 13
years <- c(1995:2010)
# load in the inference results from the UK
load(here::here("UK_data","inference.results.2013.rda")) #inference.results.2013 
contact_ids_input <- as.matrix(inference.results.2013[[1]][[1]]$contact.ids[1,])
#THIS NEEDS EXTRACTING
data(polymod_uk)

relevant_polymod <- polymod_uk
age_group_labels <- data.frame(id =c(1:21), 
                               label =rep(c("Age0","Age1-4","Age5-14","Age15-24",
                                        "Age25-44","Age45-64", "Age65+"),3))
risk_group_labels <- data.frame(id =c(1:21), 
                               label =c(rep("Risk_group1",num_age_groups),
                               rep("Risk_group2",num_age_groups),
                               rep("Risk_group3",num_age_groups)))
vaccine_scenario_names <- c("Seasonal (2013)","Seasonal (2019)","Improved (minimal)","Improved (efficacy)","Improved (breadth)",
"Universal")


##### SOURCE AND RUN ######Í

# The vaccination scenario <- edit to get the required scenarios
source(here::here("Vacc_epi_model","1_0_vaccination_scenario_list_UK.R"))
#The list of epidemics in Kenya
source(here::here("Vacc_epi_model","2_0_epidemics_list_UK.R"))

# Run all the modelling!
source(here::here("Vacc_epi_model", "0_Main_NextGen.R"))
# Run all the economics!
source(here::here("Economics", "5_0_run_economics.R"))

