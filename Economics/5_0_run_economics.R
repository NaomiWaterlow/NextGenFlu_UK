### run the economics sections

# load libraries
library(qs)
# load in saved epi model output file if want to run econ alone
load(file = here::here("UK_output", "total_cases_time.Rdata"))

# Calculate the annual non-death Qalys lost and costs
source(here::here("Economics","5_1_outcomes_costs.R"))

# Calculate the annual death QALYS lost
source(here::here("Economics","5_2_deaths.R"))

# Discounting and combining
source(here::here("Economics","5_3_discounting.R"))

# Plots etc.
source(here::here("Economics","5_4_econ_plots.R"))
