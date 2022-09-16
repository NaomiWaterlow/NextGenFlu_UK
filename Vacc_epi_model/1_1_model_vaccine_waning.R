gen_seeiir_ag_vacc_waning <- odin::odin({
  # Number of groups
  no_groups <- user()
  # INITIAL CONDITIONS
  # population_stratified size by age/risk group
  pop[] <- user()
  # Start vaccinated by age/risk group
  V0[] <- user()
  # R0[] <- user()
  RV0[] <- user()
  # Initial infection by age/risk group
  num_vac_start[] <- user()
  
  # MODEL PARAMETERS
  
  # Vaccine related variables 
  dates[] <- user()
  calendar[,] <- user()
  # waning of vaccine immunity
  omega <- user()
  # efficacy
  alpha[] <- user()
  
  # Vaccination. The rate is a step function that changes at each date according
  # to the passed calendar
  vI[] <- interpolate(dates, calendar, "constant")
  
  # Vaccination is given as a fraction vaccination, here we scale it to 
  # a rate
  #sumN[] <- if (vI[i]>0) (S[i]+E1[i]+E2[i]+I1[i]+I2[i]+R[i]) else 0
  v[] <-vI[i] #if (sumN[i]>0) vI[i]*pop[i] else 0
  
  # THE DERIVATIVES OF THE SEEIIR MODEL
  deriv(S[])  <- + omega*Sv[i] + omega*Rv[i] - v[i] * S[i] 
  deriv(Sv[])  <- - omega*Sv[i] + v[i] * (1-alpha[i]) * S[i] 
  deriv(Rv[])  <- - omega*Rv[i]  + v[i] * (alpha[i] * S[i])
  deriv(VT[]) <- vI[i]*pop[i]
  
  # Initial value of the variables
  initial(S[1:no_groups]) <- pop[i]*(1-V0[i])
  initial(Sv[1:no_groups]) <- (pop[i]*V0[i])*(1-RV0[i])
  initial(Rv[1:no_groups]) <- (pop[i]*V0[i])*(RV0[i])
  initial(VT[1:no_groups]) <- num_vac_start[i]
  
  # Set dimension of all variables/parameters
  dim(dates) <- user()
  dim(calendar) <- user()
  dim(pop) <- no_groups
  dim(V0) <- no_groups
  dim(RV0) <- no_groups
  dim(num_vac_start) <- no_groups
  dim(v) <- no_groups
  dim(vI) <- no_groups
  dim(alpha) <- no_groups
  
  dim(S) <- no_groups
  dim(Sv) <- no_groups
  dim(Rv) <- no_groups
  dim(VT) <- no_groups
  
})

gen_seeiir_ag_vacc_waning_NH <- odin::odin({
  # Number of groups
  no_groups <- user()
  
  # INITIAL CONDITIONS
  # population_stratified size by age/risk group
  pop[] <- user()
  # Initial proportion in each compartment
  allS[] <- user()
  allSv[] <- user()
  allRv[] <- user()
  num_vac_start[] <- user()
  # MODEL PARAMETERS
  # Vaccine related variables 
  dates[] <- user()
  calendar[,] <- user()
  # waning of vaccine immunity
  omega <- user()
  
  # efficacy
  alpha[] <- user()
  
  # Vaccination. The rate is a step function that changes at each date according
  # to the passed calendar
  vI[] <- interpolate(dates, calendar, "constant")
  # Vaccination is given as a fraction vaccination, here we scale it to 
  # a rate
  #sumN[] <- if (vI[i]>0) (S[i]+E1[i]+E2[i]+I1[i]+I2[i]+R[i]) else 0
  v[] <- vI[i]#if (sumN[i]>0) vI[i]*sumN[i]/pop[i] else 0
  
  # THE DERIVATIVES OF THE SEEIIR MODEL
  # Derivatives of the not vaccinated group
  deriv(S[])  <- + omega*Sv[i] + omega*Rv[i] - v[i] * S[i] 
  # Derivatives vaccination group
  deriv(Sv[])  <- - omega*Sv[i]  + v[i] * (1-alpha[i]) * S[i] 
  deriv(Rv[])  <- - omega*Rv[i]  + v[i] * (alpha[i] * S[i])
  deriv(VT[]) <- vI[i]*pop[i]
  
  # Initial value of the variables
  initial(S[1:no_groups]) <- allS[i]
  
  initial(Sv[1:no_groups]) <- allSv[i]
  initial(Rv[1:no_groups]) <- allRv[i]
  initial(VT[1:no_groups]) <- num_vac_start[i]
  
  # Set dimension of all variables/parameters
  dim(dates) <- user()
  dim(calendar) <- user()
  dim(pop) <- no_groups
  dim(allS) <- no_groups
  dim(allSv) <- no_groups
  dim(allRv) <- no_groups
  dim(num_vac_start) <- no_groups
  dim(v) <- no_groups
  dim(vI) <- no_groups
  # dim(sumN) <- no_groups  
  dim(alpha) <- no_groups
  
  dim(S) <- no_groups
  dim(Sv) <- no_groups
  dim(Rv) <- no_groups
  dim(VT) <- no_groups
})

infectionODEs <- function(population_stratified, initial_infected, calendar_input, contacts_matrixformat,
                          susceptibility, transmissibility, infection_delays, interval 
                          ,waning_rate, initial_vaccinated_prop, initial_Rv_prop,
                          year_to_run, efficacy_NH
) {
  
  # Extract the date used from the vaccine calendar
  begin_date <- as.Date(paste0(year_to_run, "-03-01"))
  end_date <- as.Date(paste0(year_to_run, "-09-01"))
  t <- as.numeric(seq(begin_date, end_date, interval))
  
  no_groups <- length(population_stratified)
  
  # adds a new top row, with the start date od simulation and 0 vaccination
  calendar_input$calendar <- calendar_input$calendar[c(nrow(calendar_input$calendar),1:nrow(calendar_input$calendar)),]
  calendar_input$dates <- as.numeric(c(t[1], calendar_input$dates))
  if(calendar_input$dates[1] == calendar_input$dates[2]){calendar_input$dates <- calendar_input$dates[-2]
  calendar_input$calendar <- calendar_input$calendar[-2,]}
  # Run the model over the first 6 months
  # age the vaccinated population_stratified by 1 year. 
  initial_vaccinated_prop <- age_population_1year(population_stratified, old_proportions=initial_vaccinated_prop)
  initial_Rv_prop <- age_population_1year(population_stratified, old_proportions = initial_Rv_prop)
  #Assume that all R become susceptible again at the end of the year.
  initial_R_prop <- rep(0,no_groups)
  
  mod <- gen_seeiir_ag_vacc_waning$new(no_groups = no_groups,
                                       pop = population_stratified, V0 = initial_vaccinated_prop,
                                       RV0 = initial_Rv_prop,
                                       alpha = calendar_input$efficacy[1:no_groups],
                                       omega = waning_rate,
                                       dates = calendar_input$dates,
                                       calendar = calendar_input$calendar[,1:no_groups],
                                       num_vac_start = rep(0,num_age_groups*3)
  )
  
  # run the model 
  y <- mod$run(t, hmax = NULL, method = "euler", hini = 0.25, atol = 1)
  y_tail <- tail(y,1)
  
  # Change the dates to NH 
  begin_date <- as.Date(paste0(year_to_run, "-09-01"))
  end_date <- as.Date(paste0(as.character(year_to_run+1), "-03-01"))
  t <- as.numeric(seq(begin_date, end_date, interval))
  #Update the vaccination to NH
  keepers <- which(calendar_input$dates>= begin_date )
  calendar_input$dates <- calendar_input$dates[keepers] 
  
  # choose either the last date in the vaccine calendar or the last time in the times
  if(!is.na(calendar_input$dates[1])){temp <- calendar_input$dates} else {temp <- tail(t,1)}
  #specify the inputs dates as the time to start and the last date
  input_dates <- c(t[1],temp)
  # remove if it's the same dates
  if(input_dates[1] == input_dates[2]){
    input_dates <- input_dates[-1]
    calendar_input$calendar <- calendar_input$calendar[c(keepers),]
    # save back into the calendar
    calendar_input$dates <- input_dates
    calendar_input$efficacy <- efficacy_NH
  } else{
    # save back into the calendar
    calendar_input$dates <- input_dates
    calendar_input$efficacy <- efficacy_NH
    if(length(input_dates)>2){
      # add the last row (which is always 0) to the front, for the extra one.
      calendar_input$calendar <- calendar_input$calendar[c(nrow(calendar_input$calendar),keepers),]
    } else{
      calendar_input$calendar =matrix(rep(0,num_age_groups*3*length(input_dates)), ncol = num_age_groups*3)
      calendar_input$efficacy <- rep(0,num_age_groups*3)
    } 
  }
  # carry on the runi

  mod2 <- gen_seeiir_ag_vacc_waning_NH$new(no_groups = no_groups,
                                           pop = population_stratified,
                                           allS = y_tail[2:((2+no_groups)-1)],
                                           allSv = y_tail[(2+no_groups):((2+(no_groups*2))-1)],
                                           allRv = y_tail[(2+(2*no_groups)):((2+(no_groups*3))-1)],
                                           num_vac_start = y_tail[(2+(3*no_groups)):((2+(no_groups*4))-1)],
                                           alpha = calendar_input$efficacy[1:no_groups],
                                           omega = waning_rate,
                                           dates =  calendar_input$dates,
                                           calendar = calendar_input$calendar[,1:no_groups]
                                           #    ageing =ageing
  )
  
  y2 <- mod2$run(t, hmax = NULL, method = "euler", hini = 0.25, atol = 1)
  y <- rbind(y, y2[-c(1),])
  y <- data.table(y)
  colnames(y) <- gsub(pattern = "\\[", replacement="", x =colnames(y))
  colnames(y) <- gsub(pattern = "\\]", replacement="", x =colnames(y))

  
  # caclulate the proportion vaccinated in each age group
  for(agp in 1:(num_age_groups*3)){
    prop_v_label <- paste0("prop_v", agp)
    S_label <- paste0("S", agp)
    y[, eval(prop_v_label) := (population_stratified[agp]-(get(S_label)))/population_stratified[agp]]
  }
  # calculate the proportion in the Rv compartment in each age group
  for(agp in 1:(num_age_groups*3)){
    prop_Rv_label <- paste0("prop_Rv", agp)
    Rv_label <- paste0("Rv", agp)
    Sv_label <- paste0("Sv", agp)
    y[, eval(prop_Rv_label) := get(Rv_label)/(get(Sv_label) + get(Rv_label))]
  }
  # calculate the total number vaccinated in each age group
  for(agp in 1:(num_age_groups*3)){
    Vaccinated_label <- paste0("Vaccinated",agp)
    VT_label <- paste0("VT",agp)
    y[, eval(Vaccinated_label) := get(VT_label)]
  }
  
  
  y[is.na(y)] <- 0
  
  return(y[,((3*num_age_groups*4)+2):ncol(y)])
}


# Defining the incidence function
#TODO put the relevant function inputs in
incidence_function <- function(demography_input, 
                               calendar_input,
                               sampled_contact_ids,
                               sampled_parameters,
                               waning_rate,
                               vaccination_ratio_input,
                               year_to_run,efficacy_NH) {
  
  time_column = "Time"
  # create the contact matrix
  contacts_matrixformat <- contact_matrix(as.matrix(relevant_polymod),
                             demography_input, age_groups_model)

  age_group_sizes <- stratify_by_age(demography_input, age_groups_model)
  population_stratified <- stratify_by_risk(age_group_sizes, risk_ratios_input)

  initial_infected <- rep(0, num_age_groups)
  
  initial_infected <- stratify_by_risk(initial_infected, risk_ratios_input)
  
  if(is.null(names(vaccination_ratio_input))){
    pv_input <- vaccination_ratio_input
    pRv_input <- vaccination_ratio_input
  } else {
    pv_input <- vaccination_ratio_input[grep(pattern = "prop_v", names(vaccination_ratio_input))]
    pRv_input <-  vaccination_ratio_input[grep(pattern = "prop_Rv", names(vaccination_ratio_input))]
  }
  
  susceptibility <- rep(1,num_age_groups)
  
  for(sus_i in 1:num_age_groups){
    susceptibility[sus_i] <-  sampled_parameters[susceptibility_pattern[sus_i]]
  }
  infections_out <- infectionODEs(population_stratified, initial_infected, calendar_input, contacts_matrixformat,
                susceptibility = susceptibility,
                transmissibility = sampled_parameters[transmisibility_location],
                infection_delays = infection_delays, interval = 1,
                waning_rate = waning_rate,
                initial_vaccinated_prop = pv_input,
                initial_Rv_prop = pRv_input,
                year_to_run = year_to_run,
                efficacy_NH = efficacy_NH
  )
  
  return(infections_out)


}


# this function loops over the posterior samples and creates runs for each. 
vacc_model_1 <- function(demography_input, 
                         calendar_input,
                         waning_rate,
                         vaccination_ratio_input,
                         year_to_run,
                         efficacy_NH) 
{

  vacc_model_out <- incidence_function(demography_input = demography_input,
                                       calendar_input = calendar_input,
                     sampled_parameters = rep(0,num_parameters_posteriors), 
                     waning_rate = waning_rate,
                     vaccination_ratio_input = vaccination_ratio_input,
                     year_to_run = year_to_run,
                     efficacy_NH = efficacy_NH)
  
  
  
      return(vacc_model_out)
    }




#Function that updates the coverage
change_coverage <- function(data, final_uptake) {

  sums <- data[nrow(data),]
  # If final uptake is zero in a group then we need to make some kind of assumption on uptake rate over time
  if (any(sums == 0)) {
    warning("No prior information on uptake rate. Using constant uptake rate")
    #browser()
    col <- which(sums == 0)
    data[,col] <- seq(0, (nrow(data)-1))
    sums <- data[nrow(data),]    
  }
  for(i in 1:nrow(data)) {
    data[i,] <- data[i,]*final_uptake/sums
  }
  data
}

# age the population_stratified by 1 year
age_population_1year <- function(population_stratified, old_proportions){

  # - proportion of the age group that will move into the next age group (proportion_ageing)
  proportion_ageing <- c()
  # - relative population_stratified size of the age group (pop_weighting)
  pop_weighting <- c()
  # storage 
  new_proportions_all <- c()
  # for each risk group 
# browser()
  for(l in 1:3){

    # - old proportions
    old_proportions_sub <- old_proportions[(l-1)*num_age_groups+(1:num_age_groups)]
    population_stratified_sub <- population_stratified[l*(1:num_age_groups)]
    # vector for storing new proprotions
    new_proportions <- c()
    # for each age group

    for(k in 1:(length(age_groups_model)+1)){

      # specify end of each age group
      if(k == (length(age_groups_model)+1)){
        age_grp_end <-max_age } else {
          age_grp_end <- age_groups_model[k]} 
      # specify start of each ate group and calculate population_stratified weightings
      if(k==1){
        age_grp_start <- 0
        # length of age group
        age_grp_length <- age_grp_end-age_grp_start
        #calculate relative population_stratified weighting - assumign in bottom same proprtion born
        pop_weighting_temp <- 1/age_grp_length
        #age_grp_length_prev
        age_grp_length_prev <- 1
      } else {
        age_grp_start <- age_groups_model[k-1]
        # length of age group
        age_grp_length <- age_grp_end-age_grp_start
        # calculate relative population_stratified sizes (by length of age group)
        pop_weighting_temp <-c(age_grp_length_prev/age_grp_length)
        # save the previous one for next time
        age_grp_length_prev <- age_grp_length
      }

      # calculate the proportion ageing
      if(l==1){
        proportion_ageing <- c(proportion_ageing,(1/age_grp_length))
        # calculate the relative population_stratified sizese
        pop_weighting <-c(pop_weighting,pop_weighting_temp) 
      }
      
      if(k==1){
        
        # in from the previous age group - assume the proportion is 0
        new_top <- ((pop_weighting[k]*0) + 
                      # those already in the current age group
                      (old_proportions_sub[k]*1 )- 
                      # those leaving the age group
                      (old_proportions_sub[k]* proportion_ageing[k])) 
        
        # denominator: orginal pop, add those coming in, minus those leaving
        new_bottom <- (1) + (pop_weighting[k]*1) - 
          (1*proportion_ageing[k])
        
        new_prop_temp <- new_top/new_bottom
      }else{ 
        # in from the previous age group
        new_top <- ((pop_weighting[k]*old_proportions_sub[k-1]*proportion_ageing[k-1]) + 
                      # those already in the current age group
                      (old_proportions_sub[k]*1 )- 
                      # those leaving the age group
                      (old_proportions_sub[k]*proportion_ageing[k] )) 
        
        # denominator: orginal pop, add those coming in, minus those leaving (proportions)
        new_bottom <- 1 + (pop_weighting[k]*proportion_ageing[k-1]) - 
          (1*proportion_ageing[k])
        
        new_prop_temp <- new_top/new_bottom
      }
      new_proportions <- c(new_proportions, new_prop_temp)
    
    }
    new_proportions_all <- c(new_proportions_all, new_proportions)
  }

  new_proportions_all[is.nan(new_proportions_all)] <- 0
  new_proportions_all[is.na(new_proportions_all)] <- 0
  if(any(new_proportions_all >1)){browser()}
  if(any(new_proportions_all >1)){stop("proportion in ageing is bigger than one!")}
  return(new_proportions_all)
  
}


