
# the incidence function
incidence_function_2 <- function(demography_input, 
                               parameters,
                               calendar_input,
                               contact_ids_sample,
                               waning_rate,
                               vaccination_ratio_input,
                               begin_date, 
                               end_date ,  
                               year_to_run, 
                               efficacy_now  , 
                               efficacy_next ,
                               efficacy_next2 , 
                               previous_summary ) {
  


  contacts_matrixformat <- contact_matrix(as.matrix(relevant_polymod[contact_ids_sample,]),
                                          demography_input, age_groups_model)
  
  age_group_sizes <- stratify_by_age(demography_input, age_groups_model)
  
  population_stratified <- stratify_by_risk(age_group_sizes, risk_ratios_input)

  initial_infected <- rep(10^parameters[initial_infection_location], num_age_groups)
  initial_infected <- stratify_by_risk(initial_infected, risk_ratios_input)
  
  susceptibility <- rep(1,num_age_groups)
  
  for(sus_i in 1:num_age_groups){
    susceptibility[sus_i] <-  parameters[susceptibility_pattern[sus_i]]
  }


  infectionODEs_epidemic_yearcross(population_stratified = population_stratified, 
                                   initial_infected = initial_infected, 
                                   calendar_input = calendar_input,
                                   contacts_matrixformat,
                                   susceptibility = susceptibility, 
                                   transmissibility = parameters[transmisibility_location],
                                   infection_delays = infection_delays, interval = 1,
                                   waning_rate = waning_rate,
                                   initial_vaccinated_prop = unlist(vaccination_ratio_input[[1]]),
                                   initial_Rv_prop = unlist(vaccination_ratio_input[[2]]),
                                   initial_R_prop = unlist(vaccination_ratio_input[[3]]), 
                                   begin_date = begin_date, 
                                   end_date = end_date,  
                                   year_to_run = year_to_run, 
                                   efficacy_now = efficacy_now, 
                                   efficacy_next = efficacy_next,
                                   efficacy_next2 = efficacy_next2, 
                                   previous_summary = previous_summary
  )
  
  
}


# function to set up infection model - adapted from Js cocd
epidemic_scenarios_yearcross <- function (demography_input, 
                                          calendar_input,
                                          contact_ids_input,
                                          parameters,
                                          vaccination_ratio_input,
                                          begin_date, 
                                          end_date,
                                          year_to_run,
                                          efficacy_now, efficacy_next,
                                          efficacy_next2,
                                          previous_summary,
                                          ..., verbose = T) 
{
  
  # combine the samples of parameters and contacts to pass through together
  pc <- cbind(parameters, contact_ids_input)
  sample_counter <<- 0
  # loop over each sample

  epi_out <- apply(pc, 1,function(pars_contacts) 
    incidence_function_2(demography_input = demography_input, 
                       parameters = pars_contacts[1:ncol(parameters)],
                       calendar_input = calendar_input,
                       contact_ids_sample = pars_contacts[(ncol(parameters)+1):ncol(pc)],
                       waning_rate = waning_rate,
                       vaccination_ratio_input = vaccination_ratio_input,
                       begin_date = begin_date, 
                       end_date = end_date,  
                       year_to_run = year_to_run, 
                       efficacy_now = efficacy_now, 
                       efficacy_next = efficacy_next,
                       efficacy_next2 = efficacy_next2, 
                       previous_summary = previous_summary))

  
  return(epi_out)
}



infectionODEs_epidemic_yearcross <- function(population_stratified,
                                             initial_infected, 
                                             calendar_input,
                                             contacts_matrixformat,
                                             susceptibility, transmissibility, infection_delays, interval 
                                             ,waning_rate, initial_vaccinated_prop, initial_Rv_prop, initial_R_prop,
                                             begin_date, end_date,
                                             year_to_run,
                                             efficacy_now, efficacy_next, efficacy_next2, 
                                             previous_summary, ...
) {

  
  if(change_susceptibility_switch == "POP_ADD_WANING"){
    
    sample_counter <<- sample_counter+1
    # check that a previous epdiemic existed, else 0
    
    if(!all(is.na(previous_summary))){

      previous_summary[, actual_change := prop_change]
      susc_change = unlist(previous_summary[sample==unique(previous_summary$sample)[sample_counter],"actual_change"])
    } else {susc_change = 0}
    # work out the appropiate susceptibility
    
    for(subsetter in 1:(length(susc_change)/num_age_groups)){
      
      add_amount <-susc_change[(1+(num_age_groups*(subsetter-1))):(num_age_groups*subsetter)] 
      add_amount[which(is.na(add_amount))] <- 0
      susceptibility = susceptibility + add_amount
    }
  }
  
  if(change_susceptibility_switch == "FIXED_REDUCTION" & 
     reduce_susceptibility == T){
    susceptibility = susceptibility*1.2
  }
  # alter the exxesive calues to be within reasn
  susceptibility[which(is.na(susceptibility))] <- 0
  
  if(any(susceptibility >= 1)){ 
    susceptibility[which(susceptibility>1)] <- 1}
  if(any(susceptibility < 0)){ 
    susceptibility[which(susceptibility<0)] <- 0
  }
  
  # Extract the date used from the vaccine calendar
  # so check months compared to the NH vs SH runs
  # store end date for later use
  
  begin_date2 <- begin_date
  if(begin_date +243 < end_date){
    end_date2 <- end_date} else {end_date2 <- begin_date+243}
  if (month(begin_date) >= 3 & month(begin_date) <9){
    end_date <- as.Date(paste0(year_to_run, "-09-01"))
    start_h <- "SH"
  } else {
    end_date <- as.Date(paste0(as.character(year_to_run+1), "-03-01"))
    start_h <- "NH"
  }
  

  # define model timings
  t <- as.numeric(seq(begin_date, end_date, interval))
  # define age group inputs
  no_groups <- length(population_stratified)
  # Contacts matrix only covers one set of age groups, here we "repeat" it to also cover 
  # risk groups
  new_cij <- matrix(rep(0,no_groups*no_groups), nrow = no_groups)
  for (k in 1:3) {
    for (l in 1:3) {
      lk <- (k - 1)*num_age_groups + 1
      ll <- (l - 1)*num_age_groups + 1
      new_cij[lk:(lk + num_age_groups - 1), ll:(ll + num_age_groups - 1)] <- contacts_matrixformat
    }
  }

  # extract the relevant dates from the calendar (i.e. exclude those before start time)
  #Update the vaccination calendar
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
    calendar_input$efficacy <- efficacy_now
  } else{
    # save back into the calendar
    calendar_input$dates <- input_dates
    calendar_input$efficacy <- efficacy_now
    if(length(input_dates)>2){
      # add the last row (which is always 0) to the front, for the extra one.
      #browser()
      calendar_input$calendar <- calendar_input$calendar[c(nrow(calendar_input$calendar),keepers),]
    } else{
      calendar_input$calendar = matrix(rep(0,num_age_groups*3*length(input_dates)), ncol = num_age_groups*3)
      calendar_input$efficacy <- rep(0,num_age_groups*3)
    } 
  }
  #Assume that all R become susceptible again at the start of each posterior
  initial_R_prop <- rep(0,no_groups)
  # specify the model
 # browser()
  mod <- gen_seeiir_ag_vacc_waning$new(no_groups = no_groups,
                                       cij = new_cij,
                                       trans = transmissibility,
                                       pop = population_stratified,
                                       I0 = initial_infected,
                                       V0 = initial_vaccinated_prop,
                                       R0 = initial_R_prop,
                                       RV0 = initial_Rv_prop,
                                       susc = rep(susceptibility,3),
                                       alpha = calendar_input$efficacy[1:no_groups],
                                       omega = waning_rate,
                                       dates = calendar_input$dates,
                                       calendar = matrix(calendar_input$calendar, ncol = num_age_groups*3),
                                       gamma1 = 2/infection_delays[1],
                                       gamma2 = 2/infection_delays[2], 
                                       num_vac_start = rep(0,no_groups) # don't need to be tracked
                                       
  )
  
  #run the model
  y <- mod$run(t, hmax = NULL, method = "euler", hini = 0.25, atol = 1)
  # extract last row for inputting into next model 
  y_tail <- c(tail(y, 1))
  
  # Change the dates and efficacy to the other hemisphere, and specify whether the time
  # frame extends beyond into the nexxt hemisphere
  if(start_h == "NH"){
    if(month(begin_date)<9){
      begin_date <- as.Date(paste0(year(begin_date), "-03-01"))
    } else{
      begin_date <- as.Date(paste0(year(begin_date)+1, "-03-01"))}
    if(end_date2>as.Date(paste0(year(begin_date), "-09-01"))){
      double_trouble <- T 
      t <- as.numeric(seq(begin_date, as.Date(paste0(year(begin_date), "-09-01")), interval))
    }else{
      double_trouble <- F
      t <- as.numeric(seq(begin_date, end_date2, interval))
    }
  } else if(start_h == "SH"){
    begin_date <- as.Date(paste0(year_to_run, "-09-01"))
    if(end_date2>as.Date(paste0(year_to_run+1, "-03-01"))){
      double_trouble <- T 
      t <- as.numeric(seq(begin_date, as.Date(paste0(year_to_run+1, "-03-01")), interval))
    }else{
      double_trouble <- F
      t <- as.numeric(seq(begin_date, end_date2, interval))
    }
  }

  #Update the vaccination calendar
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
    calendar_input$efficacy <- efficacy_next
  } else{
    # save back into the calendar
    calendar_input$dates <- input_dates
    calendar_input$efficacy <- efficacy_next
    if(length(input_dates)>2){
      # add the last row (which is always 0) to the front, for the extra one.
#browser()
      calendar_input$calendar <- calendar_input$calendar[c(nrow(calendar_input$calendar),keepers),]
    } else{
      calendar_input$calendar =matrix(rep(0,num_age_groups*3*length(input_dates)), ncol = num_age_groups*3)
      calendar_input$efficacy <- rep(0,num_age_groups*3)
    } 
  }
  
  # if one more NH season:   
  if(start_h == "NH" & double_trouble == F){
    
    # extract the proprtion of each age group in each compartment
    age_proportions <- data.frame(matrix(nrow = 12, ncol = no_groups))
    for(test in 1:no_groups){
      
      temp <-  y_tail[c(2,
                        2+no_groups,2+(no_groups*2),
                        2+(no_groups*3),2+(no_groups*4),
                        2+(no_groups*5),
                        2+(no_groups*7),2+(no_groups*8),
                        2+(no_groups*9),2+(no_groups*10),
                        2+(no_groups*11),2+(no_groups*12))+(test-1)]/
        sum(y_tail[c(2,
                     2+no_groups,2+(no_groups*2),
                     2+(no_groups*3),2+(no_groups*4),
                     2+(no_groups*5),
                     2+(no_groups*7),2+(no_groups*8),
                     2+(no_groups*9),2+(no_groups*10),
                     2+(no_groups*11),2+(no_groups*12))+(test-1)])  
      age_proportions[,test] <- temp
      age_proportions[which(is.nan(age_proportions[,test])),test] <- 0
    }

    # age the proportions
    for(gap in 1:nrow(age_proportions)){

      age_proportions[gap,] <- age_population_1year(population_stratified,unlist(age_proportions[gap,]))
    }
    
    # all births start susceptible
    age_proportions[1,1] <- 1
   # browser()
    # specify the new model
  #  browser()
    mod2 <- gen_seeiir_ag_vacc_waning_yearcross$new(no_groups = no_groups, cij = new_cij, trans = transmissibility,
                                                    pop = population_stratified,
                                                    allS = c(unlist(age_proportions[1,])),
                                                    allE1 = c(unlist(age_proportions[2,])),
                                                    allE2 = c(unlist(age_proportions[3,])),
                                                    allI1 = c(unlist(age_proportions[4,])),
                                                    allI2 = c(unlist(age_proportions[5,])),
                                                    allR = c(unlist(age_proportions[6,])),
                                                    allCI =  y_tail[(2+(6*no_groups)):(1+(7*no_groups))],
                                                    allSv = c(unlist(age_proportions[7,])),
                                                    allEv1 = c(unlist(age_proportions[8,])),
                                                    allEv2 = c(unlist(age_proportions[9,])),
                                                    allIv1 = c(unlist(age_proportions[10,])),
                                                    allIv2 = c(unlist(age_proportions[11,])),
                                                    allRv = c(unlist(age_proportions[12,])),
                                                    susc = rep(susceptibility,3),
                                                    alpha = calendar_input$efficacy[1:no_groups],
                                                    omega = waning_rate,
                                                    dates = calendar_input$dates,
                                                    calendar = matrix(calendar_input$calendar, ncol = num_age_groups*3),
                                                    gamma1 = 2/infection_delays[1],
                                                    gamma2 = 2/infection_delays[2], 
                                                    num_vac_start = rep(0,no_groups) # no need to track
                                                    
    )
    # run the new model
    y2 <- mod2$run(t, hmax = NULL, method = "euler", hini = 0.25, atol = 1)
    # combine the output with previous model 
    y <- rbind(y, y2[-1,])
    
    # Else if there is one more southern hemisphere season ( no need to age)
  } else if (start_h == "SH" & double_trouble == F){
   # browser()
    # specify new model inputs

    mod2 <- gen_seeiir_ag_vacc_waning_NH$new(no_groups = no_groups, cij = new_cij, trans = transmissibility,
                                             pop = population_stratified,
                                             allS = y_tail[2:(1+no_groups)],
                                             allE1 = y_tail[(2+no_groups):(1+(2*no_groups))],
                                             allE2 = y_tail[(2+(2*no_groups)):(1+(3*no_groups))],
                                             allI1 = y_tail[(2+(3*no_groups)):(1+(4*no_groups))],
                                             allI2 = y_tail[(2+(4*no_groups)):(1+(5*no_groups))],
                                             allR = y_tail[(2+(5*no_groups)):(1+(6*no_groups))],
                                             allCI = y_tail[(2+(6*no_groups)):(1+(7*no_groups))],
                                             allSv = y_tail[(2+(7*no_groups)):(1+(8*no_groups))],
                                             allEv1 = y_tail[(2+(8*no_groups)):(1+(9*no_groups))],
                                             allEv2 = y_tail[(2+(9*no_groups)):(1+(10*no_groups))],
                                             allIv1 = y_tail[(2+(10*no_groups)):(1+(11*no_groups))],
                                             allIv2 = y_tail[(2+(11*no_groups)):(1+(12*no_groups))],
                                             allRv = y_tail[(2+(12*no_groups)):(1+(13*no_groups))],
                                             susc = rep(susceptibility,3),
                                             alpha = calendar_input$efficacy[1:no_groups],
                                             omega = waning_rate,
                                             dates = calendar_input$dates,
                                             calendar =  matrix(calendar_input$calendar, ncol = num_age_groups*3),
                                             gamma1 = 2/infection_delays[1], gamma2 = 2/infection_delays[2], 
                                             num_vac_start = rep(0,no_groups) # no need to track
    )
    # run new model
    y2 <- mod2$run(t, hmax = NULL, method = "euler", hini = 0.25, atol = 1)
    # combine the output with the previous
    y <- rbind(y, y2[-1,])
    
    # if next is SH followed by another Nothern hemisphere
  } else if (start_h == "SH" & double_trouble == T){
   # browser()
    # specify the new run
    mod2 <- gen_seeiir_ag_vacc_waning_NH$new(no_groups = no_groups, cij = new_cij, trans = transmissibility,
                                             pop = population_stratified,
                                             allS = y_tail[2:(1+no_groups)],
                                             allE1 = y_tail[(2+no_groups):(1+(2*no_groups))],
                                             allE2 = y_tail[(2+(2*no_groups)):(1+(3*no_groups))],
                                             allI1 = y_tail[(2+(3*no_groups)):(1+(4*no_groups))],
                                             allI2 = y_tail[(2+(4*no_groups)):(1+(5*no_groups))],
                                             allR = y_tail[(2+(5*no_groups)):(1+(6*no_groups))],
                                             allCI = y_tail[(2+(6*no_groups)):(1+(7*no_groups))],
                                             allSv = y_tail[(2+(7*no_groups)):(1+(8*no_groups))],
                                             allEv1 = y_tail[(2+(8*no_groups)):(1+(9*no_groups))],
                                             allEv2 = y_tail[(2+(9*no_groups)):(1+(10*no_groups))],
                                             allIv1 = y_tail[(2+(10*no_groups)):(1+(11*no_groups))],
                                             allIv2 = y_tail[(2+(11*no_groups)):(1+(12*no_groups))],
                                             allRv = y_tail[(2+(12*no_groups)):(1+(13*no_groups))],
                                             susc = rep(susceptibility,3),
                                             alpha = calendar_input$efficacy[1:no_groups],
                                             omega = waning_rate,
                                             dates = calendar_input$dates,
                                             calendar = matrix(calendar_input$calendar, ncol = num_age_groups*3),
                                             gamma1 = 2/infection_delays[1], gamma2 = 2/infection_delays[2], 
                                             num_vac_start = rep(0,no_groups) # no need to track
    )
    # run the new model
    y2 <- mod2$run(t, hmax = NULL, method = "euler", hini = 0.25, atol = 1)
    # extract end for later yse
    y2_tail <- tail(y2,1)
    # add to previous output
    y <- rbind(y, y2[-1,])
    

    # extract the proprtion of each age group in each compartment
    age_proportions <- data.frame(matrix(nrow = 12, ncol = no_groups))
    for(test in 1:no_groups){
      
      temp <-  y2_tail[c(2,
                         2+no_groups,2+(no_groups*2),
                         2+(no_groups*3),2+(no_groups*4),
                         2+(no_groups*5),
                         2+(no_groups*7),2+(no_groups*8),
                         2+(no_groups*9),2+(no_groups*10),
                         2+(no_groups*11),2+(no_groups*12))+(test-1)]/
        sum(y2_tail[c(2,
                      2+no_groups,2+(no_groups*2),
                      2+(no_groups*3),2+(no_groups*4),
                      2+(no_groups*5),
                      2+(no_groups*7),2+(no_groups*8),
                      2+(no_groups*9),2+(no_groups*10),
                      2+(no_groups*11),2+(no_groups*12))+(test-1)])  
      
      
      age_proportions[,test] <- temp
      age_proportions[which(is.nan(age_proportions[,test])),test] <- 0
      
    }

    # age the proportions
    for(gap in 1:nrow(age_proportions)){
      # this is ageing - input is a vetor of the proportions in a compartment (e.g. S) by age
      age_proportions[gap,] <- age_population_1year(population_stratified,unlist(age_proportions[gap,]))
    }
    
    #all born into S compartment
    for(tst in 1:no_risk_groups){
      age_proportions[1,1+(num_age_groups*(tst-1))] <- 1
    }
    
    # update the vaccination calendar etc.
    efficacy_now <- efficacy_next2
    begin_date <- as.Date(paste0(year_to_run+1, "-03-01"))
    t <- as.numeric(seq(begin_date, end_date2, interval))
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
      calendar_input$efficacy <- efficacy_now
    } else{
      # save back into the calendar
      calendar_input$dates <- input_dates
      calendar_input$efficacy <- efficacy_now
      if(length(input_dates)>2){
        # add the last row (which is always 0) to the front, for the extra one.
#browser()
        calendar_input$calendar <- calendar_input$calendar[c(nrow(calendar_input$calendar),keepers),]
      } else{
        calendar_input$calendar =matrix(rep(0,num_age_groups*3*length(input_dates)), ncol = num_age_groups*3)
        calendar_input$efficacy <- rep(0,num_age_groups*3)
      } 
    }
    
    
    # specify the enw model
    mod3 <- gen_seeiir_ag_vacc_waning_yearcross$new(no_groups = no_groups, cij = new_cij, trans = transmissibility,
                                                    pop = population_stratified,
                                                    allS = c(unlist(age_proportions[1,])),
                                                    allE1 = c(unlist(age_proportions[2,])),
                                                    allE2 = c(unlist(age_proportions[3,])),
                                                    allI1 = c(unlist(age_proportions[4,])),
                                                    allI2 = c(unlist(age_proportions[5,])),
                                                    allR = c(unlist(age_proportions[6,])),
                                                    allCI =  y2_tail[(2+(6*no_groups)):(1+(7*no_groups))],
                                                    allSv = c(unlist(age_proportions[7,])),
                                                    allEv1 = c(unlist(age_proportions[8,])),
                                                    allEv2 = c(unlist(age_proportions[9,])),
                                                    allIv1 = c(unlist(age_proportions[10,])),
                                                    allIv2 = c(unlist(age_proportions[11,])),
                                                    allRv = c(unlist(age_proportions[12,])),
                                                    susc = rep(susceptibility,3),
                                                    alpha = calendar_input$efficacy[1:no_groups],
                                                    omega = waning_rate,
                                                    dates = calendar_input$dates,
                                                    calendar = matrix(calendar_input$calendar, ncol = num_age_groups*3),
                                                    gamma1 = 2/infection_delays[1],
                                                    gamma2 = 2/infection_delays[2], 
                                                    num_vac_start = rep(0,no_groups) # no need to track
                                                    
    )
    # run the new model 
    y3 <- mod3$run(t, hmax = NULL, method = "euler", hini = 0.25, atol = 1)
    # combine with previous outputs
    y <- rbind(y, y3[-1,])
    
    
    # if there is a NH season followed by an SH season    
  } else if(start_h == "NH" & double_trouble == T){
    
    # extract the proprtion of each age group in each compartment
    age_proportions <- data.frame(matrix(nrow = 12, ncol = no_groups))
    for(test in 1:no_groups){
      
      temp <-  y_tail[c(2,
                        2+no_groups,2+(no_groups*2),
                        2+(no_groups*3),2+(no_groups*4),
                        2+(no_groups*5),
                        2+(no_groups*7),2+(no_groups*8),
                        2+(no_groups*9),2+(no_groups*10),
                        2+(no_groups*11),2+(no_groups*12))+(test-1)]/
        sum(y_tail[c(2,
                     2+no_groups,2+(no_groups*2),
                     2+(no_groups*3),2+(no_groups*4),
                     2+(no_groups*5),
                     2+(no_groups*7),2+(no_groups*8),
                     2+(no_groups*9),2+(no_groups*10),
                     2+(no_groups*11),2+(no_groups*12))+(test-1)]) 
      
      
      age_proportions[,test] <- temp
      age_proportions[which(is.nan(age_proportions[,test])),test] <- 0
      if(any(age_proportions[,test]<0)){browser()}
      if(any(age_proportions[,test]>1)){browser()}
    }

    # age the proportions
    for(gap in 1:nrow(age_proportions)){
      age_proportions[gap,] <- age_population_1year(population_stratified,unlist(age_proportions[gap,]))
    }
    # all born into S compartment
    age_proportions[1,1] <- 1
    
    # set the model inputs
    mod2 <- gen_seeiir_ag_vacc_waning_yearcross$new(no_groups = no_groups, cij = new_cij, trans = transmissibility,
                                                    pop = population_stratified,
                                                    allS = c(unlist(age_proportions[1,])),
                                                    allE1 = c(unlist(age_proportions[2,])),
                                                    allE2 = c(unlist(age_proportions[3,])),
                                                    allI1 = c(unlist(age_proportions[4,])),
                                                    allI2 = c(unlist(age_proportions[5,])),
                                                    allR = c(unlist(age_proportions[6,])),
                                                    allCI =  y_tail[(2+(6*no_groups)):(1+(7*no_groups))],
                                                    allSv = c(unlist(age_proportions[7,])),
                                                    allEv1 = c(unlist(age_proportions[8,])),
                                                    allEv2 = c(unlist(age_proportions[9,])),
                                                    allIv1 = c(unlist(age_proportions[10,])),
                                                    allIv2 = c(unlist(age_proportions[11,])),
                                                    allRv = c(unlist(age_proportions[12,])),
                                                    susc = rep(susceptibility,3),
                                                    alpha = calendar_input$efficacy[1:no_groups],
                                                    omega = waning_rate,
                                                    dates = calendar_input$dates,
                                                    calendar = matrix(calendar_input$calendar, ncol = num_age_groups*3),
                                                    gamma1 = 2/infection_delays[1],
                                                    gamma2 = 2/infection_delays[2], 
                                                    num_vac_start = rep(0,no_groups) # no need to track
                                                    
    )
    # run the model
    y2 <- mod2$run(t, hmax = NULL, method = "euler", hini = 0.25, atol = 1)
    # extract end for later use
    y2_tail <- tail(y2,1)
    #combine with the previous output
    y <- rbind(y, y2[-1,])
    
    # update the vaccination calendar etc.
    efficacy_now <- efficacy_next2
    begin_date <- as.Date(paste0(year(begin_date), "-09-01"))
    t <- as.numeric(seq(begin_date, end_date2, interval))
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
      calendar_input$efficacy <- efficacy_now
    } else{
      # save back into the calendar
      calendar_input$dates <- input_dates
      calendar_input$efficacy <- efficacy_now
      if(length(input_dates)>2){
        # add the last row (which is always 0) to the front, for the extra one.
 #browser()
        calendar_input$calendar <- calendar_input$calendar[c(nrow(calendar_input$calendar),keepers),]
      } else{
        calendar_input$calendar =matrix(rep(0,num_age_groups*3*length(input_dates)), ncol = num_age_groups*3)
        calendar_input$efficacy <- rep(0,num_age_groups*3)
      } 
    }
    
     # browser()
      # set the model inputs again
      mod3 <- gen_seeiir_ag_vacc_waning_NH$new(no_groups = no_groups, cij = new_cij, trans = transmissibility,
                                               pop = population_stratified,
                                               allS = y2_tail[2:(1+no_groups)],
                                               allE1 = y2_tail[(2+no_groups):(1+(2*no_groups))],
                                               allE2 = y2_tail[(2+(2*no_groups)):(1+(3*no_groups))],
                                               allI1 = y2_tail[(2+(3*no_groups)):(1+(4*no_groups))],
                                               allI2 = y2_tail[(2+(4*no_groups)):(1+(5*no_groups))],
                                               allR = y2_tail[(2+(5*no_groups)):(1+(6*no_groups))],
                                               allCI = y2_tail[(2+(6*no_groups)):(1+(7*no_groups))],
                                               allSv = y2_tail[(2+(7*no_groups)):(1+(8*no_groups))],
                                               allEv1 = y2_tail[(2+(8*no_groups)):(1+(9*no_groups))],
                                               allEv2 = y2_tail[(2+(9*no_groups)):(1+(10*no_groups))],
                                               allIv1 = y2_tail[(2+(10*no_groups)):(1+(11*no_groups))],
                                               allIv2 = y2_tail[(2+(11*no_groups)):(1+(12*no_groups))],
                                               allRv = y2_tail[(2+(12*no_groups)):(1+(13*no_groups))],
                                               susc = rep(susceptibility,3),
                                               alpha = calendar_input$efficacy[1:no_groups],
                                               omega = waning_rate,
                                               dates = calendar_input$dates,
                                               calendar = matrix(calendar_input$calendar, ncol = num_age_groups*3),
                                               gamma1 = 2/infection_delays[1], gamma2 = 2/infection_delays[2],
                                               num_vac_start = rep(0,no_groups)
      )
      #run the model
      y3 <- mod3$run(t, hmax = NULL, method = "euler", hini = 0.25, atol = 1)
      # combine with the previous output
      y <- rbind(y, y3[-1,])
    }

  
  # calculate the cumulative values
  y <- mod2$transform_variables(y)$cumI
  # Returning the differences in cumulative infections from one timestep to the other
  y <- data.frame(y[2:(nrow(y)), ] - y[1:(nrow(y) - 1), ])
  #remove the last point as it's the start of the next week
  y <- y[1:(nrow(y)-1),]
  # add the dates
  y$time <- seq(begin_date2,length.out = nrow(y), by =interval)
  y<-data.table(y)
  # reformat the dates

  y[,week:= ISOweek(as.Date(time, origin="1970-01-01"))]
  y[,time := NULL]
  y_out <- unique(y[, lapply(.SD, sum, na.rm=T), by = "week"])

  return(y_out)
  
}



run_epidemic_model_yearcross <- function(vaccine_scenarios, year_in_question, begin_date, end_date, 
                                         epidemics_list, epidemic, scenario, immunity_input, 
                                         contact_ids_input, posterior_subset, flu_type,
                                         year_to_run,previous_summary){
  #specify the relevant vaccination parameters
  
  waning_rate = vaccine_scenarios[[scenario]][["waning_rate"]]
  
  # vaccine calendar - either between the specified dates or over the whole years
  if(length(vaccine_scenarios[[scenario]][["dates"]])>1){
    # check whether looping over a year
    if(as.Date(paste0(years[i], vaccine_scenarios[[scenario]][["dates"]][1])) >
       as.Date(paste0(years[i], vaccine_scenarios[[scenario]][["dates"]][2]))){
      dates = seq(from = as.Date(paste0(years[i], vaccine_scenarios[[scenario]][["dates"]][1])),
                  to = as.Date(paste0(years[i]+1, vaccine_scenarios[[scenario]][["dates"]][2])),
                  by = 7)
    } else{ 
      dates = seq(from = as.Date(paste0(years[i], vaccine_scenarios[[scenario]][["dates"]][1])),
                  to = as.Date(paste0(years[i], vaccine_scenarios[[scenario]][["dates"]][2])),
                  by = 7)  }
  } else {
    dates = seq(from = as.Date(paste0(years[i], vaccine_scenarios[[scenario]][["dates"]][1])),
                to = as.Date(paste0(years[i+1], vaccine_scenarios[[scenario]][["dates"]][1]))-1,
                by = 7)}
  # Coverage
  
  if(location == "Kenya"){
    target_coverage <-  vaccine_scenarios[[scenario]][["coverage"]]
  }
  if(location == "UK"){
    target_coverage <-  vaccine_scenarios[[scenario]][["coverage"]][i,2:22]
  }

  # new_coverage = change_coverage(matrix(rep(0,num_age_groups*3*length(dates)), 
  #                                       ncol = num_age_groups*3),
  #                                target_coverage)
  new_coverage <-  sweep(coverage_timing, 2, target_coverage, "*")
  # specify the demography
  if(location == "Kenya"){
    demography_input <- popken[,which(years == epidemics_list[[epidemic]]["year"])+1]}
  # calculate the vaccination calender
  # determine the right efficacy to use here 
  starting_year <- year(begin_date)
  if(begin_date < as.Date(paste0(starting_year,"-03-01"))) {
    # required efficacy = previous year NH
    required_efficacy <- grep(starting_year-1, x = lookup_year)
    required_efficacy2 <- grep("NH", x = lookup_year[required_efficacy])
    efficacy_now_spot <-required_efficacy[required_efficacy2]
  } else if(begin_date >= as.Date(paste0(starting_year,"-03-01")) &
            begin_date < as.Date(paste0(starting_year,"-09-01"))) {
    # required efficacy = curent year SH
    required_efficacy <- grep(starting_year, x = lookup_year)
    required_efficacy2 <- grep("SH", x = lookup_year[required_efficacy])
    efficacy_now_spot <-required_efficacy[required_efficacy2]
  } else if(begin_date >= as.Date(paste0(starting_year,"-09-01"))){
    # required efficacy = current year NH
    required_efficacy <- grep(starting_year, x = lookup_year)
    required_efficacy2 <- grep("NH", x = lookup_year[required_efficacy])
    efficacy_now_spot <-required_efficacy[required_efficacy2]
  }
  
  if(flu_type =="AH1N1"){
    efficacy_now <- vaccine_scenarios[[scenario]][["efficacy_H1"]][,efficacy_now_spot]
    efficacy_next <- vaccine_scenarios[[scenario]][["efficacy_H1"]][,efficacy_now_spot+1]
    efficacy_next2 <- vaccine_scenarios[[scenario]][["efficacy_H1"]][,efficacy_now_spot+2]
  } else if(flu_type =="AH3N2"){
    efficacy_now <- vaccine_scenarios[[scenario]][["efficacy_H3"]][,efficacy_now_spot]
    efficacy_next <- vaccine_scenarios[[scenario]][["efficacy_H3"]][,efficacy_now_spot+1]
    efficacy_next2 <- vaccine_scenarios[[scenario]][["efficacy_H3"]][,efficacy_now_spot+2]
  } else if(flu_type =="B"){
    efficacy_now <- vaccine_scenarios[[scenario]][["efficacy_B"]][,efficacy_now_spot]
    efficacy_next <- vaccine_scenarios[[scenario]][["efficacy_B"]][,efficacy_now_spot+1]
    efficacy_next2 <- vaccine_scenarios[[scenario]][["efficacy_B"]][,efficacy_now_spot+2]
  }
  calendar_input = as_vaccination_calendar(efficacy = rep(0,3*num_age_groups)
                                           , dates = as.Date(dates) 
                                           , coverage = as.data.frame(new_coverage)
                                           , no_age_groups = num_age_groups
                                           , no_risk_groups = 3)
  
  # work out vaccination adjustments based on what proportion of 0-5s vaccinated
  # if not all dates fall within the first year

  if(!all(calendar_input$dates <= end_first_year_vaccination)){
    # identify at which time points the vaccination changes
    change_time <- tail(which(calendar_input$dates <= end_first_year_vaccination),1)
    if(length(change_time)==0){change_time <- 1}
    # multiply the relevant vaccination time steps by the calendar
    calendar_input$calendar[change_time:nrow(calendar_input$calendar),] <-
      sweep(calendar_input$calendar[change_time:nrow(calendar_input$calendar),],
            MARGIN = 2, 
            rep(unlist(vaccine_scenarios[[scenario]]["prop_group_vacc"]),3),
            FUN="*")
  }
  
  no_groups <- num_age_groups*3
  # extract immunity inputs
  prop_vacc_start <- list(prop_vaccine_compartments = as.numeric(c(immunity_input[1:no_groups])),
                          prop_R_vaccinated = as.numeric(c(immunity_input[(no_groups+1):(2*no_groups)])), 
                          prop_R = as.numeric(rep(0,3*num_age_groups)))
  
  # Run the model and calculate the cumulative infections
  # outputs total infections in age group over the time period, for each posterior sample
#  browser()
  total_infections_ages <- epidemic_scenarios_yearcross(demography_input = demography_input,
                                                        contact_ids_input = contact_ids_input,
                                                        parameters = posterior_subset,
                                                        calendar_input = calendar_input, 
                                                        waning_rate = waning_rate, 
                                                        vaccination_ratio_input = prop_vacc_start, 
                                                        begin_date = begin_date, 
                                                        end_date = end_date, 
                                                        year_to_run = year_to_run, 
                                                        efficacy_now = efficacy_now, 
                                                        efficacy_next = efficacy_next, 
                                                        efficacy_next2 = efficacy_next2, 
                                                        previous_summary = previous_summary)
  
  
  
  # calculate the sum of cases at the end over the season  
  #total_infections <- rowSums(total_infections_ages)

  return(total_infections_ages)
}


gen_seeiir_ag_vacc_waning_yearcross <- odin::odin({
  # Number of groups
  no_groups <- user()
  
  # INITIAL CONDITIONS
  # Population size by age/risk group
  pop[] <- user()
  # Initial proportion in each compartment
  allS[] <- user()
  allE1[] <- user()
  allE2[] <- user()
  allI1[] <- user()
  allI2[] <- user()
  allR[] <- user()
  allCI[] <- user()
  allSv[] <- user()
  allEv1[] <- user()
  allEv2[] <- user()
  allIv1[] <- user()
  allIv2[] <- user()
  allRv[] <- user()
  num_vac_start[] <- user()
  
  # MODEL PARAMETERS
  # Susceptibility
  susc[] <- user()
  
  # Transmissibility
  trans <- user()
  
  # Latent periods
  gamma1 <- user()
  gamma2 <- user()
  
  # Vaccine related variables 
  dates[] <- user()
  calendar[,] <- user()
  # waning of vaccine immunity
  omega <- user()
  
  # efficacy
  alpha[] <- user()
  
  # Contact matrix
  cij[,] <- user()
  
  # Force of infection
  lambda[] <- trans * susc[i] * (sum(sij[i,]))
  
  # Vaccination. The rate is a step function that changes at each date according
  # to the passed calendar
  vI[] <- interpolate(dates, calendar, "constant")
  # Vaccination is given as a fraction vaccination, here we scale it to 
  # a rate
  #sumN[] <- if (vI[i]>0) (S[i]+E1[i]+E2[i]+I1[i]+I2[i]+R[i]) else 0
  v[] <- vI[i]#if (sumN[i]>0) vI[i]*pop[i]/sumN[i] else 0
  
  # Transmission matrix
  sij[,] <- cij[i,j] * (I1[j] + I2[j] + I1v[j] + I2v[j])
  
  # Newly infected
  newInf[] <- lambda[i] * S[i]
  newInfv[] <- lambda[i] * Sv[i]
  
  # THE DERIVATIVES OF THE SEEIIR MODEL
  # Derivatives of the not vaccinated group
  deriv(S[])  <- + omega*Sv[i] - newInf[i] - v[i] * S[i] 
  deriv(E1[]) <- + omega*E1v[i] + newInf[i] - gamma1 * E1[i] - v[i] * E1[i] 
  deriv(E2[]) <- + omega*E2v[i] + gamma1 * (E1[i] - E2[i]) - v[i] * E2[i]
  deriv(I1[]) <- + omega*I1v[i] + gamma1 * E2[i]  - gamma2 * I1[i] - v[i] * I1[i] 
  deriv(I2[]) <- + omega*I2v[i] + gamma2 * (I1[i] - I2[i]) - v[i] * I2[i] 
  deriv(R[])  <- + omega*Rv[i] + gamma2 * I2[i] - v[i] * R[i] 
  
  # Derivatives vaccination group
  deriv(Sv[])  <- - omega*Sv[i]  - newInfv[i] + v[i] * (1-alpha[i]) * S[i] 
  deriv(E1v[]) <- - omega*E1v[i] + newInfv[i] - gamma1 * E1v[i] + v[i] * E1[i]
  deriv(E2v[]) <- - omega*E2v[i] + gamma1 * (E1v[i] - E2v[i]) + v[i] * E2[i]
  deriv(I1v[]) <- - omega*I1v[i] + gamma1 * E2v[i]  - gamma2 * I1v[i] + v[i] * I1[i]
  deriv(I2v[]) <- - omega*I2v[i] + gamma2 * (I1v[i] - I2v[i]) + v[i] * I2[i]
  deriv(Rv[])  <- - omega*Rv[i]  + gamma2 * I2v[i] + v[i] * (R[i] + alpha[i] * S[i])
  deriv(RT[]) <- 0
  
  # Tracking the cumulative amount of infections over time for output of incidence
  deriv(cumI[]) <- newInf[i] + newInfv[i]
  
  # Initial value of the variables
  initial(S[1:no_groups]) <- pop[i]*allS[i]
  initial(E1[1:no_groups]) <- pop[i]*allE1[i]
  initial(E2[1:no_groups]) <- pop[i]*allE2[i]
  initial(I1[1:no_groups]) <- pop[i]*allI1[i]
  initial(I2[1:no_groups]) <- pop[i]*allI2[i]
  initial(R[1:no_groups]) <- pop[i]*allR[i]
  initial(cumI[1:no_groups]) <- allCI[i]
  
  initial(Sv[1:no_groups]) <-pop[i]*allSv[i]
  initial(E1v[1:no_groups]) <- pop[i]*allEv1[i]
  initial(E2v[1:no_groups]) <- pop[i]*allEv2[i]
  initial(I1v[1:no_groups]) <- pop[i]*allIv1[i]
  initial(I2v[1:no_groups]) <- pop[i]*allIv2[i]
  initial(Rv[1:no_groups]) <- pop[i]*allRv[i]
  initial(RT[1:no_groups]) <- num_vac_start[i]
  
  # Set dimension of all variables/parameters
  dim(dates) <- user()
  dim(calendar) <- user()
  
  dim(pop) <- no_groups
  dim(allS) <- no_groups
  dim(allE1) <- no_groups
  dim(allE2) <- no_groups
  dim(allI1) <- no_groups
  dim(allI2) <- no_groups
  dim(allR) <- no_groups
  dim(allCI) <- no_groups
  dim(allSv) <- no_groups
  dim(allEv1) <- no_groups
  dim(allEv2) <- no_groups
  dim(allIv1) <- no_groups
  dim(allIv2) <- no_groups
  dim(allRv) <- no_groups
  dim(susc) <- no_groups
  dim(lambda) <- no_groups
  dim(v) <- no_groups
  dim(vI) <- no_groups
  dim(num_vac_start) <- no_groups
  # dim(sumN) <- no_groups  
  dim(alpha) <- no_groups
  dim(cij) <- c(no_groups, no_groups)
  dim(sij) <- c(no_groups, no_groups)
  
  dim(S) <- no_groups
  dim(E1) <- no_groups
  dim(E2) <- no_groups
  dim(I1) <- no_groups
  dim(I2) <- no_groups
  dim(R) <- no_groups
  dim(Sv) <- no_groups
  dim(E1v) <- no_groups
  dim(E2v) <- no_groups
  dim(I1v) <- no_groups
  dim(I2v) <- no_groups
  dim(Rv) <- no_groups
  dim(cumI) <- no_groups
  dim(newInf) <- no_groups
  dim(newInfv) <- no_groups
  dim(RT) <- no_groups
})

gen_seeiir_ag_vacc_waning <- odin::odin({
  # Number of groups
  no_groups <- user()
  
  # INITIAL CONDITIONS
  # Population size by age/risk group
  pop[] <- user()
  # Start vaccinated by age/risk group
  V0[] <- user()
  R0[] <- user()
  RV0[] <- user()
  # Initial infection by age/risk group
  I0[] <- user()
  num_vac_start[] <- user()
  
  # MODEL PARAMETERS
  # Susceptibility
  susc[] <- user()
  
  # Transmissibility
  trans <- user()
  
  # Latent periods
  gamma1 <- user()
  gamma2 <- user()
  
  # Vaccine related variables 
  dates[] <- user()
  calendar[,] <- user()
  # waning of vaccine immunity
  omega <- user()
  
  # efficacy
  alpha[] <- user()
  
  # Contact matrix
  cij[,] <- user()
  
  # Force of infection
  lambda[] <- trans * susc[i] * (sum(sij[i,]))
  
  # Vaccination. The rate is a step function that changes at each date according
  # to the passed calendar
  vI[] <- interpolate(dates, calendar, "constant")
  
  # Vaccination is given as a fraction vaccination, here we scale it to 
  # a rate
  #sumN[] <- if (vI[i]>0) (S[i]+E1[i]+E2[i]+I1[i]+I2[i]+R[i]) else 0
  v[] <-vI[i] #if (sumN[i]>0) vI[i]*pop[i] else 0
  # Transmission matrix
  sij[,] <- cij[i,j] * (I1[j] + I2[j] + I1v[j] + I2v[j])
  
  # Newly infected
  newInf[] <- lambda[i] * S[i]
  newInfv[] <- lambda[i] * Sv[i]
  
  # THE DERIVATIVES OF THE SEEIIR MODEL
  # Derivatives of the not vaccinated group
  deriv(S[])  <- + omega*Sv[i] - newInf[i] - v[i] * S[i] 
  deriv(E1[]) <- + omega*E1v[i] + newInf[i] - gamma1 * E1[i] - v[i] * E1[i] 
  deriv(E2[]) <- + omega*E2v[i] + gamma1 * (E1[i] - E2[i]) - v[i] * E2[i]
  deriv(I1[]) <- + omega*I1v[i] + gamma1 * E2[i]  - gamma2 * I1[i] - v[i] * I1[i] 
  deriv(I2[]) <- + omega*I2v[i] + gamma2 * (I1[i] - I2[i]) - v[i] * I2[i] 
  deriv(R[])  <- + omega*Rv[i] + gamma2 * I2[i] - v[i] * R[i] 
  
  # Derivatives vaccination group
  deriv(Sv[])  <- - omega*Sv[i]  - newInfv[i] + v[i] * (1-alpha[i]) * S[i] 
  deriv(E1v[]) <- - omega*E1v[i] + newInfv[i] - gamma1 * E1v[i] + v[i] * E1[i]
  deriv(E2v[]) <- - omega*E2v[i] + gamma1 * (E1v[i] - E2v[i]) + v[i] * E2[i]
  deriv(I1v[]) <- - omega*I1v[i] + gamma1 * E2v[i]  - gamma2 * I1v[i] + v[i] * I1[i]
  deriv(I2v[]) <- - omega*I2v[i] + gamma2 * (I1v[i] - I2v[i]) + v[i] * I2[i]
  deriv(Rv[])  <- - omega*Rv[i]  + gamma2 * I2v[i] + v[i] * (R[i] + alpha[i] * S[i])
  deriv(VT[]) <- vI[i]*pop[i]
  
  # Tracking the cumulative amount of infections over time for output of incidence
  deriv(cumI[]) <- newInf[i] + newInfv[i]
  
  # Initial value of the variables
  initial(S[1:no_groups]) <- pop[i]*(1-V0[i])*(1-R0[i]) - I0[i]
  initial(E1[1:no_groups]) <- 0
  initial(E2[1:no_groups]) <- 0
  initial(I1[1:no_groups]) <- I0[i]
  initial(I2[1:no_groups]) <- 0
  initial(R[1:no_groups]) <- pop[i]*(1-V0[i])*(R0[i])
  initial(cumI[1:no_groups]) <- 0
  
  initial(Sv[1:no_groups]) <- (pop[i]*V0[i])*(1-RV0[i])
  initial(E1v[1:no_groups]) <- 0
  initial(E2v[1:no_groups]) <- 0
  initial(I1v[1:no_groups]) <- 0
  initial(I2v[1:no_groups]) <- 0
  initial(Rv[1:no_groups]) <- (pop[i]*V0[i])*(RV0[i])
  initial(VT[1:no_groups]) <- num_vac_start[i]
  
  # Set dimension of all variables/parameters
  dim(dates) <- user()
  dim(calendar) <- user()
  
  dim(pop) <- no_groups
  dim(I0) <- no_groups
  dim(V0) <- no_groups
  dim(R0) <- no_groups
  dim(RV0) <- no_groups
  dim(num_vac_start) <- no_groups
  dim(susc) <- no_groups
  dim(lambda) <- no_groups
  dim(v) <- no_groups
  dim(vI) <- no_groups
  #dim(sumN) <- no_groups  
  dim(alpha) <- no_groups
  dim(cij) <- c(no_groups, no_groups)
  dim(sij) <- c(no_groups, no_groups)
  
  dim(S) <- no_groups
  dim(E1) <- no_groups
  dim(E2) <- no_groups
  dim(I1) <- no_groups
  dim(I2) <- no_groups
  dim(R) <- no_groups
  dim(Sv) <- no_groups
  dim(E1v) <- no_groups
  dim(E2v) <- no_groups
  dim(I1v) <- no_groups
  dim(I2v) <- no_groups
  dim(Rv) <- no_groups
  dim(cumI) <- no_groups
  dim(newInf) <- no_groups
  dim(newInfv) <- no_groups
  dim(VT) <- no_groups
})
gen_seeiir_ag_vacc_waning_NH <- odin::odin({
  # Number of groups
  no_groups <- user()
  
  # INITIAL CONDITIONS
  # Population size by age/risk group
  pop[] <- user()
  # Initial proportion in each compartment
  allS[] <- user()
  allE1[] <- user()
  allE2[] <- user()
  allI1[] <- user()
  allI2[] <- user()
  allR[] <- user()
  allCI[] <- user()
  allSv[] <- user()
  allEv1[] <- user()
  allEv2[] <- user()
  allIv1[] <- user()
  allIv2[] <- user()
  allRv[] <- user()
  num_vac_start[] <- user()
  # MODEL PARAMETERS
  # Susceptibility
  susc[] <- user()
  
  # Transmissibility
  trans <- user()
  
  # Latent periods
  gamma1 <- user()
  gamma2 <- user()
  
  # Vaccine related variables 
  dates[] <- user()
  calendar[,] <- user()
  # waning of vaccine immunity
  omega <- user()
  
  # efficacy
  alpha[] <- user()
  
  # Contact matrix
  cij[,] <- user()
  
  # Force of infection
  lambda[] <- trans * susc[i] * (sum(sij[i,]))
  
  # Vaccination. The rate is a step function that changes at each date according
  # to the passed calendar
  vI[] <- interpolate(dates, calendar, "constant")
  # Vaccination is given as a fraction vaccination, here we scale it to 
  # a rate
  #sumN[] <- if (vI[i]>0) (S[i]+E1[i]+E2[i]+I1[i]+I2[i]+R[i]) else 0
  v[] <- vI[i]#if (sumN[i]>0) vI[i]*sumN[i]/pop[i] else 0
  
  # Transmission matrix
  sij[,] <- cij[i,j] * (I1[j] + I2[j] + I1v[j] + I2v[j])
  
  # Newly infected
  newInf[] <- lambda[i] * S[i]
  newInfv[] <- lambda[i] * Sv[i]
  
  # THE DERIVATIVES OF THE SEEIIR MODEL
  # Derivatives of the not vaccinated group
  deriv(S[])  <- + omega*Sv[i] - newInf[i] - v[i] * S[i] 
  deriv(E1[]) <- + omega*E1v[i] + newInf[i] - gamma1 * E1[i] - v[i] * E1[i] 
  deriv(E2[]) <- + omega*E2v[i] + gamma1 * (E1[i] - E2[i]) - v[i] * E2[i]
  deriv(I1[]) <- + omega*I1v[i] + gamma1 * E2[i]  - gamma2 * I1[i] - v[i] * I1[i] 
  deriv(I2[]) <- + omega*I2v[i] + gamma2 * (I1[i] - I2[i]) - v[i] * I2[i] 
  deriv(R[])  <- + omega*Rv[i] + gamma2 * I2[i] - v[i] * R[i] 
  
  # Derivatives vaccination group
  deriv(Sv[])  <- - omega*Sv[i]  - newInfv[i] + v[i] * (1-alpha[i]) * S[i] 
  deriv(E1v[]) <- - omega*E1v[i] + newInfv[i] - gamma1 * E1v[i] + v[i] * E1[i]
  deriv(E2v[]) <- - omega*E2v[i] + gamma1 * (E1v[i] - E2v[i]) + v[i] * E2[i]
  deriv(I1v[]) <- - omega*I1v[i] + gamma1 * E2v[i]  - gamma2 * I1v[i] + v[i] * I1[i]
  deriv(I2v[]) <- - omega*I2v[i] + gamma2 * (I1v[i] - I2v[i]) + v[i] * I2[i]
  deriv(Rv[])  <- - omega*Rv[i]  + gamma2 * I2v[i] + v[i] * (R[i] + alpha[i] * S[i])
  deriv(VT[]) <- vI[i]*pop[i]
  # Tracking the cumulative amount of infections over time for output of incidence
  deriv(cumI[]) <- newInf[i] + newInfv[i]
  
  # Initial value of the variables
  initial(S[1:no_groups]) <- allS[i]
  initial(E1[1:no_groups]) <- allE1[i]
  initial(E2[1:no_groups]) <- allE2[i]
  initial(I1[1:no_groups]) <- allI1[i]
  initial(I2[1:no_groups]) <- allI2[i]
  initial(R[1:no_groups]) <- allR[i]
  initial(cumI[1:no_groups]) <- allCI[i]
  
  initial(Sv[1:no_groups]) <- allSv[i]
  initial(E1v[1:no_groups]) <- allEv1[i]
  initial(E2v[1:no_groups]) <- allEv2[i]
  initial(I1v[1:no_groups]) <- allIv1[i]
  initial(I2v[1:no_groups]) <- allIv2[i]
  initial(Rv[1:no_groups]) <- allRv[i]
  initial(VT[1:no_groups]) <- num_vac_start[i]
  
  # Set dimension of all variables/parameters
  dim(dates) <- user()
  dim(calendar) <- user()
  
  dim(pop) <- no_groups
  dim(allS) <- no_groups
  dim(allE1) <- no_groups
  dim(allE2) <- no_groups
  dim(allI1) <- no_groups
  dim(allI2) <- no_groups
  dim(allR) <- no_groups
  dim(allCI) <- no_groups
  dim(allSv) <- no_groups
  dim(allEv1) <- no_groups
  dim(allEv2) <- no_groups
  dim(allIv1) <- no_groups
  dim(allIv2) <- no_groups
  dim(allRv) <- no_groups
  dim(num_vac_start) <- no_groups
  dim(susc) <- no_groups
  dim(lambda) <- no_groups
  dim(v) <- no_groups
  dim(vI) <- no_groups
  # dim(sumN) <- no_groups  
  dim(alpha) <- no_groups
  dim(cij) <- c(no_groups, no_groups)
  dim(sij) <- c(no_groups, no_groups)
  
  dim(S) <- no_groups
  dim(E1) <- no_groups
  dim(E2) <- no_groups
  dim(I1) <- no_groups
  dim(I2) <- no_groups
  dim(R) <- no_groups
  dim(Sv) <- no_groups
  dim(E1v) <- no_groups
  dim(E2v) <- no_groups
  dim(I1v) <- no_groups
  dim(I2v) <- no_groups
  dim(Rv) <- no_groups
  dim(cumI) <- no_groups
  dim(newInf) <- no_groups
  dim(newInfv) <- no_groups
  dim(VT) <- no_groups
})
