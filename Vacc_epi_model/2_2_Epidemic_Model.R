# Epidemic  Model 

# set up storage system

ascertainment_H3 <- data.frame()
ascertainment_H1 <- data.frame()
ascertainment_B <- data.frame()
parameter_store <- list()

annual_change <- 0.1
storage_counter <- 0
storage_prop <- data.table(matrix(ncol=11 ))
storage_data <- list()

colnames(storage_prop) <- c("scenario", "sample", "variable", "epidemic", "V1", "base_case_value", "pop", "prop_change","f_type", "year", "annual_change")
for(epidemic in 1:length(epidemics_list)){
  print(epidemic)
  set.seed(200)
  # specify the time frame
  begin_date <- epidemics_list[[epidemic]][["start_date"]]
  end_date <- epidemics_list[[epidemic]][["end_date"]]
  year_in_question <- which(years == epidemics_list[[epidemic]]["year"])
  flu_type <- epidemics_list[[epidemic]][["flutype"]]
  
  #specify the previous epidemic of that subtyoe, if there is one.
  if(change_susceptibility_switch != "OFF"){
    previous_epidemic <- epidemics_list[[epidemic]][["previous_epi"]]}else{
      previous_epidemic <- NA}

  # load the posterior samples from the relevant area!
  if(location == "Kenya"){
    if(use_presampled == F){
      posterior_samples <- read.csv(here::here("Posteriors", paste0(begin_date, " to ", end_date, " ", flu_type, " ", "UK", ".csv")))
    } else { posterior_samples <- read.csv(here::here("Posteriors", paste0(begin_date, " to ", end_date, " ", flu_type, " ", "UK_presampled_suscchange", ".csv")))[,-1]}
    sample_set <- sample(1:nrow(posterior_samples), size=posterior_sample_size)
    if(save_samples == T){
      write.csv(posterior_samples[sample_set,],
                file = here::here("Posteriors", paste0(begin_date, " to ", end_date, " ", flu_type, " ", "UK_presampled_suscchange", ".csv")))
    }
    posterior_subset <- posterior_samples[sample_set,2:10]
    contact_ids_input <- as.matrix(posterior_samples[sample_set,12:579])

    
  } else if(location =="UK"){
    #recode flu types to the right ones
    if(flu_type == "AH3N2"){flu_type_short <- "H3"}
    if(flu_type == "AH1N1"){flu_type_short <- "H1"}
    if(flu_type == "B"){flu_type_short <- "B"}
    posterior_samples <- inference.results.2013[[year_in_question]][[flu_type_short]]$batch
    sample_set <- sample(1:nrow(posterior_samples), size=posterior_sample_size)
    posterior_subset <- as.data.frame(posterior_samples[sample_set,])
    contact_ids_input <- inference.results.2013[[year_in_question]][[flu_type_short]]$contact.ids[sample_set,]
  } else{stop("Unknown location! SHould be Kenya or UK")}
  
  posterior_subset[,"epidemic"] <- epidemic
  parameter_store[[epidemic]] <- posterior_subset
  
  #print progress
  
  # save the ascertainment rates for later in the season
  if(flu_type == "AH3N2"){
    ascertainment_H3 <- rbind(ascertainment_H3, data.frame(posterior_subset[,1:3], flu_type, epidemic))
  } else if(flu_type == "AH1N1"){
    ascertainment_H1 <- rbind(ascertainment_H1, data.frame(posterior_subset[,1:3], flu_type, epidemic))
  } else if(flu_type == "B"){
    ascertainment_B <- rbind(ascertainment_B, data.frame(posterior_subset[,1:3], flu_type, epidemic))
  } else {stop("NOT A VAILD FLU TYPE")}
  
  #by scenario

  for(scenario in target_scenarios){
    storage_counter <- storage_counter + 1
    target_week <- date2ISOweek(begin_date)
    target_week <- substring(target_week,1, nchar(target_week)-2)
    # extract the relevant immunity
    immunity_row <- vaccination_ratio_store2[week == target_week &
                                               Vacc_scenario == scenario &
                                               virus_type == flu_type]
    # Here need the base scenario of

    # run the model
    scen<-scenario
    previous_summary <- storage_prop[scenario == scen]
    # add a time since epidemic column
    previous_summary[,time_since := year_in_question- year]
  
    # if not (no vaccine or current vaccine)
    # note down for later use
    if(change_susceptibility_switch == "FIXED_REDUCTION" &
       (scenario != target_scenarios[c(1) ]) & !is.na(previous_epidemic)){
      reduce_susceptibility <- T
    } else {reduce_susceptibility <- F}
    
    epidemic_infections <- run_epidemic_model_yearcross(vaccine_scenarios,
                                                        year_in_question,
                                                        begin_date,
                                                        end_date,
                                                        epidemics_list,
                                                        epidemic, scenario,
                                                        immunity_input =unlist(immunity_row),
                                                        contact_ids_input,
                                                        posterior_subset,
                                                        flu_type,
                                                        year_to_run = years[year_in_question],
                                                        previous_summary = previous_summary[f_type == flu_type])

    # format the output
    
    storage_temp <- rbindlist(epidemic_infections, idcol = "sample")
    storage_temp$epidemic = epidemic
    storage_temp$scenario = scenario
    # store the epidemic model output
    storage_data[[storage_counter]] <- storage_temp
    #  infections in the previous epidemic
    columns_to_sum <- colnames(storage_data[[storage_counter]])[c(1:((num_age_groups)*3)+2)]
    epi <- epidemic
    current_summary <- storage_data[[storage_counter]][epidemic==epi,]
   # current_summary[,total_cases := NULL]
    # format
    # current_summary <- current_summary[,-2]
    current_summary <- melt.data.table(current_summary, id.vars =
                                         c("sample", "epidemic", "scenario", "week"))
    # calculate sum
    current_summary2 <- current_summary[, sum(value), by =
                                          c("scenario", "sample", "variable","epidemic")]
    base_case <- current_summary2[scenario == 1]
    # work out the base case
    current_summary2[base_case, on = c("sample", "variable", "epidemic"), base_case_value := i.V1 ]
    # add the population sizes
    for(ypl in 1:length(columns_to_sum)){
      label <- columns_to_sum[ypl]
      current_summary2[variable == label, pop := as.numeric(population_risked[1,ypl]) ]
    }
    #Do the calculation of the change in sucseptibility for each version
 
    if(change_susceptibility_switch =="POP_ADD_WANING"){
      current_summary2[,prop_change := (base_case_value-V1)/pop]
    } else {current_summary2[,prop_change := 1]}
    
    current_summary2$f_type <- flu_type
    current_summary2$year <- year_in_question
    current_summary2$annual_change <- annual_change

  }
  storage_prop <- rbind(storage_prop,current_summary2)
    }
  


#stop in case you dont want to save etc. 
# stop()

# save the output as takes a while to run
#save(storage_data, file = paste0("storage_data_",name_run,".Rdata"))
# saveRDS(ascertainment_H3, file = here("output", "ascertainment_H3.RDS"))
# saveRDS(ascertainment_H1, file = here("output", "ascertainment_H1.RDS"))
# saveRDS(ascertainment_B, file = here("output", "ascertainment_B.RDS"))
# # option to load in the outputs 
# storage_data <- readRDS(file = here("output", "storage_data.RDS"))
# ascertainment_H3 <- readRDS(file = here("output", "ascertainment_H3.RDS"))
# ascertainment_H1 <- readRDS(file = here("output", "ascertainment_H1.RDS"))
# ascertainment_B <- readRDS(file = here("output", "ascertainment_B.RDS"))

# format for plotting
storage_data <- rbindlist(storage_data)
storage_data[,total_cases := 0]
for(i in 1:length(columns_to_sum)){
  storage_data[,total_cases := total_cases +get(columns_to_sum[i])]
}

total_cases_time <-storage_data
total_cases_time[,Date := ISOweek2date(paste0(week,"-1"))]
total_cases_time$epidemic <- as.factor(total_cases_time$epidemic)
total_cases_time$scenario <- as.factor(total_cases_time$scenario)

total_cases_time_temp <- total_cases_time[,]


for(i in 1:length(epidemics_list)){
  total_cases_time_temp[epidemic==i,Year := epidemics_list[[i]]$year]
  total_cases_time_temp[epidemic==i,Virus := epidemics_list[[i]]$flutype]

}

#total_cases_time_temp <- total_cases_time_temp["AH1N1" %in% Virus]

total_cases_time_temp$scenario <- as.character(total_cases_time_temp$scenario)
total_cases_time_temp$sample <- as.character(total_cases_time_temp$sample)
total_cases_time_temp$Year <- as.character(total_cases_time_temp$Year)
total_cases_time_temp$Virus <- as.character(total_cases_time_temp$Virus)

total_cases_time_temp$scenario <- factor(total_cases_time_temp$scenario,
                                         levels=c("1", "2", "3", "4", "5", "6"))

total_cases_time_temp <- as.data.frame(total_cases_time_temp)

# plot
total_cases_time_temp$epidemic <- as.character(total_cases_time_temp$epidemic)
total_cases_time_temp <- data.table(total_cases_time_temp)
# 
ghm<- total_cases_time_temp[]

total_cases_time_temp <- data.frame(total_cases_time_temp)
ghm$sample <- as.factor(ghm$sample)
ghm$scenario <- as.factor(ghm$scenario)
ghm$epidemic <- as.factor(ghm$epidemic)
ghm <- as.data.table(ghm)
ghml <- ghm[epidemic==25]
ghm_m <- melt.data.table(ghml, id.vars = c("sample",  "epidemic", "scenario", "Date",
                               "Virus"), measure.vars = c(columns_to_sum))
  #ghm_m <- ghm_m[sample %in% set_of_sampels]

for_vaccination <- dcast.data.table(ghm_m, sample + epidemic + scenario + Date + Virus ~ variable,
                         value.var = "value")
for_vaccination[, Age1 := X1 + X8 + X15]
for_vaccination[, Age2 := X2 + X9 + X16]
for_vaccination[, Age3 := X3 + X10 + X17]
for_vaccination[, Age4 := X4 + X11 + X18]
for_vaccination[, Age5 := X5 + X12 + X19]
for_vaccination[, Age6 := X6 + X13 + X20]
for_vaccination[, Age7 := X7 + X14 + X21]
for_vaccination[, c("X1", "X2", "X3", "X4", "X5", "X6", 
                    "X7", "X8", "X9", "X10", "X11", "X12", 
                    "X13", "X14", "X15", "X16", "X17", "X18", 
                    "X19", "X20", "X21") := NULL]

for_vaccination_m <- melt.data.table(for_vaccination, id.vars = c("sample", "epidemic", "scenario", 
                                               "Date", "Virus"))


ggplot(for_vaccination_m[scenario==1],aes(x = Date, y = value,
                                 colour = scenario,
                                 group =sample)) +
  geom_path(alpha = 0.5) +
  facet_grid(variable~scenario, scales = "free_y") +
  theme_linedraw() +
  labs(x = "Date", y = "Total cases", title = paste0("1995 H3N2")) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  geom_vline(xintercept = as.Date("2013-12-12"), alpha = 0.4)

# total_cases_time_temp2 <- total_cases_time_temp#[1:14000,]
# 
# 
# ggplot(total_cases_time_temp2,aes(x = Date, y = total_cases,
#                  colour = scenario,
#                  group =sample)) +
#   geom_path(alpha = 0.5) +
#   facet_grid(Virus~.) +
#   theme_linedraw() +
#   labs(x = "Date", y = "Total cases", title = "Replicate of UK in this model") +
#   theme(axis.title = element_text(size = 12),
#         axis.text = element_text(size = 12))

  #storage_data[, total_cases := NULL]

# store_props[, mean_change := mean(prop_change), by= c( "scenario", "variable", "epidemic")]
# store_props$scenario <- as.factor(store_props$scenario)
# 
# ggplot(store_props, aes(x = epidemic, y = mean_change, fill = scenario))+
#   geom_bar(stat="identity", position="dodge") +
#   facet_grid(variable~.)

# 
# 
# extract_age <- "V4"
# extract_scenario <- 122
# colnames(storage_prop) <- c("sample", "scenario", "epidemic", "type", "V1", 
                       #     "V2","V3", "V4","V5", "V6", "system", "virus")
# storage_prop <- data.table(storage_prop)
# to_plot <- data.table(melt(storage_prop, id.vars = c("sample", "scenario",
#                                                      "epidemic", "type", "system","virus")))
# to_plot$value <- as.numeric(to_plot$value)
# to_plot <- na.omit(to_plot)
# to_plot$epidemic <- factor(to_plot$epidemic, levels = c(2,3,4,5,7,8,9,10,11))
# #to_plot[,mean_v :=mean(value), by=c("scenario", "epidemic", "type", "variable")]
# ggplot(to_plot[variable==extract_age &
#                  scenario == extract_scenario],
#        aes(x = epidemic, y = value, fill =type, colour = type)) +
#   geom_point()+ 
#   facet_grid(virus~system, scales="free_y") + 
#   geom_hline(yintercept=c(0,1)) + 
#   labs(title=paste0("Comparison - scenario ", extract_scenario, 
#                     " - age ", extract_age), y = "susceptibility")

