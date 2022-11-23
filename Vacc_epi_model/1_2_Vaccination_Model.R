# This is the vaccination model (model 1) 


################################# SETUP ########################################
# storage frames
#TODO check that this works - changed for multi country
population2 <- data.frame(matrix(ncol = length(age_groups_model)+4))

# for each vaccine scenario
for(scenario in target_scenarios){
   #  for(scenario in c(1,30,60,90)){
   
   for(virus_type in c("AH1N1", "AH3N2", "B")){
      #specify the date input
      waning_rate <- vaccine_scenarios[[scenario]][["waning_rate"]]
      if(virus_type =="AH1N1"){efficacy <- vaccine_scenarios[[scenario]][["efficacy_H1"]]
      } else if(virus_type =="AH3N2"){efficacy <- vaccine_scenarios[[scenario]][["efficacy_H3"]]
      } else if(virus_type =="B"){efficacy <- vaccine_scenarios[[scenario]][["efficacy_B"]]}
      
      # create store for immunological setup
      prop_v_store <- data.frame(matrix(nrow=num_years,ncol = num_years+1))
      prop_Rv_store <- data.frame(matrix(nrow=num_years,ncol = num_years+1))
      prop_R_store <- data.frame(matrix(nrow=num_years,ncol = num_years+1))
      population <- data.frame(matrix(nrow=num_years,ncol = num_age_groups+3))
      
      for(i in 1:(num_years+1)){
         #proportion vaccinated by age group from the previous year
         if(i ==1 ){prop_vacc_start <- c(rep(0,num_age_groups*(3)))} else {
            prop_vacc_start <- unlist(tail(vaccination_ratio_output,1))
         }
         
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
                        by = 7)
         }
         
         if(location == "Kenya"){
            demography_input <- popken[,i+1]
            target_coverage <-  vaccine_scenarios[[scenario]][["coverage"]]
         }
         if(location == "UK"){
            demography_input <- demography_UK[,(i+1)]
            target_coverage <-  vaccine_scenarios[[scenario]][["coverage"]][i,2:22]
         }
         # Input the relevant coverage
         new_coverage <-  sweep(coverage_timing, 2, target_coverage, "*")
         # Udpate the vaccination calendar with the new inputs
         calendar_input = as_vaccination_calendar(efficacy = c(efficacy[,(i*2)-1]),
                                            dates = as.Date(dates),
                                            coverage = as.data.frame(new_coverage),
                                            no_age_groups = num_age_groups,
                                            no_risk_groups = 3)
         year_to_run <- year(dates[1])
         
         # work out vaccination adjustments based on what proportion of 0-5s vaccinated
         # first year vaccinate everyone
  
         if(year_to_run != years[[1]]){
            # work out the coverage in the 1-5 age group based on which ages vaccinated
            calendar_input$calendar[,] <- sweep(calendar_input$calendar[,], 2,
                                          rep(unlist(vaccine_scenarios[[scenario]]["prop_group_vacc"]),3),
                                          FUN="*")
         }
         
    
         # Model the number of cases for each age group on each run of the ODE function
         vaccination_ratio_output = vacc_model_1(demography_input = demography_input,
                                                      calendar_input = calendar_input,
                                                      waning_rate = waning_rate, 
                                                      vaccination_ratio_input = prop_vacc_start, 
                                                      year_to_run = year_to_run,
                                                      efficacy_NH = c(efficacy[,(i*2)]))
         # extracted vaccination columns
         vaccine_colnames <-names(vaccination_ratio_output)[grepl( "Vaccinated" , names(vaccination_ratio_output))]
         total_vaccinated <- vaccination_ratio_output[, ..vaccine_colnames]
         # change from cumulative to daily
         total_vaccinated[, (vaccine_colnames) := lapply(.SD, function(x) x - shift(x, type = "lag", n=1L)) ]
         total_vaccinated[, Date :=  c(as.Date(paste0(year_to_run,"-03-01")):as.Date(paste0(year_to_run+1,"-03-01")))]
         total_vaccinated[,Scenario := scenario]
         total_vaccinated[is.na(total_vaccinated)] = 0
         
         # input into storage
         if(i ==1 ){
            vaccination_ratio_store <- data.frame(vaccination_ratio_output, 
                                                  "Date" = c(as.Date(paste0(year_to_run,"-03-01")):as.Date(paste0(year_to_run+1,"-03-01")))
                                                  ,"Vacc_scenario" = scenario,
                                                  "virus_type" = virus_type)
            if(virus_type == "AH1N1" & scenario == target_scenarios[1]){
               total_vaccines <- total_vaccinated} else if (virus_type == "AH1N1"){
                  total_vaccines <- rbind(total_vaccines, total_vaccinated)
               }
            
         }else{
            
            vaccination_ratio_store <- rbind(vaccination_ratio_store,
                                             data.frame(vaccination_ratio_output,
                                                        "Date" = c(as.Date(paste0(year_to_run,"-03-01")):as.Date(paste0(year_to_run+1,"-03-01"))),
                                                        "Vacc_scenario" = scenario,
                                                        "virus_type" = virus_type))
            if(virus_type == "AH1N1"){
               total_vaccines <- rbind(total_vaccines, total_vaccinated)}
         }
       
         population2[i,] <- c(stratify_by_age(demography_input, age_groups_model), years[i], scenario, virus_type)
      }
      
      population2 <- na.omit(population2)
      vaccination_ratio_store <- data.table(vaccination_ratio_store)
      vaccination_ratio_store[,week_all := date2ISOweek(as.Date(Date ,origin = "1970-01-01"))]
      vaccination_ratio_store[,week :=  substring(week_all,1, nchar(week_all)-2)]
      
      # extract the value at teh begining of the week (as else get a lot of variables to store!)
      vaccination_ratio_store_weekly <- vaccination_ratio_store[match(unique(vaccination_ratio_store$week),
                                                                      vaccination_ratio_store$week),]
      
      
      if(virus_type =='AH1N1' ){
         vaccination_ratio_store_t <- vaccination_ratio_store_weekly
      }else{
         vaccination_ratio_store_t <- rbind(vaccination_ratio_store_t, vaccination_ratio_store_weekly)
      }
   }
   # input into storage
   print(scenario)
   # the output is proportions at each time point. 
   # so for storage want the first day of the week value
   
   if(scenario == 1){
      vaccination_ratio_store2 <- vaccination_ratio_store_t
   }else{
      vaccination_ratio_store2 <- rbind(vaccination_ratio_store2, vaccination_ratio_store_t)
   }
}

vaccination_ratio_store2 <- data.table(vaccination_ratio_store2)
vaccination_ratio_store3 <-melt.data.table(vaccination_ratio_store2,
                                           id.vars = c("Date", "Vacc_scenario", "virus_type", "week_all", 
                                                                                 "week"))



population2[,1] <- as.numeric(population2[,1])
population2[,2] <- as.numeric(population2[,2])
population2[,3] <- as.numeric(population2[,3])
population2[,4] <- as.numeric(population2[,4])
population2[,5] <- as.numeric(population2[,5])
population2[,6] <- as.numeric(population2[,6])
population2[,7] <- as.numeric(population2[,7])

population_risked <- cbind(
   sweep(population2[,1:7], MARGIN = 2, (1-risk_ratios_input[1,]), "*"), 
   sweep(population2[,1:7], MARGIN = 2, (risk_ratios_input[1,]), "*"), 
   sweep(population2[,1:7], MARGIN = 2, 0, "*"))

# Will have to do it seperately for each of the three ones, given can't just search for eg. 1, as get 1,12,14,etc.

for(i in 1:nrow(age_group_labels)){
   vaccination_ratio_store3[variable ==paste0("Vaccinated",as.character(age_group_labels[i,1])), age_group := age_group_labels[i,2]]
   vaccination_ratio_store3[variable ==paste0("prop_v",as.character(age_group_labels[i,1])), age_group := age_group_labels[i,2]]
   vaccination_ratio_store3[variable ==paste0("prop_Rv",as.character(age_group_labels[i,1])), age_group := age_group_labels[i,2]]
}
for(i in 1:nrow(risk_group_labels)){
   vaccination_ratio_store3[variable ==paste0("Vaccinated",as.character(risk_group_labels[i,1])), risk_group := risk_group_labels[i,2]]
   vaccination_ratio_store3[variable ==paste0("prop_v",as.character(risk_group_labels[i,1])), risk_group := risk_group_labels[i,2]]
   vaccination_ratio_store3[variable ==paste0("prop_Rv",as.character(risk_group_labels[i,1])), risk_group := risk_group_labels[i,2]]
}


vaccination_ratio_store3[grep(pattern="_v", variable), type := "prop_vaccinated"]
vaccination_ratio_store3[grep(pattern="_R", variable), type := "prop_R"]
vaccination_ratio_store3[grep(pattern="_Rv", variable), type := "prop_Rv"]
vaccination_ratio_store3[grep(pattern="Vacc", variable), type := "total_vacc"]

vaccination_ratio_store3 <- data.table(dcast(vaccination_ratio_store3, Vacc_scenario+virus_type+week+week_all+Date+age_group+risk_group~type, value.var = "value"))

vaccination_ratio_store3[,Year := year(as.Date(Date, origin = "1970-01-01"))]


# addd the population sizes for each age group
for(year_temp in 1:length(years)){
   for(agegp in 1:num_age_groups){
      vaccination_ratio_store3[age_group==age_group_labels[agegp,"label"] &
                                  Year == years[year_temp] &
                                  risk_group == "Risk_group1", 
                               population := as.numeric(population2[year_temp,agegp])*(1-high_risk[agegp])]
      vaccination_ratio_store3[age_group==age_group_labels[agegp,"label"] &
                                  Year == years[year_temp] &
                                  risk_group == "Risk_group2", 
                               population := as.numeric(population2[year_temp,agegp])*(high_risk[agegp])]
      vaccination_ratio_store3[age_group==age_group_labels[agegp,"label"] &
                                  Year == years[year_temp] &
                                  risk_group == "Risk_group3", 
                               population := 0]
   }
}



vaccination_ratio_store3[,percent_immune := (((as.numeric(population)*as.numeric(prop_vaccinated)*as.numeric(prop_Rv))/
                                                 as.numeric(population)))*100]
vaccination_ratio_store3[,percent_vacc := (as.numeric(total_vacc)/as.numeric(population))*100]

for(i in 1:length(vaccine_scenarios)){
   
   vaccination_ratio_store3[Vacc_scenario ==i,"waning"] <- vaccine_scenarios[[i]]$waning_rate
   vaccination_ratio_store3[Vacc_scenario ==i,"efficacy1"] <- vaccine_scenarios[[i]]$efficacy_H3[1,1]
   vaccination_ratio_store3[Vacc_scenario ==i,"efficacy2"] <- vaccine_scenarios[[i]]$efficacy_H3[1,7]
   vaccination_ratio_store3[Vacc_scenario ==i,"dates"] <- length(vaccine_scenarios[[i]]$dates)
   vaccination_ratio_store3[Vacc_scenario ==i,"coverage"] <- vaccine_scenarios[[i]]$coverage[1]
   
}
vaccination_ratio_store3[,efficacy := paste0(efficacy1, "_", efficacy2)]
vaccination_ratio_store3[,waning := (1/waning)/365.25]

# used to look up vaccine scenario numbers
unique(vaccination_ratio_store3[waning == 5 & efficacy == "0.9_0.9" &
                                   dates ==1 & coverage == 0.9]$Vacc_scenario)


# Generate plot


vaccination_ratio_store3$age_group <- factor(vaccination_ratio_store3$age_group, levels = c(
   unique(age_group_labels[,"label"])
))


vaccination_ratio_store3[Vacc_scenario == target_scenarios[1], scenario_nice := vaccine_scenario_names[1]]
vaccination_ratio_store3[Vacc_scenario == target_scenarios[2], scenario_nice := vaccine_scenario_names[2]]
vaccination_ratio_store3[Vacc_scenario == target_scenarios[3], scenario_nice := vaccine_scenario_names[3]]
vaccination_ratio_store3[Vacc_scenario == target_scenarios[4], scenario_nice := vaccine_scenario_names[4]]
vaccination_ratio_store3[Vacc_scenario == target_scenarios[5], scenario_nice := vaccine_scenario_names[5]]
vaccination_ratio_store3[Vacc_scenario == target_scenarios[6], scenario_nice := vaccine_scenario_names[6]]


vaccination_ratio_store3$scenario_nice <- factor(vaccination_ratio_store3$scenario_nice, 
                                                 levels = vaccine_scenario_names )


if(no_risk_groups == 1){
   plot_subset <- vaccination_ratio_store3[risk_group %in% c("Risk_group1") &
                                              Vacc_scenario %in% target_scenarios ]
} else if (no_risk_groups ==2){
   plot_subset <- vaccination_ratio_store3[risk_group %in% c("Risk_group1", 
                                                             "Risk_group2") &
                                              Vacc_scenario %in% target_scenarios]
}

plot_subset$Date <- as.Date(plot_subset$Date, origin = "1970-01-01")
   IMMUNITY <- ggplot(plot_subset, aes(x = Date, y =percent_immune, colour= age_group,
                              group = interaction(age_group, virus_type),
                              linetype = virus_type)) +
         geom_line() + 
         facet_grid(scenario_nice~risk_group) +
         labs(y= "Percentage immune through vaccination",
              colour = "Age group", 
              x = "Date", 
              linetype = "Virus") +
         theme_linedraw() +
         theme(axis.text = element_text(size =15),
               axis.title = element_text(size = 15),
               legend.text = element_text(size=15),
               legend.title = element_text(size=15))
   
 tiff(here::here(paste0(name_run,"_immunity.tiff")), height = 2000, width = 3000, res = 300)
 IMMUNITY
 dev.off()
 
 
 tester <- plot_subset
tester <- tester[virus_type == "AH1N1"]
tester2 <- dcast.data.table(tester, Vacc_scenario  + week + week_all + Date + age_group + scenario_nice ~ risk_group, 
      value.var = "total_vacc")
tester2[, Total_vacc := Risk_group1 + Risk_group2]
tester2[,vacc_at_week := Total_vacc - shift(Total_vacc, type = "lag", n=1L), by = c("Vacc_scenario", "age_group")]
tester2[vacc_at_week <0, vacc_at_week :=0]
# plot the percentage of whole population administered a vaccine over the year
VACC_GIVEN_STANDARD <- ggplot(tester2[Date < as.Date("1996-09-01") & Vacc_scenario==2], aes(x = Date, y = vacc_at_week/100000, colour = age_group)) + 
   geom_line() + 
   #facet_grid(.~scenario_nice) + 
   theme_linedraw() + 
   labs(x = "week of year", y = "Weekly number of vaccinations given (100'000s)", colour = "Age group") + 
   theme(axis.title = element_text(size = 12), 
         axis.text = element_text(size = 12), 
         strip.background = element_rect(fill = "azure4"))

total_vaccines[, Date := as.Date(Date, origin = "1970-01-01")]
save(total_vaccines, file = here::here( "UK_output", paste0("Vaccine_model_output_",name_run,".Rdata")))

one_set <- plot_subset[virus_type == "AH1N1",]

one_set[Date < as.Date("1996-09-01"), season := 1995 ]
one_set[Date < as.Date("1997-09-01")  & Date >= as.Date("1996-09-01"), season := 1996 ]
one_set[Date < as.Date("1998-09-01")  & Date >= as.Date("1997-09-01"), season := 1997 ]
one_set[Date < as.Date("1999-09-01")  & Date >= as.Date("1998-09-01"), season := 1998 ]
one_set[Date < as.Date("2000-09-01")  & Date >= as.Date("1999-09-01"), season := 1999 ]
one_set[Date < as.Date("2001-09-01")  & Date >= as.Date("2000-09-01"), season := 2000 ]
one_set[Date < as.Date("2002-09-01")  & Date >= as.Date("2001-09-01"), season := 2001 ]
one_set[Date < as.Date("2003-09-01")  & Date >= as.Date("2002-09-01"), season := 2002 ]
one_set[Date < as.Date("2004-09-01")  & Date >= as.Date("2003-09-01"), season := 2003 ]
one_set[Date < as.Date("2005-09-01")  & Date >= as.Date("2004-09-01"), season := 2004 ]
one_set[Date < as.Date("2006-09-01")  & Date >= as.Date("2005-09-01"), season := 2005 ]
one_set[Date < as.Date("2007-09-01")  & Date >= as.Date("2006-09-01"), season := 2006 ]
one_set[Date < as.Date("2008-09-01")  & Date >= as.Date("2007-09-01"), season := 2007 ]
one_set[Date < as.Date("2009-09-01")  & Date >= as.Date("2008-09-01"), season := 2008 ]

one_set[, given := total_vacc - shift(total_vacc, type = "lag", n=1L), by = c("season", "scenario_nice")]
one_set[given < 0, given := 0]
one_set[ is.na(given), given := 0]

# one_set_c uses years (as in annual) and temp uses seasons

one_set_c <- dcast.data.table(one_set, Vacc_scenario + Year + age_group + scenario_nice  ~ risk_group,
                           value.var = "given", fun.aggregate = sum)
one_set_c[, Vaccinations := Risk_group1 + Risk_group2]

temp <- one_set
temp_c <- dcast.data.table(temp, Vacc_scenario + season + age_group + scenario_nice  ~ risk_group,
                           value.var = "given", fun.aggregate = sum)
temp_c[, Vaccinations := Risk_group1 + Risk_group2]

VACCS_GIVEN <- ggplot(temp_c, aes(x = season, y =Vaccinations/1000000, fill = age_group, group = age_group)) + 
   geom_bar(stat="identity") + 
   facet_grid(. ~ scenario_nice) + 
   theme_linedraw() + 
   labs(y = "Yearly vaccinations given (in millions)", fill = "Age group", title = "b")+
theme(axis.title = element_text(size = 12), 
      axis.text = element_text(size = 12), 
      strip.background = element_rect(fill = "azure4"), 
      axis.text.x = element_text(angle=-90))

