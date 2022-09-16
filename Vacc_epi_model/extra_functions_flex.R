# functions that are still needed 

# this needs to be made flexible for age groups
# proportion of the second age group to be vaccinated...
who_vaccinate <- function(){
  
  waning_years <- 1/(vaccine_scenarios[[scenario]][["waning_rate"]]*365.25)
  if(waning_years %in% c(5,4,3)){prop_group_vacc = 1/5} else 
    if (waning_years %in% c(2)){ prop_group_vacc = 2/5 } else
      if(waning_years %in% c(0.5,1)){prop_group_vacc = 1} else 
       
         # don;t make any adjustments when no vaccination
        if(scenario ==1){prop_group_vacc =1}
  
  return(prop_group_vacc)
}

