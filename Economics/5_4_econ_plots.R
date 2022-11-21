# plots etc. for economics
# this calculates the threshold prices of the different vaccines. 

# Need the INMB to be 0. 

# check that the incremental is compared to number 2 not number 1
base_scenario_to_use
# check the threshold value
threshold <- 20000

min_vacc_cost <- function(x, sample_no, scenario_no){
  

  delivery <- x+(9.64/inflator)
  temp_annual_costs <- annual_costs[sample == sample_no & scenario == scenario_no]
  temp_annual_costs[, t_vacc_costs := vaccines_given * delivery]
  temp_annual_costs[, t_total_costs := t_vacc_costs + outcome_costs]

  temp_annual_costs[, t_discounted_costs := t_total_costs*discounted_rate]
  
  t_sum_costs <- temp_annual_costs[, sum(t_discounted_costs), by = c("sample", "scenario")]
  t_sum_costs$scenario <- factor(t_sum_costs$scenario)
  
  temp_summary_stats <- summary_stats[sample == sample_no & scenario == scenario_no]
  
  temp_summary_stats[t_sum_costs, on = c("sample", "scenario"), t_discounted_costs := i.V1]
  
  temp_summary_stats[,t_incremental_costs := t_discounted_costs - base_costs]
  temp_summary_stats[, t_INMB :=  ((-incremental_qalys *threshold) - t_incremental_costs)/1000000]

  # want the t_INMB to be 0. 
  tot_dif <- (temp_summary_stats[, sum(t_INMB)])^2
  return(tot_dif)
}


vaccine_thresholds <- matrix(ncol = 3, nrow = 5*n_samples)

if(base_scenario_to_use == 2){
  
  for(scen in 2:6){
    
    for(sam in 1:n_samples){
      
      vaccine_price <- optim(par = c(5), 
                             fn = min_vacc_cost,
                             method = "Brent",
                             lower = 0, 
                             upper= 1500,
                             sample_no = sam, 
                             scenario_no = scen)$par
      
      vaccine_thresholds[(n_samples*(scen-2))+sam,] <- c(sam, scen, vaccine_price)
      
    }
  }
}

vaccine_thresholds <- data.table(vaccine_thresholds)
colnames(vaccine_thresholds) <- c("sample", "scenario", "vaccine_price")


vaccine_thresholds <- vaccine_thresholds[,quantile(vaccine_price, probs = c(0.025, 0.5, 0.975)), by = c("scenario")]
vaccine_thresholds$type <- rep(c("lower", "median", "upper"), 5)

vaccine_thresholds_c <- dcast.data.table(vaccine_thresholds, scenario~ type, value.var = "V1")

vaccine_thresholds_c[scenario ==1, Scenario := vaccine_scenario_names[1]]
vaccine_thresholds_c[scenario ==2, Scenario := vaccine_scenario_names[2]]
vaccine_thresholds_c[scenario ==3, Scenario := vaccine_scenario_names[3]]
vaccine_thresholds_c[scenario ==4, Scenario := vaccine_scenario_names[4]]
vaccine_thresholds_c[scenario ==5, Scenario := vaccine_scenario_names[5]]
vaccine_thresholds_c[scenario ==6, Scenario := vaccine_scenario_names[6]]

vaccine_thresholds_c <- vaccine_thresholds_c[,c(5,2,3,4)]


print("vaccine price threshold are ")
print(vaccine_thresholds_c)
vaccine_thresholds_c[,2:4] <- round(vaccine_thresholds_c[,2:4],1)

table_test <- tableGrob(vaccine_thresholds_c, rows = NULL)
table_title <- textGrob("C: Vaccine Threshold prices (£)", gp = gpar(fontsize = 12))
padding <- unit(5, "mm")
table_here2 <- gtable_add_rows(
  table_test, 
  heights = grobHeight(table_title) + padding, 
  pos= 0
)

table_here2 <- gtable_add_grob(table_here2, 
                              table_title,
                              t =1, l = 1, 
                              b = 1, r = 4)

tiff(filename = here::here(paste0(name_run,"_ECONOMICS.tiff")), height = 2000, width = 3000, res = 300)


grid.arrange(PLANE, INMBS, table_here2, layout_matrix = rbind(c(2,2,2,1,1,1,1),
                                                              c(2,2,2,1,1,1,1),
                                                              c(3,3,3,3,3,3,3)))
dev.off()






