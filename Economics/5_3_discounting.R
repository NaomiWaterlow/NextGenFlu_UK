# Discounting and combining all the economics

# calculate years since base
annual_nondeath_outcomes[,discounting_years := as.numeric(as.character(Year)) - 1995]
annual_nondeath_outcomes[, discounted_rate := (1+qaly_discount_rate) ^ -discounting_years]
annual_nondeath_outcomes[, discounted_QALYS := QALYS*discounted_rate]

annual_costs[,discounting_years := as.numeric(as.character(Year)) - 1995]
annual_costs[, discounted_rate := (1-discount_rate) ^ discounting_years]
annual_costs[, discounted_costs := total_costs*discounted_rate]

deaths_summarised[,discounting_years := as.numeric(as.character(Year)) - 1995 ]
deaths_summarised[,discounted_rate := (1-qaly_discount_rate) ^ discounting_years ]
deaths_summarised[, discounted_QALYS := death_QALYS*discounted_rate]

sum_QALYS <- annual_nondeath_outcomes[, sum(discounted_QALYS), by = c("sample", "scenario", "Year")]
colnames(sum_QALYS)[4] <- "non_death_QALYS"
sum_QALYS_d <- deaths_summarised[, sum(discounted_QALYS), by = c("sample", "scenario", "Year")]
sum_QALYS[sum_QALYS_d, on = c("sample", "scenario", "Year"), death_QALYS := i.V1]
sum_QALYS[, total_QALYS := non_death_QALYS + death_QALYS]

sum_costs <- annual_costs[, sum(discounted_costs), by = c("sample", "scenario", "Year")]

NON_DEATH_QALYS <- ggplot(sum_QALYS, 
       aes(x = Year, y = V1, colour = scenario)) + 
  geom_point() + 
  facet_grid(scenario~.) + 
  labs(y = "discounted non-death QALYS lost") + 
  theme_linedraw()

NON_DEATH_COSTS <- ggplot(sum_costs, 
                          aes(x = Year, y = V1, colour = scenario)) + 
  geom_point() + 
  facet_grid(scenario~.) + 
  labs(y = "discounted costs") + theme_linedraw()

summary_stats <- annual_nondeath_outcomes[, sum(discounted_QALYS), by = c("sample", "scenario")]
colnames(summary_stats)[3] <- "discounted_QALYS"
summary_stats$discounted_costs <- annual_costs[, sum(discounted_costs), by = c("sample", "scenario")]$V1

base_scenario <- summary_stats[scenario==base_scenario_to_use]
summary_stats[base_scenario, on = "sample", base_QALYS := i.discounted_QALYS ]
summary_stats[base_scenario, on = "sample", base_costs := i.discounted_costs ]

summary_stats[,incremental_qalys := discounted_QALYS - base_QALYS]
summary_stats[,incremental_costs := discounted_costs - base_costs]

summary_stats[, icer := incremental_costs/incremental_qalys]
summary_stats[, icer_gained := incremental_costs/-incremental_qalys]

summary_stats[scenario == 1, scenario_name := vaccine_scenario_names[1]]
summary_stats[scenario == 2, scenario_name := vaccine_scenario_names[2]]
summary_stats[scenario == 3, scenario_name := vaccine_scenario_names[3]]
summary_stats[scenario == 4, scenario_name := vaccine_scenario_names[4]]
summary_stats[scenario == 5, scenario_name := vaccine_scenario_names[5]]
summary_stats[scenario == 6, scenario_name := vaccine_scenario_names[6]]
summary_stats$scenario_name <- factor(summary_stats$scenario_name, 
                                      levels = vaccine_scenario_names)

if(base_scenario_to_use ==1){
ICERS <- ggplot(summary_stats[!(scenario %in% c(1))], aes(x = scenario_name, y = icer_gained, fill = scenario_name)) + 
  geom_boxplot() + 
  theme_linedraw() +
  labs(x = "Scenario", y = "ICER (Inc cost per Inc QALY gained)") + 
  scale_fill_manual(values = c("orange1", "#91CF60", "#92C5DE", "#3288BD","purple" ))
} else if(base_scenario_to_use > 1){
  ICERS <- ggplot(summary_stats[!(scenario %in% c(1,2))], aes(x = scenario_name, y = icer_gained, fill = scenario_name)) + 
    geom_boxplot() + 
    theme_linedraw() +
    labs(x = "Scenario", y = "ICER (Inc cost per Inc QALY gained)") + 
    scale_fill_manual(values = c( "#91CF60", "#92C5DE", "#3288BD","purple" ))
} 


# now do the threshold stuff

threshold <- 20000

summary_stats[, INMB :=  ((-incremental_qalys *threshold) - incremental_costs)/1000000]

INMBS <- ggplot(summary_stats[scenario_name != vaccine_scenario_names[1],], aes(x = scenario_name, y = INMB, fill = scenario_name)) + 
  geom_boxplot() + 
  theme_linedraw() + 
  labs(x = "Scenario", y = "INMB (in millions)", title="a") + 
  scale_fill_manual(values = c("orange1", "#91CF60", "#92C5DE", "#3288BD","purple" )) + 
  theme(legend.position = "None", 
        axis.text.x = element_text(angle = -90), 
        axis.title.x = element_blank()) + 
  scale_x_discrete(labels = function(y) str_wrap(y, width = 10))

summary_stats[, quantile(INMB, probs = c(0.025, 0.5, 0.975)), by = "scenario_name"]
summary_stats[, quantile(icer_gained, probs = c(0.025, 0.5, 0.975), na.rm =T), by = "scenario_name"]
summary_stats[, quantile(discounted_QALYS, probs = c(0.025, 0.5, 0.975), na.rm =T), by = "scenario_name"]

ICERS
INMBS

PLANE <- ggplot(summary_stats[!(scenario %in% c(1))], 
       aes(x = -incremental_qalys/1000000, y = incremental_costs/1000000, colour = scenario_name)) + 
  geom_point() + 
  theme_linedraw() + 
  labs(x = "Incremental QALYS gained (millions)", y = "Incremental costs (millions)", 
       colour = "Scenario", title = "b") + 
  scale_colour_manual(values = c("orange1", "#91CF60", "#92C5DE", "#3288BD","purple" ))



inmbs <- summary_stats[, quantile(INMB, probs = c(0.025, 0.5, 0.975)), by = "scenario"]  
inmbs$type <- rep(c("lower", "median", "upper"), 6)
inmbs <- dcast.data.table(inmbs, scenario~ type, value.var = "V1")
print("INMBs")
print(inmbs)

icers <- summary_stats[, quantile(icer_gained, probs = c(0.025, 0.5, 0.975), na.rm = T), by = "scenario"]  
icers$type <- rep(c("lower", "median", "upper"), 6)
icers <- dcast.data.table(icers, scenario~ type, value.var = "V1")
print("icers")
print(icers)

hospitalisations <- annual_nondeath_outcomes[variable.1 == "f_hosp", quantile(V1, probs = c(0.025,0.5, 0.975)), by = c("scenario") ]
hospitalisations$type <- rep(c("lower", "median", "upper"), 6)
hospitalisations <- dcast.data.table(hospitalisations, scenario~ type, value.var = "V1")
print("hospitalisations")
print(hospitalisations)
annual_nondeath_outcomes[variable.1 == "f_hosp", min(V1), by = c("scenario") ]
annual_nondeath_outcomes[variable.1 == "f_hosp", max(V1), by = c("scenario") ]


deathys <- deaths_summarised[, quantile(V1, probs = c(0.025,0.5,0.975)), by = c("scenario") ]
deathys$type <- rep(c("lower", "median", "upper"), 6)
deathys <- dcast.data.table(deathys, scenario~ type, value.var = "V1")
print("deaths")
print(deathys)


thresholds_to_run <- seq(0,100000, by = 5000)

for(i in 1:length(thresholds_to_run)){
  
  target_threshold <- thresholds_to_run[i]
  
  summary_stats[, threshold_target :=  ((-incremental_qalys *target_threshold) - incremental_costs)]
  
  summary_stats[ threshold_target < 0, threshold_INMB := 0]
  summary_stats[ threshold_target >= 0, threshold_INMB := 1]
  
  print(summary_stats[, sum(threshold_INMB), by = c("scenario")])
  
}


# summary_stats[, sum(get())]


INMBS <- ggplot(summary_stats[scenario_name != vaccine_scenario_names[1],], aes(x = scenario_name, y = INMB, fill = scenario_name)) + 
  geom_boxplot() + 
  theme_linedraw() + 
  labs(x = "Scenario", y = "INMB (in millions)", title="a") + 
  scale_fill_manual(values = c("orange1", "#91CF60", "#92C5DE", "#3288BD","purple" )) + 
  theme(legend.position = "None", 
        axis.text.x = element_text(angle = -90), 
        axis.title.x = element_blank()) + 
  scale_x_discrete(labels = function(y) str_wrap(y, width = 10))










