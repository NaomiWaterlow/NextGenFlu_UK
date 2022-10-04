# Discounting and combining all the economics

# calculate years since base
annual_nondeath_outcomes[,discounting_years := as.numeric(Year) - 1995]
annual_nondeath_outcomes[, discounted_rate := (1-discount_rate) ^ discounting_years]
annual_nondeath_outcomes[, discounted_QALYS := QALYS*discounted_rate]

annual_costs[,discounting_years := as.numeric(Year) - 1995]
annual_costs[, discounted_rate := (1-discount_rate) ^ discounting_years]
annual_costs[, discounted_costs := total_costs*discounted_rate]

sum_QALYS <- annual_nondeath_outcomes[, sum(discounted_QALYS), by = c("sample", "scenario", "Year")]
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

base_scenario <- summary_stats[scenario==1]
summary_stats[base_scenario, on = "sample", base_QALYS := i.discounted_QALYS ]
summary_stats[base_scenario, on = "sample", base_costs := i.discounted_costs ]

summary_stats[,incremental_qalys := discounted_QALYS - base_QALYS]
summary_stats[,incremental_costs := discounted_costs - base_costs]

summary_stats[, icer := incremental_costs/incremental_qalys]

ggplot(summary_stats, aes(x = scenario, y = icer)) + 
  geom_boxplot()



