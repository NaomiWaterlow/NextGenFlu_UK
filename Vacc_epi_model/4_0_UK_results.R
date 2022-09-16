# UK look at results

total_cases_time[scenario ==1, scenario_nice := vaccine_scenario_names[1]]
total_cases_time[scenario ==2, scenario_nice := vaccine_scenario_names[2]]
total_cases_time[scenario ==3, scenario_nice := vaccine_scenario_names[3]]
total_cases_time[scenario ==4, scenario_nice := vaccine_scenario_names[4]]
total_cases_time[scenario ==5, scenario_nice := vaccine_scenario_names[5]]
total_cases_time[scenario ==6, scenario_nice := vaccine_scenario_names[6]]

total_cases_time$scenario_nice <- factor(total_cases_time$scenario_nice, 
                                         levels = vaccine_scenario_names)

#total_cases_time[, total_cases := X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14]

ggplot(total_cases_time, aes(x = Date, y = total_cases, group = sample, colour = scenario)) + 
  geom_line() + 
  facet_grid(scenario_nice~Virus) + 
  scale_colour_manual(values = c("#d73027","orange1", "#91CF60", "#92C5DE", "#3288BD","purple" )) +
  theme_linedraw() + 
  theme(legend.position = "none")


summary_table <- total_cases_time[,sum(total_cases), by = c("Date", "scenario","week", "scenario_nice", "sample")]
summary_table[,cumulative_sum := cumsum(V1), by = c("scenario", "scenario_nice", "sample")]
summary_table2 <- summary_table[,quantile(cumulative_sum, 0.5), by = c("Date", "scenario","week", "scenario_nice")]
summary_table2$upper <- summary_table[,quantile(cumulative_sum, 0.975), by = c("Date", "scenario","week", "scenario_nice")]$V1
summary_table2$lower <- summary_table[,quantile(cumulative_sum, 0.025), by = c("Date", "scenario","week", "scenario_nice")]$V1


SUMMARY <- ggplot(summary_table2, aes(x = as.Date(Date, origin = "1970-01-01"))) + 
  geom_line(aes(y = V1)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper,
                  fill= scenario_nice), alpha = 0.5)+
  facet_grid(.~scenario_nice) + 
  theme_bw() +
  scale_fill_manual(values = c("#d73027","orange1", "#91CF60", "#92C5DE", "#3288BD","purple" )) +
  labs(x = "Date", y = "Cumulative Infections", fill = "Vaccine", title = "B") + 
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12), 
        legend.text =  element_text(size = 10), 
        axis.text.x = element_text(angle = -90, hjust = 1), 
        legend.title = element_text(size = 12), 
        strip.background = element_blank(),
        strip.text.x = element_blank())

save(total_cases_time, file = here::here("UK_output", "total_cases_time.Rdata"))
