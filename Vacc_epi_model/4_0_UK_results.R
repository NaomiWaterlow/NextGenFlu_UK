# UK look at results

library(cowplot)
n_samples <- posterior_sample_size
total_cases_time[scenario ==1, scenario_nice := vaccine_scenario_names[1]]
total_cases_time[scenario ==2, scenario_nice := vaccine_scenario_names[2]]
total_cases_time[scenario ==3, scenario_nice := vaccine_scenario_names[3]]
total_cases_time[scenario ==4, scenario_nice := vaccine_scenario_names[4]]
total_cases_time[scenario ==5, scenario_nice := vaccine_scenario_names[5]]
total_cases_time[scenario ==6, scenario_nice := vaccine_scenario_names[6]]

total_cases_time$scenario_nice <- factor(total_cases_time$scenario_nice, 
                                         levels = vaccine_scenario_names)

#total_cases_time[, total_cases := X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14]
total_cases_time_sub <- total_cases_time[sample %in% seq(1,n_samples, n_samples/100),]
 DETAIL_EPIS <- ggplot(total_cases_time_sub, aes(x = Date, y = total_cases, group = sample, colour = scenario)) + 
  geom_line() + 
  facet_grid(scenario_nice~Virus) + 
  scale_colour_manual(values = c("#d73027","orange1", "#91CF60", "#92C5DE", "#3288BD","purple" )) +
  theme_linedraw() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = -90), 
        strip.text.y = element_text(size = 7)) + 
  labs(x = "", y = "Infections")

 tiff(here::here(paste0(name_run,"_DETAILED_TIMELINE.tiff")),  width = 3250, height = 2000, res = 300, )
DETAIL_EPIS
 dev.off()
 
 
summary_table <- total_cases_time[,sum(total_cases), by = c("Date", "scenario","week", "scenario_nice", "sample")]
summary_table[,cumulative_sum := cumsum(V1), by = c("scenario", "scenario_nice", "sample")]
summary_table2 <- summary_table[,quantile(cumulative_sum, 0.5), by = c("Date", "scenario","week", "scenario_nice")]
summary_table2$upper <- summary_table[,quantile(cumulative_sum, 0.975), by = c("Date", "scenario","week", "scenario_nice")]$V1
summary_table2$lower <- summary_table[,quantile(cumulative_sum, 0.025), by = c("Date", "scenario","week", "scenario_nice")]$V1





SUMMARY <- ggplot(summary_table2, aes(x = as.Date(Date, origin = "1970-01-01"))) + 
  geom_line(aes(y = V1/1000000)) + 
  geom_ribbon(aes(ymin = lower/1000000, ymax = upper/1000000,
                  fill= scenario_nice), alpha = 0.5)+
  facet_grid(.~scenario_nice) + 
  theme_bw() +
  scale_fill_manual(values = c("#d73027","orange1", "#91CF60", "#92C5DE", "#3288BD","purple" )) +
  labs(x = "Date", y = "Cumulative Infections (in millions)", fill = "Vaccine", title = "a") + 
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 11), 
        strip.text = element_text(size = 12), 
        legend.text =  element_text(size = 11), 
        axis.text.x = element_text(angle = -90, vjust = 0.5), 
        legend.title = element_text(size = 12), 
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        legend.key.size = unit(0.5, "cm"))

legend1 <- get_legend(SUMMARY)

# VACCS_GIVEN <- ggplot(one_set_c, aes(x = Date, y =Vaccinations)) + 
#   geom_line(aes(colour = age_group)) + 
#   theme_linedraw() + 
#   labs(y = "Total vaccinations given", 
#        title = "B", 
#        colour = "Age group") + 
#   theme(axis.title = element_text(size = 12), 
#         axis.text = element_text(size = 11), 
#         strip.text = element_text(size = 9), 
#         legend.text =  element_text(size = 11), 
#         axis.text.x = element_text(angle = -90, vjust = 0.5), 
#         legend.title = element_text(size = 12), 
#         legend.key.size = unit(0.5, "cm"),
#        panel.spacing = unit(0.5, "lines"),
#        strip.background =element_rect(fill="gray31")) +
#   facet_grid(. ~ scenario_nice) 
  
legend2 <- get_legend(VACCS_GIVEN)

tiff(here::here(paste0(name_run,"_INFECTIONS_VACCINATIONS.tiff")),  width = 3500, height = 2000, res = 300, )
grid.arrange(SUMMARY + theme(legend.position = "NONE"),legend1,
             VACCS_GIVEN+ theme(legend.position = "NONE"), legend2,  layout_matrix = 
               rbind(c(1,1,1,1,2), 
                     c(3,3,3,3,4)))
dev.off()


save(total_cases_time, file = here::here("UK_output", paste0(name_run,"_total_cases_time.Rdata")))

summary_for_text <- total_cases_time[,sum(total_cases),
                                     by = c("sample", "scenario", "scenario_nice")]
base_summary <- summary_for_text[scenario == base_scenario_to_use]
summary_for_text[base_summary, on = c("sample" ), base_total := i.V1 ]
summary_for_text[ ,percent_reduc := round(((base_total - V1) / base_total)*100, digits = 1)]
print("% of infections averted")
print(summary_for_text[, quantile(percent_reduc, probs = c(0.025, 0.5, 0.975)), by = c("scenario_nice")])

temp_c[is.na(Vaccinations), Vaccinations := 0]
total_vacc <- temp_c[, sum(Vaccinations), by = "Vacc_scenario"]

print("total vaccines given (millions)")
print(total_vacc$V1/1000000)
print(paste0("ratio of scenarios 4 and 6 is ", total_vacc[4,"V1"]/
               total_vacc[6,"V1"]))

colnames(total_vacc)[1] <- "scenario"
total_vacc$scenario <- factor(total_vacc$scenario)
summary_for_text[total_vacc, on = "scenario", tot_vacc := i.V1]
summary_for_text[, extra_averted :=  (base_total-V1)/tot_vacc ]

print("Extra cases averted per vaccine, compared to current scenario")
print("min")
print(summary_for_text[, min(extra_averted), by = "scenario"])
print(summary_for_text[, max(extra_averted), by = "scenario"])


