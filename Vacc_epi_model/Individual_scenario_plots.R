# individual scenario plots

FOI_summary[coverage == 0.5 & efficacy == "0.7_0.7"& dates == 2 & waning == 3 ]


scenario_to_plot <- c(1,4,75,77,122)

vaccination_ratio_store3$age_group <- factor(vaccination_ratio_store3$age_group, levels = c(
  "Age0", "Age1-5", "Age6-14", "Age15-19", "Age20-49", "Age50+"
))
plot_subset <- vaccination_ratio_store3[Vacc_scenario %in% c(scenario_to_plot)]
plot_subset$Date <- as.Date(plot_subset$Date, origin = "1970-01-01")

IMMUNITY <- ggplot(plot_subset, aes(x = Date, y =percent_immune, colour= age_group,
                              group = interaction(age_group, virus_type),
                              linetype = virus_type)) +
  #scale_colour_manual(values = c("#c6dbef", "#9ecae1", "#6baed6", "#3182bd","#08519c", "navyblue"))+
        geom_line() + 
        facet_grid(Vacc_scenario~.) +
        labs(y= "Percentage immune",
             colour = "Age group", 
             x = "Date", 
             linetype = "Virus", 
             title = "A") +
        theme_linedraw()# +
        # theme(axis.text = element_text(size =15),
        #       axis.title = element_text(size = 15),
        #       legend.text = element_text(size=15),
        #       legend.title = element_text(size=15)) 



total_cases_time_temp <- total_cases_time[scenario %in% c(1,4,83,85,134)]
total_cases_time_temp <- as.data.frame(total_cases_time_temp)

labels_year <- c(
  `1` = "2010 - AH3N2",
  `2` = "2011 - B",
  `3` = "2011 - B",
  `4` = "2012 - AH3N2",
  `5` = "2013 - B",
  `6` = "2014 - AH1N1",
  `7` = "2016 - B",
  `8` = "2016 - AH3N2",
  `9` = "2018 - B",
  `10` = "2018 - AH1N1",
  `11` = "2018 - AH3N2")

EPIDEMICS_IND <-ggplot(total_cases_time_temp,aes(x = Date, y = total_cases, group =interaction(sample, scenario),
                                                 colour = scenario)) +
  geom_line(alpha = 0.5) + 
  facet_grid(~epidemic, scales = "free_x", labeller = as_labeller(labels_year)) +
  theme_linedraw() + 
  labs(x = "Date", y = "Total cases", 
       colour = "Scenario", title  = "D")+ 
#scale_colour_manual(values = c("steelblue2", "navyblue")) +
  scale_colour_manual(values = c("#d73027","#fc8d59", "orange1", "#91cf60", "#1a9850") )+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
  # theme(axis.title = element_text(size = 12), 
  #       axis.text = element_text(size = 12))


all_combined_sub <- all_combined[Vacc_scenario == scenario_to_plot]

SURVIVAL <- ggplot(all_combined_sub, aes(x = Date, y = cumulative)) +
  geom_line() + theme_linedraw() +
  labs(x = "Date", y = "Cumulative infections", title  = "B") + 
  # theme(axis.title = element_text(size = 12), 
  #       axis.text = element_text(size = 12)) + 
  geom_ribbon(aes(ymin=cum_upper,ymax=cum_lower), alpha = 0.3, colour = NA)


#TODO

FOI_summary[, ]

FOI_summary_sub <- FOI_summary[c(3, scenario_to_plot),]
FOI_summary_sub[, total_FOI := AH3N2 + AH1N1 + FluB]
FOI_summary_sub <- FOI_summary_sub[, c("Vacc_scenario", "epidemic", "total_FOI")]
to_plot <- melt.data.table(FOI_summary_sub, id.vars = "Vacc_scenario")
to_plot$Vacc_scenario <- as.factor(to_plot$Vacc_scenario)
to_plot$nice_names <- "NextGen"
to_plot[Vacc_scenario == 3, nice_names := "Base"]
to_plot[variable == "total_FOI", variable := "inter-epi"]


RATIOS <- ggplot(to_plot, aes(x = nice_names, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position="dodge") +
  theme_linedraw() + 
  scale_fill_manual(values = c("navyblue", "steelblue2"))+
  labs( x = "Scenario", y = "Infections", fill = "type", title = "C")



grid.arrange(IMMUNITY, EPIDEMICS_IND, SURVIVAL, RATIOS, 
             layout_matrix = rbind(c(1,1,1,3),
                                   c(4,2,2,2)))






