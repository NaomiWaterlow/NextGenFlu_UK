# rerun the fluevidence synthesis Baguelin fits. 
# for comparison with those in the adapted model. 
library(grid)
library(gridExtra)
# read in the data from the table
# for each peak want 3 criteria:
  # - peak height
  # - peak duration
  # - number of days where cases above 5


extracted_data <- data.frame(year = c("2003"), 
           age = 1,
           peak_height = 16, 
           peak_date = as.Date(74, origin = "2003-09-01"),
           peak_duration = 64
           )

extracted_data[2,]<- c(2003, 2, 35, as.character(as.Date(75, origin = "2003-09-01")), 87)
extracted_data[3,]<- c(2003, 3, 77, as.character(as.Date(73, origin = "2003-09-01")), 113)
extracted_data[4,]<- c(2003, 4, 27, as.character(as.Date(74, origin = "2003-09-01")), 80)
extracted_data[5,]<- c(2003, 5, 15, as.character(as.Date(75, origin = "2003-09-01")), 63)
extracted_data <- data.table(extracted_data)
extracted_data[,week := week(peak_date)]


extracted_data <- data.frame(year = c("2007"), 
                             age = 1,
                             peak_height = 1, 
                             peak_date = as.Date(94, origin = "2007-10-01"),
                             peak_duration = Inf
)

extracted_data[2,]<- c(2007, 2, 5, as.character(as.Date(91, origin = "2007-10-01")), 38)
extracted_data[3,]<- c(2007, 3, 24, as.character(as.Date(95, origin = "2007-10-01")), 73)
extracted_data[4,]<- c(2007, 4, 6, as.character(as.Date(94, origin = "2007-10-01")), 24)
extracted_data[5,]<- c(2007, 5, 1, as.character(as.Date(94, origin = "2007-10-01")), Inf)
extracted_data <- data.table(extracted_data)
extracted_data[,week := week(peak_date)]


### TJE FORST EYAR


extracted_data <- data.frame(year = c("1995"), 
                             age = 1,
                             peak_height = 48, 
                             peak_date = as.Date(81, origin = "1995-09-01"),
                             peak_duration = 78
)

extracted_data[2,]<- c(1995, 2, 113, as.character(as.Date(76, origin = "1995-09-01")), 89)
extracted_data[3,]<- c(1995, 3, 235, as.character(as.Date(75, origin = "1995-09-01")), 91)
extracted_data[4,]<- c(1995, 4, 81, as.character(as.Date(84, origin = "1995-09-01")), 48)
extracted_data[5,]<- c(1995, 5, 47, as.character(as.Date(86, origin = "1995-09-01")), 81)
extracted_data <- data.table(extracted_data)
extracted_data[,week := ISOweek(peak_date)]



#These are therefore my overal characteristics for the 2003-2004 season. 

# This is the relvant compariosn years. 
ghm_m
# this is the reporting rates
ascertainment_H3 <- data.table(ascertainment_H3)
ascertainment_H1 <- data.table(ascertainment_H1)
asc_sub <- ascertainment_H3[epidemic ==25]
asc_sub <- ascertainment_H1[epidemic ==38]
asc_sub <- ascertainment_H3[epidemic ==1]

asc_sub[,sample := factor(1:posterior_sample_size)]
asc_sub_m <- melt(asc_sub, id.vars=c("flu_type", "epidemic", "sample"))
colnames(asc_sub_m)[4] <- "variable2"
# addit
# age groups 0-14 year
# old, 15-44 year old and 65+ year old.
# link which age groups have which reporting
for_vaccination_m <- for_vaccination_m[scenario ==1,]
for_vaccination_m[variable == "Age1", reporting_rate := 1]
for_vaccination_m[variable == "Age2", reporting_rate := 1]
for_vaccination_m[variable == "Age3", reporting_rate := 1]
for_vaccination_m[variable == "Age4", reporting_rate := 2]
for_vaccination_m[variable == "Age5", reporting_rate := 2]
for_vaccination_m[variable == "Age6", reporting_rate := 2]
for_vaccination_m[variable == "Age7", reporting_rate := 3]
for_vaccination_m[reporting_rate == 1, variable2 := "V1"]
for_vaccination_m[reporting_rate == 2, variable2 := "V2"]
for_vaccination_m[reporting_rate == 3, variable2 := "V3"]

for_vaccination_m[asc_sub_m, on= c("sample", "variable2"), asc := i.value ]
# for(i in 1:nrow(ghm_m)){
#   
#   ghm_m[i,"reported"] <- rbinom(n=1, size = as.numeric(round(ghm_m[i,"value"])), prob = as.numeric(ghm_m[i,"asc"]))
# }
for_vaccination_m[,reported := value*asc ]

# which age groups are combined?
# 1 and 2 | 4 and 5
for_vaccination_m[variable =="Age1", final_grouping := 1]
for_vaccination_m[variable =="Age2", final_grouping := 1]
for_vaccination_m[variable =="Age3", final_grouping := 2]
for_vaccination_m[variable =="Age4", final_grouping := 3]
for_vaccination_m[variable =="Age5", final_grouping := 3]
for_vaccination_m[variable =="Age6", final_grouping := 4]
for_vaccination_m[variable =="Age7", final_grouping := 5]

over_time <- for_vaccination_m[,sum(reported), by =c('sample',"Date","final_grouping" )]
over_time[, week_year := paste0(ISOweek(Date))]
over_time_week <- over_time[,sum(V1), by = c("sample", "final_grouping", "week_year")]
over_time_mean <- over_time_week[,mean(V1), by =c("final_grouping", "week_year")]
over_time_max <- over_time_week[,max(V1), by =c("final_grouping", "week_year")]
over_time_min <- over_time_week[,min(V1), by =c("final_grouping", "week_year")]

# compare peak height
compare_height <- over_time_min[, max(V1)/100, by = "final_grouping"]# as extracted data is in 100s
compare_height$mean <- over_time_mean[, max(V1)/100, by = "final_grouping"]$V1# as extracted data is in 100
compare_height$upper <- over_time_max[, max(V1)/100, by = "final_grouping"]$V1# as extracted data is in 100
colnames(compare_height) <- c("age", "Lower", "Median", "Upper")
compare_height
extracted_data[,c("age", "peak_height")] 
extracted_data$age <- as.numeric(extracted_data$age)
compare_height[extracted_data, on = "age", Baguelin := peak_height ]
compare_height[age == 1, Age := "0 - 4 y"]
compare_height[age == 2, Age := "5 - 14 y"]
compare_height[age == 3, Age := "15 - 44 y"]
compare_height[age == 4, Age := "45 - 64 y"]
compare_height[age == 5, Age := "65+ y"]

compare_height[, age := NULL]

compare_height <- compare_height[,c(5,4,2,1,3)]
compare_height[,2:5] <- round(compare_height[,2:5])


table_test <- tableGrob(compare_height, rows = NULL)
table_title <- textGrob("B:", gp = gpar(fontsize = 15))
padding <- unit(5, "mm")
 table_here <- gtable_add_rows(
   table_test, 
   heights = grobHeight(table_title) + padding, 
   pos= 0
 )
 table_here <- gtable_add_grob(table_here, 
                               table_title,
                              t =1, l = 1, 
                              b = 1, r = 1)

# compare peak week
over_time_mean[ V1 %in% over_time_mean[, max(V1), by = "final_grouping"]$V1]
extracted_data[,c("age", "week")]

over_time_mean[over_time, on=c("week_year"), Date := i.Date]
over_time_week[over_time, on=c("week_year"), Date := i.Date]

over_time_mean[over_time_min, on="week_year", lower := i.V1]
over_time_mean[over_time_max, on="week_year", upper := i.V1]

# # compare duration - need to do this visually because of the weird kinks

ggplot(over_time_week, aes(x = Date, y = V1/100, group = sample)) + 
  geom_line(alpha = 0.25) + 
  facet_grid(final_grouping~., scale="free_y") + 
  theme_linedraw() + 
  labs(title = "Standard")

over_time_week[, mean := mean(V1), by=c("Date", "final_grouping")]
over_time_week[, median := median(V1), by=c("Date", "final_grouping")]
over_time_week[, p0.25 := quantile(V1, probs = 0.25), by=c("Date", "final_grouping")]
over_time_week[, p0.75 := quantile(V1, probs =0.75), by=c("Date", "final_grouping")]
over_time_week[, p0.025 := quantile(V1, probs = 0.025), by=c("Date", "final_grouping")]
over_time_week[, p0.975 := quantile(V1, probs =0.975), by=c("Date", "final_grouping")]


over_time_week[final_grouping == 1, Age := "0 - 4 y"]
over_time_week[final_grouping == 2, Age := "5 - 14 y"]
over_time_week[final_grouping == 3, Age := "15 - 44 y"]
over_time_week[final_grouping == 4, Age := "45 - 64 y"]
over_time_week[final_grouping == 5, Age := "65+ y"]

over_time_week$Age <- factor(over_time_week$Age, 
                                        levels = c("0 - 4 y","5 - 14 y"
                                        ,"15 - 44 y","45 - 64 y","65+ y"))


COMP_PLOT <- ggplot(over_time_week, aes(x = Date, y = median/100, group = sample)) + 
  geom_ribbon(aes(ymin = p0.025/100, ymax = p0.975/100), alpha =0.05, fill = "purple") +
  geom_ribbon(aes(ymin = p0.25/100, ymax = p0.75/100), alpha =0.5, fill = "purple") + 
  geom_line() + 
  facet_grid(Age~., scale="free_y") + 
  theme_linedraw() + 
  lims(x = c(as.Date("1995-08-28"),as.Date("1996-06-01") )) + 
  labs(y = "Number of positive ILI cases (in hundreds)", title = "A: 1995 H3N2") 

tiff(filename = "1995AH3N2.tiff", height = 2000, width = 3000, res = 300)

grid.arrange(COMP_PLOT, table_here, layout_matrix = rbind(c(1,1,1,2,2), 
                                                          c(1,1,1,2,2)))

dev.off()





posterior_subset$sample <- c(1:100)
posterior_subset <- data.table(posterior_subset)
posterior_subset$sample <- as.factor(posterior_subset$sample)
over_time_week[posterior_subset, on = "sample", sus1 := i.V6 ]
over_time_week[, sus1_lab := "low"]
over_time_week[sus1 >= 0.15, sus1_lab := "high"]

over_time_week[posterior_subset, on = "sample", trans := i.V5 ]
over_time_week[, sus1_lab := "low"]
over_time_week[trans > 0.18, sus1_lab := "high"]


over_time_week[, combo := sus1/trans]
over_time_week[, combo_lab := "low"]
over_time_week[combo > 0.75, combo_lab := "high"]


ggplot(over_time_week, aes(x = Date, y = V1/100, group = sample)) + 
  geom_line(alpha = 0.25) + 
  facet_grid(sus1_lab~final_grouping, scale="free_y") + 
  theme_linedraw()



# 6,7 and 8 are susceptibilities
# 5 is transmissibility
# initial infected = 10^parameter9
# 1,2,3 are ascertainments
#4? could be outside infection?

# extracted_data[,c("age", "week")]
# ggplot(over_time_mean, aes(x = Date, y = V1/100)) + 
#   geom_line() + 
# facet_grid(final_grouping~.) + 
#   geom_line(aes(y = lower/100), colour = "orange")+
#   geom_line(aes(y = upper/100), colour = "blue")


