# List of the vccination scenarios required

# NOTE: All the dates are releative to a year start date of 2009-01-01

# specify the number of days per year (leap years!)
year_days <- c(365,366,365,365,365,366,365,365,365,366,365,365,365,366,365,365,
               365,366,365,365,365,366,365,365,365,366,365,365,365,366,365)

#pre09 currently made up


H1_matches <- c(

  "1995_NH" = "MATCH",
  "1996_SH" = "MATCH",
  "1996_NH" = "NOMATCH",
  "1997_SH" = "NOMATCH",
  "1997_NH" = "MATCH",
  "1998_SH" = "MATCH",
  "1998_NH" = "NOMATCH",
  "1999_SH" = "NOMATCH",
  "1999_NH" = "NOMATCH",
  "2000_SH" = "NOMATCH",
  "2000_NH" = "MATCH",
  "2001_SH" = "MATCH",
  "2001_NH" = "MATCH",
  "2002_SH" = "MATCH",
  "2002_NH" = "MATCH",
  "2003_SH" = "MATCH",
  "2003_NH" = "MATCH",
  "2004_SH" = "MATCH",
  "2004_NH" = "MATCH",
  "2005_SH" = "MATCH",
  "2005_NH" = "MATCH",
  "2006_SH" = "MATCH",
  "2006_NH" = "MATCH",
  "2007_SH" = "MATCH",
  "2007_NH" = "MATCH",
  "2008_SH" = "MATCH",
  "2008_NH" = "MATCH",
  "2009_SH" = "MATCH",
  "2009_NH" = NA, 
  "2010_SH" = NA
)

H3_matches <- c(

  "1995_NH" = "NOMATCH",
  "1996_SH" = "NOMATCH",
  "1996_NH" = "MATCH",
  "1997_SH" = "MATCH",
  "1997_NH" = "NOMATCH",
  "1998_SH" = "NOMATCH",
  "1998_NH" = "MATCH",
  "1999_SH" = "MATCH",
  "1999_NH" = "MATCH",
  "2000_SH" = "MATCH",
  "2000_NH" = "NOMATCH",
  "2001_SH" = "NOMATCH",
  "2001_NH" = "MATCH",
  "2002_SH" = "MATCH",
  "2002_NH" = "MATCH",
  "2003_SH" = "MATCH",
  "2003_NH" = "NOMATCH",
  "2004_SH" = "NOMATCH",
  "2004_NH" = "NOMATCH",
  "2005_SH" = "NOMATCH",
  "2005_NH" = "MATCH",
  "2006_SH" = "MATCH",
  "2006_NH" = "MATCH",
  "2007_SH" = "MATCH",
  "2007_NH" = "MATCH",
  "2008_SH" = "MATCH",
  "2008_NH" = "MATCH",
  "2009_SH" = "MATCH",
  "2009_NH" = NA, 
  "2010_SH" = NA
)

B_matches <- c(

  "1995_NH" = "NOMATCH",
  "1996_SH" = "NOMATCH",
  "1996_NH" = "NOMATCH",
  "1997_SH" = "NOMATCH",
  "1997_NH" = "NOMATCH",
  "1998_SH" = "NOMATCH",
  "1998_NH" = "NOMATCH",
  "1999_SH" = "NOMATCH",
  "1999_NH" = "NOMATCH",
  "2000_SH" = "MATCH",
  "2000_NH" = "NOMATCH",
  "2001_SH" = "NOMATCH",
  "2001_NH" = "NOMATCH",
  "2002_SH" = "NOMATCH",
  "2002_NH" = "NOMATCH",
  "2003_SH" = "NOMATCH",
  "2003_NH" = "NOMATCH",
  "2004_SH" = "NOMATCH",
  "2004_NH" = "NOMATCH",
  "2005_SH" = "NOMATCH",
  "2005_NH" = "NOMATCH",
  "2006_SH" = "NOMATCH",
  "2006_NH" = "NOMATCH",
  "2007_SH" = "NOMATCH",
  "2007_NH" = "NOMATCH",
  "2008_SH" = "NOMATCH",
  "2008_NH" = "NOMATCH",
  "2009_SH" = "NOMATCH",
  "2009_NH" = NA, 
  "2010_SH" = NA
)


##### Makte the right sized matrices
efficacy_H3 <- matrix(nrow= 3*num_age_groups, ncol = length(H3_matches))
for(l in 1:nrow(efficacy_H3)){
  efficacy_H3[l,] <- H3_matches
}
efficacy_H3_copy <- matrix(nrow= 3*num_age_groups, ncol = length(H3_matches))

efficacy_H1 <- matrix(nrow= 3*num_age_groups, ncol = length(H1_matches))
for(l in 1:nrow(efficacy_H1)){
  efficacy_H1[l,] <- H1_matches
}
efficacy_H1_copy <- matrix(nrow= 3*num_age_groups, ncol = length(H1_matches))

efficacy_B <- matrix(nrow= 3*num_age_groups, ncol = length(B_matches))
for(l in 1:nrow(efficacy_B)){
  efficacy_B[l,] <- B_matches
}
efficacy_B_copy <- matrix(nrow= 3*num_age_groups, ncol = length(B_matches))

source(here::here("Vacc_epi_model","1_1a_UK_coverage.R"))
# replciate each to be the same for the year NH and the next year SH
UK_coverage <- UK_coverage[rep(seq_len(nrow(UK_coverage)), each = 2), ]
UK_coverage <- cbind(UK_coverage, matrix(0, nrow = nrow(UK_coverage), ncol = 7))
# Add the two NA rows at the end
UK_coverage <- rbind(UK_coverage, c(2009,rep(NA,21)))
UK_coverage <- rbind(UK_coverage, c(2009,rep(NA,21)))

# looked up the 2019 values. They are 
# 72.5% in >65s
# 45 in high risk groups under 65
#43.8% in 2 to 3 year olds
# primary school = 60.4% 

UK_coverage_new <- UK_coverage
# low risk under 1
UK_coverage_new[,2] <- 0
# low risk 1 - 4
UK_coverage_new[,3] <- 0.438
# low risk 5-14
UK_coverage_new[,4] <- 0.604
# low risk 15-25 (assuming only first two years)
UK_coverage_new[,5] <- 0.604 *2/10
# low risk 25-4
UK_coverage_new[,6] <- 0
# low risk 45-64
UK_coverage_new[,7] <- 0.352 * (15/20)
# low risk 65+
UK_coverage_new[,8] <- 0.724
# high risk under 1
UK_coverage_new[,9] <- 0.449
# high risk 1 - 4
UK_coverage_new[,10] <- 0.449
# high risk 5-14
UK_coverage_new[,11] <- 0.449
# high risk 15-2
UK_coverage_new[,12] <- 0.449
# high risk 25-4
UK_coverage_new[,13] <- 0.449
# high risk 45-64
UK_coverage_new[,14] <- 0.449
# high risk 65+
UK_coverage_new[,15] <- 0.724

### efficaciess (age depedndent)
match_efficacy <- rep(c(rep(0.7,6),0.46),3)
nomatch_efficacy <- rep(c(rep(0.42,6),0.28),3)

efficacy_B_copy[which(efficacy_B=="MATCH")] <- as.numeric(match_efficacy)
efficacy_B_copy[which(efficacy_B=="NOMATCH")] <- as.numeric(nomatch_efficacy)

efficacy_H3_copy[which(efficacy_H3=="MATCH")] <- as.numeric(match_efficacy)
efficacy_H3_copy[which(efficacy_H3=="NOMATCH")] <-as.numeric(nomatch_efficacy)

efficacy_H1_copy[which(efficacy_H1=="MATCH")] <- as.numeric(match_efficacy)
efficacy_H1_copy[which(efficacy_H1=="NOMATCH")] <- as.numeric(nomatch_efficacy)

#### for lower efficactys (cdc based)
badmatch_efficacy <- rep(rep(0.43,7),3)
badnomatch_efficacy <- rep(rep(0.14,7),3)

bad_efficacy_B_copy <- efficacy_B_copy
bad_efficacy_H3_copy <- efficacy_H3_copy
bad_efficacy_H1_copy <- efficacy_H1_copy

bad_efficacy_B_copy[which(efficacy_B=="MATCH")] <- as.numeric(badmatch_efficacy)
bad_efficacy_B_copy[which(efficacy_B=="NOMATCH")] <- as.numeric(badnomatch_efficacy)
bad_efficacy_H3_copy[which(efficacy_H3=="MATCH")] <- as.numeric(badmatch_efficacy)
bad_efficacy_H3_copy[which(efficacy_H3=="NOMATCH")] <-as.numeric(badnomatch_efficacy)
bad_efficacy_H1_copy[which(efficacy_H1=="MATCH")] <- as.numeric(badmatch_efficacy)
bad_efficacy_H1_copy[which(efficacy_H1=="NOMATCH")] <- as.numeric(badnomatch_efficacy)


#improved
improved_efficacy_match <- rep(c(rep(0.9,6),0.7),3)
improved_efficacy_nomatch <- rep(c(rep(0.7,6),0.4),3)

im_efficacy_B_copy <- efficacy_B_copy
im_efficacy_H3_copy <- efficacy_H3_copy
im_efficacy_H1_copy <- efficacy_H1_copy

im_efficacy_B_copy[which(efficacy_B=="MATCH")] <- as.numeric(improved_efficacy_match)
im_efficacy_B_copy[which(efficacy_B=="NOMATCH")] <- as.numeric(improved_efficacy_nomatch)
im_efficacy_H3_copy[which(efficacy_H3=="MATCH")] <- as.numeric(improved_efficacy_match)
im_efficacy_H3_copy[which(efficacy_H3=="NOMATCH")] <-as.numeric(improved_efficacy_nomatch)
im_efficacy_H1_copy[which(efficacy_H1=="MATCH")] <- as.numeric(improved_efficacy_match)
im_efficacy_H1_copy[which(efficacy_H1=="NOMATCH")] <- as.numeric(improved_efficacy_nomatch)

im_breadth_B_copy <- efficacy_B_copy
im_breadth_H3_copy <- efficacy_H3_copy
im_breadth_H1_copy <- efficacy_H1_copy

im_breadth_B_copy[which(efficacy_B=="MATCH")] <- as.numeric(match_efficacy)
im_breadth_B_copy[which(efficacy_B=="NOMATCH")] <- as.numeric(match_efficacy)
im_breadth_H3_copy[which(efficacy_H3=="MATCH")] <- as.numeric(match_efficacy)
im_breadth_H3_copy[which(efficacy_H3=="NOMATCH")] <-as.numeric(match_efficacy)
im_breadth_H1_copy[which(efficacy_H1=="MATCH")] <- as.numeric(match_efficacy)
im_breadth_H1_copy[which(efficacy_H1=="NOMATCH")] <- as.numeric(match_efficacy)

universal_B_copy <- efficacy_B_copy
universal_H3_copy <- efficacy_H3_copy
universal_H1_copy <- efficacy_H1_copy

universal_B_copy[which(efficacy_B=="MATCH")] <- as.numeric(improved_efficacy_match)
universal_B_copy[which(efficacy_B=="NOMATCH")] <- as.numeric(improved_efficacy_match)
universal_H3_copy[which(efficacy_H3=="MATCH")] <- as.numeric(improved_efficacy_match)
universal_H3_copy[which(efficacy_H3=="NOMATCH")] <-as.numeric(improved_efficacy_match)
universal_H1_copy[which(efficacy_H1=="MATCH")] <- as.numeric(improved_efficacy_match)
universal_H1_copy[which(efficacy_H1=="NOMATCH")] <- as.numeric(improved_efficacy_match)

#vaccine_scenarios
vaccine_scenarios <- list(
  # current seasonal
  list(
    waning_rate = 1/(0.5*365.25), 
    efficacy_H3 = efficacy_H3_copy, 
    efficacy_H1 = efficacy_H1_copy, 
    efficacy_B = efficacy_B_copy, 
    dates = c("-09-01", "-02-28"),
    coverage = UK_coverage,#test, 
    prop_group_vacc = rep(1,num_age_groups)
  ), 
  # current seasonal with children
  list(
    waning_rate = 1/(0.5*365.25), 
    efficacy_H3 = bad_efficacy_H3_copy, 
    efficacy_H1 = bad_efficacy_H1_copy, 
    efficacy_B = bad_efficacy_B_copy, 
    dates = c("-09-01", "-02-28"),
    coverage = UK_coverage_new,#test, 
    prop_group_vacc = rep(1,num_age_groups)
  ), 
  # imporvd minimal with children
  list(
    waning_rate = 1/(1*365.25), 
    efficacy_H3 = bad_efficacy_H3_copy, 
    efficacy_H1 = efficacy_H3_copy, 
    efficacy_B = efficacy_B_copy, 
    dates = c("-09-01", "-02-28"),
    coverage = UK_coverage_new,#test, 
    prop_group_vacc = rep(1,num_age_groups)
  ), 
  #improved efficacy with children
  list(
    waning_rate = 1/(2*365.25), 
    efficacy_H3 = im_efficacy_H3_copy, 
    efficacy_H1 = im_efficacy_H1_copy, 
    efficacy_B = im_efficacy_B_copy, 
    dates = c("-09-01", "-02-28"),
    coverage = UK_coverage_new,#test, 
    prop_group_vacc = c(1,0.5,0.5,0.5,0.5,0.5,0.5)
  ), 
  #improved breadth with children
  list(
    waning_rate = 1/(3*365.25), 
    efficacy_H3 = im_breadth_H3_copy, 
    efficacy_H1 = im_breadth_H1_copy, 
    efficacy_B = im_breadth_B_copy, 
    dates = c("-09-01", "-02-28"),
    coverage = UK_coverage_new,#test, 
    prop_group_vacc = c(1,rep(1/3, 6))
  ), 
  #universal
  list(
    waning_rate = 1/(5*365.25), 
    efficacy_H3 = universal_H3_copy, 
    efficacy_H1 = universal_H1_copy, 
    efficacy_B = universal_B_copy, 
    dates = c("-09-01", "-02-28"),
    coverage = UK_coverage_new,#test, 
    prop_group_vacc = c(1,rep(1/5, 6))
  )
)

# 
# 
# 
# 
# efficacies <- list(
#   efficacy_constant_0.9 = c(0.9,0.9),
#   efficacy_constant_0.7 = c(0.7,0.7),
#   efficacy_mismatch_0.4 = c(0.7,0.4),
#   efficacy_mismatch_0.7 = c(0.9,0.7)
# )
# 
# # start in march as this is the "year end" / "birthday"
# campaigns <- list(
#   dates_campaign = c("-03-02","-06-01"), # march to june 
#   dates_yearround = c("-03-02") # all year round
# )
# 
# 
# # coverages
# 
# 
# 
# 
# # this is the coverage over the dates period
# # i.e. over one year in the year round scenario, and over the 3 months in the 3 month scenario
# coverages <- list(
#   coverage_low = c(0.5,0.5,rep(0,5),0.5,0.5,rep(0,5)),
#   coverage_mid = c(0.75,0.75,rep(0,5),0.75,0.75,rep(0,5)),
#   coverage_high = c(0.9,0.9,rep(0,5),0.9,0.9,rep(0,5))
# )
# 
# durations <- c(
#   1/(365.25*0.5),
#   1/(365.25*1),
#   1/(365.25*2),
#   1/(365.25*3),
#   1/(365.25*4),
#   1/(365.25*5)
# )
# 
# vaccine_scenarios <- list()
# i <- 1
# 
# 
# 
# efficacy_H3 <- matrix(nrow= 3*num_age_groups, ncol = length(H3_matches))
# for(l in 1:nrow(efficacy_H3)){
#   efficacy_H3[l,] <- H3_matches
# }
# efficacy_H3_copy <- matrix(nrow= 3*num_age_groups, ncol = length(H3_matches))
# 
# efficacy_H1 <- matrix(nrow= 3*num_age_groups, ncol = length(H1_matches))
# for(l in 1:nrow(efficacy_H1)){
#   efficacy_H1[l,] <- H1_matches
# }
# efficacy_H1_copy <- matrix(nrow= 3*num_age_groups, ncol = length(H1_matches))
# 
# efficacy_B <- matrix(nrow= 3*num_age_groups, ncol = length(B_matches))
# for(l in 1:nrow(efficacy_B)){
#   efficacy_B[l,] <- B_matches
# }
# efficacy_B_copy <- matrix(nrow= 3*num_age_groups, ncol = length(B_matches))
# 
# 
# vaccine_scenarios[[1]] <- list(waning_rate = 1, 
#                                efficacy_H3 = matrix(0,nrow=3*num_age_groups,ncol=length(H3_matches)), 
#                                efficacy_H1 = matrix(0,nrow=3*num_age_groups,ncol=length(H1_matches)),
#                                efficacy_B = matrix(0,nrow=3*num_age_groups,ncol=length(B_matches)), 
#                                dates = c("-03-02"),  # all year round
#                                coverage = rep(0,length(coverages[[1]])), 
#                                prop_group_vacc = rep(0,num_age_groups))
# 
# 
# 
# 
# for(scen_duration in durations){
#   for(scen_coverage  in coverages){
#     for(scen_campaign in campaigns){
#       for(scen_efficacy in efficacies){
#         
#         efficacy_B_copy[which(efficacy_B=="MATCH")] <- as.numeric(scen_efficacy[1])
#         efficacy_B_copy[which(efficacy_B=="NOMATCH")] <- as.numeric(scen_efficacy[2])
#         
#         efficacy_H3_copy[which(efficacy_H3=="MATCH")] <- as.numeric(scen_efficacy[1])
#         efficacy_H3_copy[which(efficacy_H3=="NOMATCH")] <- as.numeric(scen_efficacy[2])
#         
#         efficacy_H1_copy[which(efficacy_H1=="MATCH")] <- as.numeric(scen_efficacy[1])
#         efficacy_H1_copy[which(efficacy_H1=="NOMATCH")] <- as.numeric(scen_efficacy[2])
#         
#         waning_years <- 1/(scen_duration*365.25)
#         if(waning_years %in% c(5,4,3)){prop_group_vacc = c(1,1/4,0,0,0,0,0)} else 
#           if (waning_years %in% c(2)){ prop_group_vacc = c(1,1/2,0,0,0,0,0) } else
#             if(waning_years %in% c(0.5,1)){prop_group_vacc = c(1,1,0,0,0,0,0)} 
#         
#         
#         vaccine_scenarios[[i+1]] = list(
#           waning_rate = scen_duration, 
#           efficacy_H3 = efficacy_H3_copy,
#           efficacy_H1 = efficacy_H1_copy,
#           efficacy_B = efficacy_B_copy,
#           dates = scen_campaign, 
#           coverage = scen_coverage,
#           prop_group_vacc = prop_group_vacc
#         )
#         i <- i+1
#       }
#     }
#   }
# }
# 
 lookup_year <- names(B_matches)
# 
# 

 