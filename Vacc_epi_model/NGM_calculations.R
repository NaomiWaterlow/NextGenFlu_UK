# Next generation matrix calculations

# this needs to be the age-specific FOIs <- these are sampled.
library(MASS)

# these are inputs
gamma1 <- 2/0.8
gamma2 <- 2/1.8

calculate_R0 <- function(transmission, contacts, gamma1, gamma2, demography){

  Transmission <- matrix(0,nrow =4*6, ncol = 4*6)
  for (i in 1:6){
    for (j in 1:6) {
      Transmission[((i-1)*4)+1, c((((j-1)*4)+3),(((j-1)*4)+4))] <- transmission*
        contacts[i,j]*demography[j]
    }
  }
  
  Transition <- matrix(0,nrow=4*6, ncol = 4*6)
  for(i in 1:6){
    for(j in 1:6){
      if(i == j){
        Transition[((i-1)*4)+1,(((i-1)*4)+1)] <- -gamma1
        Transition[((i-1)*4)+2,(((i-1)*4)+1)] <- gamma1
        Transition[((i-1)*4)+2,(((i-1)*4)+2)] <- -gamma1
        Transition[((i-1)*4)+3,(((i-1)*4)+2)] <- gamma1
        Transition[((i-1)*4)+3,(((i-1)*4)+3)] <- -gamma2
        Transition[((i-1)*4)+4,(((i-1)*4)+3)] <- gamma2
        Transition[((i-1)*4)+4,(((i-1)*4)+4)] <- -gamma2
      }
    }
  }
  Transition_inverse<- ginv(Transition)
  NGM <- -Transmission%*%Transition_inverse
  
  Eigen<- unlist((eigen(NGM)[1]))
  R0 <-  max(Eigen)
}


storage_R0 <- data.frame()

# calculate for each season
for(epidemic in 1:length(epidemics_list)){
    set.seed(200)
  # specify the time frame
  begin_date <- epidemics_list[[epidemic]][["start_date"]]
  end_date <- epidemics_list[[epidemic]][["end_date"]]
  year_in_question <- which(years == epidemics_list[[epidemic]]["year"])
  flu_type <- epidemics_list[[epidemic]][["flutype"]]
  demography <- popken[,1+year_in_question]
  demography2 <- c(demography[1], sum(demography[2:6]), sum(demography[7:15]), sum(demography[16:20]),
                                     sum(demography[21:50]), sum(demography[51:86]))
  # load the posterior samples
  if(use_presampled == F){
    posterior_samples <- read.csv(here("Posteriors", paste0(begin_date, " to ", end_date, " ", flu_type, " ", "UK", ".csv")))
  } else { posterior_samples <- read.csv(here("Posteriors", paste0(begin_date, " to ", end_date, " ", flu_type, " ", "UK_presampled_suscchange", ".csv")))[,-1]}
  sample_set <- sample(1:nrow(posterior_samples), size=posterior_sample_size)
  posterior_subset <- posterior_samples[sample_set,2:10]
  contact_ids <- as.matrix(posterior_samples[sample_set,12:579])
  # contacts
  for(sample_num in 1:nrow(posterior_subset)){
    # extract info
    trans <- posterior_subset[sample_num,5]
    # reformat contacts
    contacts <- contact_matrix(as.matrix(na.omit(polymod.ken[contact_ids[sample_num,],])), demography, 
                               age_group_limits = c(1, 6, 15, 20, 50))

    overall_R0 <- calculate_R0(transmission = trans,
                               contacts = contacts,
                               gamma1 = gamma1, 
                               gamma2 = gamma2,
                               demography = demography2)
    print(overall_R0)
    storage_R0 <- rbind(storage_R0, c(overall_R0,sample_num, epidemic))
    
  }
}
  
colnames(storage_R0) <- c("R0", "sample number", "epidemic")  
storage_R0 <- as.data.table(storage_R0)
storage_R0$epidemic <- as.factor(storage_R0$epidemic)
mean_R0 <- storage_R0[,mean(R0)]
mean_R0
min(storage_R0$R0)
max(storage_R0$R0)
R0S <- ggplot(storage_R0, aes(x = epidemic, y = R0)) + geom_boxplot() + 
  theme_bw()+ 
  geom_hline(yintercept = mean_R0, colour = "red", linetype = 2) + 
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12), 
        legend.text =  element_text(size = 10), 
        legend.title = element_text(size = 12), 
        strip.background = element_blank(),
        strip.text.x = element_blank())

tiff(here("Supp_R0s.tiff"), height = 2000, width = 3000, res = 300)
R0S
dev.off()
