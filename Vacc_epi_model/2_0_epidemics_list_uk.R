# epidemics_list.R 
time_labels <- c()
epidemics_list <- list()
for(i in 1:14){
  
  this_year <- years[i]
  epidemics_list[[(i*3)-2]] <- list(start_date = as.Date(c(paste0(this_year,"-09-01"))), 
                                    end_date = as.Date(c(paste0(this_year,"-09-01")))+364, 
                                    year = as.character(this_year), 
                                    flutype = "AH3N2", 
                                    previous_epi = (i*3)-5
  )
  
  names(epidemics_list)[[(i*3)-2]] <- paste0("epidemic",(i*3)-2)
  
  epidemics_list[[(i*3)-1]] <-  list(start_date = as.Date(c(paste0(this_year,"-09-01"))), 
                                     end_date = as.Date(c(paste0(this_year,"-09-01")))+364, 
                                     year = as.character(years[i]), 
                                     flutype = "AH1N1", 
                                     previous_epi = (i*3)-4
  )
  
  names(epidemics_list)[[(i*3)-1]] <- paste0("epidemic",(i*3)-1)
  
  epidemics_list[[(i*3)]] <-  list(start_date = as.Date(c(paste0(this_year,"-09-01"))), 
                                   end_date = as.Date(c(paste0(this_year,"-09-01")))+364, 
                                   year = as.character(years[i]), 
                                   flutype = "B", 
                                   previous_epi = (i*3)-3
  )
  
  names(epidemics_list)[[(i*3)]] <- paste0("epidemic",(i*3))
  
  time_labels <- c(time_labels, paste0(this_year,"-09-01"))
  
}

epidemics_list[[1]]$previous_epi <- NA
epidemics_list[[2]]$previous_epi <- NA
epidemics_list[[3]]$previous_epi <- NA
