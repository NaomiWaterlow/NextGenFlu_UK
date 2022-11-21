#### Economic analysis - deaths
# This has the deaths by age  and risk group in. 
deaths_summarised <-death_outcome[, sum(value), by = c("sample", "scenario", "Year", "age" )]

library(data.table)
library(ISOcodes)

#load UN population and life tables sourced from https://population.un.org/wpp/Download/Standard/CSV/
 UNLT  <- fread(here::here("WPP2022_Life_Table_Medium.csv"))[MidPeriod == 2019.5 & Sex == "Total"]  # only need combined sexes for 2025
# UNPOP <- fread(paste0(pth,"/WPP2019_PopulationBySingleAgeSex_2020-2100.csv"))[Time == 2022]       # only need 2022 projections


#---------------------------------------------------------------------------------------------------
# FUNCTION: cov_dLE - calculates discounted life expectancy for each of the
# CovidM age bands (equivalent to YLL per death)
#---------------------------------------------------------------------------------------------------

# qx = probability of dying between age x and x+1
# lx = number of 100,000 reference pop surviving to age x
# dx = instantaneous death rate = -ln{1-qx}
# Lx = person years lived between age x and x+1 = [l(x) + l(x+1)]/2 
# Tx = total person years lived above age x
# dLEx = discounted life expectancy at age x
#
cov_dLE <- function(
    LT,                            # data table with UN life tables,
    r               = 0,           # discount rate
    smr             = 1,           # SMRs adjustment for covid co-morbidities (so if 1.5, assume 50% higher default.)
    selectCountries = c("GBR"),    # vector of iso3 codes of countries to run
    selectTime      = "2020-2025", # which UN life-table time-period to use 
    selectSex       = "Total",     # which UN life-table sex to use
    weight_method   = "pop_ifr",   # weight method to average LE by age group: "lx" "lxqx" "equal" "pop_ifr"
    POP             = NULL         # data table of populations to be supplied if using weight_method=="pop_ifr"
){
  
  require(data.table)
  require(ISOcodes)
  # Covid age-specific IFR
  #ifr_levin = function(age) exp(-7.56 + 0.121 * age) / (100 + exp(-7.56 + 0.121 * age))
  
  # age bands to match the model
  AgeBands = data.table(
    AgeBand = seq(1,6,1),
    start   = c(0,1,5,15,45,65),
    end     = c(0,4,14,44,64,100)
  )

  #### converts geographical codes between formats - may not need
  # UN M49 country code to ISO3 mapping
  UN_M.49_Countries <- as.data.table(UN_M.49_Countries)[, LocID := as.integer(Code)]
  
  LT <- LT[     # Convert UN M49 country code to ISO3 (also drops regions)           
    UN_M.49_Countries, 
    on = .(LocID == LocID)
  ][            # only life tables for selected time period, sex, and countries
    Time == selectTime & Sex == selectSex & ISO_Alpha_3 %in% selectCountries    
  ][
    ,
    .(         # keep only selected vars
      country = ISO_Alpha_3,
      AgeGrpStart,
      AgeGrpSpan,
      qx
    )
  ][            # Add AgeGrpEnd
     # bneed an extra age group in order to do teh calculation. 
    , AgeGrpEnd := AgeGrpStart + AgeGrpSpan
  ][AgeGrpStart == 100, AgeGrpEnd := 101]
  
  # expand age groups from 0 to maximum age
  out <- data.table(Age = seq(0, max(LT$AgeGrpStart)))  
  
  out <- LT[
    out, 
    on=.(AgeGrpStart <= Age, AgeGrpEnd > Age)
  ][
    order(country, AgeGrpStart)
  ][,AgeGrpEnd := NULL]
  
  setnames(out,"AgeGrpStart","Age")
  
  # convert age group qx to estimated yearly qx. qx is risk of death by the end of the year period
  out[, qx := 1 - (1 - qx)^(1 / AgeGrpSpan)]
  
  # instantaneous death rate (Briggs et al)
  out[, dx := -log((1 - qx))]
  
  # lx - number of people alive at the start of age x
  out[Age == 0, lx := 100000] # starting age
  for (c in selectCountries){
    for (a in 1:max(out$Age)){
      out[
        country == c & Age == a,
        lx := out[country == c & Age == a - 1, lx] *        # alive at start of previous age group
          exp(-out[country == c & Age == a - 1, dx] * smr)  # deaths during previous age group
      ]
    }
  }
  
  # Lx
  for (c in selectCountries){
    for (a in 0:(max(out$Age) - 1)){
      out[
        country == c & Age == a,
        Lx :=  0.5 * (lx + out[country == c & Age == a + 1, lx])       
      ]
    }
  }
  

  out[Age == max(out$Age), Lx := lx] # final age
  
  # discounted Tx
  for (c in selectCountries){
    for (a in 0:max(out$Age)){
      out[
        country == c & Age == a,
        dTx := sum(out[country == c & Age >= a, Lx / (1 + r)^(Age - a)]) #1 / 1 +0.356 ^years to discount
      ]
    }
  }
  
  # discounted LEx
  out[, dLEx := dTx/lx]
  
  # Age groups for output 
  out[, joinAge := Age]
  out <- out[AgeBands, on = .(joinAge >= start, joinAge <= end)]
  
  out <- out[Age != 100] # drop age 100
  
  # dLEx for age bands weighted by lx
  
  if (weight_method == "lx"){ # weighting based on people alive at that age in the lifetable
    out <- out[
      ,
      .(
        disc.rate = r,
        SMR = smr,
        d_LEx = sum(lx * dLEx)/sum(lx)
      ),
      by = .(country, AgeBand)
    ][
      order(country, AgeBand)
    ]
  } else if (weight_method == "lxqx"){# number of people alive and the risk of death at each age
 
    out <- out[
      , 
      .(
        disc.rate = r,
        SMR = smr,
        d_LEx = sum(lx * qx * dLEx)/sum(lx * qx)
      ),
      by = .(country, AgeBand)
    ][
      order(country, AgeBand)
    ]
  } else if (weight_method == "equal") { # eacg age is weighted equally (i.e. what people ususally use without thinking)
    out <- out[
      , 
      .(
        disc.rate = r,
        SMR = smr,
        d_LEx = mean(dLEx)
      ),
      by=.(country, AgeBand)
    ][
      order(country, AgeBand)
    ]
  } else if (weight_method == "pop_ifr") { # actually population projections (UN World pop). # and actauly  ifrs from levin instead of qx
    
    POP <- POP[     # Convert UN M49 country code to ISO3 (also drops regions)           
      UN_M.49_Countries, 
      on = .(LocID == LocID)
    ][
      , 
      country := ISO_Alpha_3
    ][ country %in% selectCountries & AgeGrp <= 99][
      , 
      IFR := ifr_levin(AgeGrp)
    ]
    
    out <- out[
      POP[, .(country = country, Age = AgeGrp, Pop = PopTotal, IFR = IFR)],
      on = .(Age == Age, country == country)
    ][
      order(country, AgeBand)
    ]
    
    out <- out[
      , 
      .(
        disc.rate = r,
        SMR = smr,
        d_LEx = sum(Pop * IFR * dLEx)/sum(Pop * IFR)
      ),
      by = .(country, AgeBand)
    ][
      order(country, AgeBand)
    ]
  } else {out <- "Error: Invalid argument for weight_method"}
  
  
  return(out[, c("disc.rate","SMR") := NULL]) # for now dropping discount rate & SMR from output as this is known implicitly
}


 # Run the thing
 LT_out <- cov_dLE(LT = UNLT, 
                   r = discount_rate, 
                   smr = 1, 
                   selectCountries = ("GBR"), 
                   selectTime = "2019", # this may need sorting later! Could run seperately for each section, or make flexible?
                   selectSex       = "Total",     # which UN life-table sex to use
                   weight_method   = "lxqx", # probably want to use
                   POP = NULL
 )
 
 # match the age bands
 LT_out[AgeBand ==1, age := "[0,0.5)"]
 LT_out[AgeBand ==2, age := "[0.5,5)"]
 LT_out[AgeBand ==3, age := "[5,15)"]
 LT_out[AgeBand ==4, age := "[15,45)"]
 LT_out[AgeBand ==5, age := "[45,65)"]
 LT_out[AgeBand ==6, age := "[65,+)"]

 
# NOT taking account of population size changing each year!
deaths_summarised[LT_out, on = "age", d_LEx := d_LEx ] 
deaths_summarised[, death_QALYS := V1 *d_LEx]
