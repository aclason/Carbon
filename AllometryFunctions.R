# April 2022
# By: Leah Walker

##################### ALLOMETRY FUNCTIONS ############################
# Based on previous studies, we are calculating the height of trees using allometry rather than using the diameter-only Carbon equations

# This diameter-height function is the same one described in Canham et al. (1999)
# The MaxHt and B variables are also from Canham et al. (1999) - for the ICH
# The values for the SBS were taken from the SBS base parameter file, but the original source remains unknown


DiamHgtFN <- function(Species, DBH){
  if(is.na(Species)){
    print(paste("Species is not found"))
    HT <- NA
  } else if(Species == "Sx"){
    HT <- 1.35 + (35.000000 - 1.35)*(1 - exp(-(0.0299364)*DBH))
  } else if(Species == "Pl"){
    HT <- 1.35 + (23.346836 - 1.35)*(1 - exp(-(0.0707280)*DBH)) 
  } else if(Species == "Bl"){
    HT <- 1.35 + (30.000000 - 1.35)*(1 - exp(-(0.03496783)*DBH))
  } else if(Species == "Ba"){
    HT <- 1.35 + (30.000000 - 1.35)*(1 - exp(-(0.03496783)*DBH)) # using Bl
  } else if(Species == "At"){
    HT <- 1.35 + (33.530000 - 1.35)*(1 - exp(-(0.0374600)*DBH)) 
  } else if(Species == "Lw"){
    HT <- 1.35 + (35.000000 - 1.35)*(1 - exp(-(0.0299364)*DBH)) 
  } else if(Species == "Fd"){
    HT <- 1.35 + (35.000000 - 1.35)*(1 - exp(-(0.0299364)*DBH)) 
  } else if(Species == "Ac"){
    HT <- 1.35 + (33.530000 - 1.35)*(1 - exp(-(0.0374600)*DBH)) 
  } else if(Species == "Ep"){
    HT <- 1.35 + (33.530000 - 1.35)*(1 - exp(-(0.0374600)*DBH)) 
  }
  return(HT)
}


DiamHgtFN_ICH <- function(Species, DBH){
  if(is.na(Species)){
    print(paste("Species is not found"))
    HT <- NA
  } else if(Species == "Hw"){
    HT <- 1.35 + (39.48 - 1.35)*(1 - exp(-(0.0299)*DBH))
  } else if(Species == "Cw"){
    HT <- 1.35 + (39.54 - 1.35)*(1 - exp(-(0.0241)*DBH)) 
  } else if(Species == "Ba"){
    HT <- 1.35 + (40.0 - 1.35)*(1 - exp(-(0.0263)*DBH))
  } else if(Species == "Bl"){
    HT <- 1.35 + (40.0 - 1.35)*(1 - exp(-(0.0264)*DBH))
  } else if(Species == "Sx"){
    HT <- 1.35 + (45.0 - 1.35)*(1 - exp(-(0.0264)*DBH)) 
  } else if(Species == "Pl"){
    HT <- 1.35 + (40.0 - 1.35)*(1 - exp(-(0.0333)*DBH)) 
  } else if(Species == "At"){
    HT <- 1.35 + (39.14- 1.35)*(1 - exp(-(0.0352)*DBH)) 
  } else if(Species == "Ac"){
    HT <- 1.35 + (39.47 - 1.35)*(1 - exp(-(0.0347)*DBH)) 
  } else if(Species == "Ep"){
    HT <- 1.35 + (33.18 - 1.35)*(1 - exp(-(0.0454)*DBH)) 
  }
  return(HT)
}

    