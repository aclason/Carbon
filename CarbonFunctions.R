#### Carbon Functions

######### Mineral soil carbon: #######
Min_SOC <- function(Soc, BD, depth, CoarseFrags){
  Min_SOC <- Soc * BD * depth * (1- CoarseFrags)
  return(Min_SOC)
}

############ Tree carbon (kg/tree) ############
#could make this more generic by accepting different ways of writing species for instance.
# Biomass is calculated using Ung et al. 2008 allometric equations. biomass is in kg. 
# The carbon multiplier (0.5) is applied directly here, resulting in carbon in kg/tree

# Standing dead (SD) carbonA decay class and species specificstructural reduction
# factor is applied to each of the components of the allometric equation i.e. bark, bole, top. This accounts for the loss 
# of biomass through decay (Domke et al. 2011). A species and decay class density reduction factor is applied, this 
# accounts for the density loss through decay (Harmon et al. 2011). Carbon concentration is 0.5 (Harmon et al. 2013).

# SDCarbon (Mg/ha) = BIOMASS x SRF x DCRF x Cconc
# SDCarbon (Mg/ha) = ((Ywood*SRF) + (Ybark*SRF) + (Yfoliage*SRF) + (Ybranches*SRF)) X DCRF X Cconc
TreeCarbonFN <- function(Species,DBH,HT,Tree_class){
  if(is.na(Species)){
    print(paste("Species is not found"))
    Sp_C <- NA
  } else if(Species=="At"){
     if(Tree_class < 3 ){
      Sp_C <-((0.0143*DBH^1.9369*HT^1.0579)+(0.0063*DBH^2.0744*HT^0.6691)+
                (0.0150*DBH^2.9068*HT^-0.6306)+(0.0284*DBH^1.6020))*0.5
      } else if (Tree_class == 3){
        Sp_C <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0.92)+
                        ((0.0150*DBH^2.9068*HT^-0.6306)*1))*0.97*0.5
      } else if (Tree_class == 4){
        Sp_C <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0.66)+
                        ((0.0150*DBH^2.9068*HT^-0.6306)*0.5))*0.75*0.5
      } else if(Tree_class == 5){
        Sp_C <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0.39)+
                        ((0.0150*DBH^2.9068*HT^-0.6306)*0.2))*0.868*0.5
      } else if(Tree_class == 6){
        Sp_C <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0.21)+
                        ((0.0150*DBH^2.9068*HT^-0.6306)*0.1))*0.613*0.5
      } else if(Tree_class == 7){
        Sp_C <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0)+
                        ((0.0150*DBH^2.9068*HT^-0.6306)*0))*0.613*0.5
      } else if(Tree_class == 8){
        Sp_C <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0)+
                        ((0.0150*DBH^2.9068*HT^-0.6306)*0))*0.613*0.5
      }
    }else if(Species=="Ac"){
      if (Tree_class < 3){
          Sp_C <-((0.0051*DBH^1.0697*HT^2.2748)+(0.0009*DBH^1.3061*HT^2.0109)+
                    (0.0131*DBH^2.5760)+(0.0224*DBH^1.8368))*0.5
        } else if (Tree_class == 3){
          Sp_C <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0.92)+
                          ((0.0131*DBH^2.5760)*1))*1.006*0.5
        } else if (Tree_class == 4){
          Sp_C <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0.66)+
                          ((0.0131*DBH^2.5760)*0.5))*0.793*0.5
        } else if (Tree_class == 5){
          Sp_C <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0.39)+
                        ((0.0131*DBH^2.5760)*0.2))*0.868*0.5
        } else if (Tree_class == 6){
          Sp_C <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0.21)+
                        ((0.0131*DBH^2.5760)*0.1))*0.613*0.5
        } else if (Tree_class == 7){
          Sp_C <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0)+
                        ((0.0131*DBH^2.5760)*0))*0.613*0.5
        } else if (Tree_class == 8){
          Sp_C <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0)+
                        ((0.0131*DBH^2.5760)*0))*0.613*0.5
      }
    } else if(Species=="Cw"){
      Sp_C <-((0.0188*DBH^1.3376*HT^1.5293)+(0.0002*DBH^2.4369*HT^1.1315)+
        (0.0611*DBH^1.9208)+(0.1097*DBH^1.5530))*0.5
    } else if(Species=="Bl"){
      if(Tree_class < 3){
        Sp_C <-((0.0220*DBH^1.6469*HT^1.1714)+(0.0061*DBH^1.8603*HT^0.7693)+
                  (0.0265*DBH^3.6747*HT^-1.5958)+(0.0509*DBH^2.9909*HT^-1.2271))*0.5
        } else if (Tree_class == 3){
          Sp_C <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0.92)+
                        ((0.0265*DBH^3.6747*HT^-1.5958)*1))*1.04*0.5
        } else if (Tree_class == 4){
          Sp_C <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0.66)+
                        ((0.0265*DBH^3.6747*HT^-1.5958)*0.5))*1.068*0.5
        } else if (Tree_class == 5){
          Sp_C <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0.39)+
                        ((0.0265*DBH^3.6747*HT^-1.5958)*0.2))*1*0.5 
        } else if (Tree_class == 6){
          Sp_C <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0.21)+
                        ((0.0265*DBH^3.6747*HT^-1.5958)*0.1))*0.696*0.5
        } else if (Tree_class == 7){
          Sp_C <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0)+
                        ((0.0265*DBH^3.6747*HT^-1.5958)*0))*0.696*0.5
        } else if (Tree_class == 8){
          Sp_C <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0)+
                        ((0.0265*DBH^3.6747*HT^-1.5958)*0))*0.696*0.5
        }  
      } else if(Species=="Ep"){
        if(Tree_class < 3){
          Sp_C <-((0.0333*DBH^2.0794*HT^0.6811)+(0.0079*DBH^1.9905*HT^0.6553)+
                    (0.0253*DBH^3.1518*HT^-0.9083)+(0.1361*DBH^2.2978*HT^-1.0934))*0.5
      } else if(Tree_class == 3){
        Sp_C <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0.92)+
                        ((0.0253*DBH^3.1518*HT^-0.9083)*1))*1.016*0.5
      } else if(Tree_class == 4) {
        Sp_C <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0.66)+
                        ((0.0253*DBH^3.1518*HT^-0.9083)*0.5))*0.713*0.5
      } else if(Tree_class == 5) {
        Sp_C <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0.39)+
                        ((0.0253*DBH^3.1518*HT^-0.9083)*0.2))*0.777*0.5
      } else if(Tree_class == 6) {
        Sp_C <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0.21)+
                        ((0.0253*DBH^3.1518*HT^-0.9083)*0.1))*0.439*0.5
      } else if(Tree_class == 7) {
        Sp_C <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0)+
                        ((0.0253*DBH^3.1518*HT^-0.9083)*0))*0.439*0.5
      } else if(Tree_class == 8) {
        Sp_C <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0)+
                        ((0.0253*DBH^3.1518*HT^-0.9083)*0))*0.439*0.5
      }
    } else if(Species=="Hw"){
      Sp_C <-((0.0113*DBH^1.9332*HT^1.1125)+(0.0019*DBH^2.3356*HT^0.6371)+
        (0.0609*DBH^2.0021)+(0.2656*DBH^2.0107*HT^-0.7963))*0.5
    } else if(Species=="Pl"){
      if(Tree_class < 3){
        Sp_C <-((0.0239*DBH^1.6827*HT^1.1878)+(0.0117*DBH^1.6398*HT^0.6524)+
                  (0.0285*DBH^3.3764*HT^-1.4395)+(0.0769*DBH^2.6834*HT^-1.2484))*0.5
      } else if(Tree_class == 3){
        Sp_C <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0.92)+
                        ((0.0285*DBH^3.3764*HT^-1.4395)*1))*0.98*0.5
      } else if(Tree_class == 4){
        Sp_C <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0.66)+
                        ((0.0285*DBH^3.3764*HT^-1.4395)*0.5))*1.04*0.5  
      } else if(Tree_class == 5){
        Sp_C <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0.39)+
                        ((0.0285*DBH^3.3764*HT^-1.4395)*0.2))*1.02*0.5
      } else if(Tree_class == 6){
        Sp_C <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0.21)+
                        ((0.0285*DBH^3.3764*HT^-1.4395)*0.1))*0.727*0.5
      } else if(Tree_class == 7){
        Sp_C <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0)+
                        ((0.0285*DBH^3.3764*HT^-1.4395)*0))*0.727*0.5  
      } else if(Tree_class == 8){
        Sp_C <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0)+
                        ((0.0285*DBH^3.3764*HT^-1.4395)*0))*0.727*0.5              
      }
    } else if(Species=="Sx"){
      if(Tree_class < 3){
      Sp_C <-((0.0133*DBH^1.3303*HT^1.6877)+(0.0086*DBH^1.6216*HT^0.8192)+
        (0.0428*DBH^2.7965*HT^-0.7328)+(0.0854*DBH^2.4388*HT^-0.7630))*0.5
      } else if(Tree_class == 3){  
        Sp_C <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0.92)+
                        ((0.0428*DBH^2.7965*HT^-0.7328)*1))*0.996*0.5
      } else if(Tree_class == 4){
        Sp_C <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0.66)+
                        ((0.0428*DBH^2.7965*HT^-0.7328)*0.5))*0.943*0.5
      } else if(Tree_class == 5){
        Sp_C <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0.39)+
                        ((0.0428*DBH^2.7965*HT^-0.7328)*0.2))*0.991*0.5
      } else if(Tree_class == 6){
        Sp_C <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0.21)+
                        ((0.0428*DBH^2.7965*HT^-0.7328)*0.1))*0.555*0.5
      } else if(Tree_class == 7){
        Sp_C <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0)+
                        ((0.0428*DBH^2.7965*HT^-0.7328)*0))*0.555*0.5
      } else if(Tree_class == 8){
        Sp_C <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0)+
                        ((0.0428*DBH^2.7965*HT^-0.7328)*0))*0.555*0.5
      }
    } else if(Species=="Fd"){
      if(Tree_class < 3){
      Sp_C <- ((0.0191*DBH^1.5365*HT^1.3634)+(0.0083*DBH^2.4811)+
        (0.0351*DBH^2.2421)+(0.0718*DBH^2.2935*HT^-0.4744))*0.5
      } else if(Tree_class == 3){
        Sp_C <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0.92)+
                         ((0.0351*DBH^2.2421)*1))*0.892*0.5
      } else if(Tree_class == 4){
        Sp_C <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0.66)+
                         ((0.0351*DBH^2.2421)*0.5))*0.831*0.5
      } else if(Tree_class == 5){
        Sp_C <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0.39)+
                         ((0.0351*DBH^2.2421)*0.2))*0.591*0.5
      } else if(Tree_class == 6){
        Sp_C <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0.21)+
                         ((0.0351*DBH^2.2421)*0.1))*0.433*0.5
      } else if(Tree_class == 7){
        Sp_C <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0)+
                         ((0.0351*DBH^2.2421)*0))*0.433*0.5
      } else if(Tree_class == 8){
        Sp_C <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0)+
                         ((0.0351*DBH^2.2421)*0))*0.433*0.5
      }
    } else if(Species=="UC"){
      if(Tree_class<3){
        Sp_C <-((0.0276*DBH^1.6868*HT^1.0953)+(0.0101*DBH^1.8486*HT^0.5525)+
                  (0.0313*DBH^2.9974*HT^-1.0383)+(0.1379*DBH^2.3981*HT^-1.0418))*0.5
      } else if(Tree_class == 3){
        Sp_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.92)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*1))*1.005*0.5
      } else if(Tree_class == 4){
        Sp_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.66)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0.5))*1.017*0.5
      } else if(Tree_class == 5){
        Sp_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.39)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0.2))*1.004*0.5
      } else if(Tree_class == 6){
        Sp_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.21)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0.1))*0.659*0.5
      } else if(Tree_class == 7){
        Sp_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0))*0.659*0.5
      } else if(Tree_class == 8){
        Sp_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0))*0.659*0.5
    }
      } else if(Species=="Lw"){ #Using Unknown conifer
        if (Tree_class < 3){
          Sp_C <-((0.0276*DBH^1.6868*HT^1.0953)+(0.0101*DBH^1.8486*HT^0.5525)+
                    (0.0313*DBH^2.9974*HT^-1.0383)+(0.1379*DBH^2.3981*HT^-1.0418))*0.5
      } else if(Tree_class == 3){
        Sp_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.92)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*1))*1.005*0.5
      } else if(Tree_class == 4){
        Sp_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.66)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0.5))*1.017*0.5
      } else if(Tree_class == 5){
        Sp_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.39)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0.2))*1.004*0.5
      } else if(Tree_class == 6){
        Sp_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.21)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0.1))*0.659*0.5
      } else if(Tree_class == 7){
        Sp_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0))*0.659*0.5
      } else if(Tree_class == 8){
        Sp_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0))*0.659*0.5
      }
    } else {
      print(paste("Species",Species,"not found"))
      Sp_C <- NA
    }
  return(Sp_C)
}


TreeBiomassFN <- function(Species,DBH,HT){
  if(is.na(Species)){
    print(paste("Species is not found"))
    Sp_C <- NA
  } else if(Species=="At"){
      Sp_C <-((0.0143*DBH^1.9369*HT^1.0579)+(0.0063*DBH^2.0744*HT^0.6691)+
                (0.0150*DBH^2.9068*HT^-0.6306)+(0.0284*DBH^1.6020))*0.5
  }else if(Species=="Ac"){
      Sp_C <-((0.0051*DBH^1.0697*HT^2.2748)+(0.0009*DBH^1.3061*HT^2.0109)+
                (0.0131*DBH^2.5760)+(0.0224*DBH^1.8368))*0.5
  } else if(Species=="Cw"){
    Sp_C <-((0.0188*DBH^1.3376*HT^1.5293)+(0.0002*DBH^2.4369*HT^1.1315)+
              (0.0611*DBH^1.9208)+(0.1097*DBH^1.5530))*0.5
  } else if(Species=="Bl"){
      Sp_C <-((0.0220*DBH^1.6469*HT^1.1714)+(0.0061*DBH^1.8603*HT^0.7693)+
                (0.0265*DBH^3.6747*HT^-1.5958)+(0.0509*DBH^2.9909*HT^-1.2271))*0.5
  } else if(Species=="Ep"){
      Sp_C <-((0.0333*DBH^2.0794*HT^0.6811)+(0.0079*DBH^1.9905*HT^0.6553)+
                (0.0253*DBH^3.1518*HT^-0.9083)+(0.1361*DBH^2.2978*HT^-1.0934))*0.5
  } else if(Species=="Hw"){
    Sp_C <-((0.0113*DBH^1.9332*HT^1.1125)+(0.0019*DBH^2.3356*HT^0.6371)+
              (0.0609*DBH^2.0021)+(0.2656*DBH^2.0107*HT^-0.7963))*0.5
  } else if(Species=="Pl"){
      Sp_C <-((0.0239*DBH^1.6827*HT^1.1878)+(0.0117*DBH^1.6398*HT^0.6524)+
                (0.0285*DBH^3.3764*HT^-1.4395)+(0.0769*DBH^2.6834*HT^-1.2484))*0.5
  } else if(Species=="Sx"){
      Sp_C <-((0.0133*DBH^1.3303*HT^1.6877)+(0.0086*DBH^1.6216*HT^0.8192)+
                (0.0428*DBH^2.7965*HT^-0.7328)+(0.0854*DBH^2.4388*HT^-0.7630))*0.5
  } else if(Species=="Fd"){
      Sp_C <- ((0.0191*DBH^1.5365*HT^1.3634)+(0.0083*DBH^2.4811)+
                 (0.0351*DBH^2.2421)+(0.0718*DBH^2.2935*HT^-0.4744))*0.5
  } else if(Species=="UC"){
      Sp_C <-((0.0276*DBH^1.6868*HT^1.0953)+(0.0101*DBH^1.8486*HT^0.5525)+
                (0.0313*DBH^2.9974*HT^-1.0383)+(0.1379*DBH^2.3981*HT^-1.0418))*0.5
  } else if(Species=="Lw"){ #Using Unknown conifer
      Sp_C <-((0.0276*DBH^1.6868*HT^1.0953)+(0.0101*DBH^1.8486*HT^0.5525)+
                (0.0313*DBH^2.9974*HT^-1.0383)+(0.1379*DBH^2.3981*HT^-1.0418))*0.5
  } else {
    print(paste("Species",Species,"not found"))
    Sp_C <- NA
  }
  return(Sp_C)
}
SDcarbonFN <- function(Species,Tree_class,DBH,HT){
  if(is.na(Species)){
    print(paste("Species is not found"))
    SD_Sp_TC_C <- NA
  } else if (Species=="At"){
    if (Tree_class == "3"){
      SD_Sp_TC_C <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0.92)+
                      ((0.0150*DBH^2.9068*HT^-0.6306)*1))*0.97*0.5
    } else if (Tree_class == "4"){
      SD_Sp_TC_C <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0.66)+
                      ((0.0150*DBH^2.9068*HT^-0.6306)*0.5))*0.75*0.5
    } else if(Tree_class == "5"){
      SD_Sp_TC_C <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0.39)+
                      ((0.0150*DBH^2.9068*HT^-0.6306)*0.2))*0.868*0.5
    } else if(Tree_class == "6"){
      SD_Sp_TC_C <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0.21)+
                      ((0.0150*DBH^2.9068*HT^-0.6306)*0.1))*0.613*0.5
    } else if(Tree_class == "7"){
      SD_Sp_TC_C <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0)+
                      ((0.0150*DBH^2.9068*HT^-0.6306)*0))*0.613*0.5
    } else if(Tree_class == "8"){
      SD_Sp_TC_C <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0)+
                      ((0.0150*DBH^2.9068*HT^-0.6306)*0))*0.613*0.5
    }
  } else if (Species=="Ac"){
    if (Tree_class == "3"){
      SD_Sp_TC_C <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0.92)+
                    ((0.0131*DBH^2.5760)*1))*1.006*0.5
    } else if (Tree_class == "4"){
      SD_Sp_TC_C <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0.66)+
                      ((0.0131*DBH^2.5760)*0.5))*0.793*0.5
    } else if (Tree_class == "5"){
      SD_Sp_TC_C <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0.39)+
                      ((0.0131*DBH^2.5760)*0.2))*0.868*0.5
    } else if (Tree_class == "6"){
      SD_Sp_TC_C <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0.21)+
                      ((0.0131*DBH^2.5760)*0.1))*0.613*0.5
    } else if (Tree_class == "7"){
      SD_Sp_TC_C <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0)+
                      ((0.0131*DBH^2.5760)*0))*0.613*0.5
    }  else if (Tree_class == "8"){
      SD_Sp_TC_C <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0)+
                      ((0.0131*DBH^2.5760)*0))*0.613*0.5
    }
  } else if (Species == "Bl"){
    if (Tree_class == "3"){
    SD_Sp_TC_C <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0.92)+
                    ((0.0265*DBH^3.6747*HT^-1.5958)*1))*1.04*0.5
    } else if (Tree_class == "4"){
      SD_Sp_TC_C <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0.66)+
                      ((0.0265*DBH^3.6747*HT^-1.5958)*0.5))*1.068*0.5
    } else if (Tree_class == "5"){
      SD_Sp_TC_C <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0.39)+
                      ((0.0265*DBH^3.6747*HT^-1.5958)*0.2))*1*0.5 
    } else if (Tree_class == "6"){
      SD_Sp_TC_C <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0.21)+
                      ((0.0265*DBH^3.6747*HT^-1.5958)*0.1))*0.696*0.5
    } else if (Tree_class == "7"){
      SD_Sp_TC_C <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0)+
                      ((0.0265*DBH^3.6747*HT^-1.5958)*0))*0.696*0.5
    } else if (Tree_class == "8"){
      SD_Sp_TC_C <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0)+
                      ((0.0265*DBH^3.6747*HT^-1.5958)*0))*0.696*0.5
    }  
  } else if (Species == "Ep"){
    if (Tree_class == "3"){
    SD_Sp_TC_C <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0.92)+
                    ((0.0253*DBH^3.1518*HT^-0.9083)*1))*1.016*0.5
    } else if (Tree_class == "4") {
      SD_Sp_TC_C <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0.66)+
                      ((0.0253*DBH^3.1518*HT^-0.9083)*0.5))*0.713*0.5
    } else if (Tree_class == "5") {
      SD_Sp_TC_C <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0.39)+
                      ((0.0253*DBH^3.1518*HT^-0.9083)*0.2))*0.777*0.5
    } else if (Tree_class == "6") {
      SD_Sp_TC_C <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0.21)+
                      ((0.0253*DBH^3.1518*HT^-0.9083)*0.1))*0.439*0.5
    } else if (Tree_class == "7") {
      SD_Sp_TC_C <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0)+
                      ((0.0253*DBH^3.1518*HT^-0.9083)*0))*0.439*0.5
    } else if (Tree_class == "8") {
      SD_Sp_TC_C <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0)+
                      ((0.0253*DBH^3.1518*HT^-0.9083)*0))*0.439*0.5
    }
  } else if (Species == "Pl"){
    if (Tree_class == "3"){
    SD_Sp_TC_C <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0.92)+
                    ((0.0285*DBH^3.3764*HT^-1.4395)*1))*0.98*0.5
    } else if (Tree_class == "4"){
      SD_Sp_TC_C <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0.66)+
                      ((0.0285*DBH^3.3764*HT^-1.4395)*0.5))*1.04*0.5  
    } else if (Tree_class == "5"){
      SD_Sp_TC_C <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0.39)+
                      ((0.0285*DBH^3.3764*HT^-1.4395)*0.2))*1.02*0.5
    } else if (Tree_class == "6"){
      SD_Sp_TC_C <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0.21)+
                      ((0.0285*DBH^3.3764*HT^-1.4395)*0.1))*0.727*0.5
    } else if (Tree_class == "7"){
      SD_Sp_TC_C <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0)+
                      ((0.0285*DBH^3.3764*HT^-1.4395)*0))*0.727*0.5  
    } else if (Tree_class == "8"){
      SD_Sp_TC_C <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0)+
                      ((0.0285*DBH^3.3764*HT^-1.4395)*0))*0.727*0.5              
    }
  } else if (Species == "Sx"){
    if (Tree_class == "3"){  
    SD_Sp_TC_C <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0.92)+
                    ((0.0428*DBH^2.7965*HT^-0.7328)*1))*0.996*0.5
    } else if (Tree_class == "4"){
      SD_Sp_TC_C <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0.66)+
                      ((0.0428*DBH^2.7965*HT^-0.7328)*0.5))*0.943*0.5
    } else if (Tree_class == "5"){
      SD_Sp_TC_C <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0.39)+
                      ((0.0428*DBH^2.7965*HT^-0.7328)*0.2))*0.991*0.5
    } else if (Tree_class == "6"){
      SD_Sp_TC_C <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0.21)+
                      ((0.0428*DBH^2.7965*HT^-0.7328)*0.1))*0.555*0.5
    } else if (Tree_class == "7"){
      SD_Sp_TC_C <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0)+
                      ((0.0428*DBH^2.7965*HT^-0.7328)*0))*0.555*0.5
    } else if (Tree_class == "8"){
      SD_Sp_TC_C <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0)+
                      ((0.0428*DBH^2.7965*HT^-0.7328)*0))*0.555*0.5
    }
  } else if (Species == "Fd"){
    if (Tree_class == "3"){
    SD_Sp_TC_C <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0.92)+
                    ((0.0351*DBH^2.2421)*1))*0.892*0.5
    } else if (Tree_class == "4"){
      SD_Sp_TC_C <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0.66)+
                       ((0.0351*DBH^2.2421)*0.5))*0.831*0.5
    } else if (Tree_class == "5"){
      SD_Sp_TC_C <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0.39)+
                       ((0.0351*DBH^2.2421)*0.2))*0.591*0.5
    } else if (Tree_class == "6"){
      SD_Sp_TC_C <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0.21)+
                       ((0.0351*DBH^2.2421)*0.1))*0.433*0.5
    } else if (Tree_class == "7"){
      SD_Sp_TC_C <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0)+
                       ((0.0351*DBH^2.2421)*0))*0.433*0.5
    } else if (Tree_class == "8"){
      SD_Sp_TC_C <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0)+
                       ((0.0351*DBH^2.2421)*0))*0.433*0.5
    }  
  } else if (Species == "UC"){
    if (Tree_class == "3"){
    SD_Sp_TC_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.92)+
                    ((0.0313*DBH^2.9974*HT^-1.0383)*1))*1.005*0.5
    } else if (Tree_class == "4"){
      SD_Sp_TC_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.66)+
                      ((0.0313*DBH^2.9974*HT^-1.0383)*0.5))*1.017*0.5
    } else if (Tree_class == "5"){
      SD_Sp_TC_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.39)+
                      ((0.0313*DBH^2.9974*HT^-1.0383)*0.2))*1.004*0.5
    } else if (Tree_class == "6"){
      SD_Sp_TC_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.21)+
                      ((0.0313*DBH^2.9974*HT^-1.0383)*0.1))*0.659*0.5
    } else if (Tree_class == "7"){
      SD_Sp_TC_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0)+
                      ((0.0313*DBH^2.9974*HT^-1.0383)*0))*0.659*0.5
    } else if (Tree_class == "8"){
      SD_Sp_TC_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0)+
                      ((0.0313*DBH^2.9974*HT^-1.0383)*0))*0.659*0.5
    }
  } else if(Species=="Lw"){ #Using Unknown conifer
    if (Tree_class == "3"){
      SD_Sp_TC_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.92)+
                      ((0.0313*DBH^2.9974*HT^-1.0383)*1))*0.994*0.5
    } else if (Tree_class == "4"){
      SD_Sp_TC_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.66)+
                      ((0.0313*DBH^2.9974*HT^-1.0383)*0.5))*0.951*0.5
    } else if (Tree_class == "5"){
      SD_Sp_TC_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.39)+
                      ((0.0313*DBH^2.9974*HT^-1.0383)*0.2))*0.902*0.5
    } else if (Tree_class == "6"){
      SD_Sp_TC_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.21)+
                      ((0.0313*DBH^2.9974*HT^-1.0383)*0.1))*0.605*0.5
    } else if (Tree_class == "7"){
      SD_Sp_TC_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0)+
                      ((0.0313*DBH^2.9974*HT^-1.0383)*0))*0.605*0.5
    } else if (Tree_class == "8"){
      SD_Sp_TC_C <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0)+
                      ((0.0313*DBH^2.9974*HT^-1.0383)*0))*0.605*0.5
    }
  } else {
    print(paste("Species",Species,"not found"))
    SD_Sp_TC_C <- NA
  }
  return(SD_Sp_TC_C)
}

# cwdCARBON (Mg/ha) = volume(m3/ha)
#                     x structural reduction factor # decay class specific (Fraver et al. 2013)
#                     x Absolute density(g/cm3) # species and decay class specific (Harmon et al. 2008)
#                     x CarbonConcentration # species and decay class specific (Harmon et al. 2013)

cwdCarbonFN <- function(volume_ha, Decay_class, Species){
  if(is.na(Species)){
    print(paste("Species is not found"))
    DC_Sp_C <- NA
  } else if (Species == "Pl"){
    if(Decay_class == "1"){
      DC_Sp_C <-(volume_ha*1*0.378*0.497)
    } else if (Decay_class == "2"){
      DC_Sp_C <-(volume_ha*1*0.367*0.496)
    } else if (Decay_class == "3"){
      DC_Sp_C <-(volume_ha*1*0.276*0.499)
    } else if (Decay_class == "4"){
      DC_Sp_C <-(volume_ha*0.8*0.169*0.519)
    } else if (Decay_class == "5"){
      DC_Sp_C <-(volume_ha*0.412*0.164*0.526)
    }
  } else if (Species == "Sx"){
    if (Decay_class == "1"){
      DC_Sp_C <-(volume_ha*1*0.393*0.496)
    } else if (Decay_class == "2"){
      DC_Sp_C <-(volume_ha*1*0.285*0.498)
    } else if (Decay_class == "3"){
      DC_Sp_C <-(volume_ha*1*0.28*0.505)
    } else if (Decay_class == "4"){
      DC_Sp_C <-(volume_ha*0.8*0.136*0.521)
    } else if (Decay_class == "5") {
      DC_Sp_C <-(volume_ha*0.412*0.129*0.535)
    }
  } else if (Species == "Bl"){
    if (Decay_class == "1"){
      DC_Sp_C <-(volume_ha*1*0.371*0.498)
    } else if (Decay_class == "2"){
      DC_Sp_C <-(volume_ha*1*0.288*0.501)
    } else if (Decay_class == "3"){
      DC_Sp_C <-(volume_ha*1*0.233*0.498)
    } else if (Decay_class == "4"){
      DC_Sp_C <-(volume_ha*0.8*0.152*0.501)
    } else if (Decay_class == "5") {
      DC_Sp_C <-(volume_ha*0.412*0.117*0.521)
    }
  } else if (Species == "UC"){
    if (Decay_class == "1"){
      DC_Sp_C <-(volume_ha*1*0.381*0.496)
    } else if (Decay_class == "2"){
      DC_Sp_C <-(volume_ha*1*0.313*0.498)
    } else if (Decay_class == "3"){
      DC_Sp_C <-(volume_ha*0.8*0.152*0.521)
    } else if (Decay_class == "4"){
      DC_Sp_C <-(volume_ha*0.8*0.152*0.521)
    } else if (Decay_class == "5") {
      DC_Sp_C <-(volume_ha*0.412*0.137*0.535)
    }
  } else if (Species == "At"){
    if (Decay_class == "1"){
      DC_Sp_C <-(volume_ha*1*0.353*0.488)
    } else if (Decay_class == "2"){
      DC_Sp_C <-(volume_ha*1*0.422*0.489)
    } else if (Decay_class == "3"){
      DC_Sp_C <-(volume_ha*1*0.299*0.495)
    } else if (Decay_class == "4"){
      DC_Sp_C <-(volume_ha*0.8*0.16*0.465)
    } else if (Decay_class == "5") {
      DC_Sp_C <-(volume_ha*0.412*0.11*0.473)
    }
  } else if (Species == "Ep"){
    if (Decay_class == "1"){
      DC_Sp_C <-(volume_ha*1*0.469*0.478)
    } else if (Decay_class == "2"){
      DC_Sp_C <-(volume_ha*1*0.403*0.477)
    } else if (Decay_class == "3"){
      DC_Sp_C <-(volume_ha*1*0.352*0.481)
    } else if (Decay_class == "4"){
      DC_Sp_C <-(volume_ha*0.8*0.17*0.474)
    } else if (Decay_class == "5") {
      DC_Sp_C <-(volume_ha*0.412*0.11*0.473)
    }
  } else if (Species == "UD"){
    if (Decay_class == "1"){
      DC_Sp_C <-(volume_ha*1*0.392*0.478)
    } else if (Decay_class == "2"){
      DC_Sp_C <-(volume_ha*1*0.416*0.477)
    } else if (Decay_class == "3"){
      DC_Sp_C <-(volume_ha*1*0.317*0.481)
    } else if (Decay_class == "4"){
      DC_Sp_C <-(volume_ha*0.8*0.163*0.474)
    } else if (Decay_class == "5"){
      DC_Sp_C <-(volume_ha*0.412*0.11*0.473)
    }
  } else if (Species == "U"){
    if (Decay_class == "1"){
      DC_Sp_C <-(volume_ha*1*0.386*0.487)
    } else if (Decay_class == "2"){
      DC_Sp_C <-(volume_ha*1*0.365*0.488)
    } else if (Decay_class == "3"){
      DC_Sp_C <-(volume_ha*1*0.29*0.493)
    } else if (Decay_class == "4"){
      DC_Sp_C <-(volume_ha*0.8*0.158*0.498)
    } else if (Decay_class == "5"){
      DC_Sp_C <-(volume_ha*0.412*0.123*0.504)
    }
  } else {
    DC_Sp_C <- 0
  }
  return(DC_Sp_C)  
}


# fwdCARBON (Mg/ha) = volume(m3/ha)
#                     x Live wood density(g/cm3) # use unknown species (Harmon et al. 2008)
#                     x Decay reduction factor for each size class (Harmon and Fasth website)
#                     x CarbonConcentration # use 50% (Harmon and Fasth website)

fwdCarbonFN <- function(Diam_class, volume){
  if(is.na(Diam_class)){
    print(paste("Diam_class is not found"))
    C <- NA
  } else {
    if (Diam_class == "1.1-2.5"){
      C <- (volume*0.41*0.81*0.5)
    } else if (Diam_class == "2.6-5"){
      C <- (volume*0.41*1*0.5)
    } else if (Diam_class == "5.1-7.5"){
      C <- (volume*0.39*0.99*0.5)
    } else {
      print(paste("Diam_class",Species,"not found"))
      C <- NA
    }
  }
  return(C)
}