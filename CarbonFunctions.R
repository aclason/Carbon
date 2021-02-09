#### Carbon Functions

#I'm making changes
change <-1



#Mineral soil carbon:
Min_SOC <- function(Soc, BD, depth, CoarseFrags){
  Min_SOC <- Soc * BD * depth * (1- CoarseFrags)
  return(Min_SOC)
}

#Tree carbon - could make this more generic by accepting different ways of writing species for instance.
TreeBiomassFN <- function(Species,DBH,HT){
  if(is.na(Species)){
    print(paste("Species is not found"))
    Sp_C <- NA
  } else {
    if(Species=="At"){
      Sp_C <-(0.0143*DBH^1.9369*HT^1.0579)+(0.0063*DBH^2.0744*HT^0.6691)+
        (0.0150*DBH^2.9068*HT^-0.6306)+(0.0284*DBH^1.6020)
    } else if(Species=="Ac"){
      Sp_C <-(0.0051*DBH^1.0697*HT^2.2748)+(0.0009*DBH^1.3061*HT^2.0109)+
        (0.0131*DBH^2.5760)+(0.0224*DBH^1.8368)
    } else if(Species=="Cw"){
      Sp_C <-(0.0188*DBH^1.3376*HT^1.5293)+(0.0002*DBH^2.4369*HT^1.1315)+
        (0.0611*DBH^1.9208)+(0.1097*DBH^1.5530)
    } else if(Species=="Bl"){
      Sp_C <-(0.0220*DBH^1.6469*HT^1.1714)+(0.0061*DBH^1.8603*HT^0.7693)+
        (0.0265*DBH^3.6747*HT^-1.5958)+(0.0509*DBH^2.9909*HT^-1.2271)
    } else if(Species=="Ep"){
      Sp_C <-(0.0333*DBH^2.0794*HT^0.6811)+(0.0079*DBH^1.9905*HT^0.6553)+
        (0.0253*DBH^3.1518*HT^-0.9083)+(0.1361*DBH^2.2978*HT^-1.0934)
    } else if(Species=="Hw"){
      Sp_C <-(0.0113*DBH^1.9332*HT^1.1125)+(0.0019*DBH^2.3356*HT^0.6371)+
        (0.0609*DBH^2.0021)+(0.2656*DBH^2.0107*HT^-0.7963)
    } else if(Species=="Pl"){
      Sp_C <-(0.0239*DBH^1.6827*HT^1.1878)+(0.0117*DBH^1.6398*HT^0.6524)+
        (0.0285*DBH^3.3764*HT^-1.4395)+(0.0769*DBH^2.6834*HT^-1.2484)
    } else if(Species=="Sx"){
      Sp_C <-(0.0133*DBH^1.3303*HT^1.6877)+(0.0086*DBH^1.6216*HT^0.8192)+
        (0.0428*DBH^2.7965*HT^-0.7328)+(0.0854*DBH^2.4388*HT^-0.7630)
    } else if(Species=="Fd"){
      Sp_C <- (0.0191*DBH^1.5365*HT^1.3634)+(0.0083*DBH^2.4811)+
        (0.0351*DBH^2.2421)+(0.0718*DBH^2.2935*HT^-0.4744)
    } else if(Species=="UC"){
      Sp_C <-(0.0276*DBH^1.6868*HT^1.0953)+(0.0101*DBH^1.8486*HT^0.5525)+
        (0.0313*DBH^2.9974*HT^-1.0383)+(0.1379*DBH^2.3981*HT^-1.0418)
    } else if(Species=="Lw"){ #Using Unknown conifer
      Sp_C <-(0.0276*DBH^1.6868*HT^1.0953)+(0.0101*DBH^1.8486*HT^0.5525)+
        (0.0313*DBH^2.9974*HT^-1.0383)+(0.1379*DBH^2.3981*HT^-1.0418)
    } else {
      print(paste("Species",Species,"not found"))
      Sp_C <- NA
    }
  }
  return(Sp_C)
}

