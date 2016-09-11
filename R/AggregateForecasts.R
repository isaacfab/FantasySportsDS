#the purpose of this function is to combine all of the individual forecasts into one function
#these functions are defined in the FantasyFootballForecasts.R file

CombinedForecasts<-function(x,y){
  espn<-FantasyFootball2015::ESPN(x,y)
  FS<-FantasyFootball2015::FantasyShark()
  #FP<-FantasyFootball2015::FantasyPros()
  FT<-FantasyFootball2015::FFToday(x,y)
  CBS<-FantasyFootball2015::CBS(x,y)
  YH<-FantasyFootball2015::Yahoo(x,y)
  NFL<-FantasyFootball2015::NFL.com(x,y)
  FFPC<-FantasyFootball2015::FFPC(x,y)
  NFFC<-FantasyFootball2015::NFFC(x,y)

  combined<-espn
  colnames(combined)<-c("DescriptiveName","ESPN_f")
  combined$FantasyShark_f<-combined$ESPN_f
  #combined$FantasyPros_f<-combined$ESPN_f
  combined$FFToday_f<-combined$ESPN_f
  combined$CBS_f<-combined$ESPN_f
  combined$Yahoo_f<-combined$ESPN_f
  combined$NFL_f<-combined$ESPN_f
  combined$FFPC_f<-combined$ESPN_f
  combined$NFFC_f<-combined$ESPN_f
  combined$Average_Robust<-0
  combined$Mean<-0
  combined$SD<-0

#match Fantasy Pros
  #for(i in 1:length(espn[,1])){
  #j<-stringdist::amatch(combined$DescriptiveName[i],FP$DescriptiveName,maxDist=5)
  #  if(!is.na(j)){
  #      combined$FantasyPros_f[i]<-FP$FPTS[j]
  #  }
  #}
#match Fantasy Sharks
  for(i in 1:length(espn[,1])){
    j<-stringdist::amatch(combined$DescriptiveName[i],FS$DescriptiveName,maxDist=5)
    if(!is.na(j)){
      combined$FantasyShark_f[i]<-FS$FPTS[j]
    }
  }
#match FFToday
  for(i in 1:length(espn[,1])){
    j<-stringdist::amatch(combined$DescriptiveName[i],FT$DescriptiveName,maxDist=5)
    if(!is.na(j)){
      combined$FFToday_f[i]<-FT$FPTS[j]
    }
  }
#match CBS
  for(i in 1:length(espn[,1])){
    j<-stringdist::amatch(combined$DescriptiveName[i],CBS$DescriptiveName,maxDist=5)
    if(!is.na(j)){
      combined$CBS_f[i]<-CBS$FPTS[j]
    }
  }
#match Yahoo
  for(i in 1:length(espn[,1])){
    j<-stringdist::amatch(combined$DescriptiveName[i],YH$DescriptiveName,maxDist=5)
    if(!is.na(j)){
      combined$Yahoo_f[i]<-YH$FPTS[j]
    }
  }
#match NFL.com
  for(i in 1:length(espn[,1])){
    j<-stringdist::amatch(combined$DescriptiveName[i],NFL$DescriptiveName,maxDist=5)
    if(!is.na(j)){
      combined$NFL_f[i]<-NFL$FPTS[j]
    }
  }
#match FFPC
  for(i in 1:length(espn[,1])){
    j<-stringdist::amatch(combined$DescriptiveName[i],FFPC$DescriptiveName,maxDist=5)
    if(!is.na(j)){
      combined$FFPC_f[i]<-FFPC$FPTS[j]
    }
  }
#match NFFC
  for(i in 1:length(espn[,1])){
    j<-stringdist::amatch(combined$DescriptiveName[i],NFFC$DescriptiveName,maxDist=5)
    if(!is.na(j)){
      combined$NFFC_f[i]<-NFFC$FPTS[j]
    }
  }

combined[is.na(combined)]<-0
#calculate robust average
  for(i in 1:length(espn[,1])){
    h<-as.numeric(combined[i,2:(length(combined)-3)])
    h<-as.numeric(ICSNP::hl.loc(h))
    combined$Average_Robust[i]<-h
  }

#calculate simple average
  for(i in 1:length(espn[,1])){
    h<-as.numeric(combined[i,2:(length(combined)-3)])
    h<-mean(h)
    combined$Mean[i]<-h
  }


#calculate std dev
  for(i in 1:length(espn[,1])){
    h<-as.numeric(combined[i,2:(length(combined)-3)])
    h<-sd(h)
    combined$SD[i]<-h
  }
  combined
}
