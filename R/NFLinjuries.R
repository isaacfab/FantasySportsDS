#download a list of injuries and suspensions

#Download fantasy football injuries and suspensions from yahoo
ineligible<-function(){
  inel<-XML::readHTMLTable("http://football.fantasysports.yahoo.com/f1/injuries",stringsAsFactors = FALSE)$gamedayscalltable
  #add columns for descrpitve name and game
  inel$DescriptiveName<-""
  inel$game<-""

  for(i in 1:length(inel[,1])){
    inel$DescriptiveName[i]<-stringr::str_trim(stringr::str_split(inel$Name,"\n")[[i]],side="both")[2]
    inel$game[i]<-stringr::str_trim(stringr::str_split(inel$Name,"\n")[[i]],side="both")[6]
  }
inel$DescriptiveName<-gsub("-","",inel$DescriptiveName)
inel<-subset(inel,select=c("DescriptiveName","Injury Type","game"))
inel
}

gamedaycall<-function(){
  #scrape raw data
  gdc<-XML::readHTMLTable("http://football.fantasysports.yahoo.com/f1/gamedaycalls",stringsAsFactors = FALSE)$gamedayscalltable
  #add columns for descrpitve name and game
  gdc$DescriptiveName<-""
  gdc$game<-""

  for(i in 1:length(gdc[,1])){
    gdc$DescriptiveName[i]<-stringr::str_trim(stringr::str_split(gdc$Name,"\n")[[i]],side="both")[2]
    gdc$game[i]<-stringr::str_trim(stringr::str_split(gdc$Name,"\n")[[i]],side="both")[6]
  }

  gdc$DescriptiveName<-gsub("-","",gdc$DescriptiveName)
  gdc<-subset(gdc,select=c("DescriptiveName","Status","game"))
  gdc
  }
