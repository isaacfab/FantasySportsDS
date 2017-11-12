#download a list of injuries and suspensions

#Download fantasy football injuries and suspensions from yahoo
injury<-function(){
  inel<-XML::readHTMLTable("http://www.rotoworld.com/teams/injuries/nfl/",stringsAsFactors = FALSE)
  injuryData<-data.frame()
  for(i in 3:length(inel)){
    temp<-inel[[i]]
    colnames(temp)<-c("Name", "Description",        "POS",     "Status",  "Date",    "Injury",  "Returns")
    injuryData<-rbind(injuryData,temp)
  }
  #add columns for descrpitve name and game
  injuryData$DescriptiveName<-paste0(injuryData$Name," ",injuryData$POS)

  injuryData
}


