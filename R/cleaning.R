#nfl cleaning of data
hist.nfl.total$Opp_Score<-as.character(stringr::str_extract_all(hist.nfl.total$Score,"-([0-9]+)"))
hist.nfl.total$Opp_Score<-gsub('-','',hist.nfl.total$Opp_Score)
hist.nfl.total$Team_Score<-as.character(stringr::str_extract_all(hist.nfl.total$Score,"([0-9]+)-"))
hist.nfl.total$Team_Score<-gsub('-','',hist.nfl.total$Team_Score)
hist.nfl.total$Score<-as.character(stringr::str_extract_all(hist.nfl.total$Score,"([0-9]+)-([0-9]+)"))


dfToElasticJSON(hist.nfl.total,'nfl.json','playergame')
dfToElasticMapping(hist.nfl.total,'nflMapping.json',type='playergame')
