#This is a set of functions to scrape historical NFL data from ESPN and NFL.com

#function to return historical stats for any given week
#build this out! could be very cool opportunity
historyESPN<-function(x,y){
  hist<-data.frame()
  for(z in seq(0,550,50)){
    url<-paste("http://games.espn.go.com/ffl/leaders?&scoringPeriodId=",x,"&seasonId=",y,"&startIndex=",z,sep="")
    hold<-XML::readHTMLTable(url,stringsAsFactors = FALSE,header=TRUE)$playertable_0
    hold<-hold[-c(2,5,10,14,19,23)]
    colnames(hold)<-c("Player","OPP","GAME_OUTCOME","COMP_ATT","PASS_YDS","PASS_TD","INT","RUSH_ATT","RUSH_YDS","RUSH_TD","REC","REC_YARDS",
                      "REC_TD","TAR","2PC","FUML","ST_DST_TD","PTS")
    hold<-hold[2:length(hold[,1]),]
    hist<-rbind(hist,hold)
  }
  hist$week<-x
  hist$year<-y
  return(hist)
}

historyNFL<-function(x,y,type="REG"){
  nfl_cat<-c("Passing","Rushing","Receiving","Placekick")
  nfl_cat_table<-c("passer","rusher","receiver","kicker")
  for(i in 1:length(nfl_cat)){
    url<-paste("http://www.nfl.com/stats/weeklyleaders?week=",x,"&season=",y,"&type=",type,"&showCategory=",nfl_cat[i],sep="")
    assign(paste(nfl_cat_table[i],sep=""),XML::readHTMLTable(url,stringsAsFactors = FALSE,strip.white=TRUE,header=TRUE)[[nfl_cat_table[i]]])
  }#close for loop
    colnames(passer)<-c("Name","Team","Opp","Score","Pass_Comp","Pass_Att","Pass_Yds","Pass_TD","Int","Sack","Pass_Fumb","Pass_Rate")
    colnames(rusher)<-c("Name","Team","Opp","Score","Rush_Att","Rush_Yds","Rush_Avg","Rush_TD","Rush_Fumb")
    colnames(receiver)<-c("Name","Team","Opp","Score","Rec","Rec_Yds","Rec_Avg","Rec_TD","Rec_Fumb")
    colnames(kicker)<-c("Name","Team","Opp","Score","FG_Made","FG_Att","Xpt_Made","Xpt_Att","Kick_Pts")

    #merge the four values into one dataframe
    passer$Opp<-gsub("[\r\t\n]", "", passer$Opp)
    rusher$Opp<-gsub("[\r\t\n]", "", rusher$Opp)
    receiver$Opp<-gsub("[\r\t\n]", "", receiver$Opp)
    kicker$Opp<-gsub("[\r\t\n]", "", kicker$Opp)

    passer$Score<-gsub("[\r\t\n]", "", passer$Score)
    rusher$Score<-gsub("[\r\t\n]", "", rusher$Score)
    receiver$Score<-gsub("[\r\t\n]", "", receiver$Score)
    kicker$Score<-gsub("[\r\t\n]", "", kicker$Score)

    passer$Position<-'QB'
    rusher$Position<-'RB'
    receiver$Position<-'WR_TE'
    kicker$Position<-'K'

    hold<-merge(passer,receiver,by=c("Name","Team","Opp","Score","Position"),all=TRUE)
    hold<-merge(hold,rusher,by=c("Name","Team","Opp","Score","Position"),all=TRUE)
    hold<-merge(hold,kicker,by=c("Name","Team","Opp","Score","Position"),all=TRUE)
    hold[is.na(hold)]<-0
    hold$Away_Game<-grepl("@",hold$Opp)
    hold$week<-x
    hold$year<-y
    return(hold)
}#close function
