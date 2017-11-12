#Websites change often!  These functions need to be updated every season
#these functions are from the 2016 - 2017 NFL season
#############none of these HTML functions are ready yet!!!!###############

#Download fantasy football projections from espn
ESPN<-function(x,y){
espn<-data.frame()
#only returns 40 results at a time need a loop to collect all by changing startIndex
for (i in seq(0, 560, 40)){
  url<-paste0("http://games.espn.com/lm/tools/projections?&scoringPeriodId=",x,"&seasonId=",y,"&startIndex=",i)
  hold<-XML::readHTMLTable(url,stringsAsFactors = FALSE,header=TRUE)$playertable_0
  colnames(hold)<-hold[1,]
  hold<-hold[2:41,]
  espn<-rbind(espn,hold)
}
#remove some unwanted characters that espn thinks are important
espn$`PLAYER, TEAM POS`<-gsub("\\*","",espn$`PLAYER, TEAM POS`)
espn$`PLAYER, TEAM POS`<-gsub(",","",espn$`PLAYER, TEAM POS`)
espn$`PLAYER, TEAM POS`<-gsub("\\s\\sP","",espn$`PLAYER, TEAM POS`)
espn$`PLAYER, TEAM POS`<-gsub("\\s\\sIR","",espn$`PLAYER, TEAM POS`)
espn$`PLAYER, TEAM POS`<-gsub("\\s\\sO","",espn$`PLAYER, TEAM POS`)
espn$`PLAYER, TEAM POS`<-gsub("\\s\\sSSPD","",espn$`PLAYER, TEAM POS`)
espn$`PLAYER, TEAM POS`<-gsub("\\s\\sD","",espn$`PLAYER, TEAM POS`)
espn$`PLAYER, TEAM POS`<-gsub("\\s\\sQ","",espn$`PLAYER, TEAM POS`)
espn$`PLAYER, TEAM POS`<-gsub("D/ST\\sD/ST","DST",espn$`PLAYER, TEAM POS`)

ES<-subset(espn,select=c("PLAYER, TEAM POS","PTS"))
colnames(ES)<-c("DescriptiveName","FPTS")
ES
}

#Download fantasy football projections from fantasysharks.com
FantasyShark<-function(x){
#offensive stats
fantasyshark<-read.csv(paste0("http://www.fantasysharks.com/apps/bert/forecasts/projections.php?csv=1&Sort=&Segment=",(x+563),"&Position=99&scoring=1&League=&uid=4&uid2=&printable="),strip.white=TRUE)
fantasyshark$First<-""
fantasyshark$Last<-""
fantasyshark$Position<-as.character(fantasyshark$Pos)
fantasyshark$Position[fantasyshark$Position == "D"]<-"DST"

for(i in 1:length(fantasyshark[,1])){
  temp<-stringr::str_split(fantasyshark$Player[i],", ")[[1]]
  fantasyshark$First[i]<-temp[2]
  fantasyshark$Last[i]<-temp[1]
}

fantasyshark$DescriptiveName<-paste(fantasyshark$First," ",fantasyshark$Last," ",
                                    fantasyshark$Team," ",fantasyshark$Pos,sep="")

for(i in 1:length(fantasyshark[,1])){
  if(fantasyshark$Position[i]=="DST")
  fantasyshark$DescriptiveName[i]<-paste(fantasyshark$Last[i], " ",fantasyshark$Position[i],sep="")
}

FS<-subset(fantasyshark,select=c("DescriptiveName","Pts"))
colnames(FS)<-c("DescriptiveName","FPTS")

FS
}

#quick function for munging data
FFTdataMunge<-function(q, pos){
  colnames(q)<-q[1,]
  q<-q[2:length(q[,1]),2:length(q)]
  colnames(q)[1]<-"player"

  q$DescriptiveName<-paste(q$player, " ", q$Team, " ",pos,sep="")
  q<-subset(q,select=c("DescriptiveName","FFPts"))
  colnames(q)<-c("DescriptiveName","FPTS")
  q
}

#Download fantasy football projections from fftoday
FFToday<-function(x,y){
  #fftoday league ID is 1
  #PosID are 10 for QB, 20 for RB, 30 for WR and 40 for TE no DST
  url<-paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=",y,"&GameWeek=")
  q<-XML::readHTMLTable(paste(url,x,"&PosID=10&LeagueID=1",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  r<-XML::readHTMLTable(paste(url,x,"&PosID=20&LeagueID=1",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  w<-XML::readHTMLTable(paste(url,x,"&PosID=30&LeagueID=1",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  t<-XML::readHTMLTable(paste(url,x,"&PosID=40&LeagueID=1",sep=""),stringsAsFactors = FALSE)[11]$'NULL'

 q<-FFTdataMunge(q,"QB")
 r<-FFTdataMunge(r,"RB")
 w<-FFTdataMunge(w,"WR")
 t<-FFTdataMunge(t,"TE")

FT<-rbind(q,r,w,t)
FT
}

CBS<-function(x,y){
  #cbs league ID is 26943
  #PosID are 10 for QB, 20 for RB, 30 for WR and 40 for TE no DST
  url<-paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=",y,"&GameWeek=")
  q<-XML::readHTMLTable(paste(url,x,"&PosID=10&LeagueID=26943",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  r<-XML::readHTMLTable(paste(url,x,"&PosID=20&LeagueID=26943",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  w<-XML::readHTMLTable(paste(url,x,"&PosID=30&LeagueID=26943",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  t<-XML::readHTMLTable(paste(url,x,"&PosID=40&LeagueID=26943",sep=""),stringsAsFactors = FALSE)[11]$'NULL'

  q<-FFTdataMunge(q,"QB")
  r<-FFTdataMunge(r,"RB")
  w<-FFTdataMunge(w,"WR")
  t<-FFTdataMunge(t,"TE")

  FT<-rbind(q,r,w,t)
  FT
}

Yahoo<-function(x,y){
  #yahoo league ID is 17
  #PosID are 10 for QB, 20 for RB, 30 for WR and 40 for TE no DST
  url<-paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=",y,"&GameWeek=")
  q<-XML::readHTMLTable(paste(url,x,"&PosID=10&LeagueID=17",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  r<-XML::readHTMLTable(paste(url,x,"&PosID=20&LeagueID=17",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  w<-XML::readHTMLTable(paste(url,x,"&PosID=30&LeagueID=17",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  t<-XML::readHTMLTable(paste(url,x,"&PosID=40&LeagueID=17",sep=""),stringsAsFactors = FALSE)[11]$'NULL'

  q<-FFTdataMunge(q,"QB")
  r<-FFTdataMunge(r,"RB")
  w<-FFTdataMunge(w,"WR")
  t<-FFTdataMunge(t,"TE")

  FT<-rbind(q,r,w,t)
  FT
}

NFL.com<-function(x,y){
  #nfl.com league ID is 143908
  #PosID are 10 for QB, 20 for RB, 30 for WR and 40 for TE no DST
  url<-paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=",y,"&GameWeek=")
  q<-XML::readHTMLTable(paste(url,x,"&PosID=10&LeagueID=143908",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  r<-XML::readHTMLTable(paste(url,x,"&PosID=20&LeagueID=143908",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  w<-XML::readHTMLTable(paste(url,x,"&PosID=30&LeagueID=143908",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  t<-XML::readHTMLTable(paste(url,x,"&PosID=40&LeagueID=143908",sep=""),stringsAsFactors = FALSE)[11]$'NULL'

  q<-FFTdataMunge(q,"QB")
  r<-FFTdataMunge(r,"RB")
  w<-FFTdataMunge(w,"WR")
  t<-FFTdataMunge(t,"TE")

  FT<-rbind(q,r,w,t)
  FT
}


FFPC<-function(x,y){
  #FFPC league ID is 107437
  #PosID are 10 for QB, 20 for RB, 30 for WR and 40 for TE no DST
  url<-paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=",y,"&GameWeek=")
  q<-XML::readHTMLTable(paste(url,x,"&PosID=10&LeagueID=107437",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  r<-XML::readHTMLTable(paste(url,x,"&PosID=20&LeagueID=107437",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  w<-XML::readHTMLTable(paste(url,x,"&PosID=30&LeagueID=107437",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  t<-XML::readHTMLTable(paste(url,x,"&PosID=40&LeagueID=107437",sep=""),stringsAsFactors = FALSE)[11]$'NULL'

  q<-FFTdataMunge(q,"QB")
  r<-FFTdataMunge(r,"RB")
  w<-FFTdataMunge(w,"WR")
  t<-FFTdataMunge(t,"TE")

  FT<-rbind(q,r,w,t)
  FT
}

NFFC<-function(x,y){
  #NFFC league ID is 5
  #PosID are 10 for QB, 20 for RB, 30 for WR and 40 for TE no DST
  url<-paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=",y,"&GameWeek=")
  q<-XML::readHTMLTable(paste(url,x,"&PosID=10&LeagueID=5",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  r<-XML::readHTMLTable(paste(url,x,"&PosID=20&LeagueID=5",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  w<-XML::readHTMLTable(paste(url,x,"&PosID=30&LeagueID=5",sep=""),stringsAsFactors = FALSE)[11]$'NULL'
  t<-XML::readHTMLTable(paste(url,x,"&PosID=40&LeagueID=5",sep=""),stringsAsFactors = FALSE)[11]$'NULL'

  q<-FFTdataMunge(q,"QB")
  r<-FFTdataMunge(r,"RB")
  w<-FFTdataMunge(w,"WR")
  t<-FFTdataMunge(t,"TE")

  FT<-rbind(q,r,w,t)
  FT
}

#Download fantasy football projections from FantasyPros.com
#this site currently uses an https protocol and the XML packagee does not work!
#FantasyPros<-function(){
#qb_fp <- XML::readHTMLTable("http://www.fantasypros.com/nfl/projections/qb.php", stringsAsFactors = FALSE)$data
#rb_fp <- XML::readHTMLTable("http://www.fantasypros.com/nfl/projections/rb.php", stringsAsFactors = FALSE)$data
#wr_fp <- XML::readHTMLTable("http://www.fantasypros.com/nfl/projections/wr.php", stringsAsFactors = FALSE)$data
#te_fp <- XML::readHTMLTable("http://www.fantasypros.com/nfl/projections/te.php", stringsAsFactors = FALSE)$data
#for(i in 2:length(qb_fp)){
#  qb_fp[,i]<-as.numeric(qb_fp[,i])
#}
#qb_fp$FPTS2<-qb_fp[,4]/25+qb_fp[,5]*4-qb_fp[,6]+qb_fp[,8]/10+qb_fp[,9]*6-qb_fp[,10]

#for(i in 2:length(rb_fp)){
#  rb_fp[,i]<-as.numeric(rb_fp[,i])
#}
#rb_fp$FPTS2<-rb_fp[,3]/10+rb_fp[,4]*6+rb_fp[,6]/10+rb_fp[,7]*6-rb_fp[,8]
#Combine all positions into one data frame
#qb<-subset(qb_fp, select=c("Player", "FPTS"))
#qb$DescriptiveName<-paste(qb$Player, " QB",sep="")
#qb$Position<-"QB"
#rb<-subset(rb_fp, select=c("Player", "FPTS"))
#rb$DescriptiveName<-paste(rb$Player, " RB",sep="")
#rb$Position<-"RB"
#wr<-subset(wr_fp, select=c("Player", "FPTS"))
#wr$DescriptiveName<-paste(wr$Player, " WR",sep="")
#wr$Position<-"WR"
#te<-subset(te_fp, select=c("Player", "FPTS"))
#te$DescriptiveName<-paste(te$Player, " TE",sep="")
#te$Position<-"TE"
#FP<-rbind(qb,rb,wr,te)
#FP<-subset(FP,select=c("DescriptiveName","FPTS"))

#}



