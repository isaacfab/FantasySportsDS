#this is a set of optimization functions for draft kings line ups.  NFL and College
#you must pass in the draft kings csv line up after injuries and inelible players are filtered out


DKoptLineUpNFL<-function(x,y,z=50000){

x<-x[order(x$Position),]
pos<-as.data.frame(table(x$Position))

#build the optimzation lp

#objective function (need to adjust this to forecasted points)
f.obj<-x[[y]]

#constraints matrix
#salary amounts (draft king orign) total can be upto $50,000
salary <- as.vector(x$Salary)
req<-as.vector(x$Require)
tot<-sum(req)
#length value for convenience
l<-length(salary)
#team size must be a total of 9
team_size<-as.vector(rep(1,l))
#defense must chose 1
dst<-c(rep(1,pos[1,2]),rep(0,(l-pos[1,2])))
#quarterback must chose 1
qb<-c(rep(0,pos[1,2]),rep(1,pos[2,2]),rep(0,l-sum(pos[1,2],pos[2,2])))
#running back can choose upto 3
rb<-c(rep(0,sum(pos[1,2],pos[2,2])),rep(1,pos[3,2]),rep(0,l-sum(pos[1,2],pos[2,2],pos[3,2])))
#tight end can choose upto 2
te<-c(rep(0,sum(pos[1,2],pos[2,2],pos[3,2])),rep(1,pos[4,2]),rep(0,pos[5,2]))
#wide reciever can choose upto 4
wr<-c(rep(0,l-pos[5,2]),rep(1,pos[5,2]))
#must choose at least 2 runningbacks
rb2<-c(rep(0,sum(pos[1,2],pos[2,2])),rep(1,pos[3,2]),rep(0,l-sum(pos[1,2],pos[2,2],pos[3,2])))
#must choose at least 1 tight end
te2<-c(rep(0,sum(pos[1,2],pos[2,2],pos[3,2])),rep(1,pos[4,2]),rep(0,pos[5,2]))
#must choose at least 3 wide recievers
wr2<-c(rep(0,l-pos[5,2]),rep(1,pos[5,2]))

#create the constraints matrix
f.con<-rbind(salary,team_size,dst,qb,rb,te,wr,rb2,te2,wr2,req)

#create the direction vector
f.dir<-c("<=","==","==","==","<=","<=","<=",">=",">=",">=","==")

#create the right hand side for the model
#remember that the flex play can be either a rb te or wr
f.rhs<-c(z,9,1,1,3,2,4,2,1,3,tot)

#run the linear program function
lp_sol<-lpSolve::lp("max",f.obj,f.con,f.dir,f.rhs,all.bin=TRUE)

x$picks<-lp_sol$solution
sub_sal<-x[x$picks==1,]

#need to identify which position is flex and then sort by DK lineup requirements
t<-table(as.data.frame(sub_sal$Position))
if(t[3]>2){y="RB"}
if(t[4]>1){y="TE"}
if(t[5]>3){y="WR"}

sub_sal$Position<-as.character(sub_sal$Position)
for(i in 1:length(sub_sal[,1])){
  if(sub_sal$Position[i]==y){
    sub_sal$Position[i]<-c("FLEX")
    break
    }
}
#Draft King required order for line ups
levels<-c("QB","RB","WR","TE","FLEX","DST")
sub_sal$Position<-factor(sub_sal$Position,levels = levels)
sub_sal<-sub_sal[order(sub_sal$Position),]
sub_sal
}

DKoptLineUpCollege<-function(x,y,z=50000){

  x<-x[order(x$Position),]
  pos<-as.data.frame(table(x$Position))

  #build the optimzation lp

  #objective function (need to adjust this to forecasted points)
  f.obj<-x[[y]]

  #constraints matrix
  #salary amounts (draft king orign) total can be upto $50,000
  salary <- as.vector(x$Salary)
  #length value for convenience
  l<-length(salary)
  #team size must be a total of 9
  team_size<-as.vector(rep(1,l))
  #defense must chose 1
  dst<-c(rep(1,pos[1,2]),rep(0,(l-pos[1,2])))
  #quarterback must chose 1
  qb<-c(rep(0,pos[1,2]),rep(1,pos[2,2]),rep(0,l-sum(pos[1,2],pos[2,2])))
  #running back can choose upto 3
  rb<-c(rep(0,sum(pos[1,2],pos[2,2])),rep(1,pos[3,2]),rep(0,l-sum(pos[1,2],pos[2,2],pos[3,2])))
  #tight end can choose upto 2
  te<-c(rep(0,sum(pos[1,2],pos[2,2],pos[3,2])),rep(1,pos[4,2]),rep(0,pos[5,2]))
  #wide reciever can choose upto 4
  wr<-c(rep(0,l-pos[5,2]),rep(1,pos[5,2]))
  #must choose at least 2 runningbacks
  rb2<-c(rep(0,sum(pos[1,2],pos[2,2])),rep(1,pos[3,2]),rep(0,l-sum(pos[1,2],pos[2,2],pos[3,2])))
  #must choose at least 1 tight end
  te2<-c(rep(0,sum(pos[1,2],pos[2,2],pos[3,2])),rep(1,pos[4,2]),rep(0,pos[5,2]))
  #must choose at least 3 wide recievers
  wr2<-c(rep(0,l-pos[5,2]),rep(1,pos[5,2]))

  #create the constraints matrix
  f.con<-rbind(salary,team_size,dst,qb,rb,te,wr,rb2,te2,wr2)

  #create the direction vector
  f.dir<-c("<=","==","==","==","<=","<=","<=",">=",">=",">=")

  #create the right hand side for the model
  #remember that the flex play can be either a rb te or wr
  f.rhs<-c(z,9,1,1,3,2,4,1,1,1)

  #run the linear program function
  lp_sol<-lpSolve::lp("max",f.obj,f.con,f.dir,f.rhs,all.bin=T)

  x$picks<-lp_sol$solution
  sub_sal<-x[x$picks==1,]

}
