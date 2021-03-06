Fantasy Sports Data Science R tool kit
================
Isaac J. Faber
November, 2017

This package provides a set of functions to create optimal line-ups for use in fantasy sports leagues. The purpose of this narrative is to show how to use the FantasySportsDS package. First, load the packages.

``` r
library(devtools)
library(stringdist)
library(ICSNP)
install_github('isaacfab/FantasySportsDS')
library(FantasySportsDS)
```

For starters, you will need to read in the data from your league, Draft Kings - NFL is used as the first application. For now the package is limited to functions for the Draft Kings - NFL and the 2016 season. A '.csv' file is provided by the Draft Kings site when you build a line up. We want to create a descriptive name for each player in order to join each row with other information later. Here we also create a variable (binary only 1 or 0) that if set to 1 requires that player to be in a given lineup.

``` r
data(DKSalariesExample)

DKSalariesExample$DescriptiveName<-paste(DKSalariesExample$Name,DKSalariesExample$teamAbbrev,DKSalariesExample$Position,sep=" ")

DKSalariesExample$Require<-0

print(head(DKSalariesExample))
```

    ##   Position              Name Salary           GameInfo AvgPointsPerGame
    ## 1       WR       Julio Jones   9300 Phi@Atl 07:10PM ET           21.293
    ## 2       WR Odell Beckham Jr.   9200 NYG@Dal 08:30PM ET           26.417
    ## 3       WR  Demaryius Thomas   9100 Bal@Den 04:25PM ET           22.812
    ## 4       WR     Antonio Brown   8900  Pit@NE 08:30PM ET           25.682
    ## 5       WR        Dez Bryant   8700 NYG@Dal 08:30PM ET           19.033
    ## 6       QB     Aaron Rodgers   8600  GB@Chi 01:00PM ET           23.428
    ##   teamAbbrev          DescriptiveName Require
    ## 1        Atl       Julio Jones Atl WR       0
    ## 2        NYG Odell Beckham Jr. NYG WR       0
    ## 3        Den  Demaryius Thomas Den WR       0
    ## 4        Pit     Antonio Brown Pit WR       0
    ## 5        Dal        Dez Bryant Dal WR       0
    ## 6         GB      Aaron Rodgers GB QB       0

Several things should be observed from the raw data. First, each player has a 'salary' and a line up must be selected such that the total salary does not exceed a given cap (default is 50,000). In addition, a column has the average number of points for a given player. This number is the mean for this season. Draft Kings will include players in the line up selection that are either injured or suspended. You, obviously, want to avoid selecting these players. On the actual website their names have a flag next to them but for analysis or automation we have to remove them ourselves. The first function the FantasySportsDS package provides is an automated method for retrieving current injuries or suspensions (from Yahoo). The first function returns players that are currently ineligible to play. Keep in mind that these functions scrape current data!

``` r
data.inel<-FantasySportsDS::injury()

print(data.inel[1:3,])
```

    ##            Name
    ## 1  Drew Stanton
    ## 2  Tyvon Branch
    ## 3 Carson Palmer
    ##                                                                                                                         Description
    ## 1           kneeESPN's Chris Mortensen reports QB Drew Stanton sprained his knee in Week 10 against the Seahawks.Week to weekNov 11
    ## 2 kneeCardinals SS Tyvon Branch suffered a torn ACL in Week 10 against the Seahawks and is done for the season.Out for seasonNov 10
    ## 3                                armCarson Palmer (arm, IR) says there's a chance he could return in Week 16.Out indefinitelyNov 11
    ##   POS    Status   Date Injury          Returns  DescriptiveName
    ## 1  QB Sidelined Nov 11   knee     Week to week  Drew Stanton QB
    ## 2  DB      I.L. Nov 10   knee   Out for season  Tyvon Branch DB
    ## 3  QB      I.L. Oct 26    arm Out indefinitely Carson Palmer QB

We want to join the injury information to the draft kings DKSalariesExample.

``` r
DKSalariesExample$injury<-"None"
DKSalariesExample$gdc<-"None"
for(i in 1:length(DKSalariesExample[,1])){
    j<-stringdist::amatch(DKSalariesExample$DescriptiveName[i],data.inel$DescriptiveName,maxDist=5)
    if(!is.na(j)){
      DKSalariesExample$injury[i]<-data.inel[["Injury"]][j]
    }
}

print(DKSalariesExample[1:5,])
```

    ##   Position              Name Salary           GameInfo AvgPointsPerGame
    ## 1       WR       Julio Jones   9300 Phi@Atl 07:10PM ET           21.293
    ## 2       WR Odell Beckham Jr.   9200 NYG@Dal 08:30PM ET           26.417
    ## 3       WR  Demaryius Thomas   9100 Bal@Den 04:25PM ET           22.812
    ## 4       WR     Antonio Brown   8900  Pit@NE 08:30PM ET           25.682
    ## 5       WR        Dez Bryant   8700 NYG@Dal 08:30PM ET           19.033
    ##   teamAbbrev          DescriptiveName Require injury  gdc
    ## 1        Atl       Julio Jones Atl WR       0   None None
    ## 2        NYG Odell Beckham Jr. NYG WR       0   None None
    ## 3        Den  Demaryius Thomas Den WR       0   None None
    ## 4        Pit     Antonio Brown Pit WR       0   None None
    ## 5        Dal        Dez Bryant Dal WR       0   None None

Now you have to make some decisions on what players you want to exclude. Obviously, you want to rule out anyone who is 'Out' but there is also a category of 'Questionable.' This category often includes high profile players that may need a case-by-case decision. For this example, we will only keep players that are either not injured or are probable. You can subset however you like.

``` r
DKSalariesExample<-DKSalariesExample[DKSalariesExample$gdc=="None"|DKSalariesExample$gdc=="P",]
table(DKSalariesExample$injury)
```

    ## 
    ##            -     achilles        ankle          arm         back 
    ##            9            1            8            3            2 
    ##       biceps         calf     clavicle   concussion        elbow 
    ##            1            1            1            7            1 
    ##         foot        groin    hamstring          hip      illness 
    ##            2            3            7            1            2 
    ##         knee knee surgery          leg         neck         None 
    ##           13            9            1            3          862 
    ##     shoulder          toe  undisclosed        wrist 
    ##            6            1            1            3

Now that you have removed any players that will likely not be on the field, it is time to start thinking about how to forecast player performance. The following function scrapes fantasy point predictions from 8 fantasy football websites and gives two types of averages. The only arguments that this function accepts is an integer value for the week of the season and the year. Like the injuries, this function returns current values.

``` r
forecast<-FantasySportsDS::CombinedForecasts(1,2015)

print(forecast[1:3,])
```

    ##       DescriptiveName FFToday_f CBS_f Yahoo_f NFL_f FFPC_f NFFC_f
    ## 2 Aaron Rodgers GB QB      30.0  32.7    26.7  32.7   30.0   36.0
    ## 3    Tony Romo DAL QB      27.3  30.3    24.3  30.3   27.3   33.3
    ## 4     Tom Brady NE QB      27.0  28.0    23.0  28.0   26.0   32.0
    ##   Average_Robust     Mean       SD
    ## 2          31.35 31.35000 3.179151
    ## 3          28.80 28.80000 3.146427
    ## 4          27.50 27.33333 2.943920

Need to combine these forecasts with the data.

``` r
n<-colnames(forecast)
n<-n[2:length(n)]
for(i in 1:length(n)){
  DKSalariesExample[[n[i]]]<-0
}

#combine the forecasts and the Draft Kings file
for(i in 1:length(DKSalariesExample[,1])){
  j<-stringdist::amatch(DKSalariesExample$DescriptiveName[i],forecast$DescriptiveName,maxDist=5)
  if(!is.na(j)){
    for(k in 1:length(n)){
      DKSalariesExample[[n[k]]][i]<-forecast[[k+1]][j]
    }
  }
}
```

The quality of the lineup that is played is dependent on how good the forecasts are. There is room here for you to complete your own forecasts from any number of sources such as regression, machine learning, other expert predictions, ownership rates and so on. However, even if you have perfect forecasts you would still have the problem of knowing which combination of players results in the greatest amount of fantasy points (the possible combinations of teams is enormous).

The following function preforms a linear program and returns the optimal lineup from a given set of forecasts. In other words if your forecast are perfect this function will give you the team with the highest score. There are three functional arguments; our data, the forecast column name and the salary cap (defaults to 50,000). Here are three optimal lineups using different sources of forecasting and constraints.

``` r
#Optimal Lineup with Average Points Per Game forecast
opt<-FantasySportsDS::DKoptLineUpNFL(DKSalariesExample,"AvgPointsPerGame")
print(opt[,1:4])
```

    ##     Position              Name Salary           GameInfo
    ## 95        QB      Mark Sanchez   5400 Phi@Atl 07:10PM ET
    ## 51        RB    DeMarco Murray   6700 Phi@Atl 07:10PM ET
    ## 349       RB    Ahmad Bradshaw   3000 Ind@Buf 01:00PM ET
    ## 2         WR Odell Beckham Jr.   9200 NYG@Dal 08:30PM ET
    ## 4         WR     Antonio Brown   8900  Pit@NE 08:30PM ET
    ## 243       WR   Martavis Bryant   3900  Pit@NE 08:30PM ET
    ## 801       TE      Dennis Pitta   2500 Bal@Den 04:25PM ET
    ## 25      FLEX      Le'Veon Bell   7600  Pit@NE 08:30PM ET
    ## 783      DST           Eagles    2700 Phi@Atl 07:10PM ET

``` r
#Optimal Lineup with FFToday forecast and require that Russell Wilson is in the lineup
DKSalariesExample$Require[DKSalariesExample$DescriptiveName=="Russell Wilson Sea QB"]<-1
fft_opt<-FantasySportsDS::DKoptLineUpNFL(DKSalariesExample,"FFToday_f")
print(fft_opt[,1:4])
```

    ##     Position            Name Salary           GameInfo
    ## 17        QB  Russell Wilson   7800 Sea@StL 01:00PM ET
    ## 212       RB     Doug Martin   4500  Ten@TB 04:25PM ET
    ## 232       RB     Chris Ivory   4100 Cle@NYJ 01:00PM ET
    ## 13        WR    Randall Cobb   8000  GB@Chi 01:00PM ET
    ## 218       WR   Davante Adams   4400  GB@Chi 01:00PM ET
    ## 255       WR  Stevie Johnson   3700  Det@SD 04:05PM ET
    ## 43        TE  Rob Gronkowski   7000  Pit@NE 08:30PM ET
    ## 20      FLEX Adrian Peterson   7700  Min@SF 10:20PM ET
    ## 948      DST        Raiders    2300 Cin@Oak 04:25PM ET

``` r
#Optimal Lineup with Average forecast but lower salary cap
avg_opt<-FantasySportsDS::DKoptLineUpNFL(DKSalariesExample,"Average_Robust",45000)
print(avg_opt[,1:4])
```

    ##     Position           Name Salary           GameInfo
    ## 17        QB Russell Wilson   7800 Sea@StL 01:00PM ET
    ## 212       RB    Doug Martin   4500  Ten@TB 04:25PM ET
    ## 232       RB    Chris Ivory   4100 Cle@NYJ 01:00PM ET
    ## 218       WR  Davante Adams   4400  GB@Chi 01:00PM ET
    ## 255       WR Stevie Johnson   3700  Det@SD 04:05PM ET
    ## 449       WR    Jaron Brown   3000  NO@Ari 04:05PM ET
    ## 43        TE Rob Gronkowski   7000  Pit@NE 08:30PM ET
    ## 13      FLEX   Randall Cobb   8000  GB@Chi 01:00PM ET
    ## 948      DST       Raiders    2300 Cin@Oak 04:25PM ET
