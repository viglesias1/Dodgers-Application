"Here is the project I did for Saberseminar, I realize there are definitely ways of making this run faster
such as recursion or vectorizing some of the for loops and if I were restarting this, I would try to use those more, but when I wrote most
of the code I was just starting in R. I also have way too much hardcoding which I was doing because I was trying to finish the projectfor Saberseminar and
it was quicker for me to hardcode than do it properly"
setwd("~/Downloads")
library(dplyr)
library(ggplot2)
data<-read.csv("2016.csv") #calls in data
data<-rbind(data,read.csv("2015.csv"))
data<-rbind(data,read.csv("2014.csv"))
data<-rbind(data,read.csv("2013.csv"))
data<-rbind(data,read.csv("2012.csv")) 
data<-rbind(data,read.csv("2011.csv")) 
data<-rbind(data,read.csv("2010.csv")) 
data<-rbind(data,read.csv("2009.csv")) 
data<-rbind(data,read.csv("2008.csv")) 
games9<-unique(filter(data,inning==9)$game_pk) #picks only games that went to 9 innings
games<-unique(data$game_pk)
badgames<-vector()
for(i in 1:length(games)){
  if(games[i] %in% games9){
  }
  else{
    badgames[length(badgames)+1]<-games[i]
  }
}
'%ni%' <- Negate('%in%')
data<-data[data$game_pk %ni% badgames,]
data<-data %>% distinct(game_pk,at_bat_number,pitch_number, .keep_all = TRUE) #From what I looked at there were some errors where the pitch was recorded twice though not exact duplicates. I removed these as it didn't make sense to me.
data<-arrange(data,game_year,game_pk,at_bat_number,pitch_number) #sorts by game and at_bat_number to help later with deltas
paswitches<-rep(0,length(data$inning_topbot)) #sees where the PA changes
paswitches[1]=1
for(i in 2:length(data$inning_topbot)){
  if(!(identical(data$at_bat_number[i],data$at_bat_number[i-1]))){
    paswitches[i]=1
  }
}
paswitches<-as.data.frame(paswitches)
data<-cbind(data,paswitches)
for(i in 1:length(data$X)){#Data errors. Very annoying
  if(data$game_pk[i]==288278 & data$at_bat_number[i]==68){
    data$inning[i]=9
  }
  if(data$game_pk[i]==288506 & data$at_bat_number[i]==81){
    data$inning[i]=9
  }
}
datafilter<-filter(data, paswitches == 1) #picks the starts of PA's
datafilter<-filter(datafilter,inning<=9) #removes later than 9th (we only want to look at the first 8 innings so that we can see the score at the end of the PA, we keep 9th for now to get score at end of 8th)
re24data<-select(datafilter,game_year,game_pk,home_team,at_bat_number,inning,inning_topbot,outs_when_up,bat_score,fld_score,on_1b,on_2b,on_3b) #selects data to create base-out run expectancy matrix
re24data<-arrange(re24data,game_year,game_pk,at_bat_number) #sorts by game and at_bat_number to help later with deltas
switches<-rep(0,length(re24data$inning_topbot)) #sees where batting team changes
for(i in 2:length(re24data$inning_topbot)){
  if(re24data$inning_topbot[i]!=re24data$inning_topbot[i-1]){
    switches[i]=1
  }
  if(re24data$inning[i]!=re24data$inning[i-1]){
    switches[i]=1
  }
}
switches<-as.data.frame(switches)
re24data<-cbind(re24data,switches)
endruns<-rep(4,length(re24data$inning_topbot)) #tracks runs at the end of the inning
for(i in 2:length(re24data$inning_topbot)){#does so for the ones that ended the inning
  if(re24data$switches[i]==1){
    endruns[i-1]=re24data$fld_score[i]
    }
  }
  endruns<-as.data.frame(endruns)
  re24data<-cbind(re24data,endruns)
for(i in 1:(length(re24data$inning_topbot)-7)){#does so for the rest (took out last few because there might not be a switch left. These get removed when we remove the last inning so it's ok that they're 4 right now)
  check=0
  j=1
  while(check==0){
    if(re24data$switches[i+j]==1){
      re24data$endruns[i]=re24data$endruns[i+j-1]
      check=1
    }
    if(j>100){
      check=1
    }
  j<-j+1
  }
}
re24data<-filter(re24data,inning<9) #removes 9th
re24data<-mutate(re24data,runstoend=endruns-bat_score) #calculates runs to end of the inning
#Makes baserunners binary
re24data$on_1b<-as.numeric(re24data$on_1b!="null")
re24data$on_2b<-as.numeric(re24data$on_2b!="null")
re24data$on_3b<-as.numeric(re24data$on_3b!="null")
re24matrix<-group_by(re24data, outs_when_up,on_1b,on_2b,on_3b) %>% #Creates re24 matrix
  summarise(count = n(),
  mean = mean(runstoend, na.rm = TRUE),
  sd = sd(runstoend, na.rm = TRUE),
  se = sd(runstoend, na.rm = TRUE)/sqrt(n()-1))
re24matrix<-as.data.frame(re24matrix)
startexp<-rep(0,length(re24data$runstoend)) #calculates runs expected at start of plate appearance (takes a bit, filter might be better)
for(i in 1:length(re24data$runstoend)){
  startexp[i]<-re24matrix[re24matrix$outs_when_up==re24data$outs_when_up[i] &
                          re24matrix$on_1b==re24data$on_1b[i] &
                          re24matrix$on_2b==re24data$on_2b[i] &
                          re24matrix$on_3b==re24data$on_3b[i],6]
}
startexp<-as.data.frame(startexp)
re24data<-cbind(re24data,startexp)
postexp<-rep(0,length(re24data$startexp)) #sees runs expected at end of plate appearance
for(i in 1:length(re24data$startexp)-1){
  if(re24data$switches[i+1]==1){
  postexp[i]=0+re24data$runstoend[i]
  } 
  else{
  postexp[i]=re24data$startexp[i+1]-re24data$runstoend[i+1]+re24data$runstoend[i]
  }
}
re24data<-cbind(re24data,postexp)
re24data<-mutate(re24data,deltapa=postexp-startexp) #calculates the change in the PA
datafilter2<-filter(data,inning<9) #calls in All pitches before 9th inning not just PA's
re288data<-select(datafilter2,game_year,game_pk,at_bat_number,pitch_number,inning,inning_topbot,outs_when_up,bat_score,fld_score,on_1b,on_2b,on_3b,balls,strikes,pitch_type,batter,pitcher,description,paswitches,release_speed,release_pos_x,release_pos_z,pfx_x,pfx_z,plate_x,plate_z,game_date) #selects the desired data. there's a few variables here I don't use, but wanted to explore for future use
re288data<-arrange(re288data,game_year,game_pk,at_bat_number,pitch_number)
firsthalf<-as.numeric(months(as.Date(re288data$game_date)) %in% c("March","April","May","June")) #Picks the first half
re288data<-cbind(re288data,firsthalf)
deltapa<-rep(0,length(re288data$at_bat_number)) #Gets deltapa from the PA and runs to end from the PA
runstoend<-rep(0,length(re288data$at_bat_number))
counter<-1
for(i in 1:length(re288data$at_bat_number)){
  if(re288data$paswitches[i]==0){
    deltapa[i]<-deltapa[i-1]
    runstoend[i]<-runstoend[i-1]
  }
  else{
    deltapa[i]<-re24data[counter,18]
    runstoend[i]<-re24data[counter,15]
    counter<-counter+1
  }
}
switches<-rep(0,length(re288data$inning_topbot)) #finds pitches that are last in the half inning
for(i in 2:length(re288data$inning_topbot)){
  if(re288data$inning_topbot[i]!=re288data$inning_topbot[i-1]){
    switches[i]=1
  }
  if(re288data$inning[i]!=re288data$inning[i-1]){
    switches[i]=1
  }
}
switches<-as.data.frame(switches)
re288data<-cbind(re288data,deltapa,runstoend,switches)
re288data$on_1b<-as.numeric(re288data$on_1b!="null") #Makes baserunner data binary
re288data$on_2b<-as.numeric(re288data$on_2b!="null")
re288data$on_3b<-as.numeric(re288data$on_3b!="null")
re288data<-filter(re288data,strikes<2 | description!="foul")#removes two strike fouls as they do not change results and shouldn't be in denominator
re288matrix<-group_by(re288data, outs_when_up,on_1b,on_2b,on_3b,balls,strikes) %>% #Creates re288 matrix (this does it for the balls and strikes compared to start of PA)
  summarise(count = n(),
  mean = mean(deltapa, na.rm = TRUE))
re288matrix<-filter(re288matrix,balls<4) #This is confusing to me
for(i in 1:length(re288matrix$mean)){ #This adds in where we were at the beginning of the PA
  re288matrix[i,8]<-re288matrix[i,8]+filter(re24matrix,outs_when_up==re288matrix$outs_when_up[i] &
                                                  on_1b==re288matrix$on_1b[i] &
                                                  on_2b==re288matrix$on_2b[i] &
                                                  on_3b==re288matrix$on_3b[i])[6]
}
startexp<-rep(0,length(re288data$deltapa)) #Creates start expectancy for each pitch
indexes<-re288data$outs_when_up*96+re288data$on_1b*48+re288data$on_2b*24+re288data$on_3b*12+re288data$balls*3+re288data$strikes+1
for(i in 1:length(re288data$deltapa)){
  startexp[i]<-re288matrix[indexes[i],8]$mean
}
re288data<-cbind(re288data,startexp)
postexp<-rep(0,length(re288data$startexp)) #Creates post expectancy
for(i in 1:length(re288data$startexp)-1){
  if(re288data$switches[i+1]==1){
    postexp[i]=re288data$runstoend[i]
  } 
  else{
    postexp[i]=re288data$startexp[i+1]-re288data$runstoend[i+1]+re288data$runstoend[i]
  }
}
re288data<-cbind(re288data,postexp)
re288data<-mutate(re288data,deltapitch=postexp-startexp) #Does the delta
pitchoutcomes<-group_by(re288data, outs_when_up,on_1b,on_2b,on_3b,balls,strikes,batter,pitcher,game_year) %>%#Finds all the re288 and batter pitcher situations
  summarise(count = n(),
            mean = mean(deltapitch, na.rm = TRUE))
pitchoutcomes<-filter(pitchoutcomes,count>1) #finds the ones where it happened twice

deltasinre288<-data.frame(pitch1=character(),
                                pitch2=character(), 
                                counts=integer(),
                                sums=numeric(),
                                balls=integer(),
                                strikes=integer(),
                                on_1b=integer(),
                                on_2b=integer(),
                                on_3b=integer(),
                                outs_when_up=integer(),
                                batter=integer(),
                                pitcher=integer(),
                                game_year=integer(),
                                stringsAsFactors=FALSE) #empty data frame to track the differences between two possible pitches
#This part can be quicker if you go year by year instead of all at once. I no longer run it as it takes forever but have the data stored up.
for(i in 1:length(pitchoutcomes$count)){
  subset<-filter(re288data,outs_when_up==pitchoutcomes$outs_when_up[i] & #Finds one subset of possible RE288 state and BvP matchup
                    on_1b==pitchoutcomes$on_1b[i] &
                    on_2b==pitchoutcomes$on_2b[i] &
                    on_3b==pitchoutcomes$on_3b[i] &
                    balls==pitchoutcomes$balls[i] &
                    strikes==pitchoutcomes$strikes[i] &
                    batter==pitchoutcomes$batter[i] &  
                    pitcher==pitchoutcomes$pitcher[i] &
                    game_year==pitchoutcomes$game_year[i])
  subsetoutcomes<-group_by(subset, pitch_type) %>% #sees how it depended on pitch thrown
    summarise(count = n(),
              means = mean(deltapitch, na.rm = TRUE))
  subsetoutcomes<-arrange(subsetoutcomes,pitch_type)
  subsetoutcomes$pitch_type<-factor(as.character(subsetoutcomes$pitch_type)) #sees how many different pitches were thrown and calculates the differences
  if(length(levels(subsetoutcomes$pitch_type))>1){
    for(j in 1:length(levels(subsetoutcomes$pitch_type))){
      for(k in 1:(length(levels(subsetoutcomes$pitch_type))-1)){
        if(j>k){
          deltasinre288[nrow(deltasinre288)+1,]<-list(as.character(subsetoutcomes[j,1]$pitch_type),
                                               as.character(subsetoutcomes[k,1]$pitch_type),
                                               min(subsetoutcomes[j,2],subsetoutcomes[k,2]),
                                               min(subsetoutcomes[j,2],subsetoutcomes[k,2])*(subsetoutcomes[j,3]$means-subsetoutcomes[k,3]$means),
                                               subset$balls[1],
                                               subset$strikes[1],
                                               as.integer(paste(subset$on_1b[1])),
                                               as.integer(paste(subset$on_2b[1])),
                                               as.integer(paste(subset$on_3b[1])),
                                               subset$outs_when_up[1],
                                               subset$batter[1],
                                               as.integer(paste(subset$pitcher[1])),
                                               as.integer(paste(subset$game_year[1])))
          }
      }
    }
  }
}

deltasinre288<-filter(deltasinre288,pitch1 %in% c("FF","SL","CH","CU","FT","SI","FC","KC","FS"),pitch2 %in% c("FF","SL","CH","CU","FT","SI","FC","KC","FS")) #sets data up for the regression to see best pitches
deltasinre288$FF<-deltasinre288$counts*(as.numeric(deltasinre288$pitch1=="FF")-as.numeric(deltasinre288$pitch2=="FF"))
deltasinre288$SL<-deltasinre288$counts*(as.numeric(deltasinre288$pitch1=="SL")-as.numeric(deltasinre288$pitch2=="SL"))
deltasinre288$CH<-deltasinre288$counts*(as.numeric(deltasinre288$pitch1=="CH")-as.numeric(deltasinre288$pitch2=="CH"))
deltasinre288$CU<-deltasinre288$counts*(as.numeric(deltasinre288$pitch1=="CU")-as.numeric(deltasinre288$pitch2=="CU"))
deltasinre288$FT<-deltasinre288$counts*(as.numeric(deltasinre288$pitch1=="FT")-as.numeric(deltasinre288$pitch2=="FT"))
deltasinre288$SI<-deltasinre288$counts*(as.numeric(deltasinre288$pitch1=="SI")-as.numeric(deltasinre288$pitch2=="SI"))
deltasinre288$FC<-deltasinre288$counts*(as.numeric(deltasinre288$pitch1=="FC")-as.numeric(deltasinre288$pitch2=="FC"))
deltasinre288$KC<-deltasinre288$counts*(as.numeric(deltasinre288$pitch1=="KC")-as.numeric(deltasinre288$pitch2=="KC"))
deltasinre288$FS<-deltasinre288$counts*(as.numeric(deltasinre288$pitch1=="FS")-as.numeric(deltasinre288$pitch2=="FS"))
indexes2<-deltasinre288$outs_when_up*96+deltasinre288$on_1b*48+deltasinre288$on_2b*24+deltasinre288$on_3b*12+deltasinre288$balls*3+deltasinre288$strikes+1
deltasinre288subs<-filter(deltasinre288) #Use the desired subset
reg<-summary(lm(sums*100~0+KC+CU+CH+SL+FC+FS+SI+FT,data=deltasinre288,weights = 1/counts)) #finds the best pitches

importantpitches<-c("Knuckle-Curve","Curveball","Changeup","Slider","Cutter","Splitter","Sinker","Two-Seam Fastball","Four-Seam Fastball")
plotdf<-data.frame(importantpitches,values=c(reg$coefficients[,1],0)) #I plotted this from a different file in the presentation which you already saw. It was just a ggplot

re288data<-filter(re288data,pitch_type %in% c("FF","SL","CH","CU","FT","SI","FC","KC","FS"))
re288data$FF<-as.numeric(re288data$pitch_type=="FF")
re288data$SL<-as.numeric(re288data$pitch_type=="SL")
re288data$CH<-as.numeric(re288data$pitch_type=="CH")
re288data$CU<-as.numeric(re288data$pitch_type=="CU")
re288data$FT<-as.numeric(re288data$pitch_type=="FT")
re288data$SI<-as.numeric(re288data$pitch_type=="SI")
re288data$FC<-as.numeric(re288data$pitch_type=="FC")
re288data$KC<-as.numeric(re288data$pitch_type=="KC")
re288data$FS<-as.numeric(re288data$pitch_type=="FS")
re288data$FA<-re288data$FF+re288data$FT+re288data$SI+re288data$FS #Counted sinker here but not cutter mainly due to results above showing the cutter was similar to off-speed in effectiveness. Certainly not the only way to do this.
re288data$OS<-1-re288data$FA
velexp<-group_by(re288data,pitch_type) %>%
  summarise(count=n(),
            meanvel=mean(release_speed)/mean(1-as.numeric(release_speed==0)),
            maxvel=max(release_speed))
  
pitchexp<-group_by(re288data,outs_when_up,on_1b,on_2b,on_3b,balls,strikes) %>%#Finds all the re288 and batter pitcher situations and how often the threw each pitch
  summarise(count = n(),
            FFPCT = mean(FF),
            SLPCT = mean(SL),
            CHPCT = mean(CH),
            CUPCT = mean(CU),
            FTPCT = mean(FT),
            SIPCT = mean(SI),
            FCPCT = mean(FC),
            KCPCT = mean(KC),
            FSPCT = mean(FS),
            FAPCT = mean(FA),
            OSPCT = mean(OS))
pitchexp<-filter(pitchexp,balls<4)
pitchexp<-as.data.frame(pitchexp)
FFexp<-rep(0,length(re288data$deltapa)) #Creates expectancy for each pitch
SLexp<-rep(0,length(re288data$deltapa))
CHexp<-rep(0,length(re288data$deltapa))
CUexp<-rep(0,length(re288data$deltapa))
FTexp<-rep(0,length(re288data$deltapa))
SIexp<-rep(0,length(re288data$deltapa))
FCexp<-rep(0,length(re288data$deltapa))
KCexp<-rep(0,length(re288data$deltapa))
FSexp<-rep(0,length(re288data$deltapa))
FAexp<-rep(0,length(re288data$deltapa))
OSexp<-rep(0,length(re288data$deltapa))
indexes<-re288data$outs_when_up*96+re288data$on_1b*48+re288data$on_2b*24+re288data$on_3b*12+re288data$balls*3+re288data$strikes+1
for(i in 1:length(re288data$deltapa)){
  FFexp[i]<-pitchexp[indexes[i],8]
  SLexp[i]<-pitchexp[indexes[i],9]
  CHexp[i]<-pitchexp[indexes[i],10]
  CUexp[i]<-pitchexp[indexes[i],11]
  FTexp[i]<-pitchexp[indexes[i],12]
  SIexp[i]<-pitchexp[indexes[i],13]
  FCexp[i]<-pitchexp[indexes[i],14]
  KCexp[i]<-pitchexp[indexes[i],15]
  FSexp[i]<-pitchexp[indexes[i],16]
  FAexp[i]<-pitchexp[indexes[i],17]
  OSexp[i]<-pitchexp[indexes[i],18]
}
velocity<-as.numeric(levels(re288data$release_speed)[as.numeric(re288data$release_speed)]) #Finds the velocity of each pitch
for(i in 1:length(velocity)){
  if(is.na(velocity[i])){
    velocity[i]=0
  }
}
re288data$release_speed<-velocity
re288data<-cbind(re288data,FFexp,SLexp,CHexp,CUexp,FTexp,SIexp,FCexp,KCexp,FSexp,FAexp,OSexp) 
velexp<-79.63712+as.numeric(re288data$pitch_type=="FF")*12.6758+as.numeric(re288data$pitch_type=="FT")*12.08030+as.numeric(re288data$pitch_type=="FC")*8.38257+as.numeric(re288data$pitch_type=="FS")*5.00223+as.numeric(re288data$pitch_type=="SI")*11.38590+as.numeric(re288data$pitch_type=="SL")*4.39116+as.numeric(re288data$pitch_type=="CH")*3.64221-as.numeric(re288data$pitch_type=="CU")*2.62542
re288data$velexp<-velexp #This parts gives the average velocity for the pitch type
pitcherprops<-group_by(re288data,pitcher,game_year) %>%#Finds the pitcher proportions of pitches, value, and speed.
  summarise(count = n(),
            FFPCT = mean(FF),
            SLPCT = mean(SL),
            CHPCT = mean(CH),
            CUPCT = mean(CU),
            FTPCT = mean(FT),
            SIPCT = mean(SI),
            FCPCT = mean(FC),
            KCPCT = mean(KC),
            FSPCT = mean(FS),
            FAPCT = mean(FA),
            OSPCT = mean(OS),
            FFexp = mean(FFexp),
            SLexp = mean(SLexp),
            CHexp = mean(CHexp),
            CUexp = mean(CUexp),
            FTexp = mean(FTexp),
            SIexp = mean(SIexp),
            FCexp = mean(FCexp),
            KCexp = mean(KCexp),
            FSexp = mean(FSexp),
            FAexp = mean(FAexp),
            OSexp = mean(OSexp),
            FFdiff = mean(FF)-mean(FFexp),
            SLdiff = mean(SL)-mean(SLexp),
            CHdiff = mean(CH)-mean(CHexp),
            CUdiff = mean(CU)-mean(CUexp),
            FTdiff = mean(FT)-mean(FTexp),
            SIdiff = mean(SI)-mean(SIexp),
            FCdiff = mean(FC)-mean(FCexp),
            KCdiff = mean(KC)-mean(KCexp),
            FSdiff = mean(FS)-mean(FSexp),
            FAdiff = mean(FA)-mean(FAexp),
            OSdiff = mean(OS)-mean(OSexp),
            FFvalper100 = 100*(sum(FF*deltapitch)/sum(FF)),
            SLvalper100 = 100*(sum(SL*deltapitch)/sum(SL)),
            CHvalper100 = 100*(sum(CH*deltapitch)/sum(CH)),
            CUvalper100 = 100*(sum(CU*deltapitch)/sum(CU)),
            FTvalper100 = 100*(sum(FT*deltapitch)/sum(FT)),
            SIvalper100 = 100*(sum(SI*deltapitch)/sum(SI)),
            FCvalper100 = 100*(sum(FC*deltapitch)/sum(FC)),
            KCvalper100 = 100*(sum(KC*deltapitch)/sum(KC)),
            FSvalper100 = 100*(sum(FS*deltapitch)/sum(FS)),
            FAvalper100 = 100*(sum(FA*deltapitch)/sum(FA)),
            OSvalper100 = 100*(sum(OS*deltapitch)/sum(OS)),
            FAspeed = (sum(FA*(release_speed-velexp)*as.numeric(release_speed>0))/sum(FA*as.numeric(release_speed>0))),
            OSspeed = (sum(OS*(release_speed-velexp)*as.numeric(release_speed>0))/sum(OS*as.numeric(release_speed>0))),
            firsthalf=mean(firsthalf)
            )
pitcherprops<-arrange(pitcherprops,pitcher,game_year) 
year1<-vector() #Picks the first year of consecutive year pairs
for(i in 1:(length(pitcherprops$pitcher)-1)){
  if(pitcherprops$pitcher[i]==pitcherprops$pitcher[i+1] & pitcherprops$game_year[i]==pitcherprops$game_year[i+1]-1){
    year1[i]<-1
  }
  else{
    year1[i]<-0
  }
}
year1[5996]<-0
year1<-as.data.frame(year1)

pitcherpropscopy<-group_by(re288data,pitcher,game_year) %>%#Does the same for thesecond year,but does value in second half and proportions in the first half.
  summarise(count = n(),
            FFPCT = mean(FF),
            SLPCT = mean(SL),
            CHPCT = mean(CH),
            CUPCT = mean(CU),
            FTPCT = mean(FT),
            SIPCT = mean(SI),
            FCPCT = mean(FC),
            KCPCT = mean(KC),
            FSPCT = mean(FS),
            FAPCT = mean(FA),
            OSPCT = mean(OS),
            FFexp = mean(FFexp),
            SLexp = mean(SLexp),
            CHexp = mean(CHexp),
            CUexp = mean(CUexp),
            FTexp = mean(FTexp),
            SIexp = mean(SIexp),
            FCexp = mean(FCexp),
            KCexp = mean(KCexp),
            FSexp = mean(FSexp),
            FAexp = mean(FAexp),
            OSexp = mean(OSexp),
            FFdiff = mean(FF)-mean(FFexp),
            SLdiff = mean(SL)-mean(SLexp),
            CHdiff = mean(CH)-mean(CHexp),
            CUdiff = mean(CU)-mean(CUexp),
            FTdiff = mean(FT)-mean(FTexp),
            SIdiff = mean(SI)-mean(SIexp),
            FCdiff = mean(FC)-mean(FCexp),
            KCdiff = mean(KC)-mean(KCexp),
            FSdiff = mean(FS)-mean(FSexp),
            FAdiff = (mean(FA*firsthalf)-mean(FAexp*firsthalf))/mean(firsthalf),
            OSdiff = (mean(OS*firsthalf)-mean(OSexp*firsthalf))/mean(firsthalf),
            FFvalper100 = 100*(sum(FF*deltapitch)/sum(FF)),
            SLvalper100 = 100*(sum(SL*deltapitch)/sum(SL)),
            CHvalper100 = 100*(sum(CH*deltapitch)/sum(CH)),
            CUvalper100 = 100*(sum(CU*deltapitch)/sum(CU)),
            FTvalper100 = 100*(sum(FT*deltapitch)/sum(FT)),
            SIvalper100 = 100*(sum(SI*deltapitch)/sum(SI)),
            FCvalper100 = 100*(sum(FC*deltapitch)/sum(FC)),
            KCvalper100 = 100*(sum(KC*deltapitch)/sum(KC)),
            FSvalper100 = 100*(sum(FS*deltapitch)/sum(FS)),
            FAvalper100 = 100*(sum(FA*deltapitch*(1-firsthalf))/sum(FA*(1-firsthalf))),
            OSvalper100 = 100*(sum(OS*deltapitch*(1-firsthalf))/sum(OS*(1-firsthalf))),
            FAspeed = (sum(FA*(release_speed-velexp)*as.numeric(release_speed>0))/sum(FA*as.numeric(release_speed>0))),
            OSspeed = (sum(OS*(release_speed-velexp)*as.numeric(release_speed>0))/sum(OS*as.numeric(release_speed>0))),
            firsthalf=mean(firsthalf)
  )
pitcherpropscopy<-arrange(pitcherpropscopy,pitcher,game_year)

#Creates the second year data and combines the two
year2<-vector()
year2[1]<-0
for(i in 2:(length(pitcherprops$pitcher))){
  if(pitcherprops$pitcher[i]==pitcherprops$pitcher[i-1] & pitcherprops$game_year[i]==pitcherprops$game_year[i-1]+1){
    year2[i]<-1
  }
  else{
    year2[i]<-0
  }
}
year2<-as.data.frame(year2)
pitcherprops<-as.data.frame(pitcherprops)
pitcherpropscopy<-as.data.frame(pitcherpropscopy)
pitcherprops<-cbind(pitcherprops,year1)
pitcherpropscopy<-cbind(pitcherpropscopy,year2)
pitcherprops<-filter(pitcherprops,year1==1)
names(pitcherprops)<-paste(names(pitcherprops),"1",sep="")
pitcherpropscopy<-filter(pitcherpropscopy,year2==1)
names(pitcherpropscopy)<-paste(names(pitcherpropscopy),"2",sep="")
combinedpitcherprops<-cbind(pitcherprops,pitcherpropscopy)
combinedpitcherprops$weights<-1/sqrt(sqrt(combinedpitcherprops$count1^2*combinedpitcherprops$count2^2*combinedpitcherprops$firsthalf2*(1-combinedpitcherprops$firsthalf2)*combinedpitcherprops$FAPCT1*combinedpitcherprops$OSPCT1*combinedpitcherprops$FAPCT2*combinedpitcherprops$OSPCT2))

#This is the sample I used, but it was picked arbitrarily. You'll notice there's a ton of noise in this which makes me really want to look at methods of trying to reduce that problem.
finalsample<-combinedpitcherprops
finalsample<-filter(finalsample,weights>0.0035)
summary(lm(FAvalper1002~FAvalper1001+FAdiff1+FAdiff2+FAspeed1+FAspeed2,data=finalsample,weights=weights))
summary(lm(OSvalper1002~OSvalper1001+OSdiff1+OSdiff2+OSspeed1+OSspeed2,data=finalsample,weights=weights))
rightFAdiff2<-(-0.62119*(1-finalsample$FAexp2)-0.09952*finalsample$OSvalper1001-1.75825*finalsample$FAdiff1-0.02164-0.14094*finalsample$OSspeed1+0.24276*finalsample$OSspeed2+0.64451*finalsample$FAexp2+0.38759*finalsample$FAvalper1001-0.19344*finalsample$FAdiff1+0.24243*finalsample$FAspeed1-0.33627*finalsample$FAspeed2+0.34613)/(2*(-0.62119-0.64451))
for(i in 1:1274){
  if((rightFAdiff2[i]+finalsample$FAexp2[i])>1){
    rightFAdiff2[i]<-1-finalsample$FAexp2[i]
  }
  if((rightFAdiff2[i]+finalsample$FAexp2[i])<0){
    rightFAdiff2[i]<--finalsample$FAexp2[i]
  }
}
finalsample$rightFAdiff2<-rightFAdiff2
newvalue4=(-0.62119*finalsample$rightFAdiff2+0.09952*finalsample$OSvalper1001+1.75825*finalsample$FAdiff1+0.02164+0.14094*finalsample$OSspeed1-0.24276*finalsample$OSspeed2)*(1-finalsample$FAexp2-finalsample$rightFAdiff2)+
  (0.64451*finalsample$rightFAdiff2+0.38759*finalsample$FAvalper1001-0.19344*finalsample$FAdiff1+0.34613+0.24243*finalsample$FAspeed1-0.33627*finalsample$FAspeed2)*(finalsample$FAexp2+finalsample$rightFAdiff2)
newvalue4=newvalue4*finalsample$count2/100
oldvalue4=(-0.62119*finalsample$FAdiff2+0.09952*finalsample$OSvalper1001+1.75825*finalsample$FAdiff1+0.02164+0.14094*finalsample$OSspeed1-0.24276*finalsample$OSspeed2)*(1-finalsample$FAexp2-finalsample$FAdiff2)+
  (0.64451*finalsample$FAdiff2+0.38759*finalsample$FAvalper1001-0.19344*finalsample$FAdiff1+0.34613+0.24243*finalsample$FAspeed1-0.33627*finalsample$FAspeed2)*(finalsample$FAexp2+finalsample$FAdiff2)
oldvalue4=oldvalue4*finalsample$count2/100
Improvedvalue4=newvalue4-oldvalue4
ggplot()+
  aes((rightFAdiff2+finalsample$FAexp2))+
  geom_histogram(bins=20)+
  labs(x="Correct Fastball Proportion",y="Frequency",title="Optimal Pitch Frequencies")
ggplot()+
  aes((Improvedvalue4)/finalsample$count2*100)+
  geom_histogram(bins=20,binwidth=0.035)+
  labs(x="Improved value per 100 pitches",y="Frequency",title="Improved Value")+
  xlim(c(-0.7,0))+
  ylim(c(0,300))


