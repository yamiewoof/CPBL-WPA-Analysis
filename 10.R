#戰績走勢
setwd("c:\\baseball")
library(readr)
library(dplyr)
library(ggplot2)

pal <- c(
  "桃猿" = "#4682b4",
  "兄弟" = "#ffd700", 
  "富邦" = "#004a9c", 
  "統一" = "#ff8c00" 
)

tempd<- aggregate(forbatter[, c("year",'num', 'away.x', 'home.x')], list(forbatter$year,forbatter$num), tail, 1)[,-1]
tempd <- tempd[with(tempd, order(year,num)), ]

tempd$difff<-parse_number(as.character(tempd$home.x))-parse_number(as.character(tempd$away.x))
tempd$win<-ifelse(tempd$difff>0,"homewin",ifelse(tempd$difff<0,"awaywin","tie"))

tempd <- tempd[with(tempd, order(year,num)), ]
table(tempd$year)
tempd <- tempd %>% select(2:7)



tempd$difff<-parse_number(as.character(tempd$home.x))-parse_number(as.character(tempd$away.x))
tempd$win<-ifelse(tempd$difff>0,"homewin",ifelse(tempd$difff<0,"awaywin","tie"))
tempd <- tempd[with(tempd, order(year,num)), ]

allgame <- read.csv("log_process\\logdata\\eachgame.csv",na.strings=c("","NA"),stringsAsFactors = FALSE)
write.csv(allgame,file = sprintf("log_process\\logdata\\eachgame.csv"), row.names=FALSE)

num <- as.data.frame(c(1:231,1:233,1:237,1:238,1:239,1:20))
names(num) <- "number"
tempd$number <- num$number
tempd <- tempd %>% select(2,3,8,4:7)


allgame <- tempd
allgame[1,3]

removepunc=function(log){
  log <- gsub(" ", "", log)
  log <- gsub("　", "", log)
  log <- gsub("1", "", log)
  log <- gsub("2", "", log)
  log <- gsub("3", "", log)
  log <- gsub("4", "", log)
  log <- gsub("5", "", log)
  log <- gsub("6", "", log)
  log <- gsub("7", "", log)
  log <- gsub("8", "", log)
  log <- gsub("9", "", log)
  log <- gsub("0", "", log)
  log <- gsub("\\(", "", log)
  log <- gsub("\\)", "", log)
  return(log)
}

allgame$awayteam <- removepunc(allgame$away.x)
allgame$hometeam <- removepunc(allgame$home.x)

table(allgame$awayteam)
table(allgame$hometeam)

allgame <- allgame[with(allgame, order(year,num)), ]
allgame$WINteam <- NA
allgame$LOSSteam <- NA
allgame$TIEteam1 <- NA
allgame$TIEteam2 <- NA

for(i in 1:nrow(allgame)){
  if(allgame$win[i]=="homewin"){
    allgame$WINteam[i] <- allgame$hometeam[i]
    allgame$LOSSteam[i]<- allgame$awayteam[i]
  } else
    if(allgame$win[i]=="awaywin"){
      allgame$WINteam[i] <- allgame$awayteam[i]
      allgame$LOSSteam[i]<- allgame$hometeam[i]
    } else 
      if(allgame$win[i]=="tie"){
        allgame$TIEteam1[i] <- allgame$awayteam[i]
        allgame$TIEteam2[i] <- allgame$hometeam[i]
      }
}

##########################################################################################################
2018
##########################################################################################################
#2018戰績
fubon2018 <- data.frame(gamenum=(1:240),fubonWIN=0,fubonLOSS=0,fubonTIE=0)
brothers2018 <- data.frame(brothersWIN=0,brothersLOSS=0,brothersTIE=0)
lamigo2018 <- data.frame(lamigoWIN=0,lamigoLOSS=0,lamigoTIE=0)
lions2018 <- data.frame(lionsWIN=0,lionsLOSS=0,lionsTIE=0)
standings2018 <- cbind(fubon2018,brothers2018,lamigo2018,lions2018 )
winslosses2018 <- standings2018

winslosses2018_second <- standings2018
winslosses2018_second <- winslosses2018_second %>% slice(121:240)

allgame2018 <- subset(allgame,year==2018)

for(i in 1:nrow(allgame2018)){
  #富邦
  if(allgame2018$WINteam[i]=="富邦"){
    standings2018$fubonWIN[i] <- standings2018$fubonWIN[i] + 1
  } else 
    if(allgame2018$LOSSteam[i]=="富邦"){
      standings2018$fubonLOSS[i] <- standings2018$fubonLOSS[i] + 1
    } else 
      if(allgame2018$TIEteam1[i]=="富邦"){
        standings2018$fubonTIE[i] <- standings2018$fubonTIE[i] + 1
      } else 
        if(allgame2018$TIEteam2[i]=="富邦"){
          standings2018$fubonTIE[i] <- standings2018$fubonTIE[i] + 1
        }
  
  #兄弟
  if(allgame2018$WINteam[i]=="兄弟"){
    standings2018$brothersWIN[i] <- standings2018$brothersWIN[i] + 1
  } else 
    if(allgame2018$LOSSteam[i]=="兄弟"){
      standings2018$brothersLOSS[i] <- standings2018$brothersLOSS[i] + 1
    } else 
      if(allgame2018$TIEteam1[i]=="兄弟"){
        standings2018$brothersTIE[i] <- standings2018$brothersTIE[i] + 1
      } else 
        if(allgame2018$TIEteam2[i]=="兄弟"){
          standings2018$brothersTIE[i] <- standings2018$brothersTIE[i] + 1
        }
  
  #桃猿
  if(allgame2018$WINteam[i]=="桃猿"){
    standings2018$lamigoWIN[i] <- standings2018$lamigoWIN[i] + 1
  } else 
    if(allgame2018$LOSSteam[i]=="桃猿"){
      standings2018$lamigoLOSS[i] <- standings2018$lamigoLOSS[i] + 1
    } else 
      if(allgame2018$TIEteam1[i]=="桃猿"){
        standings2018$lamigoTIE[i] <- standings2018$lamigoTIE[i] + 1
      } else 
        if(allgame2018$TIEteam2[i]=="桃猿"){
          standings2018$lamigoTIE[i] <- standings2018$lamigoTIE[i] + 1
        }
  
  #統一
  if(allgame2018$WINteam[i]=="統一"){
    standings2018$lionsWIN[i] <- standings2018$lionsWIN[i] + 1
  } else 
    if(allgame2018$LOSSteam[i]=="統一"){
      standings2018$lionsLOSS[i] <- standings2018$lionsLOSS[i] + 1
    } else 
      if(allgame2018$TIEteam1[i]=="統一"){
        standings2018$lionsTIE[i] <- standings2018$lionsTIE[i] + 1
      } else 
        if(allgame2018$TIEteam2[i]=="統一"){
          standings2018$lionsTIE[i] <- standings2018$lionsTIE[i] + 1
        }
  
}

colSums(standings2018[1:3,])


#2018總年度
for(i in 1:nrow(standings2018)){
  winslosses2018[i,] <- colSums(standings2018[1:i,])
}
winslosses2018$gamenum <- 1:240

rankings2018 <- data.frame(gamenum=(1:240),fubon=NA,brothers=NA,lamigo=NA,lions=NA)
for(i in 1:nrow(winslosses2018)){
  fubonWL <- winslosses2018$fubonWIN[i] - winslosses2018$fubonLOSS[i]
  brothersWL <- winslosses2018$brothersWIN[i] - winslosses2018$brothersLOSS[i]
  lamigoWL <- winslosses2018$lamigoWIN[i] - winslosses2018$lamigoLOSS[i]
  lionsWL <- winslosses2018$lionsWIN[i] - winslosses2018$lionsLOSS[i]
  winloss <- c(fubonWL,brothersWL,lamigoWL,lionsWL)
  print(winloss)
  WL <- rank(-winloss,ties.method= "min")
  rankings2018$fubon[i] <- WL[1]
  rankings2018$brothers[i] <- WL[2]
  rankings2018$lamigo[i] <- WL[3]
  rankings2018$lions[i] <- WL[4]
}

rankings2018[240,]
table(rankings2018$fubon)

ggplot(data=rankings2018) + aes(x=gamenum) +
  geom_line(aes(y=fubon), col="#004a9c",size=1) +
  geom_line(aes(y=brothers), col="#ffd700",size=1) +
  geom_line(aes(y=lamigo), col="#4682b4",size=1) +
  geom_line(aes(y=lions), col="#ff8c00",size=1) +
  ylim(4, 1) +
  labs(title="整季排名趨勢", 
       subtitle="Data:2018") +
  ylab("Ranking") 

########################################################################################################

#2018上半季
rankings2018_first <- data.frame(gamenum=(1:120),fubon=NA,brothers=NA,lamigo=NA,lions=NA)
for(i in 1:120){
  fubonWL <- winslosses2018$fubonWIN[i] - winslosses2018$fubonLOSS[i]
  brothersWL <- winslosses2018$brothersWIN[i] - winslosses2018$brothersLOSS[i]
  lamigoWL <- winslosses2018$lamigoWIN[i] - winslosses2018$lamigoLOSS[i]
  lionsWL <- winslosses2018$lionsWIN[i] - winslosses2018$lionsLOSS[i]
  winloss <- c(fubonWL,brothersWL,lamigoWL,lionsWL)
  print(winloss)
  WL <- rank(-winloss,ties.method= "min")
  rankings2018_first$fubon[i] <- WL[1]
  rankings2018_first$brothers[i] <- WL[2]
  rankings2018_first$lamigo[i] <- WL[3]
  rankings2018_first$lions[i] <- WL[4]
}

rankings2018_first[120,]
table(rankings2018_first$fubon)


#畫圖

ggplot(data=rankings2018_first) + aes(x=gamenum) +
  geom_line(aes(y=fubon), col="#004a9c",size=1) +
  geom_line(aes(y=brothers), col="#ffd700",size=1) +
  geom_line(aes(y=lamigo), col="#4682b4",size=1) +
  geom_line(aes(y=lions), col="#ff8c00",size=1) +
  ylim(4, 1) +
  labs(title="上半季排名趨勢", 
       subtitle="Data:2018") +
  ylab("Ranking") 



########################################################################################################


#2018下半季
standings2018_second <- standings2018 %>% slice(121:240)

for(i in 1:nrow(standings2018_second)){
  winslosses2018_second[i,] <- colSums(standings2018_second[1:i,])
}
winslosses2018_second$gamenum <- 121:240

rankings2018_second <- data.frame(gamenum=(121:240),fubon=NA,brothers=NA,lamigo=NA,lions=NA)
for(i in 1:120){
  fubonWL <- winslosses2018_second$fubonWIN[i] - winslosses2018_second$fubonLOSS[i]
  brothersWL <- winslosses2018_second$brothersWIN[i] - winslosses2018_second$brothersLOSS[i]
  lamigoWL <- winslosses2018_second$lamigoWIN[i] - winslosses2018_second$lamigoLOSS[i]
  lionsWL <- winslosses2018_second$lionsWIN[i] - winslosses2018_second$lionsLOSS[i]
  winloss <- c(fubonWL,brothersWL,lamigoWL,lionsWL)
  print(winloss)
  WL <- rank(-winloss,ties.method= "min")
  rankings2018_second$fubon[i] <- WL[1]
  rankings2018_second$brothers[i] <- WL[2]
  rankings2018_second$lamigo[i] <- WL[3]
  rankings2018_second$lions[i] <- WL[4]
}

rankings2018_second[120,]
table(rankings2018_second$fubon)


ggplot(data=rankings2018_second) + aes(x=gamenum) +
  geom_line(aes(y=fubon), col="#004a9c",size=1) +
  geom_line(aes(y=brothers), col="#ffd700",size=1) +
  geom_line(aes(y=lamigo), col="#4682b4",size=1) +
  geom_line(aes(y=lions), col="#ff8c00",size=1) +
  ylim(4, 1) +
  labs(title="下半季排名趨勢", 
       subtitle="Data:2018") +
  ylab("Ranking") 





########################################################################################################
##########################################################################################################
2017
##########################################################################################################
#2017戰績
fubon2017 <- data.frame(gamenum=(1:240),fubonWIN=0,fubonLOSS=0,fubonTIE=0)
brothers2017 <- data.frame(brothersWIN=0,brothersLOSS=0,brothersTIE=0)
lamigo2017 <- data.frame(lamigoWIN=0,lamigoLOSS=0,lamigoTIE=0)
lions2017 <- data.frame(lionsWIN=0,lionsLOSS=0,lionsTIE=0)
standings2017 <- cbind(fubon2017,brothers2017,lamigo2017,lions2017 )
winslosses2017 <- standings2017

winslosses2017_first <- standings2017
winslosses2017_first <- winslosses2017_first %>% slice(1:120)

winslosses2017_second <- standings2017
winslosses2017_second <- winslosses2017_second %>% slice(121:240)

allgame2017 <- subset(allgame,year==2017)

for(i in 1:nrow(allgame2017)){
  #富邦
  if(allgame2017$WINteam[i]=="富邦"){
    standings2017$fubonWIN[i] <- standings2017$fubonWIN[i] + 1
  } else 
    if(allgame2017$LOSSteam[i]=="富邦"){
      standings2017$fubonLOSS[i] <- standings2017$fubonLOSS[i] + 1
    } else 
      if(allgame2017$TIEteam1[i]=="富邦"){
        standings2017$fubonTIE[i] <- standings2017$fubonTIE[i] + 1
      } else 
        if(allgame2017$TIEteam2[i]=="富邦"){
          standings2017$fubonTIE[i] <- standings2017$fubonTIE[i] + 1
        }
  
  #兄弟
  if(allgame2017$WINteam[i]=="兄弟"){
    standings2017$brothersWIN[i] <- standings2017$brothersWIN[i] + 1
  } else 
    if(allgame2017$LOSSteam[i]=="兄弟"){
      standings2017$brothersLOSS[i] <- standings2017$brothersLOSS[i] + 1
    } else 
      if(allgame2017$TIEteam1[i]=="兄弟"){
        standings2017$brothersTIE[i] <- standings2017$brothersTIE[i] + 1
      } else 
        if(allgame2017$TIEteam2[i]=="兄弟"){
          standings2017$brothersTIE[i] <- standings2017$brothersTIE[i] + 1
        }
  
  #桃猿
  if(allgame2017$WINteam[i]=="桃猿"){
    standings2017$lamigoWIN[i] <- standings2017$lamigoWIN[i] + 1
  } else 
    if(allgame2017$LOSSteam[i]=="桃猿"){
      standings2017$lamigoLOSS[i] <- standings2017$lamigoLOSS[i] + 1
    } else 
      if(allgame2017$TIEteam1[i]=="桃猿"){
        standings2017$lamigoTIE[i] <- standings2017$lamigoTIE[i] + 1
      } else 
        if(allgame2017$TIEteam2[i]=="桃猿"){
          standings2017$lamigoTIE[i] <- standings2017$lamigoTIE[i] + 1
        }
  
  #統一
  if(allgame2017$WINteam[i]=="統一"){
    standings2017$lionsWIN[i] <- standings2017$lionsWIN[i] + 1
  } else 
    if(allgame2017$LOSSteam[i]=="統一"){
      standings2017$lionsLOSS[i] <- standings2017$lionsLOSS[i] + 1
    } else 
      if(allgame2017$TIEteam1[i]=="統一"){
        standings2017$lionsTIE[i] <- standings2017$lionsTIE[i] + 1
      } else 
        if(allgame2017$TIEteam2[i]=="統一"){
          standings2017$lionsTIE[i] <- standings2017$lionsTIE[i] + 1
        }
  
}

colSums(standings2017[1:3,])


#2017總年度
for(i in 1:nrow(standings2017)){
  winslosses2017[i,] <- colSums(standings2017[1:i,])
}
winslosses2017$gamenum <- 1:240

rankings2017 <- data.frame(gamenum=(1:240),fubon=NA,brothers=NA,lamigo=NA,lions=NA)
for(i in 1:nrow(winslosses2017)){
  fubonWL <- winslosses2017$fubonWIN[i] - winslosses2017$fubonLOSS[i]
  brothersWL <- winslosses2017$brothersWIN[i] - winslosses2017$brothersLOSS[i]
  lamigoWL <- winslosses2017$lamigoWIN[i] - winslosses2017$lamigoLOSS[i]
  lionsWL <- winslosses2017$lionsWIN[i] - winslosses2017$lionsLOSS[i]
  winloss <- c(fubonWL,brothersWL,lamigoWL,lionsWL)
  print(winloss)
  WL <- rank(-winloss,ties.method= "min")
  rankings2017$fubon[i] <- WL[1]
  rankings2017$brothers[i] <- WL[2]
  rankings2017$lamigo[i] <- WL[3]
  rankings2017$lions[i] <- WL[4]
}

rankings2017[240,]
table(rankings2017$fubon)

ggplot(data=rankings2017) + aes(x=gamenum) +
  geom_line(aes(y=fubon), col="#004a9c",size=1) +
  geom_line(aes(y=brothers), col="#ffd700",size=1) +
  geom_line(aes(y=lamigo), col="#4682b4",size=1) +
  geom_line(aes(y=lions), col="#ff8c00",size=1) +
  ylim(4, 1) +
  labs(title="整季排名趨勢", 
       subtitle="Data:2017") +
  ylab("Ranking") 

########################################################################################################

#2017上半季
standings2017_first <- standings2017 %>% slice(1:120)

for(i in 1:nrow(standings2017_first)){
  winslosses2017_first[i,] <- colSums(standings2017_first[1:i,])
}
winslosses2017_first$gamenum <- 1:120

rankings2017_first <- data.frame(gamenum=(1:120),fubon=NA,brothers=NA,lamigo=NA,lions=NA)
for(i in 1:120){
  fubonWL <- winslosses2017_first$fubonWIN[i] - winslosses2017_first$fubonLOSS[i]
  brothersWL <- winslosses2017_first$brothersWIN[i] - winslosses2017_first$brothersLOSS[i]
  lamigoWL <- winslosses2017_first$lamigoWIN[i] - winslosses2017_first$lamigoLOSS[i]
  lionsWL <- winslosses2017_first$lionsWIN[i] - winslosses2017_first$lionsLOSS[i]
  winloss <- c(fubonWL,brothersWL,lamigoWL,lionsWL)
  print(winloss)
  WL <- rank(-winloss,ties.method= "min")
  rankings2017_first$fubon[i] <- WL[1]
  rankings2017_first$brothers[i] <- WL[2]
  rankings2017_first$lamigo[i] <- WL[3]
  rankings2017_first$lions[i] <- WL[4]
}

rankings2017_first[120,]
table(rankings2017_first$fubon)


#畫圖

ggplot(data=rankings2017_first) + aes(x=gamenum) +
  geom_line(aes(y=fubon), col="#004a9c",size=1) +
  geom_line(aes(y=brothers), col="#ffd700",size=1) +
  geom_line(aes(y=lamigo), col="#4682b4",size=1) +
  geom_line(aes(y=lions), col="#ff8c00",size=1) +
  ylim(4, 1) +
  labs(title="上半季排名趨勢", 
       subtitle="Data:2017") +
  ylab("Ranking") 



########################################################################################################


#2017下半季
standings2017_second <- standings2017 %>% slice(121:240)

for(i in 1:nrow(standings2017_second)){
  winslosses2017_second[i,] <- colSums(standings2017_second[1:i,])
}
winslosses2017_second$gamenum <- 121:240

rankings2017_second <- data.frame(gamenum=(121:240),fubon=NA,brothers=NA,lamigo=NA,lions=NA)
for(i in 1:120){
  fubonWL <- winslosses2017_second$fubonWIN[i] - winslosses2017_second$fubonLOSS[i]
  brothersWL <- winslosses2017_second$brothersWIN[i] - winslosses2017_second$brothersLOSS[i]
  lamigoWL <- winslosses2017_second$lamigoWIN[i] - winslosses2017_second$lamigoLOSS[i]
  lionsWL <- winslosses2017_second$lionsWIN[i] - winslosses2017_second$lionsLOSS[i]
  winloss <- c(fubonWL,brothersWL,lamigoWL,lionsWL)
  print(winloss)
  WL <- rank(-winloss,ties.method= "min")
  rankings2017_second$fubon[i] <- WL[1]
  rankings2017_second$brothers[i] <- WL[2]
  rankings2017_second$lamigo[i] <- WL[3]
  rankings2017_second$lions[i] <- WL[4]
}

rankings2017_second[120,]
table(rankings2017_second$fubon)


ggplot(data=rankings2017_second) + aes(x=gamenum) +
  geom_line(aes(y=fubon), col="#004a9c",size=1) +
  geom_line(aes(y=brothers), col="#ffd700",size=1) +
  geom_line(aes(y=lamigo), col="#4682b4",size=1) +
  geom_line(aes(y=lions), col="#ff8c00",size=1) +
  ylim(4, 1) +
  labs(title="下半季排名趨勢", 
       subtitle="Data:2017") +
  ylab("Ranking") 

########################################################################################################
########################################################################################################
##########################################################################################################
2019
##########################################################################################################
#2017戰績
fubon2019 <- data.frame(gamenum=(1:240),fubonWIN=0,fubonLOSS=0,fubonTIE=0)
brothers2019 <- data.frame(brothersWIN=0,brothersLOSS=0,brothersTIE=0)
lamigo2019 <- data.frame(lamigoWIN=0,lamigoLOSS=0,lamigoTIE=0)
lions2019 <- data.frame(lionsWIN=0,lionsLOSS=0,lionsTIE=0)
standings2019 <- cbind(fubon2019,brothers2019,lamigo2019,lions2019 )
winslosses2019 <- standings2019

winslosses2019_first <- standings2019
winslosses2019_first <- winslosses2019_first %>% slice(1:120)

winslosses2019_second <- standings2019
winslosses2019_second <- winslosses2019_second %>% slice(121:240)

allgame2019 <- subset(allgame,year==2019)

for(i in 1:nrow(allgame2019)){
  #富邦
  if(allgame2019$WINteam[i]=="富邦"){
    standings2019$fubonWIN[i] <- standings2019$fubonWIN[i] + 1
  } else 
    if(allgame2019$LOSSteam[i]=="富邦"){
      standings2019$fubonLOSS[i] <- standings2019$fubonLOSS[i] + 1
    } else 
      if(allgame2019$TIEteam1[i]=="富邦"){
        standings2019$fubonTIE[i] <- standings2019$fubonTIE[i] + 1
      } else 
        if(allgame2019$TIEteam2[i]=="富邦"){
          standings2019$fubonTIE[i] <- standings2019$fubonTIE[i] + 1
        }
  
  #兄弟
  if(allgame2019$WINteam[i]=="兄弟"){
    standings2019$brothersWIN[i] <- standings2019$brothersWIN[i] + 1
  } else 
    if(allgame2019$LOSSteam[i]=="兄弟"){
      standings2019$brothersLOSS[i] <- standings2019$brothersLOSS[i] + 1
    } else 
      if(allgame2019$TIEteam1[i]=="兄弟"){
        standings2019$brothersTIE[i] <- standings2019$brothersTIE[i] + 1
      } else 
        if(allgame2019$TIEteam2[i]=="兄弟"){
          standings2019$brothersTIE[i] <- standings2019$brothersTIE[i] + 1
        }
  
  #桃猿
  if(allgame2019$WINteam[i]=="桃猿"){
    standings2019$lamigoWIN[i] <- standings2019$lamigoWIN[i] + 1
  } else 
    if(allgame2019$LOSSteam[i]=="桃猿"){
      standings2019$lamigoLOSS[i] <- standings2019$lamigoLOSS[i] + 1
    } else 
      if(allgame2019$TIEteam1[i]=="桃猿"){
        standings2019$lamigoTIE[i] <- standings2019$lamigoTIE[i] + 1
      } else 
        if(allgame2019$TIEteam2[i]=="桃猿"){
          standings2019$lamigoTIE[i] <- standings2019$lamigoTIE[i] + 1
        }
  
  #統一
  if(allgame2019$WINteam[i]=="統一"){
    standings2019$lionsWIN[i] <- standings2019$lionsWIN[i] + 1
  } else 
    if(allgame2019$LOSSteam[i]=="統一"){
      standings2019$lionsLOSS[i] <- standings2019$lionsLOSS[i] + 1
    } else 
      if(allgame2019$TIEteam1[i]=="統一"){
        standings2019$lionsTIE[i] <- standings2019$lionsTIE[i] + 1
      } else 
        if(allgame2019$TIEteam2[i]=="統一"){
          standings2019$lionsTIE[i] <- standings2019$lionsTIE[i] + 1
        }
  
}

colSums(standings2019[1:3,])


#2019總年度
for(i in 1:nrow(standings2019)){
  winslosses2019[i,] <- colSums(standings2019[1:i,])
}
winslosses2019$gamenum <- 1:240

rankings2019 <- data.frame(gamenum=(1:240),fubon=NA,brothers=NA,lamigo=NA,lions=NA)
for(i in 1:nrow(winslosses2019)){
  fubonWL <- winslosses2019$fubonWIN[i] - winslosses2019$fubonLOSS[i]
  brothersWL <- winslosses2019$brothersWIN[i] - winslosses2019$brothersLOSS[i]
  lamigoWL <- winslosses2019$lamigoWIN[i] - winslosses2019$lamigoLOSS[i]
  lionsWL <- winslosses2019$lionsWIN[i] - winslosses2019$lionsLOSS[i]
  winloss <- c(fubonWL,brothersWL,lamigoWL,lionsWL)
  print(winloss)
  WL <- rank(-winloss,ties.method= "min")
  rankings2019$fubon[i] <- WL[1]
  rankings2019$brothers[i] <- WL[2]
  rankings2019$lamigo[i] <- WL[3]
  rankings2019$lions[i] <- WL[4]
}

rankings2019[240,]
table(rankings2019$fubon)

ggplot(data=rankings2019) + aes(x=gamenum) +
  geom_line(aes(y=fubon), col="#004a9c",size=1) +
  geom_line(aes(y=brothers), col="#ffd700",size=1) +
  geom_line(aes(y=lamigo), col="#4682b4",size=1) +
  geom_line(aes(y=lions), col="#ff8c00",size=1) +
  ylim(4, 1) +
  labs(title="整季排名趨勢", 
       subtitle="Data:2019") +
  ylab("Ranking") 

########################################################################################################
