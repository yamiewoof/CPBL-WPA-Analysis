#移除標點符號
removepunc=function(log){
  log <- gsub("-", "", log)
  log <- gsub("－", "", log)
  log <- gsub(":", "", log)
  log <- gsub(" ", "", log)
  log <- gsub("　", "", log)
  log <- gsub("、", "", log)
  log <- gsub("。", "", log)
  log <- gsub("，", "", log)
  log <- gsub("背號", "", log)
  log <- gsub("號", "", log)
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
  log <- gsub("<", "", log)
  log <- gsub("\\(", "", log)
  log <- gsub("\\)", "", log)
  log <- gsub("<U\\+00A0>", "", log)
  return(log)
}

#找先發投手的名字
starterfunction=function(pitcher){
  pitcher <- removepunc(pitcher)
  start <- unlist(gregexpr(pattern ='先發投手',pitcher)) + 4
  endloc <- nchar(pitcher)
  
  pitcher <- strsplit(pitcher,"")
  if(pitcher[[1]][start]==":" | pitcher[[1]][start]=="："){
    start <- start + 1
    starter <- pitcher[[1]][start:endloc]
  } else{
    starter <- pitcher[[1]][start:endloc]
  }
  
  starterstring <- paste(starter,collapse="")
  
  if(nchar(starterstring)>3){
    starterstring <- strsplit(starterstring,"")
    starterstring <- starterstring[[1]][1:3]
  }
  
  starterstring <- paste(starterstring,collapse="")
  
  
  return(starterstring)
}
####################################################################

#開始跑先發投手
library(dplyr)
library(tm)

alldata100 <-  thedata
alldata100$Starter <- NA
for(i in 1:nrow(alldata100)){
  if(grepl("先發投手",alldata100$log[i])){
    startername <- starterfunction(alldata100$log[i])
    alldata100$Starter[i] <- startername
  }}
Starterlist <- as.data.frame(table(alldata100$Starter))


##################################################

alldata100$PitChg <- NA
#判斷換投的行數
for(i in 1:nrow(alldata100)){
  if(i%%100==0){
    print(i)
  }
  change <- 0
  pitchercount <- 0
  other <- 0
  if(grepl("更換",alldata100$log[i]) ){
    change <- change+1
  }
  if(grepl("投手",alldata100$log[i]) ){
    pitchercount <- pitchercount+1
  }
  if(change==1&&pitchercount==1){
    alldata100$PitChg[i] <- TRUE
  }
  
  if(grepl("守備異動",alldata100$log[i]) ){
    other <- other+1
  }
  
  if(grepl("守備更動",alldata100$log[i]) ){
    other <- other+1
  }
  
  if(grepl("守備更換",alldata100$log[i]) ){
    other <- other+1
  }
  
  if(grepl("更換守備",alldata100$log[i]) ){
    other <- other+1
  }
  
  if(grepl("更換投捕",alldata100$log[i]) ){
    other <- other+1
  }
  
  if(pitchercount==1&&other>0){
    alldata100$PitChg[i] <- TRUE
  }
  
  numP <- 0
  otherP <- 0
  
  if(grepl("P",alldata100$log[i]) ){
    numP <- numP+1
  }
  
  if(grepl("NP",alldata100$log[i]) ){
    otherP <- otherP+1
  }
  
  if(grepl("PR",alldata100$log[i]) ){
    otherP <- otherP+1
  }
  
  if(grepl("PH",alldata100$log[i]) ){
    otherP <- otherP+1
  }
  
  if(grepl("PLAY",alldata100$log[i]) ){
    otherP <- otherP+1
  }
  
  if(numP==1 && otherP==0){
    alldata100$PitChg[i] <- TRUE
  }
  
  
}

PitChgSub <- subset(alldata100,PitChg==T)

####################################################################

#找後援投手名字function
relieverfunction=function(relieverlog){
  relieverlog <- removepunc(relieverlog)
  if(grepl("P",relieverlog)){
    start <- unlist(gregexpr(pattern ='P',relieverlog)) + 1
  } 
  if(grepl("投手",relieverlog)){
    start <- unlist(gregexpr(pattern ='投手',relieverlog)) + 2
  }
  
  if(grepl("更換投手",relieverlog)){
    start <- unlist(gregexpr(pattern ='更換投手',relieverlog)) + 4
  }
  
  WEI <- 0
  if(grepl("投手為",relieverlog)){
    WEI <- 1
  }
  
  
  endloc <- nchar(relieverlog)
  
  relieverlog <- strsplit(relieverlog,"")
  
  #去除"為"
  if(WEI==1){
    start <- start + 1
    starter <- relieverlog[[1]][start:endloc]
  } else{
    starter <- relieverlog[[1]][start:endloc]
  }
  
  
  relieverstring <- paste(starter,collapse="")
  
  if(nchar(relieverstring)>3){
    relieverstring <- strsplit(relieverstring,"")
    relieverstring <- relieverstring[[1]][1:3]
  }
  
  relieverstring <- paste(relieverstring,collapse="")
  
  return(relieverstring)
}

####################################################################

alldata100$Reliever <- NA
for(i in 1:nrow(alldata100)){
  if(is.na(alldata100$PitChg[i])==F){
    relievername <- relieverfunction(alldata100$log[i])
    alldata100$Reliever[i] <- relievername
  }}

Relieverlist <- as.data.frame(table(alldata100$Reliever))

####################################################################
alldata100$Pitcher <- NA
PitcherData <- alldata100


#先發投手沒寫，需要自己補上
PitcherData[1,"Starter"] <- "羅錦龍"
PitcherData[8,"Starter"] <- "希克"
# PitcherData[110,"Starter"] <- "陳鴻文"
# PitcherData[374,"Starter"] <- "廖文揚"
# PitcherData[379,"Starter"] <- "銳"
# PitcherData[589,"Starter"] <- "馬帝斯"
# PitcherData[601,"Starter"] <- "曾孟承"


NOSTARTER <- subset(PitcherData,inning=="1上"|inning=="1下")
NOSTARTER$numgame <- NOSTARTER$num
NOSTARTER$ROWNUM <- NA
for(i in 1:nrow(NOSTARTER)){
  NOSTARTER$ROWNUM[i] <- i
}

for(i in 1:nrow(PitcherData)){
  if(!is.na(PitcherData$Starter[i])){
    PitcherData$Pitcher[i] <- PitcherData$Starter[i]
  }
  if(!is.na(PitcherData$Reliever[i])){
    PitcherData$Pitcher[i] <- PitcherData$Reliever[i]
  }
}

for (i in 1:nrow(PitcherData)){
  if(i%%100==0){
    print(i)
  }
  ifelse(   grepl("上",PitcherData[i,"inning"])  ,      
            ifelse(is.na(PitcherData[i,"Pitcher"]),PitcherData[i,"Pitcher"] <- pitcherhome , pitcherhome <- PitcherData[i,"Pitcher"]) ,
            ifelse(is.na(PitcherData[i,"Pitcher"]),PitcherData[i,"Pitcher"] <- pitcheraway , pitcheraway <- PitcherData[i,"Pitcher"]) 
            
  )}











