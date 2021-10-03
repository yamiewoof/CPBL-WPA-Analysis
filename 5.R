#打者姓名欄
####################################################################################
# #修改名字
setwd("c:\\baseball")
library(dplyr)

analysisdataADJ <- analysisdataADJ[with(analysisdataADJ, order(year, num)),]

#原先抓球員名稱的function會有一些bug(e.g.兩個字的球員會抓到三個字)，下面的csv是用excel改的球員名稱
battername <- read.csv("log_process\\logdata\\batternames(v_2).csv",stringsAsFactors = FALSE,check.names = FALSE)
battername <- battername[with(battername, order(year, num)),]

analysisdataADJ$Player <- battername$Batter

abcd <- as.data.frame(table(analysisdataADJ$Player))

#球隊欄(球隊欄也有不少bug)
analysisdataADJ$awayteam <- analysisdataADJ$away.x
analysisdataADJ$hometeam <- analysisdataADJ$home.x

removepunc=function(log){
  log <- gsub(":", "", log)
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
  log <- gsub("X", "", log)
  log <- gsub("\\(", "", log)
  log <- gsub("\\)", "", log)
  return(log)
}

analysisdataADJ$hometeam <- removepunc(analysisdataADJ$hometeam)
analysisdataADJ$awayteam <- removepunc(analysisdataADJ$awayteam)


for(i in 1:nrow(analysisdataADJ)){
  if (i%%100==0){
    print(i)
  }
  
  if(analysisdataADJ$hometeam[i]=="富"){
    analysisdataADJ$hometeam[i] <- "富邦"
  }
  
  if(analysisdataADJ$hometeam[i]=="繼"){
    analysisdataADJ$hometeam[i] <- "桃猿"
  }
  
  if(analysisdataADJ$hometeam[i]=="邦悍"){
    analysisdataADJ$hometeam[i] <- "富邦"
  }
  
  if(analysisdataADJ$hometeam[i]=="恢" & analysisdataADJ$num[i]==131){
    analysisdataADJ$hometeam[i] <- "兄弟"
  }
  
  if(analysisdataADJ$awayteam[i]=="賽" & analysisdataADJ$num[i]==131){
    analysisdataADJ$awayteam[i] <- "桃猿"
  }
  
  if(analysisdataADJ$hometeam[i]=="恢" & analysisdataADJ$num[i]==157){
    analysisdataADJ$hometeam[i] <- "兄弟"
  }
  
  if(analysisdataADJ$awayteam[i]=="計" & analysisdataADJ$num[i]==157){
    analysisdataADJ$awayteam[i] <- "富邦"
  }
  
  if(analysisdataADJ$hometeam[i]=="恢" & analysisdataADJ$num[i]==175){
    analysisdataADJ$hometeam[i] <- "統一"
  }
  
  if(analysisdataADJ$awayteam[i]=="計" & analysisdataADJ$num[i]==175){
    analysisdataADJ$awayteam[i] <- "兄弟"
  }
  
  if(analysisdataADJ$hometeam[i]=="比" & analysisdataADJ$num[i]==115){
    analysisdataADJ$hometeam[i] <- "富邦"
  }
  
  if(analysisdataADJ$hometeam[i]=="比" & analysisdataADJ$num[i]==116){
    analysisdataADJ$hometeam[i] <- "統一"
  }
  
  if(analysisdataADJ$awayteam[i]=="局" & analysisdataADJ$num[i]==116){
    analysisdataADJ$awayteam[i] <- "兄弟"
  }
  
  if(analysisdataADJ$hometeam[i]=="比" & analysisdataADJ$num[i]==129){
    analysisdataADJ$hometeam[i] <- "統一"
  }
  
  if(analysisdataADJ$year[i]==2017 & analysisdataADJ$num[i]==116){
    analysisdataADJ$awayteam[i] <- "兄弟"
  }
  
  if(analysisdataADJ$hometeam[i]=="比" & analysisdataADJ$num[i]==204){
    analysisdataADJ$hometeam[i] <- "統一"
  }
  
  if(analysisdataADJ$awayteam[i]=="壘" & analysisdataADJ$num[i]==204){
    analysisdataADJ$awayteam[i] <- "桃猿"
  }
  
  if(analysisdataADJ$hometeam[i]=="開" & analysisdataADJ$num[i]==39){
    analysisdataADJ$hometeam[i] <- "桃猿"
  }
  
  if(analysisdataADJ$awayteam[i]=="至" & analysisdataADJ$num[i]==39){
    analysisdataADJ$awayteam[i] <- "兄弟"
  }
  
  if(analysisdataADJ$hometeam[i]=="開" & analysisdataADJ$num[i]==222){
    analysisdataADJ$hometeam[i] <- "統一"
  }
  
  if(analysisdataADJ$awayteam[i]=="遲" & analysisdataADJ$num[i]==222){
    analysisdataADJ$awayteam[i] <- "兄弟"
  }
  
  if(analysisdataADJ$hometeam[i]=="因" & analysisdataADJ$num[i]==131){
    analysisdataADJ$hometeam[i] <- "兄弟"
  }
  
  if(analysisdataADJ$awayteam[i]=="賽" & analysisdataADJ$num[i]==131){
    analysisdataADJ$awayteam[i] <- "統一"
  }
  
  if(analysisdataADJ$hometeam[i]=="因" & analysisdataADJ$num[i]==120){
    analysisdataADJ$hometeam[i] <- "統一"
  }
  
  if(analysisdataADJ$hometeam[i]=="因" & analysisdataADJ$num[i]==196){
    analysisdataADJ$hometeam[i] <- "統一"
  }
  
  if(analysisdataADJ$awayteam[i]=="局" & analysisdataADJ$num[i]==196){
    analysisdataADJ$awayteam[i] <- "兄弟"
  }
  
  if(analysisdataADJ$hometeam[i]=="-" & analysisdataADJ$num[i]==141){
    analysisdataADJ$hometeam[i] <- "義大"
  }
  
  if(analysisdataADJ$year[i]==2015 & analysisdataADJ$num[i]==141){
    analysisdataADJ$awayteam[i] <- "兄弟"
  }
  
  if(analysisdataADJ$hometeam[i]=="暫" & analysisdataADJ$num[i]==157){
    analysisdataADJ$hometeam[i] <- "兄弟"
  }
  
  if(analysisdataADJ$awayteam[i]=="於" & analysisdataADJ$num[i]==157){
    analysisdataADJ$awayteam[i] <- "富邦"
  }
  
  if(analysisdataADJ$awayteam[i]=="n獅"){
    analysisdataADJ$awayteam[i] <- "統一"
  }
  
  if(analysisdataADJ$awayteam[i]=="將"){
    analysisdataADJ$awayteam[i] <- "富邦"
  }
  
  if(analysisdataADJ$awayteam[i]=="兄弟隊"){
    analysisdataADJ$awayteam[i] <- "兄弟"
  }
  
  if(analysisdataADJ$awayteam[i]=="義大隊"){
    analysisdataADJ$awayteam[i] <- "義大"
  }
  
  if(analysisdataADJ$awayteam[i]=="悍將"){
    analysisdataADJ$awayteam[i] <- "富邦"
  }
  
  if(analysisdataADJ$awayteam[i]=="獅"){
    analysisdataADJ$awayteam[i] <- "統一"
  }
  
  if(analysisdataADJ$awayteam[i]=="園"){
    analysisdataADJ$awayteam[i] <- "桃猿"
  }
  
  if(analysisdataADJ$awayteam[i]=="桃源"){
    analysisdataADJ$awayteam[i] <- "桃猿"
  }
  
  if(analysisdataADJ$awayteam[i]=="。"){
    analysisdataADJ$awayteam[i] <- "兄弟"
  }
  
  
}

abc <- as.data.frame(table(analysisdataADJ$awayteam))
abc <- abc[with(abc, order(-Freq)),]

analysisdataADJ$BatterTeam <- NA
analysisdataADJ$PitcherTeam <- NA
for(i in 1:nrow(analysisdataADJ)){
  if(i%%100==0){
    print(i)
  }
  
  if(grepl("上",analysisdataADJ$inning[i])){
    analysisdataADJ$BatterTeam[i] <- analysisdataADJ$awayteam[i]
  } else
    if(grepl("下",analysisdataADJ$inning[i])){
      analysisdataADJ$BatterTeam[i] <- analysisdataADJ$hometeam[i]
    }
  
  if(grepl("上",analysisdataADJ$inning[i])){
    analysisdataADJ$PitcherTeam[i] <- analysisdataADJ$hometeam[i]
  } else
    if(grepl("下",analysisdataADJ$inning[i])){
      analysisdataADJ$PitcherTeam[i] <- analysisdataADJ$awayteam[i]
    }
  
}


################
abc <- as.data.frame(table(analysisdataADJ$BatterTeam))
abc <- abc[with(abc, order(-Freq)),]
abc

cde <- as.data.frame(table(analysisdataADJ$PitcherTeam))
cde <- cde[with(cde, order(-Freq)),]
cde

abc <- as.data.frame(table(analysisdataADJ$awayteam))
abc <- abc[with(abc, order(-Freq)),]
abc

cde <- as.data.frame(table(analysisdataADJ$hometeam))
cde <- cde[with(cde, order(-Freq)),]
cde
################

analysisdataADJ <- analysisdataADJ %>% select(1:7,"BatterTeam",PitcherTeam,8:15,"WPAadj","LI","Pitcher","PitcherWPA","wpa","WINwpa",LOSSwpa,Starter,PitChg,Reliever,awayteam,hometeam)


