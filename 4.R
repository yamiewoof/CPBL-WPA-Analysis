setwd("c:\\baseball")
library(dplyr)

analysisdata <- PitcherData

#校正wpa
analysisdata$WINwpa <- NA
analysisdata$LOSSwpa <- NA
analysisdata$WPAadj <- NA
for (i in 1:nrow(analysisdata)){
  if (i%%100==0){
    print(i)
  }
  if(analysisdata$win[i]==1){
    #主場贏 客隊wpa*-1
    if(grepl("上",analysisdata$inning[i])){
      analysisdata$LOSSwpa[i] <- analysisdata$wpa[i] * (-1)
    }
    #主場贏 主隊wpa不變
    else
      if(grepl("下",analysisdata$inning[i])){
        analysisdata$WINwpa[i] <- analysisdata$wpa[i] 
      }
  }
  else
    if(analysisdata$win[i]==0){
      #客場贏 客隊wpa不變
      if(grepl("上",analysisdata$inning[i])){
        analysisdata$WINwpa[i] <- analysisdata$wpa[i] * (-1)
      } 
      #客場贏 主隊wpa*-1
      else
        if(grepl("下",analysisdata$inning[i])){
          analysisdata$LOSSwpa[i] <- analysisdata$wpa[i]
        }
    }
  
  if(!is.na(analysisdata$WINwpa[i])){
    analysisdata$WPAadj[i] <- analysisdata$WINwpa[i]
  }
  
  if(!is.na(analysisdata$LOSSwpa[i])){
    analysisdata$WPAadj[i] <- analysisdata$LOSSwpa[i]
  }
}

#修正有得分那列wpa也要算在球員身上(因為中職得分的是多出一行 e.g.桃猿(3):(2)富邦)

analysisdataADJ <- analysisdata
#投手Wpa相反
analysisdataADJ$PitcherWPA <- -analysisdataADJ$WPAadj

PlayerNA <- subset(analysisdataADJ,is.na(Player))
scores <- subset(PlayerNA,grepl("\\):\\(",log))


for (i in 2:nrow(analysisdataADJ)){
  if (i%%100==0){
    print(i)
  }
  if(is.na(analysisdataADJ$Player[i])){
    
    if (grepl("\\):\\(",analysisdataADJ$log[i])){
      analysisdataADJ$id[i] <- analysisdataADJ$id[i-1]
      analysisdataADJ$Player[i] <- analysisdataADJ$Player[i-1]
    }
    
    if ( (analysisdataADJ$inning[i] != analysisdataADJ$inning[i-1] ) & (analysisdataADJ$inning[i]!= "1上") ){
      if (grepl("上",analysisdataADJ$log[i])){
        analysisdataADJ$id[i] <- analysisdataADJ$id[i-1]
        analysisdataADJ$Player[i] <- analysisdataADJ$Player[i-1]
        analysisdataADJ$Pitcher[i] <- analysisdataADJ$Pitcher[i-1]
      } else 
        if(grepl("下",analysisdataADJ$log[i])){
          analysisdataADJ$id[i] <- analysisdataADJ$id[i-1]
          analysisdataADJ$Player[i] <- analysisdataADJ$Player[i-1]
          analysisdataADJ$Pitcher[i] <- analysisdataADJ$Pitcher[i-1]
        }
      
      
    }
    if(grepl("1上",analysisdataADJ$log[i]) && (analysisdataADJ$num[i] != analysisdataADJ$num[i-1] ) ){
      analysisdataADJ$Pitcher[i] <- NA
    }
    
  }
  
}

for(i in 2:nrow(analysisdataADJ)){
  if(i%%100==0){
    print(i)
  }
  
  if((analysisdataADJ$num[i] != analysisdataADJ$num[i-1])){
    analysisdataADJ$WPAadj[i] <- 0
    analysisdataADJ$PitcherWPA[i] <- 0
  } 
}

times <- as.data.frame(table(analysisdataADJ$count))
times$Var1 <- as.character(times$Var1)
times$Var1 <- as.numeric(times$Var1)
times$frequency <- times$Freq / times$Var1

under10 <- subset(analysisdataADJ,count<=10)


