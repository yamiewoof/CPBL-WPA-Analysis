#由log的敘述判定比賽狀況
setwd("c:\\baseball")

library(dplyr)

#讀取14-19年資料 合併為一個表格
data2014 <- read.csv("log_process\\logdata\\logdata_2014.csv",na.strings=c("","NA"),stringsAsFactors = FALSE)
data2014 <- cbind(year = 2014,data2014)
data2015 <- read.csv("log_process\\logdata\\logdata_2015.csv",na.strings=c("","NA"),stringsAsFactors = FALSE)
data2015 <- cbind(year = 2015,data2015)
data2016 <- read.csv("log_process\\logdata\\logdata_2016.csv",na.strings=c("","NA"),stringsAsFactors = FALSE)
data2016 <- cbind(year = 2016,data2016)
data2017 <- read.csv("log_process\\logdata\\logdata_2017.csv",na.strings=c("","NA"),stringsAsFactors = FALSE)
data2017 <- cbind(year = 2017,data2017)
data2018 <- read.csv("log_process\\logdata\\logdata_2018.csv",na.strings=c("","NA"),stringsAsFactors = FALSE)
data2018 <- cbind(year = 2018,data2018)
data2019 <- read.csv("log_process\\logdata\\logdata_2019.csv",na.strings=c("","NA"),stringsAsFactors = FALSE)
data2019 <- cbind(year = 2019,data2019)
data2014to19 <- bind_rows(data2014,data2015,data2016,data2017,data2018,data2019)


allgames2014to19 <- data2014to19 %>% select(1:2,5:13, 15, 16,18,19)
allgames2014to19$base1N <- NA
allgames2014to19$base2N <- NA
allgames2014to19$base3N <- NA
allgames2014to19$basesit <- NA
allgames2014to19$outN <- NA
allgames2014to19$rem_typeN <- NA

#文字敘述類別

allgames2014to19$NOT <- NA
noout <- c("一出局","二出局","三出局","更換","更動","變動","先發","換投","暫停","啟用","盜壘成功","捕逸","盜上","安打","保送","四壞球","暴投","全壘打",")","1上","1下","2上","2下","3上","3下","4上","4下","5上","5下","6上","6下","7上","7下","8上","8下","9上","9下","10上","10下","11上","11下","12上","12下")


hit1_type <- c("一壘安打", "左外野安打", "中外野安打", "右外野安打", 
               "穿越安打", "平飛安打", "內野安打", "中間方向安打", "德州安打", "滾地安打", "不死三振","深遠壘安打")
sf_type <- c("犧牲打","犧牲球","犧牲飛球")
cs_type <- c("盜壘失敗","阻殺")
sb_type <- c("盜壘成功")
sb2B_type <- c("盜上二壘")
forceout <- c("遭到封殺")
wall <- c("全壘打牆", "全壘打大牆")
pickoff <- c("遭到牽制","牽制出局")

base1 <- c("一壘有人", "上一壘", "上到一壘",  "推進到一壘", "至一壘","攻佔一壘","攻占一壘")
base2 <- c("二壘有人", "上二壘", "上到二壘",  "推進到二壘", "至二壘","攻佔二壘","攻占二壘")
base3 <- c("三壘有人", "上三壘", "上到三壘",  "推進到三壘", "至三壘","攻佔三壘","攻占三壘")
base12 <- c("一二壘有人", "佔一、二壘", "一、二壘有人", "至一二壘", "至一、二壘","攻佔一二壘","攻占一二壘","攻佔一、二壘","攻占一、二壘","到一二壘","到一、二壘","分佔一二壘","分占一二壘","分佔一、二壘","分占一、二壘")
base13 <- c("一三壘有人", "佔一、三壘", "一、三壘有人", "至一三壘", "至一、三壘","攻佔一三壘","攻占一三壘","攻佔一、三壘","攻占一、三壘","到一三壘","到一、三壘","分佔一三壘","分占一三壘","分佔一、三壘","分占一、三壘")
base23 <- c("二三壘有人", "佔二、三壘", "二、三壘有人", "至二三壘", "至二、三壘","攻佔二三壘","攻占二三壘","攻佔二、三壘","攻占二、三壘","到二三壘","到二、三壘","分佔二三壘","分占二三壘","分佔二、三壘","分占二、三壘")
base123 <- c("滿壘")
allbases <-  c(base1,base2,base3,base12,base13,base23,base123)

onfirstbase <- c("一壘安打", "左外野安打", "中外野安打", "右外野安打", 
                 "穿越安打", "平飛安打", "內野安打", "中間方向安打", "德州安打", "滾地安打", "不死三振","保送","失誤","形成安打")
onsecondbase <- c("二壘安打")
onthirdbase  <- c("三壘安打")
alldead <- c("雙殺")

scores <- c("打點","得分","回本壘")

#加一欄判斷有無得分(**scores可能不只三種可能)
allgames2014to19$scores <- F

for ( i in 1:nrow(allgames2014to19)){
  for (j in 1:length(scores)) {
    if (grepl(scores[j], allgames2014to19$log[i])){
      allgames2014to19$scores[i] <- T
    }}
}


#處理出局情形
allgames2014to19$outnum <- NA
for (i in 2:nrow(allgames2014to19))
{
  if (is.na(allgames2014to19$out[i])){
    allgames2014to19$outnum[i] <- allgames2014to19$outnum[i-1]
  }
  if (grepl("零出局", allgames2014to19$out[i])){
    allgames2014to19$outnum[i] <- 0
  } 
  else if (grepl("一出局", allgames2014to19$out[i])){
    allgames2014to19$outnum[i] <- 1
  }
  else if (grepl("二出局", allgames2014to19$out[i])){
    allgames2014to19$outnum[i] <- 2
  }
  
}



for ( i in 1:nrow(allgames2014to19))  {
  if(i%%100==0){
    print(i)
  }
  
  if(i==1){
    inning_now <- "一上"
    base1_now <- NA
    base2_now <- NA
    base3_now <- NA
    allgames2014to19$basesit[i] <- 0
    out_now <- 0
  }
  
  #延續上個打席壘包狀況(如果下面out_now不會變動的話)
  if(i>1){
    allgames2014to19$base1N[i] <- allgames2014to19$base1N[i-1]
    allgames2014to19$base2N[i] <- allgames2014to19$base2N[i-1]
    allgames2014to19$base3N[i] <- allgames2014to19$base3N[i-1]
  }
  #換局
  if (allgames2014to19$inning[i] != inning_now){
    inning_now <- allgames2014to19$inning[i]
    out_now <- 0
    allgames2014to19$base1N[i] <- NA
    allgames2014to19$base2N[i] <- NA
    allgames2014to19$base3N[i] <- NA
    allgames2014to19$basesit[i] <- 0
  } 
  
  if (grepl("一出局", allgames2014to19$log[i])){
    out_now <- 1
  }
  
  if (grepl("二出局", allgames2014to19$log[i])){
    out_now <- 2
  }
  
  if (grepl("三出局", allgames2014to19$log[i])){
    out_now <- 2
  }
  allgames2014to19$outN[i] <- out_now
  
  
  
  #處理壘包情形
  if (grepl("全壘打", allgames2014to19$result[i])){                       
    allgames2014to19$basesit[i] <- 0
  }
  
  for (j in 1:length(base1)) {
    if (grepl(base1[j], allgames2014to19$log[i])){
      allgames2014to19$basesit[i] <- 1
    }}
  
  for (j in 1:length(base2)) {
    if (grepl(base2[j], allgames2014to19$log[i])){
      allgames2014to19$basesit[i] <- 2
    }}
  
  for (j in 1:length(base3)) {
    if (grepl(base3[j], allgames2014to19$log[i])){
      allgames2014to19$basesit[i] <- 3
    }}
  
  for (j in 1:length(base12)) {
    if (grepl(base12[j], allgames2014to19$log[i])){
      allgames2014to19$basesit[i] <- 12
    }}
  
  for (j in 1:length(base13)) {
    if (grepl(base13[j], allgames2014to19$log[i])){
      allgames2014to19$basesit[i] <- 13
    }}
  
  for (j in 1:length(base23)) {
    if (grepl(base23[j], allgames2014to19$log[i])){
      allgames2014to19$basesit[i] <- 23
    }}
  
  for (j in 1:length(base123)) {
    if (grepl(base123[j], allgames2014to19$log[i])){
      allgames2014to19$basesit[i] <- 123
    }}
  
  #########################################
  #沒出現幾壘有人
  count <- 0
  
  for (j in 1:length(allbases)) {
    if (grepl(allbases[j], allgames2014to19$log[i]) == TRUE)            
    {count <- count + 1
    }}
  if(count==0){
    for (k in 1:length(onfirstbase)){
      if (grepl(onfirstbase[k], allgames2014to19$log[i])==TRUE){       #壘包沒人，上一壘
        allgames2014to19$basesit[i] <- 1
        currently <- 1
      }}
    for (k in 1:length(onsecondbase)){
      if (grepl(onsecondbase[k], allgames2014to19$log[i])==TRUE){      #壘包沒人，上二壘
        allgames2014to19$basesit[i] <- 2
        currently <- 2
      }}
    for (k in 1:length(onthirdbase)){
      if (grepl(onthirdbase[k], allgames2014to19$log[i])==TRUE){       #壘包沒人，上三壘
        allgames2014to19$basesit[i] <- 3
        currently <- 3
      }}
    for (k in 1:length(alldead)){
      if (grepl(alldead[k], allgames2014to19$log[i])==TRUE){ #雙殺
        if (allgames2014to19$basesit[i-1]==12){
          allgames2014to19$basesit[i] <- 3  #12壘雙殺可能變別的(二壘有人)，但先假設沒寫壘包上的人是因為X-4-3最常見
        } 
        else if(allgames2014to19$basesit[i-1]==123){
          allgames2014to19$basesit[i] <- 3  #只先考慮到殺一二壘，沒考慮傳本壘和踩三壘
        } else{
          allgames2014to19$basesit[i] <- 0  #假設其他雙殺剩1壘被雙殺讓壘包淨空，其他少見的雙殺先沒考慮到
        }}}
    
    for (k in 1:length(wall)){
      if (grepl(wall[k], allgames2014to19$log[i])==TRUE){ #全壘打牆
        if (grepl("一壘安打", allgames2014to19$log[i])==TRUE){
          allgames2014to19$basesit[i] <- 1
        }
        if (grepl("二壘安打", allgames2014to19$log[i])==TRUE){
          allgames2014to19$basesit[i] <- 2
        }
        if (grepl("三壘安打", allgames2014to19$log[i])==TRUE){
          allgames2014to19$basesit[i] <- 3
        }
      }}
    
    for (k in 1:length(pickoff)){
      if (grepl(pickoff[k], allgames2014to19$log[i])==TRUE){ #牽制出局
        if (allgames2014to19$basesit[i-1]==12){
          allgames2014to19$basesit[i] <- 1
        }
        else{
          allgames2014to19$basesit[i] <- 0
        }}}
    
    ############################################################
    #連續兩位上壘
    
    #連兩位上一壘
    if (previous==1 & currently == 1){
      allgames2014to19$basesit[i] <- 12
    }
    
    #前一位在二壘、後面保送變一二壘
    if (previous==2 & currently == 1 & allgames2014to19$scores[i] == F){
      allgames2014to19$basesit[i] <- 12
    }
    
    #前一位在二壘、後面一壘安打
    if (previous==2 & currently == 1 & allgames2014to19$scores[i] == T){
      allgames2014to19$basesit[i] <- 1
    }
    
    #前一位在一壘、後面二壘安打變二三壘
    if (previous==1 & currently == 2 & allgames2014to19$scores[i] == F){
      allgames2014to19$basesit[i] <- 23
    }
    
    #前面二三壘、後面保送變滿壘
    if (previous==23 & currently == 1 & allgames2014to19$scores[i] == F){
      allgames2014to19$basesit[i] <- 123
    }
    #有些情形沒考慮到是因為打席完的壘包狀況多半會寫在log裡，只有少見例外
    
    for (k in 1:length(sf_type)){
      if (grepl(sf_type[k], allgames2014to19$log[i])){
        
        #高飛犧牲打
        if (allgames2014to19$basesit[i-1] ==  3){
          allgames2014to19$basesit[i] <- 0
        }
        else if (allgames2014to19$basesit[i-1] ==  13){
          allgames2014to19$basesit[i] <- 1
        }
        else if (allgames2014to19$basesit[i-1] ==  23){
          allgames2014to19$basesit[i] <- 2
        }
        else if (allgames2014to19$basesit[i-1] ==  123){
          allgames2014to19$basesit[i] <- 12
        }
      }}
    
    #盜壘失敗
    for (k in 1:length(cs_type)){
      if (grepl(cs_type[k], allgames2014to19$log[i])){
        if (allgames2014to19$basesit[i-1] ==  1){
          allgames2014to19$basesit[i] <- 0
        }}}
    
    #盜壘成功
    for (k in 1:length(sb_type)){
      if (grepl(sb_type[k], allgames2014to19$log[i])){
        if (allgames2014to19$basesit[i-1] ==  1){
          allgames2014to19$basesit[i] <- 2
          
        }}}
    
    currently <- 1000
  }
  
  ###############################
  #例外&bug處理
  for (k in 1:length(sb2B_type)){
    if (grepl(sb2B_type[k], allgames2014to19$log[i])){
      
      #13壘盜壘變23壘
      if (allgames2014to19$basesit[i-1] ==  13){
        allgames2014to19$basesit[i] <- 23
        
      }}}
  
  if (is.na(allgames2014to19$basesit[i])){
    allgames2014to19$basesit[i] <- allgames2014to19$basesit[i-1]
  }
  
  outcount <- 0
  for (j in 1:length(noout)) {
    if (grepl(noout[j], allgames2014to19$log[i]) == TRUE)            #沒出現幾人在壘關鍵詞
    {outcount <- outcount + 1
    }
  }
  if(outcount==0){
    allgames2014to19$NOT[i] <- TRUE
  }
  
  
  #跑沒寫出局數的列
  if(is.na(allgames2014to19$NOT[i])==FALSE && is.na(allgames2014to19$result[i])==FALSE){
    
    #三振 
    if((allgames2014to19$result[i]=="三振") && (grepl("不死三振",allgames2014to19$log[i])==FALSE) )
    {
      allgames2014to19$outN[i] <- allgames2014to19$outN[i-1] +1
    }
    
    #失誤
    if((grepl("失誤",allgames2014to19$result[i])==TRUE) && (grepl("跑者出局",allgames2014to19$log[i])==TRUE) )
    {
      allgames2014to19$outN[i] <- allgames2014to19$outN[i-1] +1
    }
    
    #刺殺
    if((grepl("刺殺",allgames2014to19$result[i])==TRUE) && (grepl("失誤",allgames2014to19$result[i])==FALSE))
    {
      allgames2014to19$outN[i] <- allgames2014to19$outN[i-1] +1
    }
    
    #封殺
    if((grepl("封殺",allgames2014to19$result[i])==TRUE) && (grepl("失誤",allgames2014to19$result[i])==FALSE))
    {
      allgames2014to19$outN[i] <- allgames2014to19$outN[i-1] +1
    }
    
    #觸擊/打者沒上壘(2014年可以看warning，但其他年待驗證)
    if((grepl("觸擊",allgames2014to19$result[i])==TRUE) && (grepl("失誤",allgames2014to19$result[i])==FALSE) && (is.na(allgames2014to19$special[i])==TRUE))
    {
      allgames2014to19$outN[i] <- allgames2014to19$outN[i-1] +1
    }
    
    #高飛犧牲打
    if((grepl("高飛犧牲打",allgames2014to19$result[i])==TRUE))
    {
      allgames2014to19$outN[i] <- allgames2014to19$outN[i-1] +1
    }
    
    #雙殺
    if((grepl("雙殺",allgames2014to19$result[i])==TRUE) )
    {
      allgames2014to19$outN[i] <- 2
    }
  }
  
  if(allgames2014to19$outN[i]==3){
    allgames2014to19$outN[i] <- 2
  }
  
  #決定rem_type
  #rem = 1
  if (    allgames2014to19$basesit[i] == 0 && allgames2014to19$outN[i] == 0   ){
    allgames2014to19$rem_typeN[i] <- 1
  }
  
  #rem = 2
  if (    allgames2014to19$basesit[i] == 0 && allgames2014to19$outN[i] == 1   ){
    allgames2014to19$rem_typeN[i] <- 2
  }
  
  #rem = 3
  if (    allgames2014to19$basesit[i] == 0 && allgames2014to19$outN[i] == 2   ){
    allgames2014to19$rem_typeN[i] <- 3
  }
  
  #rem = 4
  if (    allgames2014to19$basesit[i] == 1 && allgames2014to19$outN[i] == 0   ){
    allgames2014to19$rem_typeN[i] <- 4
  }
  
  #rem = 5
  if (    allgames2014to19$basesit[i] == 1 && allgames2014to19$outN[i] == 1   ){
    allgames2014to19$rem_typeN[i] <- 5
  }
  
  #rem = 6
  if (    allgames2014to19$basesit[i] == 1 && allgames2014to19$outN[i] == 2   ){
    allgames2014to19$rem_typeN[i] <- 6
  }
  
  #rem = 7
  if (    allgames2014to19$basesit[i] == 2 && allgames2014to19$outN[i] == 0   ){
    allgames2014to19$rem_typeN[i] <- 7
  }
  
  #rem = 8
  if (    allgames2014to19$basesit[i] == 2 && allgames2014to19$outN[i] == 1   ){
    allgames2014to19$rem_typeN[i] <- 8
  }
  
  #rem = 9
  if (    allgames2014to19$basesit[i] == 2 && allgames2014to19$outN[i] == 2   ){
    allgames2014to19$rem_typeN[i] <- 9
  }
  
  #rem = 10
  if (    allgames2014to19$basesit[i] == 3 && allgames2014to19$outN[i] == 0   ){
    allgames2014to19$rem_typeN[i] <- 10
  }
  
  #rem = 11
  if (    allgames2014to19$basesit[i] == 3 && allgames2014to19$outN[i] == 1   ){
    allgames2014to19$rem_typeN[i] <- 11
  }
  
  #rem = 12
  if (    allgames2014to19$basesit[i] == 3 && allgames2014to19$outN[i] == 2   ){
    allgames2014to19$rem_typeN[i] <- 12
  }
  
  #rem = 13
  if (    allgames2014to19$basesit[i] == 12 && allgames2014to19$outN[i] == 0   ){
    allgames2014to19$rem_typeN[i] <- 13
  }
  
  #rem = 14
  if (    allgames2014to19$basesit[i] == 12 && allgames2014to19$outN[i] == 1   ){
    allgames2014to19$rem_typeN[i] <- 14
  }
  
  #rem = 15
  if (    allgames2014to19$basesit[i] == 12 && allgames2014to19$outN[i] == 2   ){
    allgames2014to19$rem_typeN[i] <- 15
  }
  
  #rem = 16
  if (    allgames2014to19$basesit[i] == 13 && allgames2014to19$outN[i] == 0   ){
    allgames2014to19$rem_typeN[i] <- 16
  }
  
  #rem = 17
  if (    allgames2014to19$basesit[i] == 13 && allgames2014to19$outN[i] == 1   ){
    allgames2014to19$rem_typeN[i] <- 17
  }
  
  #rem = 18
  if (    allgames2014to19$basesit[i] == 13 && allgames2014to19$outN[i] == 2   ){
    allgames2014to19$rem_typeN[i] <- 18
  }
  
  #rem = 19
  if (    allgames2014to19$basesit[i] == 23 && allgames2014to19$outN[i] == 0   ){
    allgames2014to19$rem_typeN[i] <- 19
  }
  
  #rem = 20
  if (    allgames2014to19$basesit[i] == 23 && allgames2014to19$outN[i] == 1   ){
    allgames2014to19$rem_typeN[i] <- 20
  }
  
  #rem = 21
  if (    allgames2014to19$basesit[i] == 23 && allgames2014to19$outN[i] == 2   ){
    allgames2014to19$rem_typeN[i] <- 21
  }
  
  #rem = 22
  if (    allgames2014to19$basesit[i] == 123 && allgames2014to19$outN[i] == 0   ){
    allgames2014to19$rem_typeN[i] <- 22
  }
  
  #rem = 23
  if (    allgames2014to19$basesit[i] == 123 && allgames2014to19$outN[i] == 1   ){
    allgames2014to19$rem_typeN[i] <- 23
  }
  
  #rem = 24
  if (    allgames2014to19$basesit[i] == 123 && allgames2014to19$outN[i] == 2   ){
    allgames2014to19$rem_typeN[i] <- 24
  }
  
  previous <- allgames2014to19$basesit[i] #目前壘包狀況
}

allgames2014to19$rem_typeN <- as.factor(allgames2014to19$rem_typeN)
str(allgames2014to19)


##############################################################

#比較用
comparison2014to19 <- allgames2014to19
#把rem_typeN往上移一列
comparison2014to19 <- transform(comparison2014to19, rem_type = lead(rem_type))
comparison2014to19 <- transform(comparison2014to19, outnum = lead(outnum))
comparison2014to19 <- transform(comparison2014to19, out = lead(out))
comparison2014to19 <- transform(comparison2014to19, base1 = lead(base1))
comparison2014to19 <- transform(comparison2014to19, base2 = lead(base2))
comparison2014to19 <- transform(comparison2014to19, base3 = lead(base3))


comparison2014to19 <- comparison2014to19 %>% select(1:5,10:12,15,19,13,20,6,21)




