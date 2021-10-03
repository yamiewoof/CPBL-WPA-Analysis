#由歷史資料計算每打席勝率
setwd("c:/baseball")
library(readr)

cpbl2014to19 <- allgames2014to19
cpbl2014to19$gamenum <-  paste(cpbl2014to19$year,cpbl2014to19$num,sep="")
cpbl2014to19$gamenum <- as.numeric(cpbl2014to19$gamenum)

cpbl2014to19 <- cbind(rowforgame=data2014to19$rowforgame,cpbl2014to19)


tempd<-aggregate(cpbl2014to19[, c('gamenum', 'away', 'home')], list(cpbl2014to19$gamenum), tail, 1)[,-1]
perhaps<-merge(cpbl2014to19, tempd, by="gamenum")
perhaps$difff<-parse_number(as.character(perhaps$home.y))-parse_number(as.character(perhaps$away.y))         # 終場勝分數
perhaps$difff0<-parse_number(as.character(perhaps$home.x))-parse_number(as.character(perhaps$away.x))        # 比賽迄今勝分數
perhaps$win<-as.numeric(perhaps$difff>0)
perhaps$rem_type<-as.character(perhaps$rem_typeN)
perhaps$inning<-as.character(perhaps$inning)
k<-which(is.na(perhaps$rem_typeN) | perhaps$rem_type=="林智平" | perhaps$rem_type=="" |perhaps$difff==0)       # 去掉 NA、資料格式錯誤以及終賽時平手等rows
data0<-perhaps[-k,]
data1<-data0[order(data0$gamenum, data0$rowforgame),]
data1$tag<-paste(data1$inning,"",data1$rem_typeN,"",data1$difff0 )

tt<-unique(data1$tag)
ltt<-length(tt)
zz<-matrix(0,ltt,3)
for (i in 1:ltt)
{
  k1<-which(data1$tag==tt[i])
  nn<-sum(data1$win[k1])/length(k1)
  zz[i,]<-c(tt[i],nn,length(k1))
}

zz<-as.data.frame(zz)
colnames(zz)<-c("tag","we","count")
zz$we<-round(as.numeric(as.character(zz$we)),4)                 # we: WIN EXPECTANCY
zz$count<-as.numeric(as.character(zz$count))                                  # count: 此種組合在資料中共出現幾次
zz$tag<-as.character(zz$tag)

thedata<-merge(data1,zz, by="tag",all=T)  ; dim(thedata)
thedata<-thedata[order(thedata$gamenum, thedata$rowforgame),-19]     # 將文字log先拿去，按照順序排列的結果


thedata = thedata %>% mutate(wpa = we - lag(we, default = we[1]))
thedata <- thedata %>% select(4:8,13:15,18,21:23,"we","count","win","wpa")





