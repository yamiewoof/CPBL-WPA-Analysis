#算clutch
setwd("c:\\baseball")
library(dplyr)
library(ggplot2)
library(plyr)

#顏色
pal <- c(
  "桃猿" = "#4682b4",
  "兄弟" = "#ffd700", 
  "富邦" = "#004a9c", 
  "統一" = "#ff8c00" 
)

BatterWPAsum <- aggregate(forbatter$WPAadj,
                          by = list(forbatter$Player),
                          FUN = sum)

BatterWPAsum <- BatterWPAsum[with(BatterWPAsum, order(-x)), ]

names(BatterWPAsum) <- c("Player","WPAsum")
BatterWPAsum$Player  <- factor(BatterWPAsum$Player, levels = BatterWPAsum$Player)

BatWPAtop10 <- BatterWPAsum %>% slice(1:10)
BatWPAtop10

ggplot(BatWPAtop10, aes(x=Player, y=WPAsum)) + 
  geom_bar(stat="identity", width=.5, fill="springgreen4") + 
  labs(title="打者總WPA", 
       subtitle="Data:2014-2019") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

head(BatterWPAsum)


PitcherWPAsum <- aggregate(forbatter$PitcherWPA,
                           by = list(forbatter$Pitcher),
                           FUN = sum)

PitcherWPAsum <- PitcherWPAsum[with(PitcherWPAsum, order(-x)), ]

head(PitcherWPAsum)


BatWPAbyYear <-   aggregate(forbatter$WPAadj,
                            by = list(forbatter$Player,forbatter$year),
                            FUN = sum)

names(BatWPAbyYear) <- c("Player","Year","WPA")

BatWPAbyYear <- BatWPAbyYear %>%  select("Year","Player","WPA")

BatWPAbyYear2014 <- subset(BatWPAbyYear,Year==2014)
BatWPAbyYear2014 <- BatWPAbyYear2014[order(-BatWPAbyYear2014$WPA),]
BatWPAbyYear2014top10 <- slice(BatWPAbyYear2014,1:10)

BatWPAbyYear2015 <- subset(BatWPAbyYear,Year==2015)
BatWPAbyYear2015 <- BatWPAbyYear2015[order(-BatWPAbyYear2015$WPA),]
BatWPAbyYear2015top10 <- slice(BatWPAbyYear2015,1:10)

BatWPAbyYear2016 <- subset(BatWPAbyYear,Year==2016)
BatWPAbyYear2016 <- BatWPAbyYear2016[order(-BatWPAbyYear2016$WPA),]
BatWPAbyYear2016top10 <- slice(BatWPAbyYear2016,1:10)

BatWPAbyYear2017 <- subset(BatWPAbyYear,Year==2017)
BatWPAbyYear2017 <- BatWPAbyYear2017[order(-BatWPAbyYear2017$WPA),]
BatWPAbyYear2017top10 <- slice(BatWPAbyYear2017,1:10)

BatWPAbyYear2018 <- subset(BatWPAbyYear,Year==2018)
BatWPAbyYear2018 <- BatWPAbyYear2018[order(-BatWPAbyYear2018$WPA),]
BatWPAbyYear2018
BatWPAbyYear2018top10 <- slice(BatWPAbyYear2018,1:10)

top10 <- cbind(BatWPAbyYear2014top10,BatWPAbyYear2015top10,BatWPAbyYear2016top10,BatWPAbyYear2017top10,BatWPAbyYear2018top10)
top10

##########################################################################
#2017
BatWPAbyYear2017top10
BatWPAbyYear2017top10$Team <- c("桃猿","桃猿","統一","桃猿","統一","統一","富邦","桃猿","兄弟","桃猿")
BatWPAbyYear2017top10

BatWPAbyYear2017top10$Player  <- factor(BatWPAbyYear2017top10$Player, levels = BatWPAbyYear2017top10$Player)

ggplot(BatWPAbyYear2017top10, aes(x=Player, y=WPA, fill=Team)) + 
  geom_bar(stat="identity", width=.5) + 
  scale_fill_manual(values = pal) +
  labs(title="WPA Top 10", 
       subtitle="Data:2017") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6,size=13))



##########################################################################
#2018
BatWPAbyYear2018top10
BatWPAbyYear2018top10$Team <- c("桃猿","統一","富邦","兄弟","統一","富邦","統一","桃猿","桃猿","富邦")
BatWPAbyYear2018top10
#write.csv(BatWPAbyYear2018top10,file = sprintf("log_process\\logdata\\BatWPAbyYear2018top10.csv"), row.names=FALSE)

BatWPAbyYear2018top10$Player  <- factor(BatWPAbyYear2018top10$Player, levels = BatWPAbyYear2018top10$Player)
p <- ggplot(BatWPAbyYear2018top10, aes(x=Player, y=WPA, fill=Team)) + 
  geom_bar(stat="identity", width=.5) + 
  scale_fill_manual(values = pal) +
  labs(title="2018 WPA Top 10") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6,size=13))
p

library("ggpubr")
transparent.plot <- p + ggpubr::theme_transparent()
transparent.plot

subset1 <- subset(analysisdataADJ,Player=="林益全"&year==2018)

BatWPAbyYear2018top10
top5 <- BatWPAbyYear2018top10 %>% slice(1:5)
top5

p <- ggplot(top5, aes(x=Player, y=WPA, fill=Team)) + 
  geom_bar(stat="identity", width=.5) + 
  scale_fill_manual(values = pal) +
  labs(title="2018 WPA Top 5") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6,size=13))
p
##########################################################################
#算clutch前導作業
head(forbatter)

#加個好排序的行
rownum <- c(1:nrow(forbatter))
abc <- as.data.frame(rownum)
forbatter$totalrownum <- abc$rownum


#LItouse把LI=0和na(都是影響力極低的情況)變為0.1
forbatter$LItouse <- forbatter$LI
for (i in 1:nrow(forbatter)){
  if(i%%100==0){
    print(i)
  }
  if(is.na(forbatter$LItouse[i])){
    forbatter$LItouse[i] <- 0.1
  } else
    if (forbatter$LItouse[i]==0){
      forbatter$LItouse[i] <- 0.1
    } 
  
}

#加上WPA/LI
forbatter$WPA.LI <- forbatter$WPAadj / forbatter$LItouse
forbatter$WPA.LI <- round(forbatter$WPA.LI,4)

#算pLI
pp<-unique(forbatter$Player)
lpp<-length(pp)
zz<-matrix(0,lpp,3)
for (i in 1:lpp){
  if(i%%100==0){
    print(i)
  }
  k1<-which(forbatter$Player==pp[i])
  nn<-sum(forbatter$LItouse[k1])/length(k1)
  zz[i,]<-c(pp[i],nn,length(k1))
}

zz<-as.data.frame(zz)
colnames(zz)<-c("Player","pLI","playercount")
zz$pLI<-round(as.numeric(as.character(zz$pLI)),4)                 # we: WIN EXPECTANCY
zz$playercount<-as.numeric(as.character(zz$playercount))                                  # count: 此種組合在資料中共出現幾次
zz$Name<-as.character(zz$Player)
zz

forbatter<-merge(forbatter,zz, by="Player",all=T)  
forbatter<-forbatter[order(forbatter$totalrownum),-19] 
forbatter <- forbatter %>% select(2:7,Player,8,Pitcher,9:16,LItouse,WPA.LI,pLI,playercount,LI,totalrownum)


##########################################################################

#Clutch
forbatter$Clutch <- (forbatter$WPAadj/forbatter$pLI) - forbatter$WPA.LI
forbatter$Clutch <- round(forbatter$Clutch,4)

forbatter2018 <- subset(forbatter,year==2018)
clutchhitters2018 <- aggregate(forbatter2018$Clutch,
                               by = list(forbatter2018$Player),
                               FUN = sum)
names(clutchhitters2018) <- c("Player","Clutch")

clutchhitters2018 <- clutchhitters2018[order(-clutchhitters2018$Clutch),]
clutchhitters2018



##########################################################################

#2017 加上CLUTCH
dt2017 <- subset(forbatter,year==2017)

#WPA list
WPAlist <- aggregate(dt2017$WPAadj,
                     by = list(dt2017$Player),
                     FUN = sum)
names(WPAlist) <- c("Player","WPA")
WPAlist <- WPAlist[order(-WPAlist$WPA),]
head(WPAlist)


#WPA/LI list
WPA.LIlist <- aggregate(dt2017$WPA.LI,
                        by = list(dt2017$Player),
                        FUN = sum)
names(WPA.LIlist) <- c("Player","WPA.LI")
WPA.LIlist <- WPA.LIlist[order(-WPA.LIlist$WPA.LI),]
head(WPA.LIlist)


#Clutch
clutchlist <- aggregate(dt2017$Clutch,
                        by = list(dt2017$Player),
                        FUN = sum)
names(clutchlist) <- c("Player","Clutch")
clutchlist <- clutchlist[order(-clutchlist$Clutch),]
head(clutchlist)

WPAlist <- join_all(list(WPAlist,WPA.LIlist,clutchlist), by = 'Player', type = 'full')
WPAlisttop10 <- WPAlist %>% slice(1:10)

##########################################################################

#2018 加上CLUTCH
dt2018 <- subset(forbatter,year==2018)

#WPA list
WPAlist <- aggregate(dt2018$WPAadj,
                     by = list(dt2018$Player),
                     FUN = sum)
names(WPAlist) <- c("Player","WPA")
WPAlist <- WPAlist[order(-WPAlist$WPA),]
head(WPAlist)


#WPA/LI list
WPA.LIlist <- aggregate(dt2018$WPA.LI,
                        by = list(dt2018$Player),
                        FUN = sum)
names(WPA.LIlist) <- c("Player","WPA.LI")
WPA.LIlist <- WPA.LIlist[order(-WPA.LIlist$WPA.LI),]
head(WPA.LIlist)


#Clutch
clutchlist <- aggregate(dt2018$Clutch,
                        by = list(dt2018$Player),
                        FUN = sum)
names(clutchlist) <- c("Player","Clutch")
clutchlist <- clutchlist[order(-clutchlist$Clutch),]
head(clutchlist)

WPAlist <- join_all(list(WPAlist,WPA.LIlist,clutchlist), by = 'Player', type = 'full')
WPAlisttop10 <- WPAlist %>% slice(1:10)

fubon09 <- subset(forbatter,year==2018&Player=="林益全")
fubon09 <-  fubon09 %>% select(3:5,WPAadj,count,LItouse,WPA.LI,Clutch)
##########################################################################
