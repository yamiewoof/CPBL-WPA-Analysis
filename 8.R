#投手wpa，投手WPA W-L
setwd("c:\\baseball")
library(dplyr)
library(ggplot2)

pal <- c(
  "桃猿" = "#4682b4",
  "兄弟" = "#ffd700", 
  "富邦" = "#004a9c", 
  "統一" = "#ff8c00" 
)

col <- c("#FFECCE")

#後援投手處理
Relieversub <- subset(forbatter,!is.na(Reliever))
Relieverfreq <- as.data.frame(table(Relieversub$Pitcher))
names(Relieverfreq)[1] <- "Pitcher"
Relieverfreq <- Relieverfreq[with(Relieverfreq, order(-Freq)), ]
Relieverfreq <- subset(Relieverfreq,Freq>=10)
Relieverfreq

Relieversub<-merge(Relieversub,Relieverfreq, by="Pitcher")


RelieverLI <- aggregate(Relieversub$LI,
                        by = list(Relieversub$Reliever),
                        FUN = mean,
                        na.rm=T)

names(RelieverLI)[1] <- "Pitcher"
names(RelieverLI)[2] <- "LIavg"

RelieverLI <- RelieverLI[with(RelieverLI, order(-LIavg)), ]

RelieverLIsub <- RelieverLI %>% slice(1:10)
RelieverLIsub


library(ggplot2)
p<-ggplot(data=RelieverLIsub, aes(x=Pitcher, y=LIavg)) +
  geom_bar(stat="identity")
p

##################################################################################
#2018資料
Relieversub2018 <- subset(forbatter,!is.na(Reliever))
Relieversub2018 <- subset(Relieversub2018,year==2018)
Relieverfreq2018 <- as.data.frame(table(Relieversub2018$Pitcher))
names(Relieverfreq2018)[1] <- "Pitcher"
Relieverfreq2018 <- Relieverfreq2018[with(Relieverfreq2018, order(-Freq)), ]
Relieverfreq2018 <- subset(Relieverfreq2018,Freq>=10)

Relieversub2018<-merge(Relieversub2018,Relieverfreq2018, by="Pitcher")


RelieverLI2018 <- aggregate(Relieversub2018$LI,
                            by = list(Relieversub2018$Reliever),
                            FUN = mean,
                            na.rm=T)

names(RelieverLI2018)[1] <- "Pitcher"
names(RelieverLI2018)[2] <- "LIavg"

RelieverLI2018 <- RelieverLI2018[with(RelieverLI2018, order(-LIavg)), ]

RelieverLIsub2018 <- RelieverLI2018 %>% slice(1:10)
RelieverLIsub2018 


library(ggplot2)
p<-ggplot(data=RelieverLIsub2018, aes(x=Pitcher, y=LIavg)) +
  geom_bar(stat="identity")
p
##################################################################################
#2017資料
Relieversub2017 <- subset(forbatter,!is.na(Reliever))
#Relieversub2017$LI <- ifelse(is.na(Relieversub2017$LI), 0, Relieversub2017$LI)
Relieversub2017 <- subset(Relieversub2017,year==2017)
Relieverfreq2017 <- as.data.frame(table(Relieversub2017$Pitcher))
names(Relieverfreq2017)[1] <- "Pitcher"
Relieverfreq2017 <- Relieverfreq2017[with(Relieverfreq2017, order(-Freq)), ]
Relieverfreq2017 <- subset(Relieverfreq2017,Freq>=10)

Relieversub2017<-merge(Relieversub2017,Relieverfreq2017, by="Pitcher")


RelieverLI2017 <- aggregate(Relieversub2017$LI,
                            by = list(Relieversub2017$Reliever),
                            FUN = mean,
                            na.rm=T)

names(RelieverLI2017)[1] <- "Pitcher"
names(RelieverLI2017)[2] <- "LIavg"

RelieverLI2017 <- RelieverLI2017[with(RelieverLI2017, order(-LIavg)), ]

RelieverLIsub2017 <- RelieverLI2017 %>% slice(1:10)
RelieverLIsub2017 


library(ggplot2)
p<-ggplot(data=RelieverLIsub2017, aes(x=Pitcher, y=LIavg)) +
  geom_bar(stat="identity")
p
##################################################################################

#個案區
PLAYER <- subset(forbatter, Reliever=="林晨樺"&year==2017)
PLAYER <- PLAYER %>% select(3:5,7,9,11,12,"WPAadj")
rowstosee <- forbatter %>% select(3:5,7,9,11,12,"count","we","WPAadj")
rowstosee <- rowstosee %>% slice(47355:47375)

rowstosee <- forbatter %>% select(3:5,7,9,11,12,"count","we","WPAadj")
rowstosee <- rowstosee %>% slice(113000:113050)

thegame <- subset(forbatter, year=="2017" & num=="184")


##################################################################################
#WPA/W-L

#2018年第71場
year2018G71 <- subset(forbatter, year=="2018" & num=="71")
#目前先手改
year2018G71[1,"PitcherWPA"] <- 0
year2018G71[1,"WPAadj"] <- 0
year2018G71[1,"wpa"] <- 0


PitcherWPA <- aggregate(year2018G71$PitcherWPA,
                        by = list(year2018G71$Pitcher),
                        FUN = sum,
                        na.rm=T)

names(PitcherWPA)[1] <- "Pitcher"
names(PitcherWPA)[2] <- "WPAsum"


PitcherWPA <- PitcherWPA[with(PitcherWPA, order(-WPAsum)), ]
PitcherWPA

#其他分析
year2018G71$rownum <- NA
for(i in 1:nrow(year2018G71)){
  year2018G71$rownum[i] <- i
}

ggplot(data=year2018G71, aes(x=rownum, y=we, group=1)) +
  geom_line()+
  geom_point()

top5 <- year2018G71[with(year2018G71, order(-abs(wpa))), ]
top5 <- top5 %>% slice(1:5) %>% select(3:7,9:13,23,14,20,25,"PitcherWPA")
top5
##################################################################################

#2019年第11場
year2019G11 <- subset(forbatter, year=="2019" & num=="11")
#目前先手改
year2019G11[1,"PitcherWPA"] <- 0
year2019G11[1,"WPAadj"] <- 0
year2019G11[1,"wpa"] <- 0

PitcherWPA <- aggregate(year2019G11$PitcherWPA,
                        by = list(year2019G11$Pitcher),
                        FUN = sum,
                        na.rm=T)

names(PitcherWPA)[1] <- "Pitcher"
names(PitcherWPA)[2] <- "WPAsum"


PitcherWPA <- PitcherWPA[with(PitcherWPA, order(-WPAsum)), ]
PitcherWPA

#其他分析
year2019G11$rownum <- NA
for(i in 1:nrow(year2019G11)){
  year2019G11$rownum[i] <- i
}

year2019G11$inningstart <- NA
year2019G11$inningstart[1] <- "1上"
for(i in 2:nrow(year2019G11)){
  if(year2019G11$inning[i] != year2019G11$inning[i-1])
    year2019G11$inningstart[i] <- year2019G11$inning[i]
}

ggplot(data=year2019G11, aes(x=rownum, y=we, group=1)) +
  geom_line()+
  geom_point()+
  ylim(0, 1) 

top5 <- year2019G11[with(year2019G11, order(-abs(wpa))), ]
top5 <- top5 %>% slice(1:5) %>% select(3:7,9:13,23,14,20,25,"PitcherWPA")
top5
##################################################################################

#2019年第1場
year2019G1 <- subset(forbatter, year=="2019" & num=="1")
#目前先手改
year2019G1[1,"PitcherWPA"] <- 0
year2019G1[1,"WPAadj"] <- 0
year2019G1[1,"wpa"] <- 0

PitcherWPA <- aggregate(year2019G1$PitcherWPA,
                        by = list(year2019G1$Pitcher),
                        FUN = sum,
                        na.rm=T)

names(PitcherWPA)[1] <- "Pitcher"
names(PitcherWPA)[2] <- "WPAsum"




PitcherWPA <- PitcherWPA[with(PitcherWPA, order(-WPAsum)), ]
PitcherWPA

#其他分析
year2019G1$rownum <- NA
for(i in 1:nrow(year2019G1)){
  year2019G1$rownum[i] <- i
}

year2019G1$inningstart <- NA
year2019G1$inningstart[1] <- "1上"
for(i in 2:nrow(year2019G1)){
  if(year2019G1$inning[i] != year2019G1$inning[i-1])
    year2019G1$inningstart[i] <- year2019G1$inning[i]
}

ggplot(data=year2019G1, aes(x=rownum, y=we, group=1)) +
  geom_line()+
  geom_point()+
  ylim(0, 1) 

#year2019G1chart <- year2019G1wpa
year2019G1chart <- year2019G1
year2019G1chart$score <- paste(year2019G1chart$away.x,year2019G1chart$home.x,sep=":")
year2019G1chart <- year2019G1chart %>% select(inning,score,log,we,rownum)
year2019G1chart$text <- paste(year2019G1chart$inning,year2019G1chart$score,sep=" ")
year2019G1chart$text <- paste(year2019G1chart$text,year2019G1chart$log,sep="\n")
#write.csv(year2019G1chart,file = sprintf("log_process\\logdata\\year2019G1chart.csv"), row.names=FALSE)

p <- ggplot(year2019G1chart, aes(rownum,we)) + 
  geom_point(aes(text = text), size=2) +
  geom_line() +
  ylim(0, 1) + 
  labs(title="2019 G1 統一 vs 桃猿") + 
  ylab("Lamigo勝率") +
  geom_hline(yintercept = 0.5, color="red") 
p

p + theme(plot.background = element_rect(fill = "#9DD37D"))
p + theme(
  panel.background = element_rect(fill = "#9DD37D", colour = "#9DD37D",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white"),
  plot.background = element_rect(fill = "#9DD37D")
)

#install.packages("plotly")
library(plotly)
ggplotly(p)

install.packages("ggpubr")
transparent.plot <- p + ggpubr::theme_transparent()
transparent.plot
#



top5 <- year2019G1wpa[with(year2019G1wpa, order(-abs(WPAadj))), ]
top5 <- top5 %>% slice(1:5) %>% select(inning,score,Player,WPAadj,we,log)
names(top5)[5] <- "WE"
names(top5)[4] <- "WPA"
top5



#算WPA

G1wpa <- aggregate(year2019G1wpa$WPAadj,
                   by = list(year2019G1wpa$Player),
                   FUN = sum)
names(G1wpa) <- c("Player","WPAsum")
G1wpa <- G1wpa[with(G1wpa, order(-WPAsum)),]
G1wpatop5 <- G1wpa %>% slice(1:5)
G1wpatop5$Team <- c("桃猿","桃猿","桃猿","桃猿","桃猿")

G1wpatop5$Player  <- factor(G1wpatop5$Player, levels = G1wpatop5$Player)
ggplot(G1wpatop5, aes(x=Player, y=WPAsum, fill=Team)) + 
  geom_bar(stat="identity", width=.5) + 
  scale_fill_manual(values = pal) +
  labs(title="WPA Top 5") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6,size=9))
##################################################################################
#trytry
year2016G1 <- subset(forbatter, year=="2016" & num=="1")


#Batters
BatterWPA <- aggregate(forbatter$WPAadj,
                       by = list(forbatter$Player),
                       FUN = sum,
                       na.rm=T)

huCL <- subset(forbatter,Player=="胡金龍")
sum(huCL$wpa)
subsetCL <- forbatter %>% slice(7250:7260)

yr2016G5 <- subset(forbatter, year=="2016" & num=="5")



abc <- as.data.frame(table(forbatter$Player))


original <- read.csv("log_process\\logdata\\forbatter(v_2).csv")
original <- original[with(original, order(year, num)),]

