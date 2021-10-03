#打者wpa
setwd("c:\\baseball")
library(dplyr)
library(ggplot2)

#2017資料
subset2017 <- subset(forbatter,year==2017 & TeamPit4WPA!="")
table(subset2017$TeamBat4WPA)
table(subset2017$TeamPit4WPA)

#2017四隊打擊WPA
TeamBatWPAsum <- aggregate(subset2017$WPAadj,
                           by = list(subset2017$TeamBat4WPA),
                           FUN = sum)
TeamBatWPAsum <- TeamBatWPAsum[with(TeamBatWPAsum, order(-x)), ]
names(TeamBatWPAsum) <- c("Team","WPAsum")
TeamBatWPAsum$Team  <- factor(TeamBatWPAsum$Team, levels = TeamBatWPAsum$Team)
TeamBatWPAsum


ggplot(TeamBatWPAsum, aes(x=Team, y=WPAsum)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="BattingWPAsum", 
       subtitle="Data:2017") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6))

#2017四隊投球WPA

TeamPitWPAsum <- aggregate(subset2017$PitcherWPA,
                           by = list(subset2017$TeamPit4WPA),
                           FUN = sum)
TeamPitWPAsum <- TeamPitWPAsum[with(TeamPitWPAsum, order(-x)), ]
names(TeamPitWPAsum) <- c("Team","WPAsum")
TeamPitWPAsum$Team  <- factor(TeamPitWPAsum$Team, levels = TeamPitWPAsum$Team)
TeamPitWPAsum


ggplot(TeamPitWPAsum, aes(x=Team, y=WPAsum)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="PitchingWPAsum", 
       subtitle="Data:2017") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6))

#################################################################################################################
#2018資料
subset2018 <- subset(forbatter,year==2018 & TeamPit4WPA!="")
table(subset2018$TeamBat4WPA)
table(subset2018$TeamPit4WPA)

#2018四隊打擊WPA
TeamBatWPAsum <- aggregate(subset2018$WPAadj,
                           by = list(subset2018$TeamBat4WPA),
                           FUN = sum)
TeamBatWPAsum <- TeamBatWPAsum[with(TeamBatWPAsum, order(-x)), ]
names(TeamBatWPAsum) <- c("Team","WPAsum")
TeamBatWPAsum$Team  <- factor(TeamBatWPAsum$Team, levels = TeamBatWPAsum$Team)
TeamBatWPAsum


ggplot(TeamBatWPAsum, aes(x=Team, y=WPAsum)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="BattingWPAsum", 
       subtitle="Data:2018") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6))

#2018四隊投球WPA

TeamPitWPAsum <- aggregate(subset2018$PitcherWPA,
                           by = list(subset2018$TeamPit4WPA),
                           FUN = sum)
TeamPitWPAsum <- TeamPitWPAsum[with(TeamPitWPAsum, order(-x)), ]
names(TeamPitWPAsum) <- c("Team","WPAsum")
TeamPitWPAsum$Team  <- factor(TeamPitWPAsum$Team, levels = TeamPitWPAsum$Team)
TeamPitWPAsum


ggplot(TeamPitWPAsum, aes(x=Team, y=WPAsum)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="PitchingWPAsum", 
       subtitle="Data:2018") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6))

#投打總和
sumofBatPit<-merge(TeamBatWPAsum,TeamPitWPAsum,by='Team')
sumofBatPit$WPA<-sumofBatPit$WPAsum.x + sumofBatPit$WPAsum.y
sumofBatPit<- sumofBatPit[with(sumofBatPit, order(-WPA)), ]
sumofBatPit$Team  <- factor(sumofBatPit$Team, levels = sumofBatPit$Team)
sumofBatPit

ggplot(sumofBatPit, aes(x=Team, y=WPA)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="WPAsum", 
       subtitle="Data:2018") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6))

#################################################################################################################
#找問題
subset2017 <- subset(forbatter,year==2017)
subset2017 <- subset(forbatter,year==2017 & (num == 1))
table(subset2017$TeamBat4WPA)
table(subset2017$TeamPit4WPA)


#2017四隊打擊WPA
TeamBatWPAsum <- aggregate(subset2017$WPAadj,
                           by = list(subset2017$TeamBat4WPA),
                           FUN = sum)
TeamBatWPAsum <- TeamBatWPAsum[with(TeamBatWPAsum, order(-x)), ]
names(TeamBatWPAsum) <- c("Team","WPAsum")
TeamBatWPAsum$Team  <- factor(TeamBatWPAsum$Team, levels = TeamBatWPAsum$Team)
TeamBatWPAsum



ggplot(TeamBatWPAsum, aes(x=Team, y=WPAsum)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="BattingWPAsum", 
       subtitle="Data:2017") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6))

#2017四隊投球WPA

TeamPitWPAsum <- aggregate(subset2017$PitcherWPA,
                           by = list(subset2017$TeamPit4WPA),
                           FUN = sum)
TeamPitWPAsum <- TeamPitWPAsum[with(TeamPitWPAsum, order(-x)), ]
names(TeamPitWPAsum) <- c("Team","WPAsum")
TeamPitWPAsum$Team  <- factor(TeamPitWPAsum$Team, levels = TeamPitWPAsum$Team)
TeamPitWPAsum


ggplot(TeamPitWPAsum, aes(x=Team, y=WPAsum)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="PitchingWPAsum", 
       subtitle="Data:2017") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6))

#投打總和
sumofBatPit<-merge(TeamBatWPAsum,TeamPitWPAsum,by='Team')
sumofBatPit$WPA<-sumofBatPit$WPAsum.x + sumofBatPit$WPAsum.y
sumofBatPit<- sumofBatPit[with(sumofBatPit, order(-WPA)), ]
sumofBatPit$Team  <- factor(sumofBatPit$Team, levels = sumofBatPit$Team)
sumofBatPit

ggplot(sumofBatPit, aes(x=Team, y=WPA)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="WPAsum", 
       subtitle="Data:2017") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6))



#################################################################################################################


WPAsum <- aggregate(analysisdataADJ$WPAadj,
                    by = list(analysisdataADJ$year),
                    FUN = sum)
WPAsum



#################################################################################################################
#2016資料
subset2016 <- subset(forbatter,year==2016 & TeamPit4WPA!="")
table(subset2016$TeamBat4WPA)
table(subset2016$TeamPit4WPA)

#2016四隊打擊WPA
TeamBatWPAsum <- aggregate(subset2016$WPAadj,
                           by = list(subset2016$TeamBat4WPA),
                           FUN = sum)
TeamBatWPAsum <- TeamBatWPAsum[with(TeamBatWPAsum, order(-x)), ]
names(TeamBatWPAsum) <- c("Team","WPAsum")
TeamBatWPAsum$Team  <- factor(TeamBatWPAsum$Team, levels = TeamBatWPAsum$Team)
TeamBatWPAsum


ggplot(TeamBatWPAsum, aes(x=Team, y=WPAsum)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="BattingWPAsum", 
       subtitle="Data:2016") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6))

#2016四隊投球WPA

TeamPitWPAsum <- aggregate(subset2016$PitcherWPA,
                           by = list(subset2016$TeamPit4WPA),
                           FUN = sum)
TeamPitWPAsum <- TeamPitWPAsum[with(TeamPitWPAsum, order(-x)), ]
names(TeamPitWPAsum) <- c("Team","WPAsum")
TeamPitWPAsum$Team  <- factor(TeamPitWPAsum$Team, levels = TeamPitWPAsum$Team)
TeamPitWPAsum


ggplot(TeamPitWPAsum, aes(x=Team, y=WPAsum)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="PitchingWPAsum", 
       subtitle="Data:2016") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6))

#投打總和
sumofBatPit<-merge(TeamBatWPAsum,TeamPitWPAsum,by='Team')
sumofBatPit$WPA<-sumofBatPit$WPAsum.x + sumofBatPit$WPAsum.y
sumofBatPit<- sumofBatPit[with(sumofBatPit, order(-WPA)), ]
sumofBatPit$Team  <- factor(sumofBatPit$Team, levels = sumofBatPit$Team)
sumofBatPit

ggplot(sumofBatPit, aes(x=Team, y=WPA)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="WPAsum", 
       subtitle="Data:2017") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6))

#################################################################################################################

