

library(Lahman) # MLB data 패키지
library(dplyr) # 
library(corrplot) 
library(ggplot2)

View(Lahman::Batting)
View(Lahman::Pitching)
View(Lahman::People)

Lahman::Batting$playerID

MLBBatter <- subset(Lahman::Batting, Lahman::Batting$playerID != Lahman::Pitching$playerID)
dim(Lahman::Batting)
dim(Lahman::Pitching)
dim(MLBBatter)
View(MLBBatter)


View(inner)
dim(inner)


#df1[!(df1$name %in% df2$name),] and df2[!(df2$name %in% df1$name),]
MLBBatter <- Lahman::Batting[Lahman::Batting$playerID %in% Lahman::Pitching$playerID,]
View(MLBBatter)
dim(MLBBatter)
