


library(Lahman) # MLB data 패키지
library(dplyr) # 
library(corrplot) 
library(ggplot2)

View(Lahman::Pitching)
View(Lahman::People)

PitcherP <- Lahman::Pitching
birthYear <- Lahman::People %>% select (playerID, birthYear,finalGame,debut)
#은퇴년도 추가
birthYear$finalGame <- substr(birthYear$finalGame,1,4)
View(birthYear$finalGame)
#대뷰년도 추가 
birthYear$debut <- substr(birthYear$debut,1,4)


# 생일년도,데뷰년도 ,은퇴년도 추가 
PitcherPB <- merge(x=PitcherP, y=birthYear, by='playerID',all.x = T)





