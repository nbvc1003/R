library(Lahman) # MLB data 패키지
library(dplyr) # 
library(corrplot) 
library(ggplot2)


# MLB모든 타자들 + 지표
AgingOpsBatPBPlus
View(AgingOpsBatPBPlus)

# 제외 대상 선수들 
firePlayers # NY 18년도 선수중 제외대상 선수 
View(firePlayers)
class(firePlayers)


# 16, 17, 18 시즌 선수들 
selectedPlayers <- AgingOpsBatPBPlus %>% filter((yearID >= 2016 & yearID <=2018) )
View(selectedPlayers)
class(selectedPlayers)

# 행을 -> 열로 변환 작업 년도 OPS
library(reshape2)
View(dcast(selectedPlayers,playerID~yearID, value.var = "OPS", sum  ))
help(dcast)

selectedPlayersRecent <- dcast(selectedPlayers,playerID+useName  ~ yearID, value.var = "OPS", mean  )
View(selectedPlayersRecent)

# 포지션별 평균 OPS




# 제외 대상 후보 선수들의 최근 년도 성적 차트 
# firePlayers == NY 18년도 선수중 제외대상 선수 
firePlayersRecent <-merge(x=firePlayers,  y=selectedPlayersRecent, by='playerID',all.x = T) 
View(firePlayersRecent)









