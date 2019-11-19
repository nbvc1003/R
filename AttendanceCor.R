


library(Lahman) # MLB data 패키지
library(dplyr) # 
library(corrplot) 
library(ggplot2)



View(Lahman::TeamsFranchises) # 연고지..
View(Lahman::Fielding) # 선수 출전정보 
View(Lahman::fieldingLabels)
View(Lahman::Salaries) # 선수연봉





# 20191113
# 타자의 성적편차 / 성적(R, 승률, Rank) -> 상관관계 
# 매년 MLB관중수 변동 추이..
# 팀 홈런수와 / 관중수 -> 변동 추이...
# 팀 타자 agv 표준편차 / 관중수 -> 변동 추이...


# AL리그 1969 ~ 2018년 94,81년제외팀정보 + 관중수 관련 수치
AL_69_17_AT <- Lahman::Teams %>% filter(lgID =='AL' & yearID >= 1969 & yearID <= 2018 & yearID != 1994 & yearID != 1981 ) %>% 
  select(yearID, W,L,Rank, R,AB,H,X2B,X3B,HR,RA,ER,ERA,HA,HRA,E,attendance)  %>% 
  mutate(AVG = H/AB, Wrate = W/(W+L) )

View(AL_69_17_AT)
# 년도별 관중수 변동 추이
AL_69_17_YAT <- AL_69_17_AT %>% group_by(yearID)%>% summarise(total_Attandance = sum(attendance), total_HR = sum(HR)) 
View(AL_69_17_YAT)

plot(AL_69_17_YAT$yearID, AL_69_17_YAT$total_HR)

# 리그 전체 관중수 증가 추이와 개별 팀의 관중수 증가 추이 비교
# 팀의 타자성적편차와 ....등등....




