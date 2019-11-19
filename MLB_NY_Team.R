


library(Lahman) # MLB data 패키지
library(dplyr) # 
library(corrplot) 
library(ggplot2)


# 앞에서 도출한 R-OPS 모델
# m <- lm(R ~ OPS, data = AL_69_17) 
model_R_OPS <- m 
R60 <- 0.8164
# 앞에서 도출한 OPS-R 모델
model_OPS_R <- m1 



# AL리그 1969 ~ 2018년 94,81년 제거 팀정보 + 주요 수치 SLG, OBP , AVG, ISO ,GPA 추가
AL_69_18 <- Lahman::Teams %>% filter(lgID =='AL' & yearID >= 1969 & yearID <= 2018 & yearID != 1994 & yearID != 1981) %>% 
  select(franchID, yearID, W,L,R,RA,AB,H,X2B,X3B,HR,BB,SO,SB,HBP,SF,RA,ER,ERA,CG,SHO,HA,HRA,BBA,SOA,E,DP,FP,attendance)  %>% 
  mutate(SLG = (H + X2B + X3B *2 + HR*3 )/AB, OBP =(H+BB+HBP)/(AB+BB+HBP+SF) , AVG = H/AB, ISO = SLG-AVG, OPS = OBP + SLG ,GPA = (OPS*1.8 + SLG)/4)


MLB_NY_TEAM_69_18 <- AL_69_18 %>% filter(franchID == 'NYY')
View(MLB_NY_TEAM_69_18)

# 예상 승률 계산법 :  득점^2 / (득점^2 + 실점^2) 
# R * 0.8164 = RA(실점) -> (0.6 승률달성시) 
# R : RA  -> 1 : 0.8164
# RA -> 2018년도 NY 값으로 설정 



##--------------------------------------------------------------------------------------------------------------
# NY 18년 성적 RA : 669

##필수 함수 들
# 승율(EWA)와 실점(RA) 에 따른 목표 R 
er_rwa <- function(EWA, RA){
  ER = RA * sqrt(EWA/(1-EWA))
  return (ER)
}
er_rwa(0.6, 669)
#대략 820점

#예상승률
pWinRate <- function(R, RA){
    return (R^2 / (R^2 + RA^2))
  }
# 필요 OPS
needOPS <- function(R){
  ops = 0.0003725076 * R + 0.4652244697
  return (ops)
}
needOPS(820)
# 0.770


# NY팀의 실제 역사적인  R_OPS 상관의 검증 테스트 
# 팀 OPS값으로 예측된 R값을 실제 R값과 비교
# level : 신뢰수준 
#confidence  : 이미 traning데이터 안에 있는 값을 모델 바탕으로 측정할때
#predict : traing데이터 범위 밖의 값을 예측할때 사용


pred <- predict(model_OPS_R, newdata = data.frame(R = (MLB_NY_TEAM_69_18$R)),
                level = 0.95, interval = "confidence")
pred
summary(pred)

pred2 <- predict(model_OPS_R, newdata = data.frame(R = (820)),
                 level = 0.95 ,interval = "predict")
pred2
summary(pred2)
# fit      lwr       upr
# 1 0.7740176 0.746461 0.8015741


# 18 NY RA: 669
# 목표 R -> 820
# NY 18 R -> 851 현재 초과달성중
# 목표 OPS -> 0.774
# NY 18 OPS = 0.780 으로 이미 충족 


# OPS 에서 

# NYA 양키스, NYN : 매츠
NYY18Players <- Lahman::Batting %>% filter(teamID == 'NYA' & yearID==2018 ) 
NYY18Players = NYY18Players[-which(duplicated(NYY18Players$playerID)),]


View(NYY18Players)
View(Lahman::Fielding)
# 포지션 별도로 
Y2018PlayerPOS <- Lahman::Fielding %>% filter(yearID == 2018) %>% select(playerID, POS)
View(Y2018PlayerPOS)
birthYear <- Lahman::People %>% select (playerID, birthYear,debut) %>%group_by(playerID)
#대뷰년도 추가 
birthYear$debut <- substr(birthYear$debut,1,4)
View(birthYear)



NYY18Players <- merge(x=NYY18Players, y=birthYear, by='playerID',all.x = T)
NYY18Players <- merge(x=NYY18Players, y=Y2018PlayerPOS, by='playerID',all.x = T)
NYY18Players <- merge(x=NYY18Players, y=Y2018PlayerSal, by='playerID',all.x = T)
View(NYY18Players)

NYY18Players$debut <- as.numeric(NYY18Players$debut)

NYY18PlayersPlus <- NYY18Players %>% filter(POS != 'P') %>%  mutate(SLG = (H + X2B + X3B *2 + HR*3 )/AB, OBP =(H+BB+HBP)/(AB+BB+HBP+SF) , AVG = H/AB, ISO = SLG-AVG, OPS = OBP + SLG,GPA = (OPS*1.8 + SLG)/4,
       AGE = yearID - birthYear, DAGE=debut -birthYear) 




# 2018년 포지션별 최하 OPS성적 선수 리스트 채크 
# 해당 선수들 대체 영입 대상 search

NYY18PlayersPlus = NYY18PlayersPlus[-which(duplicated(NYY18PlayersPlus$playerID)),]

View(NYY18PlayersPlus)

NYY18PlayersPlus2 <- merge(x=NYY18PlayersPlus, y=MLB18PlayerPOS_MeanOPS,by='POS',all.x = T )
NYY18PlayersPlus2$dfOPS <- NYY18PlayersPlus2$OPS -NYY18PlayersPlus2$mean_OPS
View(NYY18PlayersPlus2)
# arrange(desc(OPS))
firePlayers <- NYY18PlayersPlus2 %>% filter(OPS < 0.770) %>% select(playerID, POS,OPS,dfOPS, AGE, salary)%>% arrange(dfOPS) %>% head(10)

# NY 18년도 성적 OPS 기준 0.770 (팀평균) 보다 작은 선수중 탑10 선별 
View(firePlayers)




