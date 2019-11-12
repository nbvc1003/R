


library(Lahman) # MLB data 패키지
library(dplyr) # 
library(corrplot) 
library(ggplot2)


# 앞에서 도출한 R-OPS 모델
model_R_OPS <- m 
R60 <- 0.8164
# 앞에서 도출한 OPS-R 모델
model_OPS_R <- m1 



# AL리그 1969 ~ 2018년 94,81년 제거 팀정보 + 주요 수치 SLG, OBP , AVG, ISO ,GPA 추가
AL_69_18 <- Lahman::Teams %>% filter(lgID =='AL' & yearID >= 1969 & yearID <= 2018 & yearID != 1994 & yearID != 1981) %>% 
  select(franchID, yearID, W,L,R,RA,AB,H,X2B,X3B,HR,BB,SO,SB,CS,HBP,SF,RA,ER,ERA,CG,SHO,SV,HA,HRA,BBA,SOA,E,DP,FP,attendance)  %>% 
  mutate(SLG = (H + X2B + X3B *2 + HR*3 )/AB, OBP =(H+BB+HBP)/(AB+BB+HBP+SF) , AVG = H/AB, ISO = SLG-AVG, OPS = OBP + SLG ,GPA = (OPS*1.8 + SLG)/4)


MLB_NY_TEAM_69_18 <- AL_69_18 %>% filter(franchID == 'NYY')

# 예상 승률 계산법 :  득점^2 / (득점^2 + 실점^2) 
# R * 0.8164 = RA(실점) -> (0.6 승률달성시) 
# R : RA  -> 1 : 0.8164
# RA -> 2018년도 NY 값으로 설정 

#예상승률
pWinRate <- function(R, RA){
    
    return (R^2 / (R^2 + RA^2))
  }


pWinRate(808,660)

# 승율(EWA)와 실점(RA) 에 따른 목표 R 
er_rwa <- function(EWA, RA){
  ER = RA * sqrt(EWA/(1-EWA))
  return (ER)
}


er_rwa(0.6, 669)

needOPS <- function(R){
  ops = 0.0003725076 * R + 0.4652244697
  return (ops)
}

needOPS(820)



# NY팀의 실제 역사적인  R_OPS 상관의 검증 테스트 
# 팀 OPS값으로 예측된 R값을 실제 R값과 비교
testData <- data.frame(OPS = MLB_NY_TEAM_69_18$OPS)
pred <- predict(model_R_OPS, newdata = testData,interval = "confidence")
pred
summary(pred)
View(MLB_NY_TEAM_69_18)
# 예측 결과 
# fit             lwr             upr       
# Min.   :592.0   Min.   :584.6   Min.   :599.5  
# 1st Qu.:693.8   1st Qu.:689.5   1st Qu.:698.1  
# Median :791.9   Median :787.0   Median :796.8  
# Mean   :768.9   Mean   :763.0   Mean   :774.8  
# 3rd Qu.:839.5   3rd Qu.:833.1   3rd Qu.:846.0  
# Max.   :929.8   Max.   :919.8   Max.   :939.9  
# NA's   :1       NA's   :1       NA's   :1 

# 실제 값
# 968 ~ 421

# 어느정도 부합한다. 

# 18 NY RA: 669
# 목표 R -> 820
# NY 18 R -> 851 현재 초과달성중
# 목표 OPS -> 0.7707
# NY 18 OPS = 0.780 으로 이미 충족 
testData <- data.frame(R = 820)
pred <- predict(model_OPS_R , newdata = testData,interval = "confidence")
summary(pred)




## 1 part
# 승률 공식을 통해 0.60 이 나오는 R : RA 비율 
# 18년 특정팀 RA 값 대입 필요한 R값 구한다. 

## 2 part
# 기존 모든 팀 데이터 통해서 R - OPS LM 값
# 기존 모든 팀 데이터 통해서 OPS - SLG LM 값 



 





