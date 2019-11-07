


library(Lahman) # MLB data 패키지
library(dplyr) # 
library(corrplot) 
library(ggplot2)


# 앞에서 도출한 R-OPS 모델
model_R_OPS <- m 
R60 <- 0.8164
# 앞에서 도출한 OPS-R 모델
model_OPS_R <- m1 



# AL리그 1970 ~ 2018년 팀정보 + 주요 수치 SLG, OBP , AVG, ISO 추가
AL_70_18 <- Lahman::Teams %>% filter(lgID =='AL' & yearID >= 1970 & yearID <= 2018) %>% 
  select(franchID, yearID, W,L,R,RA,AB,H,X2B,X3B,HR,BB,SO,SB,CS,HBP,SF,RA,ER,ERA,CG,SHO,SV,HA,HRA,BBA,SOA,E,DP,FP,attendance)  %>% 
  mutate(SLG = (H + X2B + X3B *2 + HR*3 )/AB, OBP =(H+BB+HBP)/(AB+BB+HBP+SF) , AVG = H/AB, ISO = SLG-AVG, OPS = OBP + SLG)


MLB_NY_TEAM_70_18 <- AL_70_18 %>% filter(franchID == 'NYY')

# 예상 승률 계산법 :  득점^2 / (득점^2 + 실점^2) 
# R * 0.8164 = RA(실점) -> (0.6 승률달성시) 
# R : RA  -> 1 : 0.8164
# RA -> 2018년도 NY 값으로 설정 


# NY팀의 실제 역사적인  R_OPS 상관의 검증 테스트 
# 팀 OPS값으로 예측된 R값을 실제 R값과 비교
testData <- data.frame(OPS = MLB_NY_TEAM_70_18$OPS)
pred <- predict(model_R_OPS, newdata = testData,interval = "confidence")
pred
summary(pred)
View(MLB_NY_TEAM_70_18)
# 예측 결과 
# fit             lwr             upr       
# Min.   :592.0   Min.   :584.6   Min.   :599.5  
# 1st Qu.:691.7   1st Qu.:687.3   1st Qu.:696.0  
# Median :791.9   Median :787.0   Median :796.8  
# Mean   :770.5   Mean   :764.5   Mean   :776.4  
# 3rd Qu.:845.7   3rd Qu.:839.0   3rd Qu.:852.4  
# Max.   :929.8   Max.   :919.8   Max.   :939.9 

# 실제 값
# 968 ~ 421

# 어느정도 부합한다. 

# 17 NY RA: 669
# 목표 R -> 669/R60 = 820
# 목표 OPS -> 0.7707
# NY 18 OPS = 0.780 으로 이미 충족 
testData <- data.frame(R = 820)
pred <- predict(model_OPS_R , newdata = testData,interval = "confidence")
summary(pred)
plot(pred)

# 
#현재 선수들의 내년 예측 OPS ?



## 1 part
# 승률 공식을 통해 0.60 이 나오는 R : RA 비율 
# 18년 특정팀 RA 값 대입 필요한 R값 구한다. 

## 2 part
# 기존 모든 팀 데이터 통해서 R - OPS LM 값
# 기존 모든 팀 데이터 통해서 OPS - SLG LM 값 

## 3 part
# OPS_SLG_OBP


 





