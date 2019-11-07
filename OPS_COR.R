
library(Lahman) # MLB data 패키지
library(dplyr) # 
library(corrplot) 
library(ggplot2)
library(car)

## 3 part
# OPS_SLG_OBP

# lm 선형 모델 


# 다중 회귀 분석 
model <- lm(OPS ~ SLG+OBP, data = AL_70_17) 
sqrt(vif(model))
# 1.447 
# 2이하 이므로 다중공선성 문제 없음..


AL_70_17$OPS
AL_70_17$SLG
AL_70_17$OBP

cor.test(AL_70_17$OPS, AL_70_17$SLG) # 0.9718
cor.test(AL_70_17$OPS, AL_70_17$OBP) # 0.8655




# NY


