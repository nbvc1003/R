
library(Lahman) # MLB data 패키지
library(dplyr) # 
library(corrplot) 
library(ggplot2)

getwd() # wd -> work directory 작업 폴더 경로조회
setwd('e:/nbvc/R/work') # 작업 폴더를 바꿔준다. 


# 승수에 가장 많이 영향을 주는 스텟을 찾아라 
# 10년치 모든 팀들의 승수와 스텟의 관계를 비교 한다. 
# 각 스텟들의 승수에 미치를 영향 상관계수를 구한다. 
# 스텟별로 어느정도 구간에서 가장큰영향을 주는지 파악한다. 
# 팀별로 가장 큰영향을 주는 구간의 스텟을 찾는다. 


# AL리그 1970 ~ 2018년 팀정보 + 주요 수치 SLG, OBP , AVG, ISO 추가
AL_70_17 <- Lahman::Teams %>% filter(lgID =='AL' & yearID >= 1970 & yearID <= 2018) %>% 
      select(yearID, W,L,R,AB,H,X2B,X3B,HR,BB,SO,SB,CS,HBP,SF,RA,ER,ERA,CG,SHO,SV,HA,HRA,BBA,SOA,E,DP,FP,attendance)  %>% 
    mutate(SLG = (H + X2B + X3B *2 + HR*3 )/AB, OBP =(H+BB+HBP)/(AB+BB+HBP+SF) , AVG = H/AB, ISO = SLG-AVG, OPS = OBP + SLG)

View(AL_70_17)
# 득점과 다른 스탯의 상관관계표 
cor_AL_70_17 <- cor(AL_70_17)
View(cor_AL_70_17)
# 상관관계 시각화 
corrplot(cor_AL_70_17, method = "number")


# 상관관계표를 Data.Frame으로 변환
df_cor_AL_70_17 <- as.data.frame(cor_AL_70_17)
# 순위 산정을 위한 idx 컬럼 추가 
df_cor_AL_70_17$idx <- rownames(df_cor_AL_70_17)


View(df_cor_AL_70_17)
# 각스텟별 각각 상관관계 조회
cor.test(AL_70_17$R, AL_70_17$SLG)
cor.test(AL_70_17$R, AL_70_17$ISO)
cor.test(AL_70_17$R, AL_70_17$HR)
cor.test(AL_70_17$R, AL_70_17$H)
cor.test(AL_70_17$R, AL_70_17$AVG)
cor.test(AL_70_17$R, AL_70_17$OBP)
cor.test(AL_70_17$R, AL_70_17$attendance)

cor.test(AL_70_17$W, AL_70_17$R)
df_cor_AL_70_17$W
df_cor_AL_70_17$R

# R 기준 상관관계 표
R_cor_top10_idx <-  df_cor_AL_70_17 %>% select(R,idx) %>% arrange(desc(R)) %>% head(10)
R_cor_top10_idx

## R과 의 상관관계 순위
# 1  1.0000000          R
# 2  0.9436271        OPS
# 3  0.8882687        SLG
# 4  0.8682655        OBP
# 5  0.7185321        AVG
# 6  0.7177488        ISO
# 7  0.7066369          H
# 8  0.6210560          W
# 9  0.6120308        X2B
# 10 0.5971858         HR



# lm 선형 모델 
View(AL_70_17)
AL_70_17$R
AL_70_17$OPS
cor.test(AL_70_17$R, AL_70_17$OPS)
m <- lm(R ~ OPS, data = AL_70_17) 


coef(m)
# (Intercept)         OPS 
# -713.4993   1957.9530 



## 회귀직선의 시각화 1 ------------------------------------
plot(AL_70_17$OPS, AL_70_17$R)
abline(m)

summary(AL_70_17$OPS)

# 신뢰구간 영역 산출 
p <- predict(m, interval = "confidence")
# 오차범위 폴리곤 그래프 1번그래프 
x <- c(AL_70_17$OPS, rev(AL_70_17$OPS))
y <- c(p[,'lwr'], rev(p[,'upr']))
polygon(x,y,col = 'grey')
##-------------------------------------------------------------

## 회귀직선의 시각화 2 -------------------------------------
ggplot(AL_70_17, aes(x=OPS, y=R))+geom_point()+
  geom_smooth(method = "lm")+theme_minimal()
## ---------------------------------------------------------------


# 팀의 OPS 와 R 상관 계수 
# 17년 팀 득점 값을 lm측정 모델에 대입 목표 R값이 되기 위함 OPS값 산출








































