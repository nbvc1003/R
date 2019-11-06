
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

# 년도별 max홈런 
HR_LEADER_EACH_Y <- Lahman::Batting %>% filter(lgID =='AL' & yearID >= 2008 & yearID <= 2017) %>%
  select(yearID, playerID,HR ) %>% 
  group_by(yearID) %>% summarise(Max_HR = max(HR),n = index) 

HR_LEADER_EACH_Y


# AL리그 2008 ~ 2017년 팀정보 + 주요 수치 SLG, OBP , AVG, ISO 추가
AL_08_17 <- Lahman::Teams %>% filter(lgID =='AL' & yearID >= 2008 & yearID <= 2017) %>% 
      select(yearID, W,L,R,AB,H,X2B,X3B,HR,BB,SO,SB,CS,HBP,SF,RA,ER,ERA,CG,SHO,SV,HA,HRA,BBA,SOA,E,DP,FP,attendance)  %>% 
    mutate(SLG = (H + X2B + X3B *2 + HR*3 )/AB, OBP =(H+BB+HBP)/(AB+BB+HBP+SF) , AVG = H/AB, ISO = SLG-AVG, OPS = OBP + SLG)

View(AL_08_17)
# 득점과 다른 스탯의 상관관계표 
cor_AL_08_17 <- cor(AL_08_17)
View(cor_AL_08_17)
# 상관관계 시각화 
corrplot(cor_AL_08_17, method = "number")


# 상관관계표를 Data.Frame으로 변환
df_cor_AL_08_17 <- as.data.frame(cor_AL_08_17)
# 순위 산정을 위한 idx 컬럼 추가 
df_cor_AL_08_17$idx <- rownames(df_cor_AL_08_17)


View(df_cor_AL_08_17)
# 각스텟별 각각 상관관계 조회
cor.test(AL_08_17$R, AL_08_17$SLG)
cor.test(AL_08_17$R, AL_08_17$ISO)
cor.test(AL_08_17$R, AL_08_17$HR)
cor.test(AL_08_17$R, AL_08_17$H)
cor.test(AL_08_17$R, AL_08_17$AVG)
cor.test(AL_08_17$R, AL_08_17$OBP)
cor.test(AL_08_17$R, AL_08_17$attendance)

cor.test(AL_08_17$W, AL_08_17$R)
df_cor_AL_08_17$W
df_cor_AL_08_17$R

# R 기준 상관관계 표
R_cor_top10_idx <-  df_cor_AL_08_17 %>% select(R,idx) %>% arrange(desc(R)) %>% head(10)
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
View(AL_08_17)
AL_08_17$R
AL_08_17$OPS
cor.test(AL_08_17$R, AL_08_17$OPS)
m <- lm(R ~ OPS, data = AL_08_17) # R = -694.9 + 1934.1 * OPS + e
coef(m)

confint(m)
deviance(m)

# 회귀직선의 시각화
plot(AL_08_17$OPS, AL_08_17$R)
abline(m)

summary(AL_08_17$OPS)
# 신뢰구간
pred <- data.frame(OPS = seq(0.6374, 0.8393, 0.001395))
# p <- predict(m, newdata = pred, interval = "confidence")
#p <- predict(m, interval = "confidence")
predict(m, newdata = data.frame(OPS=0.7),interval = "confidence")

p <- predict.lm(m, interval = "confidence")
p
length(AL_08_17$OPS)
# 오차범위 그래프 
x <- c(AL_08_17$OPS, rev(AL_08_17$OPS))
y <- c(p[,'lwr'], rev(p[,'upr']))
help(rev)
help(c)
help(predict)
# y <- c(p[,'lwr'] )

length(x)
length(y)
polygon(x,y,col = 'grey')


































