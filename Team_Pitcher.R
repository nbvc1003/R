
library(Lahman) # MLB data 패키지
library(dplyr) # 
library(corrplot) 
library(ggplot2)

help(Teams)
View(Lahman::Teams)
# AL리그 1969 ~ 2018년 94,81년제외팀정보 + 주요 수치 SLG, OBP , AVG, ISO, GPA 추가
AL_69_17_PITCHER <- Lahman::Teams %>% filter(lgID =='AL' & yearID >= 1969 & yearID <= 2018 & yearID != 1994 & yearID != 1981 ) %>% 
  select(yearID, W,L,R,AB,ER,ERA,CG,SHO,SV,IPouts,HA,HRA,BBA,SOA,E,attendance) %>%mutate(Wrate = W/(W+L), OBA = HA/IPouts, WHIP = (HA+BBA)/(IPouts/3))
# %>%   mutate(SLG = (H + X2B + X3B *2 + HR*3 )/AB, OBP =(H+BB+HBP)/(AB+BB+HBP+SF) , AVG = H/AB, ISO = SLG-AVG, OPS = OBP + SLG, GPA = (OPS*1.8 + SLG)/4)

View(AL_69_17_PITCHER)



# ER : 허용 자책점 (투수책임)
# ERA : 실점에버리지 
# CG : 완봉
# SHO : 완투 
# SV : 세이브 수 
# IPouts : 이닝 *3 
# HA : 안타허용수 
# HRA : 홈런허용 
# BBA : 사구허용
# SOA : 삼진
# attendance : 홈경기관중수 

# 

# OBA : 피안타율  피안타 / 타수  HA/IPouts
# WHIP : 이닝당 출루 허용수    출루허용수 / 이닝   HA+BBA / (IP/3)


# W와 다른 스탯의 상관관계표 
cor_AL_69_17_PITCHER <- cor(AL_69_17_PITCHER)

# 상관관계표를 Data.Frame으로 변환
df_cor_AL_69_17_PITCHER <- as.data.frame(cor_AL_69_17_PITCHER)
# 순위 산정을 위한 idx 컬럼 추가 
df_cor_AL_69_17_PITCHER$idx <- rownames(df_cor_AL_69_17_PITCHER)
View(df_cor_AL_69_17_PITCHER)

# 기준 상관관계표
W_cor_top10_idx <-  df_cor_AL_69_17_PITCHER %>% select(Wrate,idx) %>% mutate(abs = abs(Wrate)) %>%arrange(desc(abs)) %>% head(15)
W_cor_top10_idx

# 승율과 투수 지표 상관관계 결론
# ERA , 삼진= 피안타 관련성 우선




