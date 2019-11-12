
library(Lahman) # MLB data 패키지
library(dplyr) # 
library(corrplot) 
library(ggplot2)

help(Teams)
View(Lahman::Teams)
# AL리그 1969 ~ 2018년 94,81년제외팀정보 + 주요 수치 SLG, OBP , AVG, ISO, GPA 추가
AL_69_17 <- Lahman::Teams %>% filter(lgID =='AL' & yearID >= 1969 & yearID <= 2018 & yearID != 1994 & yearID != 1981 ) %>% 
  select(yearID, W,L,R,AB,H,X2B,X3B,HR,BB,SO,SB,CS,HBP,SF,RA,ER,ERA,CG,SHO,SV,HA,HRA,BBA,SOA,E,DP,FP,attendance)  %>% 
  mutate(SLG = (H + X2B + X3B *2 + HR*3 )/AB, OBP =(H+BB+HBP)/(AB+BB+HBP+SF) , AVG = H/AB, ISO = SLG-AVG, OPS = OBP + SLG, GPA = (OPS*1.8 + SLG)/4)




# CS : 도루자
# ER : 허용 자책점 (투수책임)
# ERA : 실점에버리지 
# CG : 완봉
# SHO : 완투 
# SV : 세이브 
# IPouts : 이닝 
# HA : 안타허용수 
# HRA : 홈런허용 
# BBA : 사구허용
# SOA : 삼진
# attendance : 홈경기관중수 