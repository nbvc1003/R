


library(Lahman) # MLB data 패키지
library(dplyr) # 
library(corrplot) 
library(ggplot2)

# 연령대별 OPS변동 추이 기준 영입기준 나이대 (22~34)

# 2018년도 선수 
MLB18Players <- Lahman::Batting %>% filter( yearID==2018 & AB > 100) 
View(MLB18Players)

#포지션 
MLB18PlayersPOS <- Lahman::Fielding %>% filter(yearID == 2018) %>% select(playerID, POS)
View(MLB18PlayersPOS)
birthYear <- Lahman::People %>% select (playerID, birthYear,debut) %>%group_by(playerID)
#대뷰년도 추가 
birthYear$debut <- substr(birthYear$debut,1,4)
View(birthYear)

View(Lahman::Salaries)
# 연봉 별도로 
Y2018PlayerSal <- Lahman::Salaries %>% filter(yearID == 2016 )%>% select(playerID, salary)
View(Y2018PlayerSal)

MLB18Players <- merge(x=MLB18Players, y=birthYear, by='playerID',all.x = T)
View(MLB18Players)

MLB18Players <- merge(x=MLB18Players, y=MLB18PlayersPOS, by='playerID',all.x = T)
MLB18Players <- merge(x=MLB18Players, y=Y2018PlayerSal, by='playerID',all.x = T)


MLB18Players$debut <- as.numeric(MLB18Players$debut)

MLB18PlayersPlus <- MLB18Players %>%  mutate(SLG = (H + X2B + X3B *2 + HR*3 )/AB, OBP =(H+BB+HBP)/(AB+BB+HBP+SF) , AVG = H/AB, ISO = SLG-AVG, OPS = OBP + SLG,GPA = (OPS*1.8 + SLG)/4,
                                             AGE = yearID - birthYear, DAGE=debut -birthYear)
# 선수ID 중복 제거 
MLB18PlayersPlus = MLB18PlayersPlus[-which(duplicated(MLB18PlayersPlus$playerID)),]

View(MLB18PlayersPlus)


MLB18PlayersPlus %>% filter(POS == 'OF') %>% head(10)
MLB18PlayersPlus %>% filter(POS == '3B') %>% head(10)
MLB18PlayersPlus %>% filter(POS != '1B') %>% head(10)


# 포지션별 평균 OPS
MLB18PlayerPOS_MeanOPS <- MLB18PlayersPlus %>% group_by(POS) %>% summarise(mean_OPS = mean(OPS,na.rm = T))
View(MLB18PlayerPOS_MeanOPS)



###--------------------------------------------------------------------------------------------------------------------

# 2016년도 선수 
# SLG = (H + X2B + X3B *2 + HR*3 )/AB, OBP =(H+BB+HBP)/(AB+BB+HBP+SF) , AVG = H/AB, ISO = SLG-AVG, OPS = OBP + SLG,GPA = (OPS*1.8 + SLG)/4,
MLB16Players <- Lahman::Batting %>% filter( yearID==2016 & AB > 100) 
MLB16Players %>%  mutate(SLG = (H + X2B + X3B *2 + HR*3 )/AB, OBP =(H+BB+HBP)/(AB+BB+HBP+SF) , OPS = OBP + SLG)%>%summarise(mean_OPS = mean(OPS,na.rm = T))
# 평균 OPS 0.7341

# 2017년도 선수 
MLB17Players <- Lahman::Batting %>% filter( yearID==2017 & AB > 100) 
MLB17Players %>%  mutate(SLG = (H + X2B + X3B *2 + HR*3 )/AB, OBP =(H+BB+HBP)/(AB+BB+HBP+SF) , OPS = OBP + SLG)%>%summarise(mean_OPS = mean(OPS,na.rm = T))
# 평균 OPS 0.7496



##-----------------------------------------------------------------------------------------------------------------


# NA값 평균으로 보정 
MLB18BatPBPlus$`2016` <- ifelse(is.na(MLB18BatPBPlus$`2016`), 0.7341, MLB18BatPBPlus$`2016`)
MLB18BatPBPlus$`2017` <- ifelse(is.na(MLB18BatPBPlus$`2017`), 0.7496, MLB18BatPBPlus$`2017`)
# trendOPS = 18년 OPS - 3년평균 OPS (간단한 OPS추세값)
MLB18BatPBPlus <- MLB18BatPBPlus %>% mutate(trendOPS = (MLB18BatPBPlus$`2018` -( MLB18BatPBPlus$`2018` + MLB18BatPBPlus$`2017` + MLB18BatPBPlus$`2016`)/3 ))
View(MLB18BatPBPlus)

# trendOPS < 0 인 선수 제외..
MLB18BatPBPlusTF <- MLB18BatPBPlus %>% filter(trendOPS > 0)

#MLB18BatPBPlus : 2018년도 MLB 타자리스트 + 포지션 + 최근3년OPS + 이름 + 나이
MLB18Bat_OF <- MLB18BatPBPlusTF%>% filter(POS =='OF' & AGE < 34) %>% arrange(desc(OPS)) %>% head(40)
MLB18Bat_1B <- MLB18BatPBPlusTF%>% filter(POS =='1B' & AGE < 34)%>% arrange(desc(OPS)) %>% head(10)
MLB18Bat_3B <- MLB18BatPBPlusTF%>% filter(POS =='3B' & AGE < 34)%>% arrange(desc(OPS)) %>% head(10)

#OF 20명
View(MLB18Bat_OF) 
#1B 10명
View(MLB18Bat_1B)
#3B 10명
View(MLB18Bat_3B)











