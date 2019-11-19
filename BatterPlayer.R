

library(Lahman) # MLB data 패키지
library(dplyr) # 
library(corrplot) 
library(ggplot2)
library(reshape2)

View(Lahman::Batting)
View(Lahman::People)

AgingOpsBatP <- Lahman::Batting
birthYear <- Lahman::People %>% select (playerID, birthYear,finalGame,debut,nameFirst, nameLast)
# 방송에서 사용하는 이름추가 
birthYear$useName <-  paste(substr(birthYear$nameFirst,1,1),  birthYear$nameLast, sep='. ')
View(birthYear)

#은퇴년도 추가
birthYear$finalGame <- substr(birthYear$finalGame,1,4)
View(birthYear$finalGame)
#대뷰년도 추가 
birthYear$debut <- substr(birthYear$debut,1,4)


# 생일년도,데뷰년도 ,은퇴년도 추가 
AgingOpsBatPB <- merge(x=AgingOpsBatP, y=birthYear, by='playerID',all.x = T)
View(AgingOpsBatPB)
AgingOpsBatPB$finalGame <- as.numeric(AgingOpsBatPB$finalGame)
AgingOpsBatPB$debut <- as.numeric(AgingOpsBatPB$debut)

# 타자 지표 추가 
AgingOpsBatPBPlus <- AgingOpsBatPB %>%
  mutate(SLG = (H + X2B + X3B *2 + HR*3 )/AB, OBP =(H+BB+HBP)/(AB+BB+HBP+SF) , AVG = H/AB, ISO = SLG-AVG, OPS = OBP + SLG,GPA = (OPS*1.8 + SLG)/4,
          AGE = yearID - birthYear, RAGE=finalGame - birthYear,DAGE=debut -birthYear)
View(AgingOpsBatPBPlus)


# 1970년 이전 데이터 삭제 타석(AB) < 502 삭제 42살이하 선수만
AgingOpsBatPBPlus70_AB502_AGE42 <- AgingOpsBatPBPlus %>% filter(yearID >= 1970 & AB > 502 & AGE < 42)

# AGE필드 숫자형으로 변환
AgingOpsBatPBPlus70_AB502_AGE42$AGE <- as.numeric(AgingOpsBatPBPlus70_AB502_AGE42$AGE)


# 타자기록간의 상관관계테이블 생성
AgingOpsBatPBPlus70_AB502_AGE42cor <- AgingOpsBatPBPlus70_AB502_AGE42%>% filter(AB > 0 )%>% select(H,R,X2B,X3B,HR, BB,OPS,OBP,SLG,GPA,AVG,ISO,AGE,RAGE)
# 타자들의 OPS에 영향을 미치는 수치들
# 다른 스탯의 상관관계표 
View(AgingOpsBatPBPlus70_AB502_AGE42cor)
cor_AgingOpsBatPBPlus70_AB502_AGE42cor <- cor(AgingOpsBatPBPlus70_AB502_AGE42cor)
# 상관관계 시각화 
corrplot(cor_AgingOpsBatPBPlus70_AB502_AGE42cor, method = "number")
# OPS -> SLG -> HR > OBP > R    순 관련성 높음 
# GPA 추가로 다시 검토 필요..

##-------------------------------------------------------------------------------------------
#selectedPlayersRecent <- dcast(selectedPlayers,playerID+nameGiven  ~ yearID, value.var = "OPS", mean  )

# 최근3년 OPS 추이정보
AgingOpsBatPBPlus_Recent <- AgingOpsBatPBPlus %>% filter(yearID >= 2016 & yearID <= 2018)
AgingOpsBatPBPlus_Recent <- dcast(AgingOpsBatPBPlus_Recent,playerID+useName  ~ yearID, value.var = "OPS", mean  )
View(AgingOpsBatPBPlus_Recent)

# 기본 필터링 
MLB18BatPBPlus <- AgingOpsBatPBPlus %>% filter(yearID == 2018 & teamID != 'NYA' & AGE < 34 & AB > 100) %>% select(playerID, OPS, yearID, teamID,SLG,GPA,AVG,ISO,AGE)
# 포지션 정보 (Y2018PlayerPOS) 추가
MLB18BatPBPlus <- merge(x=MLB18BatPBPlus, y=Y2018PlayerPOS, by='playerID',all.x = T)
# 3년 정보 추가 
MLB18BatPBPlus <- merge(x=MLB18BatPBPlus, y=AgingOpsBatPBPlus_Recent, by='playerID',all.x = T)
View(MLB18BatPBPlus)

# playerID 기준 중복 데이터 제거 
MLB18BatPBPlus <- MLB18BatPBPlus[-which(duplicated(MLB18BatPBPlus$playerID)),]

MLB18Bat_OF <- MLB18BatPBPlus%>% filter(POS =='OF' & AGE < 34) %>% arrange(desc(OPS)) %>% head(10)
MLB18Bat_1B <- MLB18BatPBPlus%>% filter(POS =='1B' & AGE < 34)%>% arrange(desc(OPS)) %>% head(10)
MLB18Bat_3B <- MLB18BatPBPlus%>% filter(POS =='3B' & AGE < 34)%>% arrange(desc(OPS)) %>% head(10)
View(MLB18Bat_OF)
View(MLB18Bat_1B)
View(MLB18Bat_3B)



##-------------------------------------------------------------------------------------------
# 나이별 그룹평균 OPS

View(AgingOpsBatPBPlus70_AB502_AGE42)

AgingOpsBatPBPlus70_AB502_AGE42_G <- AgingOpsBatPBPlus70_AB502_AGE42 %>% filter(OPS > 0) %>% select(OPS,AGE,GPA) %>%group_by(AGE) %>% summarise(mean_OPS = mean(OPS,na.rm = T),mean_GPA = mean(GPA,na.rm = T), count = n())
# 인원수가 80명 이상인 그룹만 남김..
AgingOpsBatPBPlus70_AB502_AGE42_G <- AgingOpsBatPBPlus70_AB502_AGE42_G %>% filter(count > 80)

View(AgingOpsBatPBPlus70_AB502_AGE42_G)
plot(AgingOpsBatPBPlus70_AB502_AGE42_G$AGE, AgingOpsBatPBPlus70_AB502_AGE42_G$count,main = "나이대별 data껀수")
# pcountChart <- barplot(AgingOpsBatPBPlus70_AB502_AGE42_G$count)
text()

boxplot(AgingOpsBatPBPlus70_AB502_AGE42_G)$stats

# 타자의 연령대별 OPS 평균 수치...
ggplot(AgingOpsBatPBPlus70_AB502_AGE42_G, aes(x=AGE, y=mean_OPS)) + geom_line() + geom_smooth(method = "lm")+ggtitle("타자의 연령대별 OPS평균 수치(AB502이상 42살이하)")

# 타자의 연령대별 GPA 평균 수치...
ggplot(AgingOpsBatPBPlus70_AB502_AGE42_G, aes(x=AGE, y=mean_GPA)) + geom_line() + geom_smooth(method = "lm")+ggtitle("타자의 연령대별 GPA평균 수치(AB502이상 42살이하)")

##----------------------------------------------------------------------------------------------------
##은퇴 나이와 통산OPS 관계 
RAGE_OPS <- AgingOpsBatPBPlus70_AB502_AGE42 %>% filter(OPS > 0 ) %>% select(OPS,RAGE) %>%group_by(RAGE) %>% summarise(mean_OPS = mean(OPS,na.rm = T), count = n())
View(RAGE_OPS)
ggplot(RAGE_OPS, aes(x=RAGE, y=mean_OPS))+geom_line()+ggtitle("은퇴나이/OPS평균")

##은퇴 나이와 통산GPA 관계 
RAGE_GPA <- AgingOpsBatPBPlus70_AB502_AGE42 %>% filter(OPS > 0 ) %>% select(GPA,RAGE) %>%group_by(RAGE) %>% summarise(mean_GPA = mean(GPA,na.rm = T), count = n())
ggplot(RAGE_GPA, aes(x=RAGE, y=mean_GPA))+geom_line()+ggtitle("은퇴나이/GPA평균")

##-------------------------------------------------------------------------------------------
# 은퇴 나이대별 각나이대별 OPS 추이
View(AgingOpsBatPBPlus70_AB502_AGE42)

# 40대
RAGE_41_50 <- AgingOpsBatPBPlus70_AB502_AGE42 %>% filter(OPS > 0 & (RAGE>40 & RAGE<=50)) %>% select(OPS,AGE,RAGE) %>%group_by(AGE) %>% summarise(mean_OPS = mean(OPS,na.rm = T), count = n())
View(RAGE_41_50)
plot(RAGE_41_50$AGE,RAGE_41_50$mean_OPS)
ggplot(RAGE_41_50, aes(x=AGE, y=mean_OPS))+geom_line()

# 30대
RAGE_31_40 <- AgingOpsBatPBPlus70_AB502_AGE42 %>% filter(OPS > 0 & (RAGE>30 & RAGE<=40)) %>% select(OPS,AGE,RAGE) %>%group_by(AGE) %>% summarise(mean_OPS = mean(OPS,na.rm = T), count = n())
View(RAGE_31_40)
plot(RAGE_31_40$AGE,RAGE_31_40$mean_OPS)
ggplot(RAGE_31_40, aes(x=AGE, y=mean_OPS))+geom_line()

# 20대
RAGE_21_30 <- AgingOpsBatPBPlus70_AB502_AGE42 %>% filter(OPS > 0 & (RAGE>20 & RAGE<=30)) %>% select(OPS,AGE,RAGE) %>%group_by(AGE) %>% summarise(mean_OPS = mean(OPS,na.rm = T), count = n())
plot(RAGE_21_30$AGE,RAGE_21_30$mean_OPS)
ggplot(RAGE_21_30, aes(x=AGE, y=mean_OPS))+geom_line()


par(mfrow= c(1,1))  # 행, 열
plot(RAgingOps$RAGE, RAgingOps$mean_OPS)
plot(RAgingOps$RAGE, RAgingOps$count)

##------------------------------------------------------------------------------------------------------------------------
# 대뷰 년도와 은퇴나이
DAGE_RAGE <- AgingOpsBatPBPlus70_AB502_AGE42 %>% filter(OPS > 0 ) %>% select(DAGE,RAGE) %>%group_by(DAGE)%>% summarise(mean_RAGE =mean(RAGE))
View(DAGE_RAGE)
ggplot(DAGE_RAGE, aes(x=DAGE, y=mean_RAGE))+geom_line()+ggtitle("대뷰나이/은퇴평균나이")
# 대뷰 년도와 통산OPS성적
DAGE_OPS <- AgingOpsBatPBPlus70_AB502_AGE42 %>% filter(OPS > 0 & RAGE > 30 ) %>% select(OPS,DAGE) %>%group_by(DAGE) %>% summarise(mean_OPS = mean(OPS,na.rm = T), count = n())
ggplot(DAGE_OPS, aes(x=DAGE, y=mean_OPS))+geom_line()+ggtitle("대뷰나이/통산OPS (30이후 은퇴선수만)")

help(ggplot)

##-------------------------------------------------------------------------------------------------------------------------













