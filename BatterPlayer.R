

library(Lahman) # MLB data 패키지
library(dplyr) # 
library(corrplot) 
library(ggplot2)

View(Lahman::Batting)
View(Lahman::People)

AgingOpsBatP <- Lahman::Batting
birthYear <- Lahman::People %>% select (playerID, birthYear,finalGame)
birthYear$finalGame <- substr(birthYear$finalGame,1,4)

class(birthYear$finalGame)

View(birthYear)

# 생일 추가 
AgingOpsBatPB <- merge(x=AgingOpsBatP, y=birthYear, by='playerID',all.x = T)
View(AgingOpsBatPB)

# mutate(SLG = (H + X2B + X3B *2 + HR*3 )/AB, OBP =(H+BB+HBP)/(AB+BB+HBP+SF) , AVG = H/AB, ISO = SLG-AVG, OPS = OBP + SLG)

AgingOpsBatPBPlus <- AgingOpsBatPB %>%
  mutate(SLG = (H + X2B + X3B *2 + HR*3 )/AB, OBP =(H+BB+HBP)/(AB+BB+HBP+SF) , AVG = H/AB, ISO = SLG-AVG, OPS = OBP + SLG,AGE = yearID - birthYear)
View(AgingOpsBatPBPlus)

# # 나이추가...
# AgingOpsBatPBPlus <- AgingOpsBatPBPlus %>% mutate(AGE = yearID - birthYear)

# 70년대 이전 삭제 AB < 502 삭제 42살이하 
AgingOpsBatPBPlus70_AB502_AGE42 <- AgingOpsBatPBPlus %>% filter(yearID >= 1970 & AB > 502 & AGE < 42)

AgingOpsBatPBPlus70_AB502_AGE42$AGE <- as.numeric(AgingOpsBatPBPlus70_AB502_AGE42$AGE)

# # 42이후 삭제 
# AgingOpsBatP <- AgingOpsBatP %>% filter(AGE < 42)

#AgingOpsBatP$AGE <- as.factor(AgingOpsBatP$AGE)

AgingOpsBatPBPlus70_AB502_AGE42_G <- AgingOpsBatPBPlus70_AB502_AGE42 %>% filter(OPS > 0) %>% select(OPS,AGE) %>%group_by(AGE) %>% summarise(mean_OPS = mean(OPS,na.rm = T), count = n())
# 인원수가 80명 이상인 그룹만 남김..
AgingOpsBatPBPlus70_AB502_AGE42_G <- AgingOpsBatPBPlus70_AB502_AGE42_G %>% filter(count > 80)

View(AgingOpsBatPBPlus70_AB502_AGE42_G)
plot(AgingOpsBatPBPlus70_AB502_AGE42_G$AGE, AgingOpsBatPBPlus70_AB502_AGE42_G$count)
# pcountChart <- barplot(AgingOpsBatPBPlus70_AB502_AGE42_G$count)
text()

boxplot(AgingOpsBatPBPlus70_AB502_AGE42_G)$stats

# 타자의 연령대별 ops 평균 수치...
ggplot(AgingOpsBatPBPlus70_AB502_AGE42_G, aes(x=AGE, y=mean_OPS)) + geom_line() + geom_smooth(method = "lm")



#타자 기록관의 상관

AgingOpsBatPBPlus70_AB502_AGE42cor <- AgingOpsBatPBPlus70_AB502_AGE42%>% filter(AB > 0 )%>% select(H,R,X2B,X3B,HR, BB,OPS,OBP,SLG,AVG,ISO,AGE)
# 타자들의 OPS에 영향을 미치는 수치들
# 다른 스탯의 상관관계표 
View(AgingOpsBatPBPlus70_AB502_AGE42cor)
cor_AgingOpsBatPBPlus70_AB502_AGE42cor <- cor(AgingOpsBatPBPlus70_AB502_AGE42cor)
# 상관관계 시각화 
corrplot(cor_AgingOpsBatPBPlus70_AB502_AGE42cor, method = "number")

# OPS -> SLG -> HR > OBP > R    순 관련성 높음 

