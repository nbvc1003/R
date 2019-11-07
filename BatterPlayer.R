

library(Lahman) # MLB data 패키지
library(dplyr) # 
library(corrplot) 
library(ggplot2)

View(Lahman::Batting)
View(Lahman::People)

AgingOpsBatP <- Lahman::Batting
birthYear <- Lahman::People %>% select (playerID, birthYear)
View(birthYear)

# 생일 추가 
AgingOpsBatP <- merge(x=AgingOpsBatP, y=birthYear, by='playerID',all.x = T)
View(AgingOpsBatP)

# mutate(SLG = (H + X2B + X3B *2 + HR*3 )/AB, OBP =(H+BB+HBP)/(AB+BB+HBP+SF) , AVG = H/AB, ISO = SLG-AVG, OPS = OBP + SLG)

AgingOpsBatP <- AgingOpsBatP %>% mutate(SLG = (H + X2B + X3B *2 + HR*3 )/AB, OBP =(H+BB+HBP)/(AB+BB+HBP+SF) , AVG = H/AB, ISO = SLG-AVG, OPS = OBP + SLG)
View(AgingOpsBatP)
# 나이추가...
AgingOpsBatP <- AgingOpsBatP %>% mutate(AGE = yearID - birthYear)

# 70년대 이전 삭제 
AgingOpsBatP <- AgingOpsBatP %>% filter(yearID >= 1970)

AgingOpsBatP$AGE <- as.factor(AgingOpsBatP$AGE)
#AgingOpsBatP$OPS <- 
AgingOpsBatP2 <- AgingOpsBatP %>% filter(OPS > 0)%>% select(OPS,AGE) %>%group_by(AGE) %>% summarise(mean_OPS = mean(OPS,na.rm = T))


View(AgingOpsBatP2)
plot(AgingOpsBatP2)
