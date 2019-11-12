## 2번


exam <- read.csv('csv_exam.csv')
#1
exam[c(2,10,12),'math'] <- NA
#2
exam %>% summarise( mean(math,na.rm = T))
#3
exam$math <- ifelse(is.na(exam$math), mean(exam$math, na.rm = T), exam$math)


library(ggplot2)
mpg <- ggplot2::mpg
## 7

#cty 


#1
mpg[c(3,4,29,42),'cty'] <- c(3,4,29,42)
View(mpg$cty)

#2
boxplot(mpg$cty)$stats

# [,1]
# [1,]  9.0
# [2,] 14.0
# [3,] 16.5
# [4,] 19.0
# [5,] 26.0

mpg <- subset(mpg, cty >= 9 | cty <= 26 )

#3
mpg%>% group_by(drv) %>% summarise(mean_cty = mean(cty))

# drv   mean_cty
# <chr>    <dbl>
#   1 4         14.5
# 2 f         19.9
# 3 r         14.1

