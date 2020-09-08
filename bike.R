library(dplyr)
library(tidyverse)


bike <- read.csv("bikeshare.csv")

head(bike,6)

library(ggplot2)

ggplot(bike, aes(count, temp)) + geom_point(alpha=.2,aes(color=temp))+theme_bw()

bike$datetime <- as.POSIXct(bike$datetime)

ggplot(bike, aes(datetime, count))+ geom_point(alpha=.5,aes(color=temp))+ scale_color_continuous(low='#fcba03',high='#d703fc')+theme_bw()

cor(bike$temp,bike$count)

ggplot(bike, aes(factor(season),count))+ geom_boxplot(aes(color=factor(season)))


bike$hour <- sapply(bike$datetime,function(x){format(x,"%H")})

head(bike
     )

p1 <- bike%>% filter(workingday==1) %>% ggplot(aes(hour,count))+geom_point(aes(color=temp),position = position_jitter(w=1,h=0))+scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange',"red"))+theme_bw()

p2<- bike%>% filter(workingday==0) %>% ggplot(aes(hour,count))+geom_point(aes(color=temp),position = position_jitter(w=1,h=0))+scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange',"red"))+theme_bw()
p2


temp.model <- lm(count~temp,data = bike)
summary(temp.model)

predict(temp.model,data.frame(temp=c(25)))


bike$hour <- sapply(bike$hour,as.numeric)        


model <- lm(count ~ . -casual - registered -datetime -atemp,bike )

summary(model)

new.step.model <- step(model)

summary(new.step.model)

set.seed(101)
