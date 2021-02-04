#### For ECO395M Exercise 1 and HW 1
##Q1####
library(readxl)
GasPrices <- read_excel("C:/Users/cheny/Desktop/UIL 2021 Spring/ECO395M/W2/Exercise1/GasPrices.xlsx")
names(GasPrices)

require(ggplot2)

#A) Gas stations charge more if they lack direct competition in sight (boxplot).
p1 <- ggplot(GasPrices, aes(x=Competitors, y=Price)) + 
  geom_boxplot()
p1

#B) The richer the area, the higher the gas price (scatter plot).
p2<-ggplot(GasPrices, aes(x=Income, y=Price)) + 
  geom_point()+
  geom_smooth(method=glm)
p2

#C) Shell charges more than other brands (bar plot).
p3<-ggplot(data=GasPrices, aes(x=Brand, y=Price)) +
  geom_col()
p3

#D) Gas stations at stoplights charge more (faceted histogram).
sp <- ggplot(GasPrices, aes(x=Income, y=Price)) + geom_point(shape=1) +geom_smooth(method=glm)
sp
sp + facet_grid(Stoplight ~ .)

#E) Gas stations with direct highway access charge more (your choice of plot).
sp2 <- ggplot(GasPrices, aes(x=Income, y=Price)) + geom_point(shape=1) +geom_smooth(method=glm)
sp2
sp2 + facet_grid(Highway ~ .)

##Q2####
bikeshare <- read_excel("C:/Users/cheny/Desktop/UIL 2021 Spring/ECO395M/W2/Exercise1/bikeshare.xlsx")
names(bikeshare)

#Plot A: a line graph showing average bike rentals (total) versus hour of the day (hr).
library(tidyverse)
t1<-bikeshare %>% 
  group_by(hr) %>% 
  summarise(average = mean(total))

ggplot(data=t1, aes(x=hr, y=average)) +
  geom_line()+
  geom_point()

#Plot B: a faceted line graph showing average bike rentals versus hour of the day, faceted according to whether it is a working day (workingday).
bikeshare0 <- bikeshare[bikeshare$holiday == 0,]
bikeshare1 <- bikeshare[bikeshare$holiday == 1,]

t2_1<-bikeshare0 %>% 
  group_by(hr) %>% 
  summarise(average = mean(total))
t2_1$holiday="weekend"

t2_2<-bikeshare1 %>% 
  group_by(hr) %>% 
  summarise(average = mean(total))
t2_2$holiday="weekday"

t2=merge(t2_1,t2_2, by="hr",all=TRUE)
t3<-rbind(t2_1,t2_2)

sp2 <- ggplot(t3, aes(x=hr, y=average)) + geom_point(shape=1) +geom_smooth(method=glm)
sp2
sp2 + facet_grid(holiday ~ .)

#Plot C: a faceted bar plot showing average ridership during the 8 AM hour by weather situation code (weathersit), faceted according to whether it is a working day or not. 
bikeshare_8<-bikeshare %>%
  filter(hr==8)

bikeshare_80 <- bikeshare_8[bikeshare_8$holiday == 0,]
bikeshare_81 <- bikeshare_8[bikeshare_8$holiday == 1,]

t2_1<-bikeshare_80 %>% 
  group_by(weathersit) %>% 
  summarise(average = mean(total))
t2_1$holiday="weekend"

t2_2<-bikeshare_81 %>% 
  group_by(weathersit) %>% 
  summarise(average = mean(total))
t2_2$holiday="weekday"

t3<-rbind(t2_1,t2_2)

sp2 <- ggplot(t3, aes(x=weathersit, y=average)) + geom_point(shape=1) +geom_smooth(method=glm)
sp2
sp2 + facet_grid(holiday ~ .)


##Q3####
air <- read_excel("C:/Users/cheny/Desktop/UIL 2021 Spring/ECO395M/W2/Exercise1/ABIA.xlsx")
names(air)

air_AUS <- air[air$Origin == "AUS"|air$Dest == "AUS",]
summary(air_AUS)
air_AUS$count=1
air_AUS$ArrDelayIndex=ifelse(air_AUS$ArrDelay<=0,0,1)
air_AUS$DepDelayIndex=ifelse(air_AUS$DepDelay<=0,0,1)

#FLIGHTS by month, date, and DoW####
p1<-air_AUS %>% 
  group_by(Month) %>% 
  summarise(Number = sum(count))
p1$Month=factor(p1$Month)
ggplot(data=p1, aes(x=Month, y=Number)) +
  geom_bar(stat="identity") 

p2<-air_AUS %>% 
  group_by(DayofMonth) %>% 
  summarise(Number = sum(count))
p2$DayofMonth=factor(p2$DayofMonth)
ggplot(data=p2, aes(x=DayofMonth, y=Number)) +
  geom_bar(stat="identity")

p3<-air_AUS %>% 
  group_by(DayOfWeek) %>% 
  summarise(Number = sum(count))
p3$DayOfWeek=factor(p3$DayOfWeek)
ggplot(data=p3, aes(x=DayOfWeek, y=Number)) +
  geom_bar(stat="identity")

#Delay ------ when will be a delay####
air_AUS_BDelay<-air_AUS %>%
  filter(ArrDelay==1 | DepDelay==1)

p1_BD<-air_AUS_BDelay %>% 
  group_by(Month) %>% 
  summarise(Number = sum(count))
p1_BD$Month=factor(p1_BD$Month)


p2_BD<-air_AUS_BDelay %>% 
  group_by(DayofMonth) %>% 
  summarise(Number = sum(count))
p2_BD$DayofMonth=factor(p2_BD$DayofMonth)

p3_BD<-air_AUS_BDelay %>% 
  group_by(DayOfWeek) %>% 
  summarise(Number = sum(count))
p3_BD$DayOfWeek=factor(p3_BD$DayOfWeek)


p1_month=merge(p1,p1_BD, by="Month",all=TRUE)
p2_date=merge(p2,p2_BD,by="DayofMonth",all=TRUE)
p3_DoW=merge(p3,p3_BD, by="DayOfWeek",all=TRUE)
names(p1_month)

p1_month$rate=p1_month$Number.y/p1_month$Number.x
p2_date$rate=p2_date$Number.y/p2_date$Number.x
p3_DoW$rate=p3_DoW$Number.y/p3_DoW$Number.x

ggplot(data=p1_month, aes(x=Month, y=rate)) +
  geom_bar(stat="identity")
ggplot(data=p2_date, aes(x=DayofMonth, y=rate)) +
  geom_bar(stat="identity")
ggplot(data=p3_DoW, aes(x=DayOfWeek, y=rate)) +
  geom_bar(stat="identity")

#Delay ------ when will be a longer delay
names(air_AUS)
summary(air_AUS)

air_AUS_q3 <- air_AUS[,c("Month","DayofMonth","ArrDelay", "DepDelay")]
air_AUS_q3 <- na.omit(air_AUS_q3)
air_AUS_q3 <- air_AUS_q3[air_AUS_q3$ArrDelay!="NA",]
air_AUS_q3 <- air_AUS_q3[air_AUS_q3$DepDelay!="NA",]

summary(air_AUS_q3)

air_AUS_q3$ArrDelay=as.numeric(air_AUS_q3$ArrDelay)
air_AUS_q3$DepDelay=as.numeric(air_AUS_q3$DepDelay)

p <- ggplot(air_AUS_q3, aes(x=factor(Month),y=factor(DayofMonth)))
p <- p + geom_tile(aes(fill=ArrDelay))
p <- p + scale_fill_gradient(low = "white", high = "red")
p <- p + theme()
p

p <- ggplot(air_AUS_q3, aes(x=factor(Month),y=factor(DayofMonth)))
p <- p + geom_tile(aes(fill=DepDelay))
p <- p + scale_fill_gradient(low = "white", high = "blue")
p <- p + theme()
p

##Q4####
library(tidyverse)
library(caret)
library(dplyr)
library(FNN)
library(modelr)

CLASSS <- read_excel("C:/Users/cheny/Desktop/UIL 2021 Spring/ECO395M/W2/Exercise1/sclass.xlsx")
summary(CLASSS$trim)

trim1 = CLASSS[CLASSS$trim=="350",]
trim2 = CLASSS[CLASSS$trim=="65 AMG",]

trim1=trim1[,c("mileage","price")]
trim2=trim2[,c("mileage","price")]

training.samples <- trim1$price %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- trim1[training.samples, ]
test.data <- trim1[-training.samples, ]

names(train.data)
knn100 = knnreg(price~mileage, data=train.data,k=100)
rmse(knn100, test.data)
knn10 = knnreg(price~mileage, data=train.data,k=10)
rmse(knn10, test.data)
knn1 = knnreg(price~mileage, data=train.data,k=1)
rmse(knn1, test.data)

p1 <- data.frame(index=c(1,10,100), 
                 RMSE=c(rmse(knn1, test.data), rmse(knn10, test.data), rmse(knn100, test.data)))


