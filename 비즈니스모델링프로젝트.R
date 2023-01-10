install.packages("dplyr")
install.pacakges("forecast")
install.packages("caret")
install.packages("ggplot2")
install.packages("extrafont")

library(dplyr)
library(forecast)
library(caret)
library(ggplot2)
library(extrafont)

font_import()

theme_set(theme_grey(base_family='NanumGothic'))


dust.df <- read.csv("SeoulFineDust.csv")

No2.df <- read.csv("SeoulNo2.csv")

View(dust.df)


dust.df <- rename(dust.df,
                  Finedust =FineDust.PM )

No2.df <- rename(No2.df,
                 no2 = NO2.ppm)


View(dust.df)
View(No2.df)

str(dust.df)

par(family="AppleGothic")

#데이터 분석



# 강남구 미세먼지가 3,4월 중   1일날 미세먼지가 심한 순위

Finedustcitydate <- dust.df %>% 
  filter(District=="강남구")%>%
  filter(Date=='1')%>%
  arrange(desc(Finedust)) 

View(Finedustcitydate)

#2020년  3월미세먼지가 심한 도시 나타내기 

Finedust_2020_3 <- dust.df %>%
  filter(Month=='3') %>%
  filter(Year=='2020')


#그래프로 2020년 3월 미세먼지가 심한 도시 나타내기 
ggplot(data = Finedust_2020_3, aes(x=District, y=Finedust)) + geom_col() + scale_y_continuous(labels = scales::comma) + xlab("지역구") + ylab("먼지")


#2020년 3월 중 미세먼지가 심한 날짜 
FinedustDate <- dust.df %>% 
  filter(Month=="3")%>%
  filter(Year=='2020')%>%
  arrange(desc(Finedust)) 

View(FinedustDate)

#미세먼지가 심한 날짜 그래프로 나타내기

ggplot(data = dust.df, aes(x=Date, y=Finedust)) + geom_col() + scale_y_continuous(labels = scales::comma) + xlab("날짜") + ylab("먼지")



#2020년 3월 미세먼지가 제일 심한 5개의 구에서 날짜별 미세먼지 현황

Finedust_2020_3 <- dust.df %>%
  filter(Month=='3') %>%
  filter(Year=='2020') %>%
  head(5)

View(Finedust_2020_3)

#Finedust_2020_3_date <- dust.df %>%
#  group_by(Finedust,Date)%>%
#  dplyr::summarise(Time = sum(time))


# 미세먼지를 이용한 다중 선형 회귀분석 모델


# 전처리 

table(dust.df$Finedust)

#결측치 확인 
table(is.na(dust.df$Finedust))
sum(is.na(dust.df$Finedust))

#결측치 처리
Finedust_clean <- dust.df%>%filter(!is.na(Finedust))
table(is.na(Finedust_clean$Finedust))

#이상치 처리
dust.df$Finedust <- ifelse(dust.df$Finedust<10, NA, dust.df$Finedust)

table(is.na(dust.df$Finedust))

Finedust_clean <- dust.df%>%filter(!is.na(Finedust))
table(is.na(Finedust_clean$Finedust))


#날씨 예측 

new.df <- data.frame(date = 10, year = 2023,District='강남구', Finedust = 35)


new.df <- data.frame(District='강남구', Finedust =10)


#partitioning for training and validation
set.seed(1)
train.index <- sample(c(1:dim(dust.df)[1]), dim(dust.df)[1]*0.6)
train.df <- dust.df[train.index,]
valid.df <- dust.df[-train.index,]


#정규화
train.norm.df <- train.df
valid.norm.df <- valid.df
dust.norm.df <- dust.df
new.norm.df <- new.df
#method에 center를 할당하면 중심을 맞춰주고( 워낮료에 자료의 평균을 빼버림), scale을 할당하면 표준편차로 나누기 때문에 두 방법을 같이 적용하면 표준화를 할 수 있다.
norm.values <- preProcess(train.df[,c(4,5)], method=c("center","scale"))
train.norm.df[,c(4,5)] <- predict( norm.values, train.df[,c(4,5)] )
valid.norm.df[,c(4,5)] <- predict( norm.values, valid.df[,c(4,5)] )
dust.norm.df[,c(4,5)] <- predict( norm.values, dust.df[,c(4,5)] )




new.norm.df<- predict(norm.values,new.df)


#다중 선형 회구 모델 with training set
Finedust.lm <- lm(Finedust ~ ., data = train.norm.df)
options(scipen=999)
summary(Finedust.lm)

# validation set for accuracy
Finedust.lm.pred <- predict(Finedust.lm, valid.norm.df)
options(scipen=999, digits=1)
#check the residuals
some.residuals <- valid.norm.df$Finedust[3:5]
Finedust.lm.pred[3:5]
#df for residuals
data.frame("Predicted" = Finedust.lm.pred[3:5], "Actual"= valid.norm.df$Finedust[3:5], "Residual" = some.residuals)
options(scipen =999, digits=3)
forecast::accuracy(Finedust.lm.pred, valid.norm.df$Finedust)

#train set for accuaracy
Finedust.lm.pred.train <- predict(Finedust.lm, train.norm.df)
options(scipen=999, digits = 4)
# check the residuals 
some.residuals <- train.norm.df$Finedust[3:5] - Finedust.lm.pred.train[3:5]
# df for residuals
data.frame("Predicted" = Finedust.lm.pred.train[3:5], "Acutal" = train.norm.df$Finedust[3:5], "Residual" = some.residuals)
options(scipen =999, digits = 0)
# check the accuracy
forecast::accuracy(Finedust.lm.pred.train, train.norm.df$Finedust)


#  show all residuals with visualization
all.residuals <- valid.norm.df$Finedust - Finedust.lm.pred
hist(all.residuals, breaks = 20, xlab = "Residuals", main = "chart")

# 산점도 그래프로 나타내기
plot(Finedust.lm.pred.train)

#new data test
new.pred <- predict(Finedust.lm, new.norm.df=data.frame())

options(scipen=999, digits=3)

new.pred





#이산화질소를 이용한 다중 선형 회귀 분석 모델 


View(No2.df)
table(No2.df$no2)

#결측치 확인 
table(is.na(No2.df$no2))
sum(is.na(No2.df$no2))

#결측치 처리
no2_clean <- No2.df%>%filter(!is.na(no2))
table(is.na(no2_clean$no2))

#이상치 처리

No2.df$no2 <- ifelse(No2.df$no2<0.01, NA, No2.df$no2)
table(is.na(No2.df$no2))

no2_clean <- No2.df%>%filter(!is.na(no2))
table(is.na(no2_clean$no2))

View(No2.df)

#날씨 예측 
new.df <- data.frame(date = 10, month = 3, no2 = 0.034)



#partitioning for training and validation
set.seed(1)
train.index <- sample(c(1:dim(No2.df)[1]), dim(No2.df)[1]*0.6)
train.df <- No2.df[train.index,]
valid.df <- No2.df[-train.index,]

View(No2.df)
#정규화
train.norm.df <- train.df
valid.norm.df <- valid.df
no2.norm.df <- No2.df
new.norm.df<- new.df


norm.values <- preProcess( train.df[,c(4,5)], method=c("center","scale"))
train.norm.df[,c(4,5)] <- predict( norm.values, train.df[,c(4,5)] )
valid.norm.df[,c(4,5)] <- predict( norm.values, valid.df[,c(4,5)] )
no2.norm.df[,c(4,5)] <- predict( norm.values, No2.df[,c(4,5)] )


#다중 선형 회구 모델 with training set
no2.lm <- lm(no2 ~ ., data = train.norm.df)
options(scipen=999)
summary(no2.lm)

View(No2.df)
# validation set for accuracy
no2.lm.pred <- predict(no2.lm, valid.norm.df)
options(scipen=999, digits=2)
#check the residuals
some.residuals <- valid.norm.df$no2[2:5]
#df for residuals
data.frame("Predicted" = no2.lm.pred[2:5], "Actual"= valid.norm.df$no2[2:5], "Residual" = some.residuals)
options(scipen =999, digits=3)
forecast::accuracy(no2.lm.pred, valid.norm.df$no2)

#train set for accuaray
no2.lm.pred.train <- predict(no2.lm, train.norm.df)
options(scipen=999, digits = 10)
# check the residuals 
some.residuals <- train.norm.df$no2[2:5] - no2.lm.pred.train[2:5]
# df for residuals
data.frame("Predicted" = no2.lm.pred.train[2:5], "Acutal" = train.norm.df$no2[2:5], "Residual" = some.residuals)
options(scipen =999, digits = 3)
# check the accuracy
forecast::accuracy(no2.lm.pred.train, train.norm.df$no2)

# // show all residuals with visualization
all.residuals <- valid.norm.df$no2 - no2.lm.pred
hist(all.residuals, breaks = 20, xlab = "Residuals", main = "chart")

plot(no2.lm.pred.train)

new.pred <- predict(no2.lm, new.norm.df=data.frame())

options(scipen=999, digits=3)

new.pred

plot(new.pred)
