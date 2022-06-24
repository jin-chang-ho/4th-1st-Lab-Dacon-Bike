# Working Directory 설정
setwd("C:\\Users\\Windows\\Desktop\\대학교\\연구실\\DACON\\R코드")

# 사용할 library
library(MASS)
library(e1071)
library(cvTools)
library(caret)
library(lubridate)
library(tidyverse)

# 평가함수
NMAE <- function(actual, predicted) {
  return(mean(abs(actual - predicted)/actual))
}

# train data 불러오기
df.raw <- read.csv("C:\\Users\\Windows\\Desktop\\대학교\\연구실\\DACON\\데이터셋\\train.csv")

# train data 전처리하기
# date 전처리
df.raw[,1] <- as.Date(df.raw[,1])

# train weekday 전처리
df.raw$weekday <- weekdays(df.raw$date)
df.raw$weekday <- factor(df.raw$weekday %in% c('토요일','일요일'), labels=c('Weekday','Holiday'))

# train season 전처리
df.raw$season <- month(df.raw$date)
df.raw$season <- ifelse(df.raw$season %in% c(3, 4, 5), "Spring",
                          ifelse(df.raw$season %in% c(6, 7, 8), "Summer",
                                 ifelse(df.raw$season %in% c(9, 10, 11), "Fall", "Winter")))
df.raw$season <- factor(df.raw$season, levels=c("Spring","Summer","Fall","Winter"))

# train sunshine_sum 전처리
df.raw <- df.raw[!is.na(df.raw$sunshine_sum),]

# 훈련시킬 new.df 만들기 (season, sum 이진화)
dummy <- dummyVars("~.", data=df.raw)
new.df <- data.frame(predict(dummy, newdata=df.raw))
new.df[,14:19] <- lapply(new.df[,14:19], as.factor)

# svm으로 PM10 예측
df.complete <- new.df[complete.cases(df.raw),]
ts.idx <- sample(1:nrow(df.complete), size=round(nrow(df.complete) / 4), replace=F)
PM10.tr <- df.complete[-ts.idx, -c(2, 7)]

PM10.model <- svm(PM10 ~ ., data=PM10.tr, type='nu-regression', kernel='radial')
PM10.na <- new.df[is.na(new.df$PM10), -c(2, 6, 7)]
new.df[is.na(new.df$PM10), 6] <- round(predict(PM10.model, PM10.na), 1)

# svm으로 PM2.5 예측
df.complete <- new.df[complete.cases(new.df),]
ts.idx <- sample(1:nrow(df.complete), size=round(nrow(df.complete)/4), replace=F)
PM2.tr <- df.complete[-ts.idx, -2]

PM2.model <- svm(PM2.5~., data=PM2.tr, type='eps-regression', kernel='linear')
PM2.na <- new.df[is.na(new.df$PM2.5), -c(2, 7)]
new.df[is.na(new.df$PM2.5), 7] <- round(predict(PM2.model, PM2.na), 1)

# svm으로 강수량 예측
df.complete <- new.df[complete.cases(new.df),]
ts.idx <- sample(1:nrow(df.complete), size=round(nrow(df.complete)/4), replace=F)
prec.tr <- df.complete[-ts.idx, ]

prec.model <- svm(precipitation~., data=prec.tr, type='eps-regression', kernel='polynomial')
prec.na <- new.df[is.na(new.df$precipitation), -2]
new.df[is.na(new.df$precipitation), 2] <- round(predict(prec.model, prec.na), 1)
new.df[new.df$precipitation < 0,2] <- 0

# test data 불러오기
test.raw <- read.csv("C:\\Users\\Windows\\Desktop\\대학교\\연구실\\DACON\\데이터셋\\test.csv")

# test data 전처리하기
# date 정보 저장
date = test.raw[,1]

# test date 전처리
test.raw[,1] <- as.Date(test.raw[,1])

# test weekday 전처리
test.raw$weekday <- weekdays(test.raw$date)
test.raw$weekday <- factor(test.raw$weekday %in% c('토요일','일요일'), labels=c('Weekday','Holiday'))

# test season 전처리
test.raw$season <- month(test.raw$date)
test.raw$season <- ifelse(test.raw$season %in% c(3, 4, 5), "Spring",
                          ifelse(test.raw$season %in% c(6, 7, 8), "Summer",
                                 ifelse(test.raw$season %in% c(9, 10, 11), "Fall", "Winter")))
test.raw$season <- factor(test.raw$season, levels=c("Spring","Summer","Fall","Winter"))

# test sunshine_sum 전처리
test.raw[is.na(test.raw$sunshine_sum), 9] <- mean(test.raw$sunshine_sum, na.rm=T)


# test.raw season, sum 이진화
dummy <- dummyVars("~.", data=test.raw)
test.raw <- data.frame(predict(dummy, newdata=test.raw))
test.raw[,13:18] <- lapply(test.raw[,13:18], as.factor)

# svm으로 강수량 예측
prec.tr <- new.df[, -13]
prec.model <- svm(precipitation ~ ., data=prec.tr, type='eps-regression', kernel='polynomial')
test.raw.na <- test.raw[is.na(test.raw$precipitation), -2]
test.raw[is.na(test.raw$precipitation), 2] <- round(predict(prec.model, test.raw.na), 1)
test.raw[test.raw$precipitation < 0, 2] <- 0

# 모델 생성과 예측
train <- new.df[,]
test <- test.raw

model <- svm(rental ~ ., data=train, type='nu-regression', kernel='radial')
pred <- round(predict(model, test))

# 제출 파일 생성
submission <- data.frame(date = date, rental = pred)
write.csv(submission, "1st_submission_jin.csv", row.names = FALSE)
