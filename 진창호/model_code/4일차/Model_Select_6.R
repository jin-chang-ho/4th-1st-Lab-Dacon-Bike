# Working Directory 설정
setwd("C:\\Users\\Windows\\Desktop\\대학교\\연구실\\DACON\\R코드")

# train_data, test_data 불러오기
train_data <- read.csv("C:\\Users\\Windows\\Desktop\\대학교\\연구실\\DACON\\데이터셋\\train.csv")
str(train_data)
summary(train_data)

test_data <- read.csv("C:\\Users\\Windows\\Desktop\\대학교\\연구실\\DACON\\데이터셋\\test.csv")
str(test_data)
summary(test_data)

# 평가 함수 정의
NMAE <- function(actual, predicted) {
  return(mean(abs(actual - predicted)/actual))
}

# train_data 기본 전처리(date 전처리 / NA 행 sunshine_sum 0으로 채우기)
library(lubridate)

train_data[1] <- as.Date(train_data[,1])
train_data[is.na(train_data$sunshine_sum), 'sunshine_sum'] <- 0
str(train_data)
summary(train_data)

# install.packages('kernlab')
library(e1071)

# 1. 미세먼지, 초미세먼지 행만 없애기, 강우량 예측
train_data1 <- train_data[!is.na(train_data$PM10), ]

train_data1 <- train_data1[!is.na(train_data1$PM2.5), ]

precipitation_train_data <- train_data1[!is.na(train_data1$precipitation), ]
precipitation_model <- svm(precipitation ~ ., data = precipitation_train_data)
precipitation_pre_data <- train_data1[is.na(train_data1$precipitation), -2]
precipitation_predict <- predict(precipitation_model, precipitation_pre_data)
precipitation_predict[precipitation_predict < 0] <- 0
precipitation_predict <- round(precipitation_predict, 1)
train_data1[names(precipitation_predict), "precipitation"] <- precipitation_predict

# train_data 출력
# write.csv(train_data1, "1st_train_jin.csv", row.names = FALSE)

# test_data 기본 전처리(date 전처리 / NA 행 sunshine_sum 0으로 채우기)
date <- test_data[1]
test_data[1] <- as.Date(test_data[,1])
test_data[is.na(test_data$sunshine_sum), 'sunshine_sum'] <- 0

# 1. 미세먼지, 초미세먼지 행만 없애기, 강우량 예측
test_data1 <- test_data

# train으로 사용한 모델 사용
precipitation_train_data <- train_data1[!is.na(train_data1$precipitation), -ncol(train_data1)]
precipitation_model <- svm(precipitation ~ ., data = precipitation_train_data)

precipitation_pre_data <- test_data1[is.na(test_data1$precipitation), -2]
precipitation_predict <- predict(precipitation_model, precipitation_pre_data)
precipitation_predict[precipitation_predict < 0] <- 0
precipitation_predict <- round(precipitation_predict, 1)
test_data1[names(precipitation_predict), "precipitation"] <- precipitation_predict

# test_data 출력
# write.csv(test_data1, "1st_test_jin.csv", row.names = FALSE)

# 최적화된 모델 생성
opt_eps_model <- svm(rental ~ ., data = train_data1, type = 'eps-regression', kernel = 'radial')

# 결과값 예측
result <- predict(opt_eps_model, test_data1)
result <- round(result)

# 제출 데이터 생성
submission <- data.frame(date = date, rental = result)
# write.csv(submission, "3th_submission_jin.csv", row.names = FALSE)
