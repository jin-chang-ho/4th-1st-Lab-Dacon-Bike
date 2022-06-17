# Working Directory 설정
setwd("C:\\Users\\Windows\\Desktop\\대학교\\연구실\\DACON\\R코드")

# train_data, test_data 불러오기
train_data <- read.csv("C:\\Users\\Windows\\Desktop\\대학교\\연구실\\DACON\\데이터셋\\train.csv")
str(train_data)
summary(train_data)

test_data <- read.csv("C:\\Users\\Windows\\Desktop\\대학교\\연구실\\DACON\\데이터셋\\test.csv")
str(test_data)
summary(test_data)

# train_data sunshine_sum 전처리하기 (sunshine_rate가 0이면 sunshine_sum을 0으로 처리함 - 일리있음)
sunshine_dataset <- train_data
sunshine_dataset[is.na(sunshine_dataset$sunshine_sum), 'sunshine_sum'] <- 0
summary(sunshine_dataset)

# train_data precipitation 전처리하기 (humidity로 예상해보기)
# install.packages('kernlab')
library('e1071')

precipitation_check_dataset <- sunshine_dataset[!is.na(sunshine_dataset$precipitation), ]
cor.test(precipitation_check_dataset$precipitation, precipitation_check_dataset$temp_mean)
cor.test(precipitation_check_dataset$precipitation, precipitation_check_dataset$humidity)
cor.test(precipitation_check_dataset$precipitation, precipitation_check_dataset$sunshine_rate) # 평균 온도 : 0.24, 습도 : 0.57, 일조율 : -0.38

precipitation_hum <- sunshine_dataset$humidity
precipitation_hum <- round(precipitation_hum, 1)
precipitation_hum <- as.data.frame(precipitation_hum)
colnames(precipitation_hum) <- c('humidity')
model_svm <- svm(precipitation ~ humidity, data = sunshine_dataset, type = 'eps-regression', kernel = 'radial')
summary(model_svm)
check_data <- predict(model_svm, precipitation_hum)
cor.test(sunshine_dataset$precipitation, check_data) # 0.79

humid_data <- as.data.frame(sunshine_dataset[is.na(sunshine_dataset$precipitation), ]$humidity)
colnames(humid_data) <- c('humidity')
predict_data <- predict(model_svm, humid_data)
predict_data <- round(predict_data, 1)

precipitation_dataset <- sunshine_dataset
precipitation_dataset[is.na(sunshine_dataset$precipitation), 'precipitation'] <- predict_data
summary(precipitation_dataset)

# train_data PM10, PM2.5 전처리하기 (연관성 있는 데이터가 없어서 제거해버림)
cor.test(precipitation_dataset$PM10, precipitation_dataset$humidity) # -0.21
cor.test(precipitation_dataset$PM10, precipitation_dataset$precipitation) # -0.22
cor.test(precipitation_dataset$PM2.5, precipitation_dataset$humidity) # -0.07
cor.test(precipitation_dataset$PM2.5, precipitation_dataset$precipitation) # -0.18

train_data <- precipitation_dataset
train_data <- train_data[!is.na(precipitation_dataset$PM10), ]
train_data <- train_data[!is.na(precipitation_dataset$PM2.5), ]
summary(train_data)

# train_data feature 뽑기
# 1번 : 강우량, 평균 온도, 미세먼지, 초미세먼지, 습도, 일조율, 평균 바람, 대여량
train_data <- train_data[, c(2, 3, 6, 7, 8, 10, 11, 13)]

# train_data 출력
# write.csv(train_data, "1st_train_jin.csv", row.names = FALSE)

# test_data feature 뽑기
date <- test_data[1]
test_data <- test_data[, c(2, 3, 6, 7, 8, 10, 11)]

# test_data precipitation 전처리하기 (train precipitation 전처리에 사용한 모델로 예측하기)
humid_data <- as.data.frame(test_data[is.na(test_data$precipitation), ]$humidity)
colnames(humid_data) <- c('humidity')
predict_data <- predict(model_svm, humid_data)
predict_data <- round(predict_data, 1)
test_data[is.na(test_data$precipitation), ] <- predict_data
str(test_data)
summary(test_data)

# test_data 출력
# write.csv(test_data, "1st_test_jin.csv", row.names = FALSE)

# install.packages('kernlab')
library('e1071')

svm(rental ~ ., data = train_data, type = 'eps-regression', kernel = 'radial')
# model3가 정확도가 가장 높다.

# 결과값 예측
result <- predict(opt_eps_model, test_data)
result <- round(result)

# 제출 데이터 생성
submission <- data.frame(date = date, rental = result)
# write.csv(submission, "1st_submission_jin.csv", row.names = FALSE)

# 0.610