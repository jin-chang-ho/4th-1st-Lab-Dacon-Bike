# Working Directory 설정
setwd("C:\\Users\\Windows\\Desktop\\대학교\\연구실\\DACON\\R코드")

# train_data, test_data 불러오기
train_data <- read.csv("C:\\Users\\Windows\\Desktop\\대학교\\연구실\\DACON\\데이터셋\\train.csv")
str(train_data)
summary(train_data)

test_data <- read.csv("C:\\Users\\Windows\\Desktop\\대학교\\연구실\\DACON\\데이터셋\\test.csv")
str(test_data)
summary(test_data)

# train_data 가장 단순한 전처리(data, precipation 열 날리기 / NA 행 sunshine_sum 0으로 채우기 / 자동 정규화(scale))
train_data <- dataset[3:13]
train_data <- na.omit(pre_dataset) # 74개 데이터가 날아감
str(train_data)


# test_data 가장 단순한 전처리(data, precipation 열 날리기 / NA 행 sunshine_sum 0으로 채우기 / 자동 정규화(scale)) 
date <- test_data[1]
test_data <- test_data[3:12]
test_data$sunshine_sum[is.na(test_data$sunshine_sum)] <- 0
str(test_data)
summary(test_data)summary(train_data)

# install.packages('kernlab')
library('e1071')

svm(rental ~ ., data = train_data, type = 'eps-regression', kernel = 'radial', gamma = '0.1', cost = '1', epsilon = '0.11')
# model3가 정확도가 가장 높다.

# 결과값 예측
result <- predict(opt_eps_model, test_data)
result <- round(result)

# 제출 데이터 생성
submission <- data.frame(date = date, rental = result)
# write.csv(submission, "1st_submission_jin.csv", row.names = FALSE)

# 0.412