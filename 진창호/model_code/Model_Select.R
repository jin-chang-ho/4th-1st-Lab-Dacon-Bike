# Working Directory 설정
setwd("C:\\Users\\Windows\\Desktop\\대학교\\연구실\\DACON\\R코드")

# Dataset 불러오기 & 특징확인
dataset <- read.csv("C:\\Users\\Windows\\Desktop\\대학교\\연구실\\DACON\\데이터셋\\train.csv")
str(dataset)
summary(dataset)

# 가장 단순한 전처리(date, precipation 열 날리기 / NA 행 없애기 / 자동 정규화(scale))
dataset_first <- dataset[3:13]
dataset_first <- na.omit(dataset_first) # 74개 데이터가 날아감
str(dataset_first)
summary(dataset_first) 

# 트레이닝 데이터, 시험 데이터 결정
quotient <- nrow(dataset_first) %/% 4
reminder <- nrow(dataset_first) %% 4

train_data <- dataset_first[1:(quotient * 3 + reminder), ]
test_data <- dataset_first[(quotient * 3 + reminder + 1):nrow(dataset_first), 1:10]
test_label <- dataset_first[(quotient * 3 + reminder + 1):nrow(dataset_first), 11]
  
# SVM 모델 생성 & 훈련 (eps-regression이랑 nu-regression은 최적화된 파라미터에서 성능이 비슷하다고 한다.)
# install.packages('kernlab')
library('e1071')

# eps-regression
model1 <- svm(rental ~ ., data = train_data, type = 'eps-regression', kernel = 'linear') # linear
model2 <- svm(rental ~ ., data = train_data, type = 'eps-regression', kernel = 'polynomial') # polynomial
model3 <- svm(rental ~ ., data = train_data, type = 'eps-regression', kernel = 'radial') # radial
model4 <- svm(rental ~ ., data = train_data, type = 'eps-regression', kernel = 'sigmoid') # sigmoid
# nu-regression
model5 <- svm(rental ~ ., data = train_data, type = 'nu-regression', kernel = 'linear') # linear
model6 <- svm(rental ~ ., data = train_data, type = 'nu-regression', kernel = 'polynomial') # polynomial
model7 <- svm(rental ~ ., data = train_data, type = 'nu-regression', kernel = 'radial') # radial
model8 <- svm(rental ~ ., data = train_data, type = 'nu-regression', kernel = 'sigmoid') # sigmoid

# 결과값 예측
result1 <- predict(model1, test_data)
result1 <- round(result1)
result2 <- predict(model2, test_data)
result2 <- round(result2)
result3 <- predict(model3, test_data)
result3 <- round(result3)
result4 <- predict(model4, test_data)
result4 <- round(result4)
result5 <- predict(model5, test_data)
result5 <- round(result5)
result6 <- predict(model6, test_data)
result6 <- round(result6)
result7 <- predict(model7, test_data)
result7 <- round(result7)
result8 <- predict(model8, test_data)
result8 <- round(result8)

# 훈련 결과 확인
mean(abs(result1 - test_label) / result1) # 0.7730902
mean(abs(result2 - test_label) / result2) # 0.8335929
mean(abs(result3 - test_label) / result3) # 0.4908192
mean(abs(result4 - test_label) / result4) # 1.299937
mean(abs(result5 - test_label) / result5) # 0.7424313
mean(abs(result6 - test_label) / result6) # 0.7724751
mean(abs(result7 - test_label) / result7) # 0.6806543
mean(abs(result8 - test_label) / result8) # -0.7693582
# model3가 정확도가 가장 높다.

# 모델 최적화 & 실험
# model3
# 전체 parameter에 대해 실험
tune.svm(rental ~ ., data = train_data, type = 'eps-regression', kernel = 'radial', gamma = seq(0, 2, 0.1), cost = seq(0.5, 1.5, 0.1), epsilon = seq(0.05, 0.15, 0.01))
# gamma (0, 2, 0.1) & cost (0.5, 1.5, 0.1) & epsilon (0.05, 0.15, 0.01)에서 실험 -> gamma : 0.1, cost : 1.5 epsilon : 0.09
# 하지만 값이 더 높아서 경험적으로 실험해봄
set.seed(1234)
opt_eps_model <- svm(rental ~ ., data = train_data, type = 'eps-regression', kernel = 'radial', gamma = '0.1', cost = '1', epsilon = '0.11')
opt_eps_result <- predict(opt_eps_model, test_data)
mean(abs(opt_eps_result - test_label) / opt_eps_result) # 0.13