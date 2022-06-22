# Working Directory 설정
setwd("C:\\Users\\Windows\\Desktop\\대학교\\연구실\\DACON\\R코드")

# train_data, test_data 불러오기
train_data <- read.csv("C:\\Users\\Windows\\Desktop\\대학교\\연구실\\DACON\\데이터셋\\train.csv")
str(train_data)
summary(train_data)

test_data <- read.csv("C:\\Users\\Windows\\Desktop\\대학교\\연구실\\DACON\\데이터셋\\test.csv")
str(test_data)
summary(test_data)

# train_data 전처리
train_data[is.na(train_data$sunshine_sum), "sunshine_sum"] <- 0

# test_data 전처리
test_data[is.na(test_data$sunshine_sum), "sunshine_sum"] <- 0

# train_data 시각화 전처리
train_data[is.na(train_data$precipitation), "precipitation"] <- -10
train_data[is.na(train_data$PM10), "PM10"] <- -10
train_data[is.na(train_data$PM2.5), "PM2.5"] <- -10

# test_data 시각화 전처리
test_data[is.na(test_data$precipitation), "precipitation"] <- -10

# train_data 특징 분포 살펴보기
# install.packages('tidyverse')
library('tidyverse')

# train_data 연도별 rental
par(mfrow=c(1,3))
plot(train_data[str_detect(train_data$date, "2018"), ]$rental, type = "l")
sum(train_data[str_detect(train_data$date, "2018"), ]$rental)
mean(train_data[str_detect(train_data$date, "2018"), ]$rental)
plot(train_data[str_detect(train_data$date, "2019"), ]$rental, type = "l")
sum(train_data[str_detect(train_data$date, "2019"), ]$rental)
mean(train_data[str_detect(train_data$date, "2019"), ]$rental)
plot(train_data[str_detect(train_data$date, "2020"), ]$rental, type = "l")
sum(train_data[str_detect(train_data$date, "2020"), ]$rental)
mean(train_data[str_detect(train_data$date, "2020"), ]$rental)
# 연도별 특징 : 사용량이 증가하는 추세를 보인다.

par(mfrow=c(3,4))

# train_data 연도별, 계절별 rental
plot(train_data[str_detect(train_data$date, "2018-03") | str_detect(train_data$date, "2018-04") | str_detect(train_data$date, "2018-05"), ]$rental, type = "l")
plot(train_data[str_detect(train_data$date, "2018-06") | str_detect(train_data$date, "2018-07") | str_detect(train_data$date, "2018-08"), ]$rental, type = "l")
plot(train_data[str_detect(train_data$date, "2018-09") | str_detect(train_data$date, "2018-10") | str_detect(train_data$date, "2018-11"), ]$rental, type = "l")
plot(train_data[str_detect(train_data$date, "2018-12") | str_detect(train_data$date, "2018-01") | str_detect(train_data$date, "2018-02"), ]$rental, type = "l")

plot(train_data[str_detect(train_data$date, "2019-03") | str_detect(train_data$date, "2019-04") | str_detect(train_data$date, "2019-05"), ]$rental, type = "l")
plot(train_data[str_detect(train_data$date, "2019-06") | str_detect(train_data$date, "2019-07") | str_detect(train_data$date, "2019-08"), ]$rental, type = "l")
plot(train_data[str_detect(train_data$date, "2019-09") | str_detect(train_data$date, "2019-10") | str_detect(train_data$date, "2019-11"), ]$rental, type = "l")
plot(train_data[str_detect(train_data$date, "2019-12") | str_detect(train_data$date, "2019-01") | str_detect(train_data$date, "2019-02"), ]$rental, type = "l")

plot(train_data[str_detect(train_data$date, "2020-03") | str_detect(train_data$date, "2020-04") | str_detect(train_data$date, "2020-05"), ]$rental, type = "l")
plot(train_data[str_detect(train_data$date, "2020-06") | str_detect(train_data$date, "2020-07") | str_detect(train_data$date, "2020-08"), ]$rental, type = "l")
plot(train_data[str_detect(train_data$date, "2020-09") | str_detect(train_data$date, "2020-10") | str_detect(train_data$date, "2020-11"), ]$rental, type = "l")
plot(train_data[str_detect(train_data$date, "2020-12") | str_detect(train_data$date, "2020-01") | str_detect(train_data$date, "2020-02"), ]$rental, type = "l")

# 계절별 특징 : 가을 == 여름 (2020년은 가을 > 여름) > 봄 > 겨울 

# PM10, PM2.5 결측값 위치 알아보기
sum(train_data[str_detect(train_data$date, "2018"), ]$PM10 == -10)
sum(train_data[str_detect(train_data$date, "2018"), ]$PM2.5 == -10)
sum(train_data[str_detect(train_data$date, "2019"), ]$PM10 == -10)
sum(train_data[str_detect(train_data$date, "2019"), ]$PM2.5 == -10)
sum(train_data[str_detect(train_data$date, "2020"), ]$PM10 == -10)
sum(train_data[str_detect(train_data$date, "2020"), ]$PM2.5 == -10)

par(mfrow=c(4,3))

# train_data 특징
plot(train_data$precipitation)

# 온도
plot(train_data$temp_mean, type = "l")
plot(train_data$temp_highest, type = "l")
plot(train_data$temp_lowest, type = "l")
plot(train_data$PM10, type = "l")
plot(train_data$PM2.5, type = "l")
plot(train_data$humidity, type = "l")
plot(train_data$sunshine_sum, type = "l")
plot(train_data$sunshine_rate, type = "l")
plot(train_data$wind_mean, type = "l")
plot(train_data$wind_max, type = "l")
plot(train_data$rental, type = "l")

# test_data 특징 살펴보기
plot(test_data$precipitation)
plot(test_data$temp_mean, type = "l")
plot(test_data$temp_highest, type = "l")
plot(test_data$temp_lowest, type = "l")
plot(test_data$PM10, type = "l")
plot(test_data$PM2.5, type = "l")
plot(test_data$humidity, type = "l")
plot(test_data$sunshine_sum, type = "l")
plot(test_data$sunshine_rate, type = "l")
plot(test_data$wind_mean, type = "l")
plot(test_data$wind_max, type = "l")

# correlation 파악하기
cor(train_data[str_detect(train_data$date, "2018"), -1])
cor(train_data[str_detect(train_data$date, "2019"), -1])
cor(train_data[str_detect(train_data$date, "2020"), -1])

number = sample(1:365, 1)
cor(train_data[str_detect(train_data$date, "2018"), c(-1, -13)], test_data[-1])
cor(train_data[str_detect(train_data$date, "2019"), c(-1, -13)], test_data[-number,-1])
train_2020 <- train_data[str_detect(train_data$date, "2020"), c(-1, -13)]
cor(train_2020[-number, ], test_data[-1])