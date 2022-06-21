library(lubridate)
library(dplyr)
library(xgboost)
library(cvTools)

# XgBoost를 사용하기 위한 실험
setwd("C:/Users/HJK/Desktop/lab/Dacon_Seoul_Bike/HJK/data")
source("../functions.R")
df <- load_df()
test <- load_test()

df$year <- year(df$date)
df$month <- month(df$date)
df$wday <- wday(df$date)
df[,14:19] <- lapply(df[,14:19], as.numeric)

test$year <- year(test$date)
test$month <- month(test$date)
test$wday <- wday(test$date)
test[,13:18] <- lapply(test[,13:18], as.numeric)
str(test)

submit <- read.csv('sample_submission.csv')
str(submit)
submit$rental <- pred
write.csv(submit, 'sample_submission3.csv', row.names=F)


# 계절별 분포
point <- ifelse(df$season.Spring == 1, 1, 
                ifelse(df$season.Summer == 1, 2, 
                       ifelse(df$season.Fall == 1, 3, 4)))
color <- c("red","green","blue","yellow")
plot(df[, 2:13],
     main="plot",
     pch=c(point),
     col=color[point])

# 8가지 조합 실험
df.raw <- read.csv("train.csv")
df.raw <- df.raw[complete.cases(df.raw$sunshine_sum),]
na.idx <- is.na(df.raw$PM10) | is.na(df.raw$PM2.5)
tmp <- df.raw[!na.idx, ]
df.raw$date <- as.Date(df.raw$date)
tmp$date <- as.Date(tmp$date)
tmp <- tmp[which(tmp$date %in% df$date),]
tmp[which(tmp$date %in% df$date), c("weekday.Weekday","weekday.Holiday","season.Spring","season.Summer","season.Fall","season.Winter")] <- df[which(tmp$date %in% df$date), 14:19]
tmp[which(tmp$date %in% df$date), 2] <- df[which(tmp$date %in% df$date), 2]
tmp[, 14:19] <- lapply(tmp[, 14:19], as.numeric)
tmp[, 14:19] <- tmp[, 14:19] - 1


# dataset 1: 미세먼지와 초미세먼지 NA 행 제거, 강수량 포함해서 예측
tr1 <- tmp %>%
  select(-rental, -date)
cl1 <- tmp$rental

# dataset 2: 미세먼지는 예측 값 사용, 초미세먼지 열 제거, 강수량 포함해서 예측
tr2 <- df %>%
  select(-rental, -date, -PM2.5)
cl2 <- df$rental

# dataset 3: 초미세먼지는 예측 값 사용, 미세먼지 열 제거, 강수량 포함해서 예측
tr3 <- df %>%
  select(-rental, -date, -PM10)
cl3 <- df$rental

# dataset 4: 미세먼지와 초미세먼지 모두 예측 값 사용, 강수량 포함 예측
tr4 <- df %>%
  select(-rental, -date)
cl4 <- df$rental

# dataset 5: 미세먼지와 초미세먼지 NA 행 제거, 강수량 미포함 예측
tr5 <- tmp %>%
  select(-rental, -date, -precipitation)
cl5 <- tmp$rental

# dataset 6: 미세먼지 예측, 초미세먼지 제거, 강수량 미포함
tr6 <- df %>%
  select(-rental, -date, -PM2.5, -precipitation)
cl6 <- df$rental

# dataset 7: 초미세먼지 예측, 미세먼지 제거, 강수량 미포함
tr7 <- df %>%
  select(-rental, -date, -PM10, -precipitation)
cl7 <- df$rental

# dataset 8: 미세먼지와 초미세먼지 예측, 강수량 미포함
tr8 <- df %>%
  select(-rental, -date, -precipitation)
cl8 <- df$rental

# 결과 확인
res <- c()
for (i in 1:8) {
  tr <- get(paste0("tr", i))
  cl <- get(paste0("cl", i))
  fold <- cvFolds(nrow(tr), K=5)
  res[i] <- kfold('xgb',tr, cl, fold)
}

for(i in 1:8) {
  cat("case", i, "\t:\t",res[i],"\n")
}
