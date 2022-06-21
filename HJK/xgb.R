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
  set.seed(100)
  fold <- cvFolds(nrow(tr), K=5)
  res[i] <- kfold('xgb',tr, cl, fold)
}

for(i in 1:8) {
  cat("case", i, "\t:\t",res[i],"\n")
}



# 계절 별 예측 시도
ds.spring <- df[which(df$season.Spring == 2),]
ds.summer <- df[which(df$season.Summer == 2),]
ds.fall <- df[which(df$season.Fall == 2), ]
ds.winter <- df[which(df$season.Winter == 2), ]

set.seed(100)
tr.spr <- sample(1:nrow(ds.spring), nrow(ds.spring)*3/4, replace=F)
set.seed(100)
tr.sum <- sample(1:nrow(ds.summer), nrow(ds.summer)*3/4, replace=F)
set.seed(100)
tr.fall <- sample(1:nrow(ds.fall), nrow(ds.fall)*3/4, replace=F)
set.seed(100)
tr.win <- sample(1:nrow(ds.winter), nrow(ds.winter)*3/4, replace=F)

se.tr1 <- ds.spring[tr.spr,] %>%
  select(-rental, -date)
se.cl1 <- ds.spring[tr.spr, ]$rental
se.ts1 <- ds.spring[-tr.spr,] %>%
  select(-rental, -date)
se.ts.cl1 <- ds.spring[-tr.spr,]$rental

se.tr2 <- ds.summer[tr.sum,] %>%
  select(-rental, -date)
se.cl2 <- ds.summer[tr.sum, ]$rental
se.ts2 <- ds.summer[-tr.sum,] %>%
  select(-rental, -date)
se.ts.cl2 <- ds.summer[-tr.sum,]$rental

se.tr3 <- ds.fall[tr.fall,] %>%
  select(-rental, -date)
se.cl3 <- ds.fall[tr.fall, ]$rental
se.ts3 <- ds.fall[-tr.fall,] %>%
  select(-rental, -date)
se.ts.cl3 <- ds.fall[-tr.fall,]$rental

se.tr4 <- ds.winter[tr.win,] %>%
  select(-rental, -date)
se.cl4 <- ds.winter[tr.win, ]$rental
se.ts4 <- ds.winter[-tr.win,] %>%
  select(-rental, -date)
se.ts.cl4 <- ds.winter[-tr.win,]$rental

result <- c()
result[1] <- XgBoost(se.tr1, se.ts1, se.cl1, se.ts.cl1)
result[2] <- XgBoost(se.tr2, se.ts2, se.cl2, se.ts.cl2)
result[3] <- XgBoost(se.tr3, se.ts3, se.cl3, se.ts.cl3)
result[4] <- XgBoost(se.tr4, se.ts4, se.cl4, se.ts.cl4)

season <- c("Spring","Summer","Fall","Winter")
for(i in 1:4) {
  cat(season[i], "\t:\t",result[i],"\n")
}
#봄에는 성능이 좋지 않지만 여름과 가을은 많이 향상된 모습을 보임
#Spring 	:	 0.3150763 
#Summer 	:	 0.1273231 
#Fall 	:	 0.1210844 
#Winter 	:	 0.1798596
