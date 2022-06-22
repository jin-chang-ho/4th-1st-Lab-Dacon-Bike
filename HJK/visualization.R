library('tidyverse')
source("../functions.R")
train.raw <- read.csv("train.csv")
train <- read.csv("real_data.csv")

df <- train.raw[!is.na(train.raw$precipitation), 1:2]
for (i in 1:nrow(df)) {
  temp <- df[i, 1]
  train[which(temp==train$date), 2] <- df[i, 2]
}
head(train.raw, 10)
head(train, 10)

tr <- load_df()
tr$year <- year(tr$date)
tr$month <- month(tr$date)
tr$wday <- wday(tr$date)
tr[,14:19] <- lapply(tr[,14:19], as.numeric)
ts <- load_test()

ts$year <- year(ts$date)
ts$month <- month(ts$date)
ts$wday <- wday(ts$date)
ts[,13:18] <- lapply(ts[,13:18], as.numeric)

tr1 <- tr %>%
  select(-rental, -date)
cl1 <- tr$rental
ts1 <- ts %>%
  select(-date)
pred <- XGB.real_test(tr1, ts1, cl1)

submit <- read.csv("sample_submission.csv")
submit$rental <- pred
write.csv(submit, "sample_submission5.csv", row.names=F)
