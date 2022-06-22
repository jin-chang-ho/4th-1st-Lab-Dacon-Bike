library('tidyverse')
library('lubridate')
setwd("C:/Users/HJK/Desktop/lab/Dacon_Seoul_Bike/HJK/data")
source("../functions.R")
train <- load_df()
test <- load_test()

plot(train$rental, train$temp_highest)

train.2018 <- train[year(train$date)==2018,]

plot(train.2018$rental, train.2018$temp_highest)

cor(train$rental, train$temp_highest)
