
########데이터 load########
bike<-read.csv("D:/단국대학교/Dacon_ddareung/Task1_데이터분석 및 전처리/train_0606.csv")
head(bike)

col_name <- colnames(bike)
col_name[2]


#####boxplot 관측#####
library(ggplot2)
setwd("E:/단국대학교/Dacon_ddareung/boxplot")

png(filename="rental.png")

ggplot(data=bike,aes(y=rental))+
  geom_boxplot(fill="yellow")
        
dev.off()


######데이터 편향 정도 관측######
install.packages("fBasics")
library(fBasics)

ske<-matrix("",nrow=1,ncol=11)
rownames(ske)<-c("편향도")
colnames(ske)<- c("precipitation","temp_mean" ,"temp_highest" , "temp_lowest" ,"PM10" , "PM2.5","humidity","sunshine_sum" ,"sunshine_rate",
                  "wind_mean","wind_max")


for(i in c(1:11)){
  bike<-read.csv("D:/단국대학교/Dacon_ddareung/dataset_0606/train.csv")
  z <- na.omit(bike[i+1])
  ske[1,i]<-skewness(z)  #편향도 측정
  
}

write.csv(ske,"D:/단국대학교/Dacon_ddareung/Task1_데이터분석 및 전처리/data_Skewness.csv")


########회귀분석#########
newdata <- bike[,c(2:13)]
plot(newdata, pch=1, col="blue",
     main="Matrix Scatterplot")

model <- lm(rental~precipitation+temp_mean+temp_highest+temp_lowest+PM10+PM2.5+humidity+sunshine_sum+sunshine_rate+wind_mean+wind_max,
            data=bike)
summary(model)



######미세먼지 농도 범주형 feature 만들기#######
PM10_label <- c()
PM2.5_label <- c()

for(i in c(1:nrow(bike))){
  if(is.na(bike[i,"PM10"])){
    PM10_label[i] <- NA
  }else if(0 <= bike[i,"PM10"] && bike[i,"PM10"] <= 30){
    PM10_label[i] <- 1
  }else if(31 <= bike[i,"PM10"] && bike[i,"PM10"] <= 80){
    PM10_label[i] <- 2
  }else if(81 <= bike[i,"PM10"] && bike[i,"PM10"] <= 150){
    PM10_label[i] <- 3
  }else if(151 <= bike[i,"PM10"]){
    PM10_label[i] <- 4
  }
}

for(i in c(1:nrow(bike))){
  if(is.na(bike[i,"PM2.5"])){
    PM2.5_label[i] <- NA
  }else if(0 <= bike[i,"PM2.5"] && bike[i,"PM2.5"] <= 15){
    PM2.5_label[i] <- 1
  }else if(16 <= bike[i,"PM2.5"] && bike[i,"PM2.5"] <= 35){
    PM2.5_label[i] <- 2
  }else if(36 <= bike[i,"PM2.5"] && bike[i,"PM2.5"] <= 75){
    PM2.5_label[i] <- 3
  }else if(76 <= bike[i,"PM2.5"]){
    PM2.5_label[i] <- 4
  }
}

bike[ , "PM10_label" ] <-PM10_label
bike[ , "PM2.5_label" ] <-PM2.5_label

######강수량 범주형 feature 만들기#######
precipitation_label <- c()

for(i in c(1:nrow(bike))){
  if(is.na(bike[i,"precipitation"])){
    precipitation_label[i] <- NA
  }else if(bike[i,"precipitation"] < 3){
    precipitation_label[i] <- 1
  }else if(3 <= bike[i,"precipitation"] && bike[i,"precipitation"]< 15){
    precipitation_label[i] <- 2
  }else if(15 <= bike[i,"precipitation"] && bike[i,"precipitation"] < 30){
    precipitation_label[i] <- 3
  }else if(bike[i,"precipitation"] >= 30){
    precipitation_label[i] <- 4  
  }
}

length(precipitation_label)

bike[ , "precipitation_label" ] <-precipitation_label

write.csv(bike,"D:/단국대학교/Dacon_ddareung/Task1_데이터분석 및 전처리/train_0606.csv")


######정규화######
# 수치형 데이터만 범주화 (rental 제외)
std <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}

normalize_bike <- std(bike["temp_mean"])



