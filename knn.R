###packages
library(foreign)
library(ISLR)
library(class)
library(ggplot2)

###read arff data
for(i in 1:5){
  assign(paste0("year", i), read.arff(paste0("data/", i, "year.arff")))
}

###try knn for separate bankrupcy for year1
any(is.na(year1))
#true na will be a challange for research this data
summary(year1)
#all of these variables are numeric so at first I use mean imputaton 
for(i in 1:ncol(year1)) {
  year1[ , i][is.na(year1[ , i])] <- mean(year1[ , i], na.rm = TRUE)
}
##try knn
#standardized
var(year1[,1])
bankruptcy <- year1[,65]
standardized.year1 <- scale(year1[,-65])
var(standardized.year1[,1])

#把前1000橫排作為測試資料
test.index <- sample(1:nrow(year1), 1000)
test.data <- standardized.year1[test.index,]
test.bankruptcy <- bankruptcy[test.index]

#剩餘則作為訓練組
train.data <- standardized.year1[-test.index,]
train.bankruptcy <- bankruptcy[-test.index]

#knn about k = 3 and get very hign accuracy like 0.95
predict.bankruptcy = knn(train.data, test.data, train.bankruptcy, k = 3)
mean(predict.bankruptcy == test.bankruptcy)

#find best k by Elbow method maybe take 5
predicted.bankruptcy = NULL
error.rate = NULL

for(i in 1:20){
  set.seed(101)
  predicted.bankruptcy = knn(train.data,test.data,train.bankruptcy,k=i)
  error.rate[i] = mean(test.bankruptcy != predicted.bankruptcy)
}

k.values <- 1:20
error.df <- data.frame(error.rate,k.values)
error.df
ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()+ geom_line(lty="dotted",color='red')
