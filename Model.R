### In this unbalanced data , we should notice that 
### if we guess that all of banks will not become bankruptcy
### we can get very high accuracy.
### So , in this classfier question , 
### we need to use sensitivity and specificity to check our model.
library(foreign)
for(i in 1:5){
  assign(paste0("year", i), read.arff(paste0("data/", i, "year.arff")))
}
bankruptcy = list(year1, year2, year3, year4, year5)
names(bankruptcy) = c("year1", "year2", "year3", "year4", "year5")
save(bankruptcy, file = "bankruptcy.rda")

for(j in 1:5){
  bankruptcy[[j]]$year = factor(j)
}
bankruptcy_all = bankruptcy$year1
for(k in 2:5){
  bankruptcy_all = rbind(bankruptcy_all, bankruptcy[[k]])
}
save(bankruptcy_all, file = "bankruptcy_all.rda")

bankruptcy_all_knn = rbind(year1_knn, year2_knn, year3_knn, year4_knn, year5_knn)
save(bankruptcy_all_knn, file = "bankruptcy_all_knn.rda")
### knn for na imputation
require(DMwR)
load("bankruptcy.rda")
for(i in 1:5){
  assign(paste0("year", i, "_knn"), knnImputation(bankruptcy[[i]]))
}
any(is.na(year1_knn))
bankruptcy_knn = list(year1_knn, year2_knn, year3_knn, year4_knn, year5_knn)
names(bankruptcy_knn) = c("year1_knn", "year2_knn", "year3_knn", "year4_knn", "year5_knn")
save(bankruptcy_knn, file = "bankruptcy_knn.rda")

### smote sampling for unbalanced data

### corrplot for dimension reduction
bankruptcy_knn = list(year1_knn, year2_knn, year3_knn, year4_knn, year5_knn)
library(corrplot)

for(i in 1:5){
  res = cor(bankruptcy_knn[[i]][1:64], method = "pearson", use = "complete.obs")
  corrplot(res, type = "upper", tl.col = "black", tl.srt = 45, order = "hclust")
}

### random forest
library(randomForest)
library(caret)
load("bankruptcy_knn.rda")
load("bankruptcy_na_knn.rda")
bankruptcy_knn = bankruptcy_na
feature = matrix(0L, nrow = 3, ncol = 5)
colnames(feature) = c("year1", "year2", "year3", "year4", "year5")
rownames(feature) = c("Accuracy", "Sensitivity", "Specificity")
for(i in 1:5){
  set.seed(999)
  trainid = sample(1:nrow(bankruptcy_knn[[i]]), 0.8 * nrow(bankruptcy_knn[[i]]))
  train = bankruptcy_knn[[i]][trainid,]
  test = bankruptcy_knn[[i]][-trainid,]
  model = randomForest(class ~ ., data = train, importance = TRUE, ntree = 10)
  pred_test = predict(model, test, type = "response")
  value = c(mean(pred_test == test$class), specificity(pred_test, test$class), sensitivity(pred_test, test$class))
  for(j in 1:3){
    feature[j,i] = value[[j]]
  }
  assign(paste0("matrix_year", i), confusionMatrix(pred_test, test$class))
  assign(paste0("importance_year", i), importance(model))
}

### if we rbind year1 to year5, can we predict the data come from which year preciously.
load("bankruptcy_all.rda")

## knn for na imputation
library(randomForest)
load("bankruptcy_all_knn.rda")
trainid = sample(1:nrow(bankruptcy_all_knn), 0.8 * nrow(bankruptcy_all_knn))
train = bankruptcy_all_knn[trainid, c(1:64, 66)]
test = bankruptcy_all_knn[-trainid, c(1:64, 66)]
model = randomForest(year ~ ., data = train)

pred_test = predict(model, test, type = "response")
mean(pred_test == test$year)


### logistic regression without split
library(ggplot2)

load("data/bankruptcy_na_knn.rda")
model = glm(class ~., data = bankruptcy_na[[1]], family=binomial(link="logit"))
summary(model)

ggplot(bankruptcy_na[[1]], aes(x = Attr2, y = class)) + geom_point(alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = binomial), se = FALSE) + 
  ylab("Bankruptcy") + ggtitle("Bankruptcy")


### decision tree
library(rpart)
library(partykit)
library(gridExtra)
load("data/bankruptcy_na_number.rda")
model = rpart(class ~ ., data = bankruptcy_na_number[[1]])
tree = as.party(model)
plot(tree)

for(i in 1:5){
  model = rpart(class ~ ., data = bankruptcy_na_number[[i]])
  png(file = paste0("png/Decision Tree", i, ".png"), height = 1440, width = 1440)
  rpart.plot(model, box.palette="RdBu", shadow.col="gray", nn=TRUE)
  dev.off()
}

for(i in 1:5){
  model = rpart(class ~ ., data = bankruptcy_na_number[[i]])
  png(file = paste0("png/Decision Tree", i, ".png"), height = 1440, width = 1440)
  tree = as.party(model)
  plot(tree, main = paste("Year", i))
  dev.off()
}
