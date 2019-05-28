### In this unbalanced data , we should notice that 
### if we guess that all of banks will not become bankruptcy
### we can get very high accuracy.
### So , in this classfier question , 
### we need to use sensitivity and specificity to check our model.
library(foreign)

### knn for na imputation
require(DMwR)
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

### smote sampling for unbalanced data

### corrplot for dimension reduction

### random forest

### if we rbind year1 to year5, can we predict the data come from which year preciously.