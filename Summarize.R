## packages
library(foreign)

## read arff data
for(i in 1:5){
  assign(paste0("year", i), read.arff(paste0("data/", i, "year.arff")))
}
bankruptcy = list(year1, year2, year3, year4, year5)

## na imputation by mean
for(i in 1:5){
  for(j in 1:ncol(bankruptcy[[i]])){
    bankruptcy[[i]][,j][is.na(bankruptcy[[i]][,j])] = mean(bankruptcy[[i]][,j], na.rm = TRUE)
  }
}

for(i in 1:5){
  print(any(is.na(bankruptcy[[i]])))
}

for(i in 1:ncol(year1)) {
  year1[ , i][is.na(year1[ , i])] <- mean(year1[ , i], na.rm = TRUE)
}

## plot each variable to column 1
par(mfrow = c(2,2))
for(i in 1:64){
  plot(y = year1[[i]], x = year1[[1]], col = year1$class)
}

## heat map
for(i in 1:64){
  year1[[i]] = as.numeric(as.character(year1[[i]]))
}
year1 = as.matrix(year1)
summary(year1)
data = year1[,-65]
data = as.matrix(data)
heatmap(data)

select = year1[,c(7,48,49,20,37,40,46,47,50,4,17,29,57,8,10,53)]
heatmap(select)

## LDA need to figure out na , how to do the imputation
library(MASS)
train_index = sample(1:7027, 5600)
model = lda(class ~ ., data = year1, na.action="na.omit", CV=TRUE)
predict(model, test)
test = select[-train_index,-17]

## PCA
library(ggbiplot)
pca = prcomp(select, scale = TRUE)
plot(pca, type = "line")
abline(h = 1, col = "blue")

#plot ggbiplot
ggbiplot(pca, obs.scale = 1, var.scale = 1, groups = year1[[65]], ellipse = TRUE, circle = TRUE)

## Correlation Matrix
library(corrplot)

for(i in 1:5){
  res = cor(bankruptcy[[i]][1:64], method = "pearson", use = "complete.obs")
  corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
}


## 