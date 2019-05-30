## packages
library(foreign)

## read arff data
for(i in 1:5){
  assign(paste0("year", i), read.arff(paste0("data/", i, "year.arff")))
}
bankruptcy = list(year1, year2, year3, year4, year5)

## na detector
load("data/bankruptcy_na_knn.rda")

for(i in 1:5){
  print(paste("Year", i))
  print(table(bankruptcy_na[[i]][65:66]))
}

## missing value visualization
library(tidyverse)

### just run the code, the ggplot won't show.
for(i in 1:5){
  missing.values <- bankruptcy[[i]] %>%
    gather(key = "key", value = "val") %>%
    mutate(is.missing = is.na(val)) %>%
    group_by(key, is.missing) %>%
    summarise(num.missing = n()) %>%
    filter(is.missing==T) %>%
    select(-is.missing) %>%
    arrange(desc(num.missing)) 
  
  missing.values %>%
    ggplot() +
    geom_bar(aes(x=key, y=num.missing), stat = 'identity') +
    labs(x='variable', y="number of missing values", title='Number of missing values') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}



missing.values <- year1 %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)


levels <-
  (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x =
         'Variable', y = "% of missing values")

percentage.plot

## na detection and want to add a variable to record if else na exists.
load("bankruptcy.rda")
for(i in 1:5){
  for(j in 1:nrow(bankruptcy[[i]])){
    bankruptcy[[i]]$na[j] = any(is.na(bankruptcy[[i]][j,]))
  }
}

bankruptcy_na = bankruptcy
for(i in 1:5){
  bankruptcy_na[[i]] = knnImputation(bankruptcy_na[[i]])
}

save(bankruptcy_na, file = "bankruptcy_na_knn.rda")

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
load("bankruptcy_knn.rda")
par(mfrow = c(4,4))
for(i in 1:5){
  for(j in 1:64){
    plot(bankruptcy_knn[[i]][[j]], col = bankruptcy_knn[[i]][[65]], xlab = "", ylab = paste("Attribute", j), main = paste("Year", i, "Attribute", j))
  }
}

## boxplot to display each year.
load("data/bankruptcy_all_knn.rda")
par(mfrow = c(4,4))
for(j in 1:64){
  boxplot(bankruptcy_all_knn[[j]] ~ bankruptcy_all_knn[[66]], col = factor(bankruptcy_all_knn[[66]]), main = paste0("Attribute", j), ylab = paste0("Attribute", j))
}

bankruptcy_all_knn_yes = bankruptcy_all_knn[which(bankruptcy_all_knn$class == 1),]
for(j in 1:64){
  boxplot(bankruptcy_all_knn_yes[[j]] ~ bankruptcy_all_knn_yes[[66]], col = factor(bankruptcy_all_knn_yes[[66]]), main = paste0("Attribute", j), ylab = paste0("Attribute", j))
}

bankruptcy_all_knn_no = bankruptcy_all_knn[which(bankruptcy_all_knn$class == 0),]
for(j in 1:64){
  boxplot(bankruptcy_all_knn_no[[j]] ~ bankruptcy_all_knn_no[[66]], col = factor(bankruptcy_all_knn_no[[66]]), main = paste0("Attribute", j), ylab = paste0("Attribute", j))
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
  corrplot(res, type = "upper", tl.col = "black", tl.srt = 45, order = "hclust")
}

## MDS 2d
load("data/bankruptcy_knn.rda")

for(i in 1:5){
  dist = dist(bankruptcy_knn[[i]][1:64])
  fit = cmdscale(dist, eig = TRUE, k = 2)
  x = fit$points[,1]
  y = fit$points[,2]
  plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", main = paste("Metric MDS Year", i), type = "n", col=as.integer(bankruptcy_knn[[i]]$class))
  text(x, y, labels = row.names(bankruptcy_knn[[i]]), cex = 0.7)
}


## MDS 3d


## MDS 3d
library(rgl)
#Caclulate Dist. 
data.dist <- dist(bankruptcy_knn[[1]][1:64]) 

#Calculate MDS 
data.mds <- cmdscale(data.dist, k=3) 

#Create x,y refs 
data.x <- data.mds[,1] 
data.y <- data.mds[,2] 
data.z <- data.mds[,3] 

#Plot 
plot3d(data.x, data.y, data.z, col=as.integer(bankruptcy_knn[[1]]$class)) 
### 3d plot the data only are on y-z axis, x = 0


### try to figure out the combing attributes.
text = "net profit/total assets/total liabilities/total assets/working capital/total assets/current assets/short-term liabilities/cash/short-term securities/receivables/short-term liabilities/operating expenses/depreciation/retained earnings/total assets/EBIT/total assets/book value of equity/total liabilities/sales/total assets/equity/total assets/gross profit/extraordinary items/financial expenses/total assets/gross profit/short-term liabilities/gross profit/depreciation/sales/gross profit/interest/total assets/total liabilities/gross profit/depreciation/gross profit/depreciation/total liabilities/total assets/total liabilities/gross profit/total assets/gross profit/sales/inventory/sales/sales/sales/profit on operating activities/total assets/net profit/sales/gross profit/total assets/equity/share capital/total assets/net profit/depreciation/total liabilities/profit on operating activities/financial expenses/working capital/fixed assets/logarithm of total assets/total liabilities/cash/sales/gross profit/interest/sales/current liabilities/cost of products sold/operating expenses/short-term liabilities/operating expenses/total liabilities/profit on sales/total assets/total sales/total assets/current assets/inventories/long-term liabilities/constant capital/total assets/profit on sales/sales/current assets/inventory/receivables/short-term liabilities/total liabilities/profit on operating activities/depreciation/profit on operating activities/sales/rotation receivables/inventory turnover in days/receivables/sales/net profit/inventory/current assets/inventory/short-term liabilities/inventory/cost of products sold/EBITDA/depreciation/total assets/EBITDA/depreciation/sales/current assets/total liabilities/short-term liabilities/total assets/short-term liabilities/cost of products sold/equity/fixed assets/constant capital/fixed assets/working capital/sales/cost of products sold/sales/current assets/inventory/short-term liabilities/sales/gross profit/depreciation/total costs/total sales/long-term liabilities/equity/sales/inventory/sales/receivables/short-term liabilities/sales/sales/short-term liabilities/sales/fixed assets"

library(tm)

text = gsub("\t", "", text)
text = gsub("\n", "", text)
text_split = strsplit(text, "/")
unique(text_split[[1]])
unique_term = data.frame(term = unique(text_split[[1]]), count = 0)

for(j in 1:length(unique(text_split[[1]]))){
  for(i in 1:length(text_split[[1]])){
    if(text_split[[1]][[i]] == unique(text_split[[1]])[[j]]){
      unique_term$count[[j]] = unique_term$count[[j]] + 1
      print(c(i,j))
    }
  }
}
