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
text = "X1	net profit / total assets 
X2	total liabilities / total assets 
X3	working capital / total assets 
X4	current assets / short-term liabilities 
X5	[(cash + short-term securities + receivables - short-term liabilities) / (operating expenses - depreciation)] * 365 
X6	retained earnings / total assets 
X7	EBIT / total assets 
X8	book value of equity / total liabilities 
X9	sales / total assets 
X10	equity / total assets 
X11	(gross profit + extraordinary items + financial expenses) / total assets 
X12	gross profit / short-term liabilities 
X13	(gross profit + depreciation) / sales 
X14	(gross profit + interest) / total assets 
X15	(total liabilities * 365) / (gross profit + depreciation) 
X16	(gross profit + depreciation) / total liabilities 
X17	total assets / total liabilities 
X18	gross profit / total assets 
X19	gross profit / sales 
X20	(inventory * 365) / sales 
X21	sales (n) / sales (n-1) 
X22	profit on operating activities / total assets 
X23	net profit / sales 
X24	gross profit (in 3 years) / total assets 
X25	(equity - share capital) / total assets 
X26	(net profit + depreciation) / total liabilities 
X27	profit on operating activities / financial expenses 
X28	working capital / fixed assets 
X29	logarithm of total assets 
X30	(total liabilities - cash) / sales 
X31	(gross profit + interest) / sales 
X32	(current liabilities * 365) / cost of products sold 
X33	operating expenses / short-term liabilities 
X34	operating expenses / total liabilities 
X35	profit on sales / total assets 
X36	total sales / total assets 
X37	(current assets - inventories) / long-term liabilities 
X38	constant capital / total assets 
X39	profit on sales / sales 
X40	(current assets - inventory - receivables) / short-term liabilities 
X41	total liabilities / ((profit on operating activities + depreciation) * (12/365)) 
X42	profit on operating activities / sales 
X43	rotation receivables + inventory turnover in days 
X44	(receivables * 365) / sales 
X45	net profit / inventory 
X46	(current assets - inventory) / short-term liabilities 
X47	(inventory * 365) / cost of products sold 
X48	EBITDA (profit on operating activities - depreciation) / total assets 
X49	EBITDA (profit on operating activities - depreciation) / sales 
X50	current assets / total liabilities 
X51	short-term liabilities / total assets 
X52	(short-term liabilities * 365) / cost of products sold) 
X53	equity / fixed assets 
X54	constant capital / fixed assets 
X55	working capital 
X56	(sales - cost of products sold) / sales 
X57	(current assets - inventory - short-term liabilities) / (sales - gross profit - depreciation) 
X58	total costs /total sales 
X59	long-term liabilities / equity 
X60	sales / inventory 
X61	sales / receivables 
X62	(short-term liabilities *365) / sales 
X63	sales / short-term liabilities 
X64	sales / fixed assets"
