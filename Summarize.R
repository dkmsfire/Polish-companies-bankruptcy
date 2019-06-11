## packages
library(foreign)

## read arff data
for(i in 1:5){
  assign(paste0("year", i), read.arff(paste0("data/", i, "year.arff")))
}
bankruptcy = list(year1, year2, year3, year4, year5)
bank_all = rbind(year1, year2, year3, year4, year5)
### sum na
for(i in 1:5){
  for(j in 1:nrow(bankruptcy[[i]])){
    bankruptcy[[i]]$na[[j]] = sum(is.na(bankruptcy[[i]][j,]))
  }
}
bankruptcy_na_number = bankruptcy
save(bankruptcy_na_number, file = "bankruptcy_na_number.rda")
## na detector
load("data/bankruptcy_na_knn.rda")

for(i in 1:5){
  print(paste("Year", i))
  print(table(bankruptcy_na[[i]][65:66]))
}

# one year attribute NA number
NAs = matrix(data = NA, nrow = 65, ncol = 5)
colnames(NAs) = c("Year1", "Year2", "Year3", "Year4", "Year5")
for(i in 1:5){
  for(j in 1:64){
    NAs[j,i] = sum(is.na(bankruptcy[[i]][,j]))
  }
  NAs[65,i] = sum(NAs[1:64,i])
}

# 5 years attribute NA number
NAs = NULL
for(i in 1:64){
  NAs[[i]] = sum(is.na(bank_all[,i]))
}
NAs[[65]] = sum(NAs)

## text mining with NAs
detector = read.csv("attribute_detector.csv")
for(y in 1:5){
  detector = read.csv("attribute_detector.csv")
  detector$NAs = NAs[,y]
  
  for(i in 1:64){
    for(j in 2:37){
      if(detector[i,j] == 1){
        detector[i,j] = detector[i,39]
      }
    }
  }
  
  for(j in 2:37){
    detector[65,j] = sum(detector[,j])
  }
  
  Raw_attribute = names(detector)[2:37]
  Count = NULL
  for(j in 2:37){
    Count[(j-1)] = detector[65,j]
  }
  
  assign(paste0("Year", y, "NA_detector"), data.frame(Raw_Attrubute = names(detector)[2:37], 
                              Count = Count))
}

NA_dectector = Year1NA_detector
names(NA_dectector) = c("Unique_Term", "Year1")
NA_dectector$Year2 = Year2NA_detector[[2]]
NA_dectector$Year3 = Year3NA_detector[[2]]
NA_dectector$Year4 = Year4NA_detector[[2]]
NA_dectector$Year5 = Year5NA_detector[[2]]
write.csv(NA_dectector, file = "NA.dectector.csv", row.names = FALSE)

detector$NAs = NAs[,1]

for(i in 1:64){
  for(j in 2:37){
    if(detector[i,j] == 1){
      detector[i,j] = detector[i,39]
    }
  }
}
for(j in 2:37){
  detector[65,j] = sum(detector[,j])
}
Raw_attribute = names(detector)[2:37]
Count = NULL
for(j in 2:37){
  Count[(j-1)] = detector[65,j]
}
detector_count = data.frame(Raw_Attrubute = names(detector)[2:37], 
                            Count = Count)

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


ggplot(data = bankruptcy_knn[[1]], aes(x = bankruptcy_knn[[1]][[65]], y = bankruptcy_knn[[1]][[1]])) +
  geom_boxplot() + xlab("Bankruptcy") + ylab(paste0("Attribute", 1))
                                             
## boxplot to display each year.
library(ggplot2)
library(gridExtra)
load("data/bankruptcy_knn.rda")

## don't know why using ggplot2 the plot is NULL
for(i in 1:5){
  for(j in 1:64){
    assign(paste0("Attribute", j), ggplot(data = bankruptcy_knn[[i]], aes(x = bankruptcy_knn[[i]][[65]], y = bankruptcy_knn[[i]][[j]]))) +
      geom_boxplot() + xlab("Bankruptcy") + ylab(paste0("Attribute", j))
  }
  png(file = paste0("Year", i, ".png"), width = 1440, height = 1440)
  grid.arrange(Attribute1, Attribute2, Attribute3, Attribute4, Attribute5, Attribute6, Attribute7, Attribute8,
               Attribute9, Attribute10, Attribute11, Attribute12, Attribute13, Attribute14, Attribute15, Attribute16,
               Attribute17, Attribute18, Attribute19, Attribute20, Attribute21, Attribute22, Attribute23, Attribute24,
               Attribute25, Attribute26, Attribute27, Attribute28, Attribute29, Attribute30, Attribute31, Attribute32,
               Attribute33, Attribute34, Attribute35, Attribute36, Attribute37, Attribute38, Attribute39, Attribute40,
               Attribute41, Attribute42, Attribute43, Attribute44, Attribute45, Attribute46, Attribute47, Attribute48,
               Attribute49, Attribute50, Attribute51, Attribute52, Attribute53, Attribute54, Attribute55, Attribute56,
               Attribute57, Attribute58, Attribute59, Attribute60, Attribute61, Attribute62, Attribute63, Attribute64)
  dev.off()
}

for(i in 1:5){
  png(file = paste0("Year", i, ".png"), width = 1440, height = 1440)
  par(mfrow = c(8,8))
  for(j in 1:64){
    boxplot(bankruptcy_knn[[i]][[j]] ~ bankruptcy_knn[[i]][[65]], ylab = paste0("Attribute", j), xlab = "Bankruptcy")
  }
  dev.off()
}

## each attribute and combining 5 years for 0 or 1
load("data/bankruptcy_all_knn.rda")
for(i in 1:5){
  for(b in 0:1){
    data = bankruptcy_all_knn[which(bankruptcy_all_knn[[65]] == b),]
    png(file = paste0("All_Year_Bankruptcy=", b, ".png"), width = 1440, height = 1440)
    par(mfrow = c(8,8))
    for(j in 1:64){
      boxplot(data[[j]] ~ data[[66]], ylab = paste0("Attribute", j), xlab = "Year")
    }
    dev.off()
  }
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
library(gridExtra)
load("data/bankruptcy_knn.rda")
for(i in 1:5){
  pca = prcomp(bankruptcy_knn[[i]][1:64], scale = TRUE)
  assign(paste0("Year", i), ggbiplot(pca, obs.scale = 1, var.scale = 1, groups = bankruptcy_knn[[i]][[65]], ellipse = TRUE, varname.size = 0, xlab = "PC1", ylab = "PC2", main = paste0("Year", i)))
  
}
png(file = "PCA.png")
grid.arrange(Year1, Year2, Year3, Year4, Year5)
dev.off()


#plot ggbiplot
ggbiplot(pca, obs.scale = 1, var.scale = 1, groups = year1[[65]], ellipse = TRUE, circle = TRUE)

## Correlation Matrix
library(corrplot)

load("data/bankruptcy.rda")
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

### PCA
library(ggbiplot)
load("data/bankruptcy_knn.rda")
for(i in 1:5){
  bankruptcy.pca = prcomp(bankruptcy_knn[[i]][1:64])
  assign(paste0("PCA", i), ggbiplot(bankruptcy.pca, obs.scale = 1, var.scale = 1, groups = bankruptcy_knn[[i]][[65]], ellipse = TRUE, circle = TRUE, varname.size = 0) + ggtitle(paste0("PCA for Year", i)))
}
png(file = "PCA.png", height = 800, width = 800)
grid.arrange(PCA1, PCA2, PCA3, PCA4, PCA5)
dev.off()
