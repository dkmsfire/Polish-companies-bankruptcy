## packages
library(foreign)

## read arff data
for(i in 1:5){
  assign(paste0("year", i), read.arff(paste0("data/", i, "year.arff")))
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

select = data[,c(7,48,49,20,37,40,46,47,50,4,17,29,57,8,10,53)]
heatmap(select)
## LDA