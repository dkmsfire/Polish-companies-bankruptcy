## packages
library(foreign)

## read arff data
for(i in 1:5){
  assign(paste0("year", i), read.arff(paste0("data/", i, "year.arff")))
}

summary(year1)

## plot each variable to column 1
par(mfrow = c(2,2))
for(i in 1:64){
  plot(y = year1[[i]], x = year1[[1]], col = year1$class)
}
