### packages
library(ggplot2)
library(dplyr)
library(gridExtra)

load("data/bankruptcy_na_knn.rda")
### draw bar chart for each year na and bankruptcy
for(i in 1:5){
  assign(paste0("NA", i), data.frame(class = bankruptcy_na[[i]][[65]], na = bankruptcy_na[[i]][[66]]))
}
NA_list = list(NA1, NA2, NA3, NA4, NA5)

for(i in 1:5){
  assign(paste0("plot", i), ggplot(NA_list[[i]], aes(class, ..count..)) + 
    geom_bar(aes(fill = na), position = "dodge") + ggtitle(paste("Year", i)))
  print(g)
}
NA_all = rbind(NA1, NA2, NA3, NA4, NA5)
plot6 = ggplot(NA_all, aes(class, ..count..)) + geom_bar(aes(fill = na), position = "dodge") + ggtitle("All Year")

png(file = "NAcount.png")
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6)
dev.off()

NAs = NULL
for(i in 1:64){
  NAs[[i]] = sum(is.na(bank_all[,i]))
}
NAs[[65]] = sum(NAs)
