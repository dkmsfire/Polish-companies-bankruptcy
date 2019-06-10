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

## chi-square 
for(i in 1:5){
  assign(paste0("NA", i), table(bankruptcy_na[[i]][65:66]))
}
NA_list = list(NA1, NA2, NA3, NA4, NA5)

for(i in 1:5){
  print(chisq.test(NA_list[[i]]))
}
## count na
NAs = NULL
for(i in 1:64){
  NAs[[i]] = sum(is.na(bank_all[,i]))
}
NAs[[65]] = sum(NAs)


### na with number to show the bankruptcy company's missing value numbers
load("data/bankruptcy_na_number.rda")
for(i in 1:5){
  assign(paste0("NumberofCompany", i), ggplot(data = bankruptcy_na_number[[i]], aes(x = 1:nrow(bankruptcy_na_number[[i]]), y = na, fill = class, color = class)) + 
    geom_point(alpha = 0.5))
}
png(file = "png/NA of Company.png")
grid.arrange(NumberofCompany1, NumberofCompany2, NumberofCompany3, NumberofCompany4, NumberofCompany5)
dev.off()

g = ggplot(data = bankruptcy_na_number[[1]], aes(x = 1:nrow(bankruptcy_na_number[[1]]), y = na, fill = class, color = class)) + 
  geom_point(alpha = 0.5)
print(g)
