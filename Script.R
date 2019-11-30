library(readr)
library(ggplot2)
library(data.table)
X201806 <- read_csv("~/INSA/5e année/NYCBike-RProject/dataset/2018/201806-citibike-tripdata.csv")
X201806$starttime <- strtrim(X201806$starttime, 10)
X201806$bikeid[X201806$bikeid>0] <- 1

temp1 <- data.frame(day=X201806$starttime,number=X201806$bikeid)
data1 <- data.table(temp1)
data1[,list(total=sum(number)),by=day]

plot1 <- ggplot(data1, aes(x=day))
plot1 <- plot1 + geom_histogram(binwidth=.5, colour="blue", fill="blue")

#View(X201806)