library(readr)
library(dplyr)
library(ggplot2)
library(data.table)
library(ggmap)

setwd("~/Documents/INSA/5A/AnaDesc/NYCBike-RProject/")
data <- list()

# Import datasets and join them in one dataframe

files = c(
  "dataset/2018/201801.csv",
  "dataset/2018/201802.csv",
  "dataset/2018/201803.csv",
  "dataset/2018/201804.csv",
  "dataset/2018/201805.csv",
  "dataset/2018/201806.csv",
  "dataset/2018/201807.csv",
  "dataset/2018/201808.csv",
  "dataset/2018/201809.csv",
  "dataset/2018/201810.csv",
  "dataset/2018/201811.csv",
  "dataset/2018/201812.csv"
)

files = c("dataset/2018/201801.csv")

for (n in 1:1) {
  data[[n]] = read.csv(files[n])
}

data = rbindlist(data)

print(head(data))
print(length(data))

# Les stations les plus fréquentées (en départ, arrivée, les deux)
# start = ddply(data, c("start.station.id"), fun = summarize, start.count = count(start.station.id))
data = data.table(data)
names = distinct(data[, .(
  station.id=start.station.id,
  station.name=start.station.name,
  station.longitude=start.station.longitude,
  station.latitude=start.station.latitude
  )])
start = data[, .(start.count=.N), by= start.station.id]
end = data[, .(end.count=.N), by= end.station.id]
most_freq = merge(names, start, by.x = "station.id", by.y = "start.station.id")
most_freq = merge(most_freq, end, by.x = "station.id", by.y = "end.station.id")

# Plot it!
nymap = get_map(center=c(-74.0, 40.7), maptype="terrain")
ggmap(nymap) + geom_point(aes(x = station.longitude, y = station.latitude, cex=start.count), data=most_freq) + xlim(-74.05, -73.9) + ylim(40.63, 40.82)

# Afflux et variation de la proportion de vélos sur une station
