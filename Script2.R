library(readr)
library(dplyr)
library(ggplot2)
library(data.table)
library(ggmap)
library(lubridate)
library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
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
most_freq$frequentation = most_freq$start.count + most_freq$end.count

# Plot it!
nymap = get_map(location=c(-74.05, 40.63, -73.9, 40.82), maptype="terrain", zoom = 13)
ggmap(nymap) +
  geom_point(aes(x = station.longitude, y = station.latitude, cex=frequentation), col = "#a31829", bg = "#fa7080", pch=21, data=most_freq) +
  xlim(-74.05, -73.9) + ylim(40.63, 40.82)

# Nombre de trajets en fonction de l'heure de la journée / le jour de la semaine
data$startdate = parse_date_time(data[,"starttime"][[1]], "%Y-%m-%d %H:%M:%OS")
data$enddate = parse_date_time(data[,"stoptime"][[1]], "%Y-%m-%d %H:%M:%OS")
data$day = weekdays(data$startdate)
data$hour = hour(data$startdate)

by_day = data[, .(day_count=.N), by= day]
by_day$day = ordered(by_day$day, levels=c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"))

ggplot(by_day, aes(x = day, y = day_count)) + geom_bar(stat="identity") + ylab("trip count")

by_hour = data[, .(hour_count=.N), by= hour]
ggplot(by_hour, aes(x = hour, y = hour_count)) + geom_line() + ylab("trip count")

# Afflux et variation de la proportion de vélos sur une station
station = most_freq[order(-start.count)][1]$station.id
input = data[end.station.id == station]
input$value = 1
input$date = input$startdate
output = data[start.station.id == station]
output$value = -1
output$date = output$enddate

trips = rbind(input, output)[order(date)]
trips$cumul = cumsum(trips$value)
trips$cumul = trips$cumul - min(trips$cumul)
capacity = max(trips$cumul)

ggplot(trips, aes(x = date, y = trips$cumul)) + geom_line()
