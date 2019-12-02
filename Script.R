library(readr)
library(ggplot2)
library(data.table)
library(scales)

#import the 12 dataset corresponding to each month of 2018
m01 <- read_csv("~/INSA/5e année/NYCBike-RProject-perso/dataset/2018/201801.csv")
m02 <- read_csv("~/INSA/5e année/NYCBike-RProject-perso/dataset/2018/201802.csv")
m03 <- read_csv("~/INSA/5e année/NYCBike-RProject-perso/dataset/2018/201803.csv")
m04 <- read_csv("~/INSA/5e année/NYCBike-RProject-perso/dataset/2018/201804.csv")
m05 <- read_csv("~/INSA/5e année/NYCBike-RProject-perso/dataset/2018/201805.csv")
m06 <- read_csv("~/INSA/5e année/NYCBike-RProject-perso/dataset/2018/201806.csv")
m07 <- read_csv("~/INSA/5e année/NYCBike-RProject-perso/dataset/2018/201807.csv")
m08 <- read_csv("~/INSA/5e année/NYCBike-RProject-perso/dataset/2018/201808.csv")
m09 <- read_csv("~/INSA/5e année/NYCBike-RProject-perso/dataset/2018/201809.csv")
m10 <- read_csv("~/INSA/5e année/NYCBike-RProject-perso/dataset/2018/201810.csv")
m11 <- read_csv("~/INSA/5e année/NYCBike-RProject-perso/dataset/2018/201811.csv")
m12 <- read_csv("~/INSA/5e année/NYCBike-RProject-perso/dataset/2018/201812.csv")

#For each month, keep only the number of rented bike per day
m01$stoptime <- 1
m01$starttime <- as.integer(as.character(substr(m01$starttime,9,10)))
m01$bikeid <- 1
temp1 <- data.table(data.frame(day=m01$starttime,number=m01$bikeid,month=m01$stoptime))
data1 <- temp1[,.(total=sum(number)),by=.(day,month)]

m02$stoptime <- 2
m02$starttime <- 31 + as.integer(as.character(substr(m02$starttime,9,10)))
m02$bikeid <- 1
temp2 <- data.table(data.frame(day=m02$starttime,number=m02$bikeid,month=m02$stoptime))
data2 <- temp2[,.(total=sum(number)),by=.(day,month)]

m03$stoptime <- 3
m03$starttime <- 60 + as.integer(as.character(substr(m03$starttime,9,10)))
m03$bikeid <- 1
temp3 <- data.table(data.frame(day=m03$starttime,number=m03$bikeid,month=m03$stoptime))
data3 <- temp3[,.(total=sum(number)),by=.(day,month)]

m04$stoptime <- 4
m04$starttime <- 91 + as.integer(as.character(substr(m04$starttime,9,10)))
m04$bikeid <- 1
temp4 <- data.table(data.frame(day=m04$starttime,number=m04$bikeid,month=m04$stoptime))
data4 <- temp4[,.(total=sum(number)),by=.(day,month)]

m05$stoptime <- 5
m05$starttime <- 121 + as.integer(as.character(substr(m05$starttime,9,10)))
m05$bikeid <- 1
temp5 <- data.table(data.frame(day=m05$starttime,number=m05$bikeid,month=m05$stoptime))
data5 <- temp5[,.(total=sum(number)),by=.(day,month)]

m06$stoptime <- 6
m06$starttime <- 152 + as.integer(as.character(substr(m06$starttime,9,10)))
m06$bikeid <- 1
temp6 <- data.table(data.frame(day=m06$starttime,number=m06$bikeid,month=m06$stoptime))
data6 <- temp6[,.(total=sum(number)),by=.(day,month)]

m07$stoptime <- 7
m07$starttime <- 182 + as.integer(as.character(substr(m07$starttime,9,10)))
m07$bikeid <- 1
temp7 <- data.table(data.frame(day=m07$starttime,number=m07$bikeid,month=m07$stoptime))
data7 <- temp7[,.(total=sum(number)),by=.(day,month)]

m08$stoptime <- 8
m08$starttime <- 213 + as.integer(as.character(substr(m08$starttime,9,10)))
m08$bikeid <- 1
temp8 <- data.table(data.frame(day=m08$starttime,number=m08$bikeid,month=m08$stoptime))
data8 <- temp8[,.(total=sum(number)),by=.(day,month)]

m09$stoptime <- 9
m09$starttime <- 244 + as.integer(as.character(substr(m09$starttime,9,10)))
m09$bikeid <- 1
temp9 <- data.table(data.frame(day=m09$starttime,number=m09$bikeid,month=m09$stoptime))
data9 <- temp9[,.(total=sum(number)),by=.(day,month)]

m10$stoptime <- 10
m10$starttime <- 274 + as.integer(as.character(substr(m10$starttime,9,10)))
m10$bikeid <- 1
temp10 <- data.table(data.frame(day=m10$starttime,number=m10$bikeid,month=m10$stoptime))
data10 <- temp10[,.(total=sum(number)),by=.(day,month)]

m11$stoptime <- 11
m11$starttime <- 305 + as.integer(as.character(substr(m11$starttime,9,10)))
m11$bikeid <- 1
temp11 <- data.table(data.frame(day=m11$starttime,number=m11$bikeid,month=m11$stoptime))
data11 <- temp11[,.(total=sum(number)),by=.(day,month)]

m12$stoptime <- 12
m12$starttime <- 335 + as.integer(as.character(substr(m12$starttime,9,10)))
m12$bikeid <- 1
temp12 <- data.table(data.frame(day=m12$starttime,number=m12$bikeid,month=m12$stoptime))
data12 <- temp12[,.(total=sum(number)),by=.(day,month)]

#concatenate the 12 tables
full_data <- rbind(data1,data2,data3,data4, data5, data6, data7, data8, data9, data10, data11, data12)

#draw the plot
#plot1 <- ggplot(data1, aes(x=day, y=total))+geom_ribbon(aes(ymin=0, ymax=data1$total), fill="blue", col="blue", alpha=0.5)
full_plot <-
  ggplot(full_data, aes(x = day, y = total)) + geom_ribbon(
    aes(ymin = 0, ymax = full_data$total),
    fill = "blue",
    col = "blue",
    alpha = 0.5
  ) + scale_x_discrete(
    name = "Months",
    limits = c(15, 46, 75, 106, 136, 167, 197, 228, 259, 289, 320, 350),
    labels =
      c(
        "15" = "Jan.",
        "46" = "Feb.",
        "75" = "Mar.",
        "106" = "Apr.",
        "136" = "May",
        "167" = "June",
        "197" = "July",
        "228" = "Aug.",
        "259" = "Sept.",
        "289" = "Oct.",
        "320" = "Nov.",
        "350" = "Dec."
      )
  ) + labs(title = "Number of NYC-bikes trip in 2018",
           x = "Month", y = "Number of trip")

#-----------------------------------------------------------------------------

#user age against trip distance
age <- 2018-m01$`birth year`[which(m01$gender>0 & m01$`birth year`>1940)]
start_lat <- m01$`start station latitude`[which(m01$gender>0 & m01$`birth year`>1940)]
start_long <- m01$`start station longitude`[which(m01$gender>0 & m01$`birth year`>1940)]
stop_lat <- m01$`end station latitude`[which(m01$gender>0 & m01$`birth year`>1940)]
stop_long <- m01$`end station longitude`[which(m01$gender>0 & m01$`birth year`>1940)]
time <- m01$tripduration[which(m01$gender>0 & m01$`birth year`>1940)]

deg2rad <- function(deg) return(deg*pi/180)
manhattan_dist <- function(long1, lat1, long2, lat2) {
  # Convert degrees to radians
  long1<-deg2rad (long1)
  lat1<-deg2rad(lat1)
  long2<-deg2rad(long2)
  lat2<-deg2rad(lat2)
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}
distance <- manhattan_dist(start_long,start_lat,stop_long,stop_lat)

age_dist_temp <- data.table(data.frame(age=age,distance=distance))
age_dist_data <- age_dist_temp[,.(distance_moyenne=mean(na.omit(distance))),by=age]

#draw the plot
#plot1 <- ggplot(data1, aes(x=day, y=total))+geom_ribbon(aes(ymin=0, ymax=data1$total), fill="blue", col="blue", alpha=0.5)
full_plot2 <-
  ggplot(age_dist_data,
         aes(x = age, y = distance_moyenne)) +
  labs(title = "Distance parcourue en fonction de l'age", y = "Distance moyenne parcoure (Km)") + geom_line()