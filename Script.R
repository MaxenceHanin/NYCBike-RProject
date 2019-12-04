library(readr)
library(ggplot2)
library(data.table)
library(scales)
library(plyr)
library(fmsb)

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
nbr_trip_month <- function(data_table,d,m) {
  data_table$stoptime <- m
  data_table$starttime <- d+ as.integer(as.character(substr(data_table$starttime,9,10)))
  data_table$bikeid <- 1
  temp <- data.table(data.frame(day=data_table$starttime,number=data_table$bikeid,month=data_table$stoptime))
  data <- temp[,.(total=sum(number)),by=.(day,month)]
  return(data)
}
data1 <-nbr_trip_month(m01,0,1)
data2 <-nbr_trip_month(m02,31,2)
data3 <-nbr_trip_month(m03,60,3)
data4 <-nbr_trip_month(m04,91,4)
data5 <-nbr_trip_month(m05,121,5)
data6 <-nbr_trip_month(m06,152,6)
data7 <-nbr_trip_month(m07,182,7)
data8 <-nbr_trip_month(m08,213,8)
data9 <-nbr_trip_month(m09,244,9)
data10 <-nbr_trip_month(m10,274,10)
data11 <-nbr_trip_month(m11,305,11)
data12 <-nbr_trip_month(m12,335,12)

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

#user age against trip distance (jan 2018)
age <- 2018-m01$`birth year`[which(m01$gender>0 & m01$`birth year`>1940)]
start_lat <- m01$`start station latitude`[which(m01$gender>0 & m01$`birth year`>1940)]
start_long <- m01$`start station longitude`[which(m01$gender>0 & m01$`birth year`>1940)]
stop_lat <- m01$`end station latitude`[which(m01$gender>0 & m01$`birth year`>1940)]
stop_long <- m01$`end station longitude`[which(m01$gender>0 & m01$`birth year`>1940)]

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
  labs(title = "Distance parcourue en fonction de l'age", y = "Distance moyenne parcourue (Km)") + geom_line()


#trip time against age
time <- m01$tripduration[which(m01$gender>0 & m01$`birth year`>1940)]/60
age_time_temp <- data.table(data.frame(age=age,time=time))
age_time_data <- age_time_temp[,.(time=mean(na.omit(time))),by=age]
full_plot3 <-
  ggplot(age_time_data,
         aes(x = age, y = time)) +
  labs(title = "Durée du trajet en fonction de l'age", y = "Temps de parcours (min)") + geom_line()

#Nombre d'utilisation par tranche d'age (janvier 2018)
age_group <- count(age)
val_max = 600000
under_20 = c(val_max,0,sum(age_group$freq[which(age_group$x<20)]))
between_20_30 = c(val_max,0,sum(age_group$freq[which(age_group$x>20 & age_group$x<30)]))
between_30_40 = c(val_max,0,sum(age_group$freq[which(age_group$x>30 & age_group$x<40)]))
between_40_50 = c(val_max,0,sum(age_group$freq[which(age_group$x>40 & age_group$x<50)]))
between_50_60 = c(val_max,0,sum(age_group$freq[which(age_group$x>50 & age_group$x<60)]))
over_60 = c(val_max,0,sum(age_group$freq[which(age_group$x>60)]))

repartition_age <- cbind(under_20,between_20_30,between_30_40,between_40_50,between_50_60,over_60)
labels = c("-20ans","20-30ans","30-40ans","40-50ans","50-60ans","+60ans")
colnames(repartition_age)<-labels 
repartition_age_dataframe = as.data.frame(repartition_age) ;  

radarchart(repartition_age_dataframe, axistype = 2, seg = 6, axislabcol = 1, plty=1,title = "Nombre d'utilisation par tranche d'âge (janvier 2018)")

# top 10 velos qui ont parcouru le plus de km (aout 2018)
start_lat8 <- m08$`start station latitude`[which(m08$gender>0 & m08$`birth year`>1940)]
start_long8 <- m08$`start station longitude`[which(m08$gender>0 & m08$`birth year`>1940)]
stop_lat8 <- m08$`end station latitude`[which(m08$gender>0 & m08$`birth year`>1940)]
stop_long8 <- m08$`end station longitude`[which(m08$gender>0 & m08$`birth year`>1940)]
distance8 <- manhattan_dist(start_long8,start_lat8,stop_long8,stop_lat8)
veloid <- m08$bikeid [which(m08$gender>0 & m08$`birth year`>1940)]
top_10_temp <- data.table(data.frame(velo=veloid,distance=distance8))
top_10_data <- top_10_temp[,.(distance=sum(na.omit(distance))),by=velo]
top_10_data <- top_10_data[order(-distance)][1:5]
top_10_data$velo = factor(top_10_data$velo, order=TRUE, levels=top_10_data$velo);
full_plot4 <- ggplot(data=top_10_data, aes(x=velo, y=distance)) + geom_histogram(stat='identity')+ labs(title = "Top 5 des vélos ayant parcours la plus grande distance en janvier 2018",
                                                                                                        x = "Id du vélo", y = "Distance parcourue (Km)")
