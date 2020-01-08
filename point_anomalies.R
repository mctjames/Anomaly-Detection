#import the needed libraries
library("depmixS4")
library(lubridate)
library(ggplot2)
library(dplyr)
library(gridExtra)

#format(as.POSIXct(strptime(df$DateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")


#import helper functions
source("proj_helper_functions.R")

train_data <- data_with_days("TrainData.txt")
test_data <- data_with_days("test5.txt")

# ------------- WINDOW 1: Monday Mornings --------------

# get data in our window: data, start time, end time, start day, end day
monday_morning <- get_window(train_data, "06:00:00", "11:59:00", 2, 2)
test_monday_morning <- get_window(test_data, "06:00:00", "11:59:00", 2, 2)


######################################################
# scatter plot of monday_mornings Global_active_power
######################################################

# monday_mornings stats
avg_morning <- aggregate(cbind(Global_active_power, Global_reactive_power, Voltage,Global_intensity, Sub_metering_1, Sub_metering_2, Sub_metering_3)~Time, monday_morning, mean, na.action = na.omit )
min_morning <- aggregate(cbind(Global_active_power,Global_reactive_power,Voltage,Global_intensity,Sub_metering_1,Sub_metering_2,Sub_metering_3)~Time,monday_morning, min, na.action = na.omit )
max_morning <- aggregate(cbind(Global_active_power,Global_reactive_power,Voltage,Global_intensity,Sub_metering_1,Sub_metering_2,Sub_metering_3)~Time,monday_morning, max, na.action = na.omit )
sd_morning <- aggregate(cbind(Global_active_power,Global_reactive_power,Voltage,Global_intensity,Sub_metering_1,Sub_metering_2,Sub_metering_3)~Time,monday_morning, sd, na.action = na.omit )

avg_morning$max <- max_morning$Global_active_power
avg_morning$min <- min_morning$Global_active_power
avg_morning$sd <- sd_morning$Global_active_power

test_avg_morning <- aggregate(cbind(Global_active_power, Global_reactive_power, Voltage,Global_intensity, Sub_metering_1, Sub_metering_2, Sub_metering_3)~Time, test_monday_morning, mean, na.action = na.omit )
test_min_morning <- aggregate(cbind(Global_active_power,Global_reactive_power,Voltage,Global_intensity,Sub_metering_1,Sub_metering_2,Sub_metering_3)~Time,test_monday_morning, min, na.action = na.omit )
test_max_morning <- aggregate(cbind(Global_active_power,Global_reactive_power,Voltage,Global_intensity,Sub_metering_1,Sub_metering_2,Sub_metering_3)~Time,test_monday_morning, max, na.action = na.omit )
test_sd_morning <- aggregate(cbind(Global_active_power,Global_reactive_power,Voltage,Global_intensity,Sub_metering_1,Sub_metering_2,Sub_metering_3)~Time,test_monday_morning, sd, na.action = na.omit )

test_avg_morning$max <- test_max_morning$Global_active_power
test_avg_morning$min <- test_min_morning$Global_active_power
test_avg_morning$sd <- test_sd_morning$Global_active_power


## POINT ANOMALIES
test_monday_morning$in_range <- apply(test_monday_morning, 1, point_anomaly, avg_data=avg_morning, var_col=3)
morning_point_anomalies <- filter(test_monday_morning, test_monday_morning$in_range == FALSE)
morning_valid <- filter(test_monday_morning, test_monday_morning$in_range == TRUE)

# scatter plot of monday_mornings Global_active_power point anomalies
plt_all_anomalies <- ggplot(data = morning_point_anomalies, aes(x=Time, y=Global_active_power)) + 
  geom_point(colour = "firebrick2") +
  ylim(c(-3, 10)) +
  labs(title = "Monday Morning Global Active Power Point Anomalies")

# plot mean/min/max of a monday morning from training data

plt_normal_avg_m <- ggplot(data = avg_morning, aes(x=Time, y=Global_active_power)) + 
  geom_pointrange(mapping = aes(ymin=min, ymax=max, color='training data')) +
  ylim(c(-3, 10)) +
  scale_x_datetime(breaks = "1 hour", minor_breaks = "10 min", date_labels = "%H:%M") +
  labs(title = "Mean/Min/Max Monday Mornings Global Active Power - Training Data", color="") +
  xlab("Time (Morning 6:00:00 - 11:59:00)") +
  scale_colour_manual(values = c('orange1'))
plt_normal_avg_m

# plot mean/min/max test Global_active_power
plt_test_avg <- ggplot(data = test_avg_morning, aes(x=Time, y=Global_active_power)) + 
  geom_pointrange(mapping = aes(ymin=min, ymax=max, , color="test data")) +
  ylim(c(-3, 10)) +
  labs(color="", title = "Mean/Min/Max Monday Mornings Global Active Power - Test Data") +
  xlab("Time (Morning 6:00:00 - 11:59:00)") +
  scale_colour_manual(values = c('dodgerblue3'))
  
plt_test_avg


plt_morning_test  <- ggplot(data = test_monday_morning, aes(x=DateTime, y=Global_active_power)) + 
  geom_point(aes(color = "normal")) +
  labs(color = "", title = "Monday Morning Global Active Power - Test Data") +
  scale_colour_manual(values=c("dodgerblue3"))

plt_morning_test

plt_test_results <-
  plt_morning_test +
  geom_point(data = morning_point_anomalies, aes(x=DateTime, y=Global_active_power, color = "anomaly")) +
  xlab("Date")  +
  labs(color = "") +
  scale_colour_manual(values = c('firebrick2', 'dodgerblue3'))
plt_test_results

# plot normal monday and all anomalies
# plt_morning_normal + geom_point(data = morning_point_anomalies, aes(x=Time, y=Global_active_power), colour = "firebrick2")


# get march 8th anomalies

march_8_all_m <- filter(test_monday_morning, test_monday_morning$Date == "2010-03-08")
march_8_anomalies_m <- filter(morning_point_anomalies, morning_point_anomalies$Date == "2010-03-08")

a <-
  plt_normal_avg_m +
  geom_point(data = march_8_all_m, aes(x=Time, y=Global_active_power, color = "normal")) +
  labs(color = "") +
  scale_colour_manual(values = c('dodgerblue3', 'orange1'))
a
    
plt_march_8_example_m <-
  a +
  geom_point(data = march_8_anomalies_m, aes(x=Time, y=Global_active_power, color = "anomaly")) +
  labs(color = "") +
  scale_colour_manual(values = c('firebrick2', 'dodgerblue3', 'orange1')) +
  labs(color = "", title="March 8, 2010 Point Anomalies")
plt_march_8_example_m


march_8_anomalies_m <- find_clusters(march_8_anomalies_m)
march_8_cluster <- filter(march_8_anomalies_m, march_8_anomalies_m$Cluster == TRUE)
march_8_noise <- filter(march_8_anomalies_m, march_8_anomalies_m$Cluster == FALSE)


plt_march_8_filtered <-
  a +
  geom_point(data = march_8_cluster, aes(x=Time, y=Global_active_power, color = "anomaly")) +
  labs(color = "") +
  scale_colour_manual(values = c('firebrick2', 'orange1', 'dodgerblue3'))

plt_march_8_filtered <-
  plt_march_8_filtered + 
  geom_point(data = march_8_noise, aes(x=Time, y=Global_active_power, color = "noise")) +
  labs(color = "", title="March 8 Filtered Point Anomalies") +
  scale_colour_manual(values = c('firebrick2', 'springgreen3', 'dodgerblue3', 'orange1'))
plt_march_8_filtered


# get april 12 anomalies
april_12_all_m <- filter(test_monday_morning, test_monday_morning$Date == "2010-04-12")
april_12_anomalies_m <- filter(morning_point_anomalies, morning_point_anomalies$Date == "2010-04-12")

b <-
  plt_normal_avg_m +
  geom_point(data = april_12_all_m, aes(x=Time, y=Global_active_power, color = "normal")) +
  labs(color = "") +
  scale_colour_manual(values = c('orange1', 'dodgerblue3'))
b
plt_april_12_example_m <-
  b +
  geom_point(data = april_12_anomalies_m, aes(x=Time, y=Global_active_power, color = "anomaly")) +
  labs(color = "") +
  scale_colour_manual(values = c('firebrick2', 'dodgerblue3', 'orange1')) +
  labs(color = "", title="April 12 Point Anomalies")
plt_april_12_example_m

april_12_anomalies_m <- find_clusters(april_12_anomalies_m)
april_12_cluster <- filter(april_12_anomalies_m, april_12_anomalies_m$Cluster == TRUE)
april_12_noise <- filter(april_12_anomalies_m, april_12_anomalies_m$Cluster == FALSE)


plt_april_12_filtered <-
  b +
  geom_point(data = april_12_cluster, aes(x=Time, y=Global_active_power, color = "anomaly")) +
  labs(color = "") +
  scale_colour_manual(values = c('firebrick2', 'orange1', 'dodgerblue3'))

plt_april_12_filtered <-
  plt_april_12_filtered + 
  geom_point(data = april_12_noise, aes(x=Time, y=Global_active_power, color = "noise")) +
  labs(color = "", title="April 12 Filtered Point Anomalies") +
  scale_colour_manual(values = c('firebrick2', 'springgreen3', 'dodgerblue3', 'orange1'))
plt_april_12_filtered







# plot standard deviation - train
plt1 <-
  ggplot(data = avg_morning, aes(x=Time, y=Global_active_power)) + 
  geom_pointrange(mapping = aes(ymin=Global_active_power-sd, ymax=Global_active_power+sd, color="training data")) +
  xlab("Time (Morning 6:00:00 - 11:59:00)") +
  scale_colour_manual(values = c('orange1')) +
  ylim(c(-1, 11)) +
  labs(color="", title = "Mean/SD Monday Mornings Global Active Power - Training Data")
plt1

# plot standard deviation - test
plt2 <-
  ggplot(data = test_avg_morning, aes(x=Time, y=Global_active_power)) + 
  geom_pointrange(mapping = aes(ymin=Global_active_power-sd, ymax=Global_active_power+sd, color="test data")) +
  xlab("Time (Morning 6:00:00 - 11:59:00)") +
  scale_colour_manual(values = c('dodgerblue3')) +
  ylim(c(-1, 11)) +
  labs(color="", title = "Mean/SD Monday Mornings Global Active Power - Test Data")
plt2

######################################################
#                 Moving Averages
######################################################

  avg_morning$index <- 1:nrow(avg_morning)
  loess_moving <- loess(Global_active_power~index, data=avg_morning, span=0.05)
  pred <- predict(loess_moving, avg_morning, se=TRUE)
  avg_morning$lwl <- replicate(360,0)
  avg_morning$upl <- pred$fit+3

smoothed <- moving_avg <- ggplot(data = avg_morning, aes(x=Time, y=Global_active_power)) + 
  geom_point(aes(color = "mean"))+
  geom_smooth(method = 'loess', span=0.05, aes(color = "moving average")) +
  scale_colour_manual(values = c('orange1', 'slateblue2')) +
  labs(title = "Moving Average Global Active Power")
smoothed

moving_avg <- ggplot(data = avg_morning, aes(x=Time, y=Global_active_power)) + 
  geom_smooth(method = 'loess', span=0.05, aes(color = "moving average")) +
  geom_line(aes(y = lwl, color = "threshold")) +
  geom_line(aes(y = upl, color = "threshold"))+
  labs(color = "") +
  scale_colour_manual(values = c('slateblue2', 'seagreen3', 'seagreen3')) +
  scale_x_datetime(breaks = "1 hour", minor_breaks = "10 min", date_labels = "%H:%M") +
  ylim(c(-5, 9)) +
  labs(title = "Moving Average With Threshold of 0 to average+5")
moving_avg

moving_avg + geom_point(data = march_8_all_m, aes(x=Time, y=Global_active_power), colour = "dodgerblue3")

test_monday_morning$in_range2 <- apply(test_monday_morning, 1, moving_avg_anomaly, avg_data=avg_morning, var_col=3)
morning_moving_anomalies <- filter(test_monday_morning, test_monday_morning$in_range2 == FALSE)

march_8_all_moving <- filter(test_monday_morning, test_monday_morning$Date == "2010-03-08")
march_8_moving_anomalies <- filter(morning_moving_anomalies, morning_moving_anomalies$Date == "2010-03-08")


c <-
  moving_avg +
  geom_point(data = march_8_all_moving, aes(x=Time, y=Global_active_power, color = "normal")) +
  labs(color = "") +
  scale_colour_manual(values = c('slateblue2', 'dodgerblue3', 'seagreen3')) +
  xlab("Time (Morning 6:00:00 - 11:59:00)") +
  labs(color = "", title="March 8 Global Active Power Data")
c

plt_march_8_moving <-
  c +
  geom_point(data = march_8_moving_anomalies, aes(x=Time, y=Global_active_power, color = "anomaly")) +
  labs(color = "") +
  scale_colour_manual(values = c('firebrick2', 'slateblue2', 'dodgerblue3', 'seagreen3')) +
  xlab("Time (Morning 6:00:00 - 11:59:00)") +
  labs(color = "", title="March 8 Moving Average Point Anomalies")
plt_march_8_moving


morning_point_anomalies <- find_clusters(morning_point_anomalies)
all_anomalies_point <- filter(morning_point_anomalies, morning_point_anomalies$Cluster == TRUE)
anomaly_intervals_point <- get_all_anomalies(all_anomalies_point)


morning_moving_anomalies <- find_clusters(morning_moving_anomalies)
all_anomalies_moving <- filter(morning_moving_anomalies, morning_moving_anomalies$Cluster == TRUE)
anomaly_intervals_moving <- get_all_anomalies(all_anomalies_moving)


