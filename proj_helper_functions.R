# helper functions

# read data from file and add day and day number
data_with_days <- function(filename) {
  
  df <- read.csv(filename, header=TRUE, sep=",")
  
  dates <- as.POSIXlt(df$Date, na.rm = T, format = "%d/%m/%Y")
  
  # add column "Day" to the dataframe, Sunday=1, Monday=2 ...
  df$Day_number <- wday(as.Date(dates))
  df$Day <- weekdays(as.Date(dates))
  df$DateTime <- as.POSIXct(paste(df$Date, df$Time), format="%d/%m/%Y %H:%M:%S")
  df$Time <- as.POSIXct(df$Time, format="%H:%M:%S")
  df$Date <- as.Date(df$Date, format="%d/%m/%Y")
  
  return(df)
}


get_window <- function(data, start_time, end_time, start_day, end_day) {
  dates <- as.POSIXlt(data$Date, na.rm = T, format = "%d/%m/%Y")
  times <- format(data$Time, na.rm = T, format = "%H:%M:%S")
  time_window <- times >= start_time & times <= end_time
  day_window <- data$Day_number >= start_day & data$Day_number <= end_day
  new_data <- subset(data, time_window & day_window)
  return(new_data)
}

point_anomaly <- function(test_data, avg_data, var_col) {
  row <- avg_data[avg_data$Time == test_data[2][1], ]
  val<- as.numeric(test_data[var_col][1])
  if (val > row$max) {
    return(FALSE)
  } 
  else if (val < row$min) {
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}

moving_avg_anomaly <- function(test_data, avg_data, var_col) {
  row <- avg_data[avg_data$Time == test_data[2][1], ]
  val<- as.numeric(test_data[var_col][1])
  if (val > row$upl) {
    return(FALSE)
  } 
  else if (val < row$lwl) {
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}


find_clusters <- function(anomaly_data) {
  anomaly_data$Cluster = FALSE
  for(i in 1:nrow(anomaly_data)){
    j = i
    start = anomaly_data[i, 'DateTime']
    window = 0
    while (window < 10 & j != nrow(anomaly_data)) {
      end = anomaly_data[j+1, 'DateTime']
      window = difftime(end, start, units = "mins")
      j = j+1
    }
    if ( j-i > 5) {
      anomaly_data[i:j, 'Cluster'] = TRUE
    }
  }

  return(anomaly_data)
}

get_all_anomalies <- function(anomaly_data) {
  anomaly_data <- find_clusters(anomaly_data)
  df <- filter(anomaly_data, anomaly_data$Cluster == TRUE)

  date <- c()
  start_time <- c()
  end_time <- c()

  i = 1
  
  while(i < nrow(anomaly_data)){
    j = i
    date <- c(date, format(anomaly_data[i, 'Date'], format="%Y-%m-%d"))
    start_time <- c(start_time, format(anomaly_data$Time[i], format="%H:%M:%S"))
    current <- format(anomaly_data[i, 'Date'], format="%Y-%m-%d")
    next_date <- current
    while(current == next_date & j < nrow(anomaly_data)) {
      next_date <- format(anomaly_data[j+1, 'Date'], format="%Y-%m-%d")
      j = j+1
    }
    end_time <- c(end_time, format(anomaly_data$Time[j-1], format="%H:%M:%S"))
    i=j+1
  }
  res <- data.frame(date, start_time, end_time)
  
  return(res)
}


