
#import the needed libraries
library("depmixS4")
library(lubridate)
library(ggplot2)
library(dplyr)
library(gridExtra)

#import helper functions
source('./proj_helper_functions.R')
#source('~/sfu/cmpt318/termProject/proj_helper_functions.R')/

#train_data <- data_with_days("TrainData.txt")
#test_data <- data_with_days("TemporaryTestData.txt")
train_data <- data_with_days("./TrainData.txt")
test_data <- data_with_days("./TemporaryTestData.txt")

# ------------- WINDOW 1: Weekday Mornings --------------

# get data in our window: data, start time, end time, start day, end day
weekday_mornings <- get_window(train_data, "06:00:00", "11:59:00", 2, 6)
test_weekday_mornings <- get_window(test_data, "06:00:00", "11:59:00", 2, 6)


#------------- WINDOW 2: Weekday Afternoons --------------

weekday_afternoons <- get_window(train_data, "12:00:00", "17:59:00", 2, 6)
test_weekday_afternoons <- get_window(test_data, "12:00:00", "17:59:00", 6, 6)

# ------------- WINDOW 3: Weekday Evenings --------------

weekday_evenings <- get_window(train_data, "18:00:00", "23:59:00", 2, 6)
# altered the windo for faster calculations
test_weekday_evenings <- get_window(test_data, "18:00:00", "23:59:00", 5, 6)

###################################################
## added smaller windows to speed up HMM testing ##
###################################################

####################################################################
monday_mornings <- get_window(train_data, "06:00:00", "11:59:00", 2,2)


#View(sunday_mornings)

# ------------- WINDOW 5: Sunday Afternoons --------------

monday_afternoons <- get_window(train_data, "12:00:00", "17:59:00", 2, 2)

# ------------- WINDOW 6: Sunday Evenings --------------

monday_evenings <- get_window(train_data, "18:00:00", "23:59:00", 2, 2)



#############################################################################
# Phase - 2  # Approach - 2 #                                           
#############################################################################

set.seed(1)

head(monday_mornings)

BICvec <- vector()
logLikVec <- vector()

states <- c(2:10)

# break the sunday mornings into day sized chuncks --60*6 = 360 minutes each sunday_morning -- all rows in sunday_morning divided by the number of minutes per day 
monday_mornings_by_day <- nrow(monday_mornings)/360
# repeat the day sized chuncks 360 times 
windowSize = rep(360, monday_mornings_by_day)

library(foreach)
library(doParallel)
numCores <- detectCores()
numCores
registerDoParallel(numCores)

# Testing the time it takes to complete depmix with nstates = 2:10. CPU utilization was around 100%
# user  system elapsed 
# 0.65    0.35   58.30 
system.time({
  results<-foreach(variable = 2:22, .packages='depmixS4', .combine = rbind) %dopar% {
  
    print(paste("number of states: ", variable))
  
    mod1 <- depmix(response = Global_active_power ~ 1, data = monday_afternoons, nstates = variable, ntime = windowSize, family=gaussian())
   
    fm1 <- fit(mod1)
    
    c(nstates=variable,logLik = logLik(fm1),BIC = BIC(fm1))
  }
})
results

# plot(results[,'nstates'],results[,'logLik'],type="o",col ="navy")
# plot(results[,'nstates'],results[,'BIC'], type="o",col ="darkgreen")
# library(plotrix)
# example(twoord.plot)

twoord.plot(results[,'nstates'],results[,'logLik'],
            results[,'nstates'],results[,'BIC'],
            xlab="nstates",
            # ylab="LogLik", lylim=range(results[,'logLik'])+c(-10,10),
            ylab="LogLik", lylim=c(-50000,0),
            # rylab="BIC",rylim=range(results[,'BIC'])+c(-10,2),
            rylab="BIC",rylim=c(0,90000),
            lcol=2,
            rcol=4,
            main="LogLik and BIC vs nstates",
            do.first="plot_bg();grid(col=\"white\",lty=1)"
            )




# Testing the time it takes to complete depmix with nstates = 2:10. CPU utilization was around 10%
# user    system elapsed 
# 183.14    0.05  183.28
system.time({
  for(variable in states){

    print(paste("number of states: ", variable))
  
    mod1 <- depmix(response = Global_active_power ~ 1, data = monday_afternoons, nstates = variable, ntime = windowSize, family=gaussian())
  
    fm1 <- fit(mod1)
  
    summary(fm1)
  
    logLikVec <- c(logLikVec, logLik(fm1))
    BICvec <- c(BICvec, BIC(fm1))
  }
})


print(logLikVec)
print(BICvec)

plot(states[2:19], logLikVec[2:19], type="o", col = "navy")
plot(states, BICvec, type="o", col ="darkgreen")




######################
# ------------ MultiVariable using Global_active_power and Global_intensity -----------
set.seed(1)

head(monday_mornings)

BICvec <- vector()
logLikVec <- vector()

states <- c(10:25)

# break the sunday mornings into day sized chuncks --60*6 = 360 minutes each sunday_morning -- all rows in sunday_morning divided by the number of minutes per day 
monday_mornings_by_day <- nrow(monday_mornings)/360
# repeat the day sized chuncks 360 times 
windowSize = rep(360, monday_mornings_by_day)


for(variable in states){
  
  print(paste("number of states: ", variable))
  
  mod2 <- depmix(response = list(Global_active_power ~ 1, Global_intensity ~ 1), data = monday_mornings, nstates = variable, ntimes = windowSize, family=list(gaussian(), gaussian()))
  
  fm2 <- fit(mod2)

  summary(fm2)
  
  logLikVec <- c(logLikVec, logLik(fm2))
  BICvec <- c(BICvec, BIC(fm2))
  
  
}

print(logLikVec)
print(BICvec)

plot(states, logLikVec, type="o", col = "red")
plot(states, BICvec, type="o", col ="brown")

###################


#####################################################################
# other attemps involved 
#Global_intensity ~ 1, Voltage ~ 1, Global_reactive_power ~ 1 -- works for 2-6 but fails after
#Global_intensity ~ 1, Global_active_power ~ 1, Global_reactive_power ~ 1  -- works for 2-10 fails after
#Global_intensity ~ 1, Sub_metering_3 ~ 1, Global_reactive_power ~ 1  --- fails at 7 states
####################################################################
set.seed(1)

head(monday_mornings)

BICvec <- vector()
logLikVec <- vector()

states <- c(2:9)

# break the sunday mornings into day sized chuncks --60*6 = 360 minutes each sunday_morning -- all rows in sunday_morning divided by the number of minutes per day 
monday_mornings_by_day <- nrow(monday_mornings)/360
# repeat the day sized chuncks 360 times 
windowSize = rep(360, monday_mornings_by_day)


for(variable in states){
  
  print(paste("number of states: ", variable))
  
  mod3 <- depmix(response = list(Global_intensity ~ 1, Global_active_power ~ 1, Global_reactive_power ~ 1), data = monday_mornings, nstates = variable, ntimes = windowSize, family=list(gaussian(),gaussian(), gaussian()))
  
  fm3 <- fit(mod3)

  summary(fm3)
  
  logLikVec <- c(logLikVec, logLik(fm3))
  BICvec <- c(BICvec, BIC(fm3))
  
  
}

print(logLikVec)
print(BICvec)

plot(states, logLikVec, type="o", col = "red")
plot(states, BICvec, type="o", col ="pink")


###############################################################################
#
# --------------------------- Afternoons ------------------------------------#
#
##############################################################################

set.seed(1)

head(monday_afternoons)

BICvec <- vector()
logLikVec <- vector()

states <- c(2:21)

# break the sunday mornings into day sized chuncks --60*6 = 360 minutes each sunday_morning -- all rows in sunday_morning divided by the number of minutes per day 
monday_afternoons_by_day <- nrow(monday_afternoons)/360
# repeat the day sized chuncks 360 times 
windowSize = rep(360, monday_afternoons_by_day)


for(variable in states){
  
  print(paste("number of states: ", variable))
  
  mod4 <- depmix(response = Global_active_power ~ 1, data = monday_afternoons, nstates = variable, ntime = windowSize, family=gaussian())
  
  fm4 <- fit(mod4)
  
  summary(fm4)
  
  logLikVec <- c(logLikVec, logLik(fm4))
  BICvec <- c(BICvec, BIC(fm4))
  
  
}

print(logLikVec)
print(BICvec)

plot(states, logLikVec, type="o", col = "navy")
plot(states, BICvec, type="o", col ="darkgreen")






######################
# ------------ MultiVariable using Global_active_power and Global_intensity -----------
set.seed(1)

head(monday_afternoons)

BICvec <- vector()
logLikVec <- vector()

states <- c(2:13)

# break the sunday mornings into day sized chuncks --60*6 = 360 minutes each sunday_morning -- all rows in sunday_morning divided by the number of minutes per day 
monday_afternoons_by_day <- nrow(monday_afternoons)/360
# repeat the day sized chuncks 360 times 
windowSize = rep(360, monday_afternoons_by_day)


for(variable in states){
  
  print(paste("number of states: ", variable))
  
  mod5 <- depmix(response = list(Global_active_power ~ 1, Global_intensity ~ 1), data = monday_afternoons, nstates = variable, ntimes = windowSize, family=list(gaussian(), gaussian()))
  
  fm5 <- fit(mod5)
  
  summary(fm5)
  
  logLikVec <- c(logLikVec, logLik(fm5))
  BICvec <- c(BICvec, BIC(fm5))
  
  
}

print(logLikVec)
print(BICvec)

plot(states, logLikVec, type="o", col = "red")
plot(states, BICvec, type="o", col ="brown")

###################


#####################################################################
# other attemps involved 
#Global_intensity ~ 1, Voltage ~ 1, Global_reactive_power ~ 1 -- works for 2-6 but fails after
#Global_intensity ~ 1, Global_active_power ~ 1, Global_reactive_power ~ 1  -- works for 2-10 fails after
#Global_intensity ~ 1, Sub_metering_3 ~ 1, Global_reactive_power ~ 1  --- fails at 7 states
####################################################################
set.seed(1)

head(monday_mornings)

BICvec <- vector()
logLikVec <- vector()

states <- c(2:9)

# break the sunday mornings into day sized chuncks --60*6 = 360 minutes each sunday_morning -- all rows in sunday_morning divided by the number of minutes per day 
monday_mornings_by_day <- nrow(monday_mornings)/360
# repeat the day sized chuncks 360 times 
windowSize = rep(360, monday_mornings_by_day)


for(variable in states){
  
  print(paste("number of states: ", variable))
  
  mod3 <- depmix(response = list(Global_intensity ~ 1, Global_active_power ~ 1, Global_reactive_power ~ 1), data = monday_mornings, nstates = variable, ntimes = windowSize, family=list(gaussian(),gaussian(), gaussian()))
  
  fm3 <- fit(mod3)
  
  summary(fm3)
  
  logLikVec <- c(logLikVec, logLik(fm3))
  BICvec <- c(BICvec, BIC(fm3))
  
  
}

print(logLikVec)
print(BICvec)

plot(states, logLikVec, type="o", col = "red")
plot(states, BICvec, type="o", col ="pink")


