
#import the needed libraries
library("depmixS4")
library(lubridate)
library(ggplot2)
library(dplyr)
library(gridExtra)

# install.packages("plotrix")
library(plotrix) 

#import helper functions
#source("proj_helper_functions.R")
source('proj_helper_functions.R')

#train_data <- data_with_days("TrainData.txt")
#test_data <- data_with_days("TemporaryTestData.txt")
train1_data <- data_with_days('TrainData23-4.csv')
train2_data <- data_with_days('TrainData24-3.csv')
train3_data <- data_with_days('TrainData34-2.csv')
test1_data <- data_with_days('ValidData23-4.csv')
test2_data <- data_with_days('ValidData24-3.csv')
test3_data <- data_with_days('ValidData34-2.csv')


# ------------- WINDOW 1: Monday Mornings --------------
monday_mornings1 <- get_window(train1_data, "06:00:00", "11:59:00", 2,2)
monday_mornings2 <- get_window(train2_data, "06:00:00", "11:59:00", 2,2)
monday_mornings3 <- get_window(train3_data, "06:00:00", "11:59:00", 2,2)
test1_monday_mornings <- get_window(test1_data, "06:00:00", "11:59:00", 2,2)
test2_monday_mornings <- get_window(test2_data, "06:00:00", "11:59:00", 2,2)
test3_monday_mornings <- get_window(test3_data, "06:00:00", "11:59:00", 2,2)


# ------------- WINDOW 2: Monday Afternoons --------------

monday_afternoons1 <- get_window(train1_data, "12:00:00", "17:59:00", 2, 2)
monday_afternoons2 <- get_window(train2_data, "12:00:00", "17:59:00", 2, 2)
monday_afternoons3 <- get_window(train3_data, "12:00:00", "17:59:00", 2, 2)
test1_monday_afternoons <- get_window(test1_data, "12:00:00", "17:59:00", 2, 2)
test2_monday_afternoons <- get_window(test2_data, "12:00:00", "17:59:00", 2, 2)
test3_monday_afternoons <- get_window(test3_data, "12:00:00", "17:59:00", 2, 2)

# ------------- WINDOW 3: Monday Evenings --------------

monday_evenings1 <- get_window(train1_data, "18:00:00", "23:59:00", 2, 2)
monday_evenings2 <- get_window(train2_data, "18:00:00", "23:59:00", 2, 2)
monday_evenings3 <- get_window(train3_data, "18:00:00", "23:59:00", 2, 2)
test1_monday_evenings <- get_window(test1_data, "18:00:00", "23:59:00", 2, 2)
test2_monday_evenings <- get_window(test2_data, "18:00:00", "23:59:00", 2, 2)
test3_monday_evenings <- get_window(test3_data, "18:00:00", "23:59:00", 2, 2)


####################################################################
#
# Testing Single Variable hmms 
#
#####################################################################

#Create the variables 
BICvec0 <- vector()
logLikVec0 <- vector()
#######################
#---Monday Mornings--#
######################

monday_mornings_by_day <- nrow(monday_mornings1)/360
windowSize = rep(360, monday_mornings_by_day)

mod <- depmix(response = Global_active_power ~ 1, data = monday_mornings1, nstates = 12, ntime = windowSize, family=gaussian())
fm <- fit(mod)
summary(fm)

logLikVec0 <- c(logLikVec0, logLik(fm))
BICvec0 <- c(BICvec0, BIC(fm))

#############################################

monday_mornings_by_day <- nrow(test1_monday_mornings)/360
windowSize = rep(360, monday_mornings_by_day)

mod1 <- depmix(response = Global_active_power ~ 1, data = test1_monday_mornings, nstates = 12, ntime = windowSize, family=gaussian())
fm1 <- fit(mod1)
summary(fm1)

logLikVec0 <- c(logLikVec0, logLik(fm1))
BICvec0 <- c(BICvec0, BIC(fm1))
#################################

#######################
#---Monday Mornings--#
######################

monday_mornings_by_day <- nrow(monday_mornings2)/360
windowSize = rep(360, monday_mornings_by_day)

mod <- depmix(response = Global_active_power ~ 1, data = monday_mornings2, nstates = 12, ntime = windowSize, family=gaussian())
fm <- fit(mod)
summary(fm)

logLikVec0 <- c(logLikVec0, logLik(fm))
BICvec0 <- c(BICvec0, BIC(fm))

#############################################



monday_mornings_by_day <- nrow(test2_monday_mornings)/360
windowSize = rep(360, monday_mornings_by_day)

mod2 <- depmix(response = Global_active_power ~ 1, data = test2_monday_mornings, nstates = 12, ntime = windowSize, family=gaussian())
fm2 <- fit(mod2)
summary(fm2)

logLikVec0 <- c(logLikVec0, logLik(fm2))
BICvec0 <- c(BICvec0, BIC(fm2))
######################

#######################
#---Monday Mornings--#
######################

monday_mornings_by_day <- nrow(monday_mornings3)/360
windowSize = rep(360, monday_mornings_by_day)

mod <- depmix(response = Global_active_power ~ 1, data = monday_mornings3, nstates = 12, ntime = windowSize, family=gaussian())
fm <- fit(mod)
summary(fm)

logLikVec0 <- c(logLikVec0, logLik(fm))
BICvec0 <- c(BICvec0, BIC(fm))

#############################################

monday_mornings_by_day <- nrow(test3_monday_mornings)/360
windowSize = rep(360, monday_mornings_by_day)

mod3 <- depmix(response = Global_active_power ~ 1, data = test3_monday_mornings, nstates = 12, ntime = windowSize, family=gaussian())
fm3 <- fit(mod3)
summary(fm3)

logLikVec0 <- c(logLikVec0, logLik(fm3))
BICvec0 <- c(BICvec0, BIC(fm3))
####################

print(logLikVec0)
print(BICvec0)

barplot(logLikVec0, main="Monday_Mornings HMM Model vs Validation Data", xlab="Dataset", ylab = "LogLik", names.arg=c("model", "test1", "test2", "test3"), col="red")

barplot(BICvec0, main="Monday_Mornings HMM Model vs Validation Data", xlab="Dataset", ylab = "BIC", names.arg=c("model", "test1", "test2", "test3"), col="blue")





#################################################################
#
#--------------afternoons ------------------------------------#
#
#################################################################




#Create the variables 
BICvec1 <- vector()
logLikVec1 <- vector()
#######################
#---Monday afternoons--#
######################

monday_afternoons_by_day <- nrow(monday_afternoons1)/360
windowSize = rep(360, monday_afternoons_by_day)

mod <- depmix(response = Global_active_power ~ 1, data = monday_afternoons1, nstates = 5, ntime = windowSize, family=gaussian())
fm <- fit(mod)
summary(fm)

logLikVec1 <- c(logLikVec1, logLik(fm))
BICvec1 <- c(BICvec1, BIC(fm))

#############################################

monday_afternoons_by_day <- nrow(test1_monday_afternoons)/360
windowSize = rep(360, monday_afternoons_by_day)

mod1 <- depmix(response = Global_active_power ~ 1, data = test1_monday_afternoons, nstates = 5, ntime = windowSize, family=gaussian())
fm1 <- fit(mod1)
summary(fm1)

logLikVec1 <- c(logLikVec1, logLik(fm1))
BICvec1 <- c(BICvec1, BIC(fm1))
#################################3

#######################
#---Monday Mornings--#
######################

monday_afternoons_by_day <- nrow(monday_afternoons2)/360
windowSize = rep(360, monday_afternoons_by_day)

mod <- depmix(response = Global_active_power ~ 1, data = monday_afternoons2, nstates = 5, ntime = windowSize, family=gaussian())
fm <- fit(mod)
summary(fm)

logLikVec1 <- c(logLikVec1, logLik(fm))
BICvec1 <- c(BICvec1, BIC(fm))

#############################################

monday_afternoons_by_day <- nrow(test2_monday_afternoons)/360
windowSize = rep(360, monday_afternoons_by_day)

mod2 <- depmix(response = Global_active_power ~ 1, data = test2_monday_afternoons, nstates = 5, ntime = windowSize, family=gaussian())
fm2 <- fit(mod2)
summary(fm2)

logLikVec1 <- c(logLikVec1, logLik(fm2))
BICvec1 <- c(BICvec1, BIC(fm2))
######################

#######################
#---Monday Mornings--#
######################

monday_afternoons_by_day <- nrow(monday_afternoons3)/360
windowSize = rep(360, monday_afternoons_by_day)

mod <- depmix(response = Global_active_power ~ 1, data = monday_afternoons3, nstates = 5, ntime = windowSize, family=gaussian())
fm <- fit(mod)
summary(fm)

logLikVec1 <- c(logLikVec1, logLik(fm))
BICvec1 <- c(BICvec1, BIC(fm))

#############################################

monday_afternoons_by_day <- nrow(test3_monday_afternoons)/360
windowSize = rep(360, monday_afternoons_by_day)

mod3 <- depmix(response = Global_active_power ~ 1, data = test3_monday_afternoons, nstates = 5, ntime = windowSize, family=gaussian())
fm3 <- fit(mod3)
summary(fm3)

logLikVec1 <- c(logLikVec1, logLik(fm3))
BICvec1 <- c(BICvec1, BIC(fm3))
####################

print(logLikVec1)
print(BICvec1)

barplot(logLikVec1, main="Monday_Afternoons HMM Model vs Validation Data", xlab="Dataset", ylab = "LogLik", names.arg=c("model", "test1", "test2", "test3"), col="red")

barplot(BICvec1, main="Monday_Afternoons HMM Model vs Validation Data", xlab="Dataset", ylab = "BIC", names.arg=c("model", "test1", "test2", "test3"), col="blue")





#################################################################
#
#--------------evenings ------------------------------------#
#
#################################################################

  
  #Create the variables 
  BICvec2 <- vector()
  logLikVec2 <- vector()
  #######################
  #---Monday Evenings--#
  ######################
  
  monday_evenings_by_day <- nrow(monday_evenings1)/360
  windowSize = rep(360, monday_evenings_by_day)
  
  mod <- depmix(response = Global_active_power ~ 1, data = monday_evenings1, nstates = 35, ntime = windowSize, family=gaussian())
  fm <- fit(mod)
  summary(fm)
  
  logLikVec2 <- c(logLikVec2, logLik(fm))
  BICvec2 <- c(BICvec2, BIC(fm))
  
  #############################################
  
  monday_evenings_by_day <- nrow(test1_monday_evenings)/360
  windowSize = rep(360, monday_evenings_by_day)
  
  mod1 <- depmix(response = Global_active_power ~ 1, data = test1_monday_evenings, nstates = 35, ntime = windowSize, family=gaussian())
  fm1 <- fit(mod1)
  summary(fm1)
  
  logLikVec2 <- c(logLikVec2, logLik(fm1))
  BICvec2 <- c(BICvec2, BIC(fm1))
  #################################3
  
  ######################
  
  monday_evenings_by_day <- nrow(monday_evenings2)/360
  windowSize = rep(360, monday_evenings_by_day)
  
  mod <- depmix(response = Global_active_power ~ 1, data = monday_evenings2, nstates = 35, ntime = windowSize, family=gaussian())
  fm <- fit(mod)
  summary(fm)
  
  logLikVec2 <- c(logLikVec2, logLik(fm))
  BICvec2 <- c(BICvec2, BIC(fm))
  
  #############################################
  
  monday_evenings_by_day <- nrow(test2_monday_evenings)/360
  windowSize = rep(360, monday_evenings_by_day)
  
  mod2 <- depmix(response = Global_active_power ~ 1, data = test2_monday_evenings, nstates = 35, ntime = windowSize, family=gaussian())
  fm2 <- fit(mod2)
  summary(fm2)
  
  logLikVec2 <- c(logLikVec2, logLik(fm2))
  BICvec2 <- c(BICvec2, BIC(fm2))
  ######################
  
  ######################
  
  monday_evenings_by_day <- nrow(monday_evenings3)/360
  windowSize = rep(360, monday_evenings_by_day)
  
  mod <- depmix(response = Global_active_power ~ 1, data = monday_evenings3, nstates = 35, ntime = windowSize, family=gaussian())
  fm <- fit(mod)
  summary(fm)
  
  logLikVec2 <- c(logLikVec2, logLik(fm))
  BICvec2 <- c(BICvec2, BIC(fm))
  
  #############################################
  
  monday_evenings_by_day <- nrow(test3_monday_evenings)/360
  windowSize = rep(360, monday_evenings_by_day)
  
  mod3 <- depmix(response = Global_active_power ~ 1, data = test3_monday_evenings, nstates = 35, ntime = windowSize, family=gaussian())
  fm3 <- fit(mod3)
  summary(fm3)
  
  logLikVec2 <- c(logLikVec2, logLik(fm3))
  BICvec2 <- c(BICvec2, BIC(fm3))
  ####################

  print(logLikVec2)
  print(BICvec2)
  
  barplot(logLikVec2, main="Monday_Evenings HMM Model vs Validation Data", xlab="Dataset", ylab = "LogLik", names.arg=c("model", "test1", "test2", "test3"), col="red")
  
  barplot(BICvec2, main="Monday_Evenings HMM Model vs Validation Data", xlab="Dataset", ylab = "BIC", names.arg=c("model", "test1", "test2", "test3"), col="blue")
  
  