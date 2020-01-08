
#import the needed libraries
library("depmixS4")
library(lubridate)
library(ggplot2)
library(dplyr)
library(gridExtra)

install.packages("plotrix")
library(plotrix) 

#import helper functions
#source("proj_helper_functions.R")
source('~/sfu/cmpt318/termProject/proj_helper_functions.R')

#train_data <- data_with_days("TrainData.txt")
#test_data <- data_with_days("TemporaryTestData.txt")
train_data <- data_with_days('~/sfu/cmpt318/termProject/TrainData.txt')
test1_data <- data_with_days('~/sfu/cmpt318/termProject/test1.txt')
View(test1_data)
test2_data <- data_with_days('~/sfu/cmpt318/termProject/test2.txt')
test3_data <- data_with_days('~/sfu/cmpt318/termProject/test3.txt')
test4_data <- data_with_days('~/sfu/cmpt318/termProject/test4.txt')
test5_data <- data_with_days('~/sfu/cmpt318/termProject/test5.txt')

# ------------- WINDOW 1: Monday Mornings --------------
monday_mornings <- get_window(train_data, "06:00:00", "11:59:00", 2,2)
test1_monday_mornings <- get_window(test1_data, "06:00:00", "11:59:00", 2,2)
test2_monday_mornings <- get_window(test2_data, "06:00:00", "11:59:00", 2,2)
test3_monday_mornings <- get_window(test3_data, "06:00:00", "11:59:00", 2,2)
test4_monday_mornings <- get_window(test4_data, "06:00:00", "11:59:00", 2,2)
test5_monday_mornings <- get_window(test5_data, "06:00:00", "11:59:00", 2,2)


# ------------- WINDOW 2: Monday Afternoons --------------

monday_afternoons <- get_window(train_data, "12:00:00", "17:59:00", 2, 2)
test1_monday_afternoons <- get_window(test1_data, "12:00:00", "17:59:00", 2, 2)
test2_monday_afternoons <- get_window(test2_data, "12:00:00", "17:59:00", 2, 2)
test3_monday_afternoons <- get_window(test3_data, "12:00:00", "17:59:00", 2, 2)
test4_monday_afternoons <- get_window(test4_data, "12:00:00", "17:59:00", 2, 2)
test5_monday_afternoons <- get_window(test5_data, "12:00:00", "17:59:00", 2, 2)

# ------------- WINDOW 3: Monday Evenings --------------

monday_evenings <- get_window(train_data, "18:00:00", "23:59:00", 2, 2)

test1_monday_evenings <- get_window(test1_data, "18:00:00", "23:59:00", 2, 2)
test2_monday_evenings <- get_window(test2_data, "18:00:00", "23:59:00", 2, 2)
test3_monday_evenings <- get_window(test3_data, "18:00:00", "23:59:00", 2, 2)
test4_monday_evenings <- get_window(test4_data, "18:00:00", "23:59:00", 2, 2)
test5_monday_evenings <- get_window(test5_data, "18:00:00", "23:59:00", 2, 2)


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

monday_mornings_by_day <- nrow(monday_mornings)/360
windowSize = rep(360, monday_mornings_by_day)

mod <- depmix(response = Global_active_power ~ 1, data = monday_mornings, nstates = 12, ntime = windowSize, family=gaussian())
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
#################################3

monday_mornings_by_day <- nrow(test2_monday_mornings)/360
windowSize = rep(360, monday_mornings_by_day)

mod2 <- depmix(response = Global_active_power ~ 1, data = test2_monday_mornings, nstates = 12, ntime = windowSize, family=gaussian())
fm2 <- fit(mod2)
summary(fm2)

logLikVec0 <- c(logLikVec0, logLik(fm2))
BICvec0 <- c(BICvec0, BIC(fm2))
######################

monday_mornings_by_day <- nrow(test3_monday_mornings)/360
windowSize = rep(360, monday_mornings_by_day)

mod3 <- depmix(response = Global_active_power ~ 1, data = test3_monday_mornings, nstates = 12, ntime = windowSize, family=gaussian())
fm3 <- fit(mod3)
summary(fm3)

logLikVec0 <- c(logLikVec0, logLik(fm3))
BICvec0 <- c(BICvec0, BIC(fm3))
####################
monday_mornings_by_day <- nrow(test4_monday_mornings)/360
windowSize = rep(360, monday_mornings_by_day)

mod4 <- depmix(response = Global_active_power ~ 1, data = test4_monday_mornings, nstates = 12, ntime = windowSize, family=gaussian())
fm4 <- fit(mod4)
summary(fm4)

logLikVec0 <- c(logLikVec0, logLik(fm4))
BICvec0 <- c(BICvec0, BIC(fm4))
########################3

monday_mornings_by_day <- nrow(test5_monday_mornings)/360
windowSize = rep(360, monday_mornings_by_day)

mod5 <- depmix(response = Global_active_power ~ 1, data = test5_monday_mornings, nstates = 12, ntime = windowSize, family=gaussian())
fm5 <- fit(mod5)
summary(fm5) 

logLikVec0 <- c(logLikVec0, logLik(fm5))
BICvec0 <- c(BICvec0, BIC(fm5))
######################
print(logLikVec0)
print(BICvec0)

barplot(logLikVec0, main="Monday_Mornings HMM Model vs Tests", xlab="Dataset", ylab = "LogLik", names.arg=c("model", "test1", "test2", "test3", "test4", "test5"), col="red")

barplot(BICvec0, main="Monday_Mornings HMM Model vs Tests", xlab="Dataset", ylab = "BIC", names.arg=c("model", "test1", "test2", "test3", "test4", "test5"), col="blue")





#################################################################
#
#--------------afternoons ------------------------------------#
#
#################################################################




#Create the variables 
BICvec1 <- vector()
logLikVec1 <- vector()
#######################
#---Monday Mornings--#
######################

monday_afternoons_by_day <- nrow(monday_afternoons)/360
windowSize = rep(360, monday_afternoons_by_day)

mod <- depmix(response = Global_active_power ~ 1, data = monday_afternoons, nstates = 5, ntime = windowSize, family=gaussian())
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

monday_afternoons_by_day <- nrow(test2_monday_afternoons)/360
windowSize = rep(360, monday_afternoons_by_day)

mod2 <- depmix(response = Global_active_power ~ 1, data = test2_monday_afternoons, nstates = 5, ntime = windowSize, family=gaussian())
fm2 <- fit(mod2)
summary(fm2)

logLikVec1 <- c(logLikVec1, logLik(fm2))
BICvec1 <- c(BICvec1, BIC(fm2))
######################

monday_afternoons_by_day <- nrow(test3_monday_afternoons)/360
windowSize = rep(360, monday_afternoons_by_day)

mod3 <- depmix(response = Global_active_power ~ 1, data = test3_monday_afternoons, nstates = 5, ntime = windowSize, family=gaussian())
fm3 <- fit(mod3)
summary(fm3)

logLikVec1 <- c(logLikVec1, logLik(fm3))
BICvec1 <- c(BICvec1, BIC(fm3))
####################
monday_afternoons_by_day <- nrow(test4_monday_afternoons)/360
windowSize = rep(360, monday_afternoons_by_day)

mod4 <- depmix(response = Global_active_power ~ 1, data = test4_monday_afternoons, nstates = 5, ntime = windowSize, family=gaussian())
fm4 <- fit(mod4)
summary(fm4)

logLikVec1 <- c(logLikVec1, logLik(fm4))
BICvec1 <- c(BICvec1, BIC(fm4))
########################3

monday_afternoons_by_day <- nrow(test5_monday_afternoons)/360
windowSize = rep(360, monday_afternoons_by_day)

mod5 <- depmix(response = Global_active_power ~ 1, data = test5_monday_afternoons, nstates = 5, ntime = windowSize, family=gaussian())
fm5 <- fit(mod5)
summary(fm5) 

logLikVec1 <- c(logLikVec1, logLik(fm5))
BICvec1 <- c(BICvec1, BIC(fm5))
######################
print(logLikVec1)
print(BICvec1)

barplot(logLikVec1, main="Monday_Afternoons HMM Model vs Tests", xlab="Dataset", ylab = "LogLik", names.arg=c("model", "test1", "test2", "test3", "test4", "test5"), col="red")

barplot(BICvec1, main="Monday_Afternoons HMM Model vs Tests", xlab="Dataset", ylab = "BIC", names.arg=c("model", "test1", "test2", "test3", "test4", "test5"), col="blue")





#################################################################
#
#--------------evenings ------------------------------------#
#
#################################################################

  
  #Create the variables 
  BICvec2 <- vector()
  logLikVec2 <- vector()
  #######################
  #---Monday Mornings--#
  ######################
  
  monday_evenings_by_day <- nrow(monday_evenings)/360
  windowSize = rep(360, monday_evenings_by_day)
  
  mod <- depmix(response = Global_active_power ~ 1, data = monday_evenings, nstates = 35, ntime = windowSize, family=gaussian())
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
  
  monday_evenings_by_day <- nrow(test2_monday_evenings)/360
  windowSize = rep(360, monday_evenings_by_day)
  
  mod2 <- depmix(response = Global_active_power ~ 1, data = test2_monday_evenings, nstates = 35, ntime = windowSize, family=gaussian())
  fm2 <- fit(mod2)
  summary(fm2)
  
  logLikVec2 <- c(logLikVec2, logLik(fm2))
  BICvec2 <- c(BICvec2, BIC(fm2))
  ######################
  
  monday_evenings_by_day <- nrow(test3_monday_evenings)/360
  windowSize = rep(360, monday_evenings_by_day)
  
  mod3 <- depmix(response = Global_active_power ~ 1, data = test3_monday_evenings, nstates = 35, ntime = windowSize, family=gaussian())
  fm3 <- fit(mod3)
  summary(fm3)
  
  logLikVec2 <- c(logLikVec2, logLik(fm3))
  BICvec2 <- c(BICvec2, BIC(fm3))
  ####################
  monday_evenings_by_day <- nrow(test4_monday_evenings)/360
  windowSize = rep(360, monday_evenings_by_day)
  
  mod4 <- depmix(response = Global_active_power ~ 1, data = test4_monday_evenings, nstates = 5, ntime = windowSize, family=gaussian())
  fm4 <- fit(mod4)
  summary(fm4)
  
  logLikVec2 <- c(logLikVec2, logLik(fm4))
  BICvec2 <- c(BICvec2, BIC(fm4))
  ########################3
  
  monday_evenings_by_day <- nrow(test5_monday_evenings)/360
  windowSize = rep(360, monday_evenings_by_day)
  
  mod5 <- depmix(response = Global_active_power ~ 1, data = test5_monday_evenings, nstates = 35, ntime = windowSize, family=gaussian())
  fm5 <- fit(mod5)
  summary(fm5) 
  
  logLikVec2 <- c(logLikVec2, logLik(fm5))
  BICvec2 <- c(BICvec2, BIC(fm5))
  ######################
  print(logLikVec2)
  print(BICvec2)
  
  barplot(logLikVec2, main="Monday_Evenings HMM Model vs Tests", xlab="Dataset", ylab = "LogLik", names.arg=c("model", "test1", "test2", "test3", "test4", "test5"), col="red")
  
  barplot(BICvec2, main="Monday_Evenings HMM Model vs Tests", xlab="Dataset", ylab = "BIC", names.arg=c("model", "test1", "test2", "test3", "test4", "test5"), col="blue")
  
  
  
  
  #############################################
  #
  #--------------- 3 value hmm afternoons
  #
  ###########################################
  
  
  
  
  #Create the variables 
  BICvec3<- vector()
  logLikVec3 <- vector()
  #######################
  #---Monday Mornings--#
  ######################
  
  monday_afternoons_by_day <- nrow(monday_afternoons)/360
  windowSize = rep(360, monday_afternoons_by_day)
  
  mod <- depmix(response = list(Global_intensity ~ 1, Global_active_power ~ 1, Global_reactive_power ~ 1), data = monday_afternoons, nstates = 7, ntimes = windowSize, family=list(gaussian(),gaussian(), gaussian()))
  fm <- fit(mod)
  summary(fm)
  
  logLikVec3 <- c(logLikVec3, logLik(fm))
  BICvec3 <- c(BICvec3, BIC(fm))
  
  #############################################
  
  monday_afternoons_by_day <- nrow(test1_monday_afternoons)/360
  windowSize = rep(360, monday_afternoons_by_day)
  
  mod1 <- depmix(response = list(Global_intensity ~ 1, Global_active_power ~ 1, Global_reactive_power ~ 1), data = test1_monday_afternoons, nstates = 7, ntimes = windowSize, family=list(gaussian(),gaussian(), gaussian()))
  fm1 <- fit(mod1)
  summary(fm1)
  
  logLikVec3 <- c(logLikVec3, logLik(fm1))
  BICvec3 <- c(BICvec3, BIC(fm1))
  #################################3
  
  monday_afternoons_by_day <- nrow(test2_monday_afternoons)/360
  windowSize = rep(360, monday_afternoons_by_day)
  
  mod <- depmix(response = list(Global_intensity ~ 1, Global_active_power ~ 1, Global_reactive_power ~ 1), data = test2_monday_afternoons, nstates = 7, ntimes = windowSize, family=list(gaussian(),gaussian(), gaussian()))
  fm2 <- fit(mod2)
  summary(fm2)
  
  logLikVec3 <- c(logLikVec3, logLik(fm2))
  BICvec3 <- c(BICvec3, BIC(fm2))
  ######################
  
  monday_afternoons_by_day <- nrow(test3_monday_afternoons)/360
  windowSize = rep(360, monday_afternoons_by_day)
  
  mod <- depmix(response = list(Global_intensity ~ 1, Global_active_power ~ 1, Global_reactive_power ~ 1), data = test3_monday_afternoons, nstates = 7, ntimes = windowSize, family=list(gaussian(),gaussian(), gaussian()))
  fm3 <- fit(mod3)
  summary(fm3)
  
  logLikVec3 <- c(logLikVec3, logLik(fm3))
  BICvec3 <- c(BICvec3, BIC(fm3))
  ####################
  monday_afternoons_by_day <- nrow(test4_monday_afternoons)/360
  windowSize = rep(360, monday_afternoons_by_day)
  
  mod <- depmix(response = list(Global_intensity ~ 1, Global_active_power ~ 1, Global_reactive_power ~ 1), data = test4_monday_afternoons, nstates = 7, ntimes = windowSize, family=list(gaussian(),gaussian(), gaussian()))
  fm4 <- fit(mod4)
  summary(fm4)
  
  logLikVec3 <- c(logLikVec3, logLik(fm4))
  BICvec3 <- c(BICvec3, BIC(fm4))
  ########################3
  
  monday_afternoons_by_day <- nrow(test5_monday_afternoons)/360
  windowSize = rep(360, monday_afternoons_by_day)
  
  mod <- depmix(response = list(Global_intensity ~ 1, Global_active_power ~ 1, Global_reactive_power ~ 1), data = test5_monday_afternoons, nstates = 7, ntimes = windowSize, family=list(gaussian(),gaussian(), gaussian()))
  fm5 <- fit(mod5)
  summary(fm5) 
  
  logLikVec2 <- c(logLikVec3, logLik(fm5))
  BICvec2 <- c(BICvec3, BIC(fm5))
  ######################
  print(logLikVec3)
  print(BICvec3)
  
  

  logValues <- c(-996.0215, -16217.6395, -15498.3082, -16215.7258, -76872.5826, -77180.0923)
  BICValues <- c(2975.457,  33317.111,  31878.448,  33313.283, 154626.997, 155242.016)
  
  barplot(logValues, main="Monday_Afternoons 3 Variable HMM Model vs Tests", xlab="Dataset", ylab = "LogLik", names.arg=c("model", "test1", "test2", "test3", "test4", "test5"), col="red")
  
  barplot(BICValues, main="Monday_Afternoons 3 Variable HMM Model vs Tests", xlab="Dataset", ylab = "BIC", names.arg=c("model", "test1", "test2", "test3", "test4", "test5"), col="blue")
