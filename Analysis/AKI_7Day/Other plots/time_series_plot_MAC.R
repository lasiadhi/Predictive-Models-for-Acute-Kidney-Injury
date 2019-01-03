# Lasith Adhikari
# Plotting time series data - MAC


# Library
library(dygraphs)
library(xts)
library(mfp) ## Load mfp for automated fractional polynomials
library(Rmisc)
library(dplyr)
library(tseriesChaos)
library(scatterplot3d)

#######################################################

setwd("/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith")
# read data
time_series_data <- read.csv("Time_series/Clean_data/IntraOp_clean_data/etiso_onlyIntraOp.csv",header = TRUE) # data
# filter and preprocess data
time_series_data$time_stamp <- as.POSIXct(time_series_data$time_stamp, format="%Y-%m-%d %H:%M:%S")
time_series_data$obser <- as.numeric(as.character(time_series_data$obser))/1.17

accts <- read.csv("Model/Data/aki7_drop_esrd1_patients/aki7Day_y_train.csv",header = TRUE) # accts
# filter accounts based on the outcome
accts_AKI <- accts[accts$aki7day==1,]
accts_noAKI <- accts[accts$aki7day==0,]

max_time <- 200
max_obser <- 3.5



make_plot <- function(time_in_mins, data, myflag, mycolor){
  #axis.POSIXct(1, at=seq(from=round(time_series_data$time_stamp[1],"hour"), to=tail(time_series_data$time_stamp,1), by="3 hours"), format="%H:%M")
  #matplot(time_series_data$time_stamp, time_series_data$obser, type="l", lty=1, col=rgb(0,0,0,0.1), xlab="Time (mins)", ylab="Observation")
  if (myflag){
    matplot(time_in_mins, data, type="l", lty=1, col=mycolor, xlab="Time (mins)", ylab="MAC", xlim = c(0, max_time), ylim = c(0,max_obser))
  }
  matlines(time_in_mins, data, type="l", lty=1, col=mycolor, xlab="Time (mins)", ylab="MAC", xlim = c(0, max_time), ylim = c(0,max_obser))
}



#svg('HR_ts_for100patients_AKI7_ci.svg')


############################ for no AKI patients

# dataframe to hold all data from no AKI patients
df_noAKI <- data.frame()
#df_noAKI['time_x'] <- as.numeric()
#df_noAKI['value_y'] <- as.numeric()
iter = 0 
myflag = TRUE
for (acc_i in accts_noAKI$acc){
  time_series_data_i <- time_series_data[which(time_series_data$acc == acc_i),]
  
  
  if(dim(time_series_data_i)[1] > 400){
    time_series_data_i$time_stamp <- (time_series_data_i$time_stamp - time_series_data_i$time_stamp[1])/60
    mycolor = rgb(0,1,0,0.1)
    make_plot(time_series_data_i$time_stamp, time_series_data_i$obser, myflag, mycolor)
    df_noAKI <- rbind(df_noAKI, time_series_data_i)
    myflag = FALSE
    iter = iter + 1
  }
  
  if (iter == 100){
    break
  }
}



#################### for AKI patients
df_AKI <- data.frame()
iter = 0
for (acc_i in accts_AKI$acc){
  time_series_data_i <- time_series_data[which(time_series_data$acc == acc_i),]
  if(dim(time_series_data_i)[1] > 400){
    time_series_data_i$time_stamp <- (time_series_data_i$time_stamp - time_series_data_i$time_stamp[1])/60
    mycolor = rgb(1,0,0,0.1)
    make_plot(time_series_data_i$time_stamp, time_series_data_i$obser, myflag, mycolor)
    df_AKI <- rbind(df_AKI, time_series_data_i)
    iter = iter + 1
  }
  
  if (iter == 100){
    break
  }
  
}


############## mean+CI for No AKI  ####################
df_noAKI$time_stamp <- round(as.numeric(df_noAKI$time_stamp),1)
df_noAKI$obser <- as.numeric(as.character(df_noAKI$obser))
#x <- CI(as.numeric(as.character(df_noAKI$obser)), ci=0.95)
mean_data_noAKI <- group_by(df_noAKI, time_stamp)%>% summarise(mean = mean(obser, na.rm = TRUE), sd = sd(obser))
mytime <- seq(0,max_time, 0.01)
matlines(mean_data_noAKI$time_stamp, mean_data_noAKI$mean, type="l", lty=1, lwd=0.5, col=rgb(0,1,0,1), xlim = c(0, max_time), ylim = c(0,max_obser))
matlines(mean_data_noAKI$time_stamp[1:5 == 5], (mean_data_noAKI$mean + 1.96 * mean_data_noAKI$sd)[1:5 == 5], type="l", lty=2, lwd=1, col=rgb(0.156, 0.443, 0.243,1), xlim = c(0, max_time), ylim = c(0,max_obser))
matlines(mean_data_noAKI$time_stamp[1:5 == 5], (mean_data_noAKI$mean - 1.96 * mean_data_noAKI$sd)[1:5 == 5], type="l", lty=2, lwd=1, col=rgb(0.156, 0.443, 0.243,1), xlim = c(0, max_time), ylim = c(0,max_obser))

############## mean+CI for AKI  ######################
df_AKI$time_stamp <- round(as.numeric(df_AKI$time_stamp),1)
df_AKI$obser <- as.numeric(as.character(df_AKI$obser))
mean_data_AKI <- group_by(df_AKI, time_stamp)%>% summarise(mean = mean(obser, na.rm = TRUE), sd = sd(obser))
matlines(mean_data_AKI$time_stamp, mean_data_AKI$mean, type="l", lty=1, lwd=0.5, col=rgb(1,0,0,1), xlim = c(0, max_time), ylim = c(0,max_obser))
matlines(mean_data_AKI$time_stamp[1:5 == 5], (mean_data_AKI$mean + 1.96 * mean_data_AKI$sd)[1:5 == 5], type="l", lty=2, lwd=1, col=rgb(1,0,0,1), xlim = c(0, max_time), ylim = c(0,max_obser))
matlines(mean_data_AKI$time_stamp[1:5 == 5], (mean_data_AKI$mean - 1.96 * mean_data_AKI$sd)[1:5 == 5], type="l", lty=2, lwd=1, col=rgb(1,0,0,1), xlim = c(0, max_time), ylim = c(0,max_obser))

legend("topright", c('Mean MAC for AKI','Mean MAC for No AKI', '95% CI for MAC (AKI)', '95% CI for MAC (No AKI)'), lty=c(1,1,2,2), lwd=c(1.5,1.5,1.5,1.5), col=c('red', rgb(0,1,0,1), rgb(1,0,0,1), rgb(0.156, 0.443, 0.243,1)))


################################################### Time delay embedding #########################################
## No AKI
gap <- 10
obs_ts <- ts(mean_data_noAKI$mean[1:gap == gap])
## AKI
obs_ts_AKI <- ts(mean_data_AKI$mean[1:gap == gap])

for (i in 1:20){
  xyz <- embedd(obs_ts, m=3, d=10*i)
  xyz_AKI <- embedd(obs_ts_AKI, m=3, d=10*i)
  par(mfrow=c(2,1))
  scatterplot3d(xyz, type="l", color= rgb(1,0,0,1), box=FALSE, main = 'NO AKI', xlim = c(0, 4), ylim = c(0, 4) , zlim = c(0, 4), angle = 90)
  scatterplot3d(xyz_AKI, type="l", color= rgb(1,0,0,1), box=FALSE, main = 'AKI', xlim = c(0, 4) , ylim = c(0, 4) , zlim = c(0, 4), angle = 90)
  print(i*10)
  Sys.sleep(4)
}




#dev.off()

# ################################# Automatically fit fractional polynomials for no AKI
# mfpOne <- mfp(formula = as.numeric(as.character(obser)) ~ fp(as.numeric(time_stamp), df = 4), data = df_noAKI)
# ## Check model for transformation
# #summary(mfpOne)
# #plot the model
# mytime <- seq(0,max_time, 0.01)
# y_obser <- predict(mfpOne, list(time_stamp = mytime),type="response")
# matlines(mytime, y_obser, type="l", lty=1, lwd=2, col=rgb(0,1,0,1), xlim = c(0, max_time), ylim = c(0,max_obser))
# 
# 
# ## Automatically fit fractional polynomials for AKI
# mfpOne <- mfp(formula = as.numeric(as.character(obser)) ~ fp(as.numeric(time_stamp), df = 4), data = df_AKI)
# ## Check model for transformation
# #summary(mfpOne)
# #plot the model
# #mytime <- seq(0,max_time, 0.01)
# y_obser <- predict(mfpOne, list(time_stamp = mytime),type="response")
# matlines(mytime, y_obser, type="l", lty=1, lwd=2, col=rgb(0,0,0,1), xlim = c(0, max_time), ylim = c(0,max_obser))




