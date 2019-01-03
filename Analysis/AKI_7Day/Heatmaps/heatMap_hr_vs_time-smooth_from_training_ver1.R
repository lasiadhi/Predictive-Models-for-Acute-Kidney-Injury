# plot heatmap risk plot for HR vs time - smoothout version - FINAL version on 11/30/2017
# for AKI-7day

#load libraries
library(ggplot2)
library(reshape2)
library(mgcv)

#library(timeSeries)
#library(zoo)

               
#setwd("/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model")
setwd('S:/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users')

# read feature data
data_all <- read.csv("Lasith/IntraOp_model-DECLARE/Model/Data/features_to_model_withPF_moreClean.csv", header=TRUE)

# read predictions for train data
data_pred <- read.csv("Lasith/IntraOp_model-DECLARE/Model/Results/AKI_7Day/IntraOp+PreOp_probs_drop_esrd/aki7Day_prediction_from_intraOP+preOP_probs_from_train2038.csv", header=TRUE)

# read predictions for test data
#data_pred <- read.csv("Lasith/IntraOp_model-DECLARE/Model/Results/AKI_3Day/IntraOp+PreOp_all_features/aki3Day_intraOP+preOP_prediction_from_test874.csv",header = TRUE)
data_train_pred <- merge(data_all, data_pred, by.x='acc', by.y='acc')

#colnames(data_train_pred)


## extract hr range columns from the above dataset along with the outcomes
ind <- which(colnames(data_train_pred)%in%c("hr_mean_base","hr_range_36_59","hr_range_59_70","hr_range_70_93","hr_range_93_104","hr_range_104_127","true_AKI_outcome","pred_from_intraPreOp"))
data_train_hr_pred <- data_train_pred[,ind]
# data_train_hr_pred$Predicted_AKI_risk <-  sapply(data_train_hr_pred$pred_from_intraOp, function(x) 
#   ifelse (x>0.34, 1,ifelse(x<=0.34,0)))  #change the cutoff 0.42 based on the model cutoff


# time verctor  (along y-axis on the plot)
time1 <- as.data.frame(seq(from = 0, to = 500, length.out = 501)) ; colnames(time1) <- "duration"


temp_fun <- function(vect_gam,outcome,time1){
  s <- as.formula("outcome~duration") ; fm <- binomial(); fm$link <- "logit"
  data_temp <- as.data.frame(cbind(vect_gam, outcome)) ; colnames(data_temp) <- c("duration","outcome")
  gam_temp <- gam(formula = s, family = fm, data = data_temp) 
  pred <- predict.gam(object = gam_temp, newdata = time1, type = "response")
  return(pred)
}


# create a dataframe with small bin sizes for hr values over the time:
df_hr_time <- data.frame(matrix(nrow=length(data_train_hr_pred$hr_range_59_70), ncol=9))
df_hr_time$X1 <- data_train_hr_pred$hr_range_36_59
df_hr_time$X3 <- data_train_hr_pred$hr_range_59_70
df_hr_time$X5 <- data_train_hr_pred$hr_range_70_93
df_hr_time$X7 <- data_train_hr_pred$hr_range_93_104
df_hr_time$X9 <- data_train_hr_pred$hr_range_104_127
#interpNA(df_hr_time, method = "linear")
#colnames(plot_data_hr) <- c("hr_43-51","interp1_43-51","interp2_43-51","hr_51-67", "interp1_51-67","interp2_51-67",)

# using timeseries
#t0 <- as.Date("2000-01-01")
#vv = na.spline(zoo(t(df_hr_time),t0+seq_len(ncol(df_hr_time))), na.rm = FALSE)
#t(coredata(vv))


# interpolate hr values accross hr_ranges: use natural cubic spline
myFun<-function(yrow){
  fmm = spline(x = 1:9, y = yrow, xout=1:9, method = "natural")
  return(fmm$y)
}
df_interp <- t(apply(df_hr_time, 1, myFun))  # 1 indicates rows


# interpolate risk for given time for all the ranges
outcome1 <- data_train_hr_pred$true_AKI_outcome
k1 <- temp_fun(df_interp[,1],outcome1,time1)
k2 <- temp_fun(df_interp[,2],outcome1,time1)
k3 <- temp_fun(df_interp[,3],outcome1,time1)
k4 <- temp_fun(df_interp[,4],outcome1,time1)
k5 <- temp_fun(df_interp[,5],outcome1,time1)
k6 <- temp_fun(df_interp[,6],outcome1,time1)
k7 <- temp_fun(df_interp[,7],outcome1,time1)
k8 <- temp_fun(df_interp[,8],outcome1,time1)
k9 <- temp_fun(df_interp[,9],outcome1,time1)


# vector for hr (along x axis on the plot)
hr1 <- as.data.frame(seq(from = 36, to = 127, length.out = 92)) ; colnames(hr1) <- "HR"


# select hr_mean_base values in 40 to 90 along with the outcome
#indx <- which(data_train_hr_pred$hr_mean_base<90 & data_train_hr_pred$hr_mean_base>40)


s <- as.formula("outcome~s(HR,k=5)") ; fm <- binomial(); fm$link <- "logit"
data_temp <- as.data.frame(cbind(data_train_hr_pred$hr_mean_base, outcome1)) ; colnames(data_temp) <- c("HR","outcome")
gam_temp <- gam(formula = s, family = fm, data = data_temp) 
hr_pred  <- predict.gam(object = gam_temp, newdata = hr1, type = "response")  #risk precitions for hr values from 40 to 80


hr <- seq(from = 36, to = 127, length.out = 92)
time <- seq(from = 0,to = 500,length.out = 501)


plot_data_hr <- data.frame(matrix(nrow=length(hr)*length(time),ncol=5)) ; colnames(plot_data_hr) <- c("hr","time","risk_hr","risk_time","risk_cor_hr")

k=1
for(i in 1:length(hr)){
  if(hr[i]<46){
    temp_time_pred <- k1 
    mid <- 40 ; corr <- diff(range(hr_pred[1:13]))/13   #hr_range_36_46
  } else if(hr[i]<56){
    temp_time_pred <- k2 
    mid <- 50 ; corr <- diff(range(hr_pred[11:20]))/10   #hr_range_46-56
  } else if(hr[i]<66){
    temp_time_pred <- k3 
    mid <- 60 ; corr <- diff(range(hr_pred[18:36]))/19   # hr_range_56-66
  } else if(hr[i]<76){
    temp_time_pred <- k4
    mid <- 71 ; corr <- diff(range(hr_pred[32:49]))/18   # hr_range_66-76
  } else if(hr[i]<86){
    temp_time_pred <- k5
    mid <- 81 ; corr <- diff(range(hr_pred[47:63]))/17   # hr_range_76-86
  } else if(hr[i]<96){
    temp_time_pred <- k6
    mid <- 91 ; corr <- diff(range(hr_pred[58:69]))/12   # hr_range_86-96
  } else if(hr[i]<107){
    temp_time_pred <- k7
    mid <- 102 ; corr <- diff(range(hr_pred[66:79]))/14   # hr_range_96-107
  } else if(hr[i]<117){
    temp_time_pred <- k8
    mid <- 112 ; corr <- diff(range(hr_pred[77:87]))/11   # hr_range_107-117
  } else if(hr[i]<127){
    temp_time_pred <- k9
    mid <- 122 ; corr <- diff(range(hr_pred[85:92]))/8   # hr_range_117-127
  }
  
  for(j in 1:length(time)){
    plot_data_hr$hr[k] <- hr[i] ; plot_data_hr$time[k] <- time[j]
    plot_data_hr$risk_hr[k] <- hr_pred[i] ; plot_data_hr$risk_time[k] <- temp_time_pred[j]
    plot_data_hr$risk_cor_hr[k] <- temp_time_pred[j]+(mid-hr[i])*corr
    k <- k+1
  }
}



#plot_data_hr$risk <- round((plot_data_hr$risk_hr+plot_data_hr$risk_time)/2,3)
plot_data_hr$risk <- round(plot_data_hr$risk_cor_hr,3)


#threshold risk to [0,1]
indexx <- which(plot_data_hr$risk > 1)
plot_data_hr$risk[indexx] <- 1.0


plot_hr <- ggplot(plot_data_hr,aes(hr,time,z=risk)) + geom_tile(aes(fill=risk)) + 
  scale_fill_gradientn(colours=c("green","red","red2"), na.value = "transparent") + 
  labs(x="Heart Rate (Beats per Minute)",y="Time (mins)", title="AKI-7Day Risk over the HR and time" ) + 
  geom_raster(aes(fill = risk), interpolate=TRUE) +
  guides(fill=guide_colorbar(barheight = 15)) #+ stat_contour(aes(x=hr,y=time,z=risk),breaks=c(0.39),size=0.5) +
  #annotate("text",x=c(80,55),y=c(25,150),label=c("Low (<=0.39)","High (>0.39)"),size=5) 
print(plot_hr)

ggsave(filename="Meixian/IDEALIST_IntraOp/Heatmaps/aki7day_hr.png", plot=plot_hr)
ggsave(filename="Meixian/IDEALIST_IntraOp/Heatmaps/aki7day_hr.eps", plot=plot_hr, dpi=300)


# v <- ggplot(plot_data_hr,aes(x=hr,y=time,z=risk))
# v + geom_density_2d()
# v + stat_density_2d(geom="raster", aes(fill=..density..),
#                 contour=FALSE)


############# testing interpolation ###############
library(akima)
library(dplyr)
interpdf <-interp2xyz(interp(x=plot_data_hr$hr, y=plot_data_hr$time, z=plot_data_hr$risk, duplicate="mean"), data.frame=TRUE)

colnames(interpdf) <-c('x','y','Risk') 
svg('HR_AKIOverall_risk_for_2039patients_with_trueOutcome.svg')
ggplot(interpdf,aes(x,y,z=Risk)) + geom_tile(aes(fill=Risk)) + 
  scale_fill_gradientn(colours=c("green","red","red2"),na.value = "transparent") + 
  labs(x="Heart Rate (Beats per Minute)",y="Time (mins)",title="AKI-7Day Risk over the HR and time" ) + 
  geom_raster(aes(fill = Risk), interpolate=TRUE) +
  guides(fill=guide_colorbar(barheight = 15)) #+ stat_contour(aes(x=x,y=y,z=Risk),breaks=c(0.34),size=0.5) + 
  #annotate("text",x=c(80,55),y=c(25,150),label=c("Low (<=0.34)","High (>0.34)"),size=5) 
dev.off()
