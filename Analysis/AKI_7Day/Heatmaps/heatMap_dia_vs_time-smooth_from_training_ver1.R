# plot heatmap risk plot for DIA vs time - smoothout version - FINAL version on 11/30/2017
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


## extract dia range columns from the above dataset along with the outcomes
ind <- which(colnames(data_train_pred)%in%c("dia_mean_base","dia_range_26_43","dia_range_43_51","dia_range_51_67","dia_range_67_75","dia_range_75_91","true_AKI_outcome","pred_from_intraPreOp"))
data_train_dia_pred <- data_train_pred[,ind]
# data_train_dia_pred$Predicted_AKI_risk <-  sapply(data_train_dia_pred$pred_from_intraOp, function(x) 
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


# create a dataframe with small bin sizes for dia values over the time:
df_dia_time <- data.frame(matrix(nrow=length(data_train_dia_pred$dia_range_43_51), ncol=9))
df_dia_time$X1 <- data_train_dia_pred$dia_range_26_43
df_dia_time$X3 <- data_train_dia_pred$dia_range_43_51
df_dia_time$X5 <- data_train_dia_pred$dia_range_51_67
df_dia_time$X7 <- data_train_dia_pred$dia_range_67_75
df_dia_time$X9 <- data_train_dia_pred$dia_range_75_91
#interpNA(df_dia_time, method = "linear")
#colnames(plot_data_dia) <- c("dia_43-51","interp1_43-51","interp2_43-51","dia_51-67", "interp1_51-67","interp2_51-67",)

# using timeseries
#t0 <- as.Date("2000-01-01")
#vv = na.spline(zoo(t(df_dia_time),t0+seq_len(ncol(df_dia_time))), na.rm = FALSE)
#t(coredata(vv))


# interpolate dia values accross dia_ranges: use natural cubic spline
myFun<-function(yrow){
  fmm = spline(x = 1:9, y = yrow, xout=1:9, method = "natural")
  return(fmm$y)
}
df_interp <- t(apply(df_dia_time, 1, myFun))  # 1 indicates rows


# interpolate risk for given time for all the ranges
outcome1 <- data_train_dia_pred$true_AKI_outcome
k1 <- temp_fun(df_interp[,1],outcome1,time1)
k2 <- temp_fun(df_interp[,2],outcome1,time1)
k3 <- temp_fun(df_interp[,3],outcome1,time1)
k4 <- temp_fun(df_interp[,4],outcome1,time1)
k5 <- temp_fun(df_interp[,5],outcome1,time1)
k6 <- temp_fun(df_interp[,6],outcome1,time1)
k7 <- temp_fun(df_interp[,7],outcome1,time1)
k8 <- temp_fun(df_interp[,8],outcome1,time1)
k9 <- temp_fun(df_interp[,9],outcome1,time1)



# vector for dia (along x axis on the plot)
dia1 <- as.data.frame(seq(from = 26, to = 91, length.out = 66)) ; colnames(dia1) <- "DIA"


# select dia_mean_base values in 40 to 90 along with the outcome
#indx <- which(data_train_dia_pred$dia_mean_base<90 & data_train_dia_pred$dia_mean_base>40)


s <- as.formula("outcome~s(DIA,k=5)") ; fm <- binomial(); fm$link <- "logit"
data_temp <- as.data.frame(cbind(data_train_dia_pred$dia_mean_base, outcome1)) ; colnames(data_temp) <- c("DIA","outcome")
gam_temp <- gam(formula = s, family = fm, data = data_temp) 
dia_pred  <- predict.gam(object = gam_temp, newdata = dia1, type = "response")  #risk precitions for dia values from 40 to 80


dia <- seq(from = 26,to = 91,length.out = 66)
time <- seq(from = 0,to = 500,length.out = 501)


plot_data_dia <- data.frame(matrix(nrow=length(dia)*length(time),ncol=5)) ; colnames(plot_data_dia) <- c("dia","time","risk_dia","risk_time","risk_cor_dia")

k=1
for(i in 1:length(dia)){
  if(dia[i]<32){
    temp_time_pred <- k1 
    mid <- 29 ; corr <- diff(range(dia_pred[1:13]))/13   #dia_range_26_32
  } else if(dia[i]<38){
    temp_time_pred <- k2 
    mid <- 35 ; corr <- diff(range(dia_pred[11:19]))/9   #dia_range_32-38
  } else if(dia[i]<46){
    temp_time_pred <- k3 
    mid <- 42 ; corr <- diff(range(dia_pred[18:21]))/4   # dia_range_38-46
  } else if(dia[i]<54){
    temp_time_pred <- k4 
    mid <- 50 ; corr <- diff(range(dia_pred[20:30]))/12   #dia_range_46_54
  } else if(dia[i]<62){
    temp_time_pred <- k5 
    mid <- 58 ; corr <- diff(range(dia_pred[28:40]))/14   #dia_range_54-62
  } else if(dia[i]<69){
    temp_time_pred <- k6 
    mid <- 65 ; corr <- diff(range(dia_pred[38:47]))/10   # dia_range_62-69
  } else if(dia[i]<77){
    temp_time_pred <- k7
    mid <- 73 ; corr <- diff(range(dia_pred[46:49]))/4   # dia_range_69-77
  } else if(dia[i]<85){
    temp_time_pred <- k8
    mid <- 81 ; corr <- diff(range(dia_pred[48:51]))/4   # dia_range_77-85
  } else if(dia[i]<91){
    temp_time_pred <- k9
    mid <- 88 ; corr <- diff(range(dia_pred[50:66]))/17   # dia_range_85-91
  }
  
  for(j in 1:length(time)){
    plot_data_dia$dia[k] <- dia[i] ; plot_data_dia$time[k] <- time[j]
    plot_data_dia$risk_dia[k] <- dia_pred[i] ; plot_data_dia$risk_time[k] <- temp_time_pred[j]
    plot_data_dia$risk_cor_dia[k] <- temp_time_pred[j]+(mid-dia[i])*corr
    k <- k+1
  }
}



#plot_data_dia$risk <- round((plot_data_dia$risk_dia+plot_data_dia$risk_time)/2,3)
plot_data_dia$risk <- round(plot_data_dia$risk_cor_dia,3)


#threshold risk to [0,1]
indexx <- which(plot_data_dia$risk > 1)
plot_data_dia$risk[indexx] <- 1.0


plot_dia <- ggplot(plot_data_dia,aes(dia,time,z=risk)) + geom_tile(aes(fill=risk)) + 
  scale_fill_gradientn(colours=c("green","red","red2"), na.value = "transparent") + 
  labs(x="Diastolic Blood Pressure (mmHG)",y="Time (mins)", title="AKI-7Day Risk over the DIA and time" ) + 
  geom_raster(aes(fill = risk), interpolate=TRUE) +
  guides(fill=guide_colorbar(barheight = 15)) #+ stat_contour(aes(x=dia,y=time,z=risk),breaks=c(0.39),size=0.5) +
  #annotate("text",x=c(80,55),y=c(25,150),label=c("Low (<=0.39)","High (>0.39)"),size=5) 
print(plot_dia)

ggsave(filename="Meixian/IDEALIST_IntraOp/Heatmaps/aki7day_dia.png", plot=plot_dia)
ggsave(filename="Meixian/IDEALIST_IntraOp/Heatmaps/aki7day_dia.eps", plot=plot_dia, dpi=300)


# v <- ggplot(plot_data_dia,aes(x=dia,y=time,z=risk))
# v + geom_density_2d()
# v + stat_density_2d(geom="raster", aes(fill=..density..),
#                 contour=FALSE)


############# testing interpolation ###############
library(akima)
library(dplyr)
interpdf <-interp2xyz(interp(x=plot_data_dia$dia, y=plot_data_dia$time, z=plot_data_dia$risk, duplicate="mean"), data.frame=TRUE)

colnames(interpdf) <-c('x','y','Risk') 
svg('DIA_AKIOverall_risk_for_2039patients_with_trueOutcome.svg')
ggplot(interpdf,aes(x,y,z=Risk)) + geom_tile(aes(fill=Risk)) + 
  scale_fill_gradientn(colours=c("green","red","red2"),na.value = "transparent") + 
  labs(x="Diastolic Blood Pressure (mmHG)",y="Time (mins)",title="AKI-7Day Risk over the DIA and time" ) + 
  geom_raster(aes(fill = Risk), interpolate=TRUE) +
  guides(fill=guide_colorbar(barheight = 15)) #+ stat_contour(aes(x=x,y=y,z=Risk),breaks=c(0.34),size=0.5) + 
  #annotate("text",x=c(80,55),y=c(25,150),label=c("Low (<=0.34)","High (>0.34)"),size=5) 
dev.off()
