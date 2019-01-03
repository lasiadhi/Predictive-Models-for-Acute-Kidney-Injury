# plot heatmap risk plot for MAP vs time - smoothout version - FINAL version on 11/30/2017
# for AKI-7day

#load libraries
library(ggplot2)
library(reshape2)
library(mgcv)

#library(timeSeries)
#library(zoo)


#setwd("/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model")
setwd('S:/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users')

# read feature data - use BP_MAP_features instead
#data_all <- read.csv("Lasith/IntraOp_model-DECLARE/Model/Data/features_to_model_withPF_moreClean.csv", header=TRUE)
data_map_features <- read.csv("Lasith/IntraOp_model-DECLARE/Time_series/Final_feature/Inv_MAP/BP_MAP_features_ver1.csv", header=TRUE)
names(data_map_features)[2:23] <- paste("map", names(data_map_features)[2:23], sep="_")

# read predictions for train data
data_pred <- read.csv("Lasith/IntraOp_model-DECLARE/Model/Results/AKI_7Day/IntraOp+PreOp_probs_drop_esrd/aki7Day_prediction_from_intraOP+preOP_probs_from_train2038.csv", header=TRUE)

# read predictions for test data
#data_pred <- read.csv("Lasith/IntraOp_model-DECLARE/Model/Results/AKI_3Day/IntraOp+PreOp_all_features/aki3Day_intraOP+preOP_prediction_from_test874.csv",header = TRUE)
data_train_pred <- merge(data_map_features, data_pred, by.x='Acc_No', by.y='acc')

#colnames(data_train_pred)


## extract map range columns from the above dataset along with the outcomes
ind <- which(colnames(data_train_pred)%in%c("map_mean_base","map_range_33_53","map_range_53_64","map_range_64_85","map_range_85_95","map_range_95_116","true_AKI_outcome","pred_from_intraPreOp"))
data_train_map_pred <- data_train_pred[,ind]
# data_train_map_pred$Predicted_AKI_risk <-  sapply(data_train_map_pred$pred_from_intraOp, function(x) 
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


# create a dataframe with small bin sizes for map values over the time:
df_map_time <- data.frame(matrix(nrow=length(data_train_map_pred$map_range_53_64), ncol=9))
df_map_time$X1 <- data_train_map_pred$map_range_33_53
df_map_time$X3 <- data_train_map_pred$map_range_53_64
df_map_time$X5 <- data_train_map_pred$map_range_64_85
df_map_time$X7 <- data_train_map_pred$map_range_85_95
df_map_time$X9 <- data_train_map_pred$map_range_95_116
#interpNA(df_map_time, method = "linear")
#colnames(plot_data_map) <- c("map_43-51","interp1_43-51","interp2_43-51","map_51-67", "interp1_51-67","interp2_51-67",)

# using timeseries
#t0 <- as.Date("2000-01-01")
#vv = na.spline(zoo(t(df_map_time),t0+seq_len(ncol(df_map_time))), na.rm = FALSE)
#t(coredata(vv))


# interpolate map values accross map_ranges: use natural cubic spline
myFun<-function(yrow){
  fmm = spline(x = 1:9, y = yrow, xout=1:9, method = "natural")
  return(fmm$y)
}
df_interp <- t(apply(df_map_time, 1, myFun))  # 1 indicates rows


# interpolate risk for given time for all the ranges
outcome1 <- data_train_map_pred$true_AKI_outcome
k1 <- temp_fun(df_interp[,1],outcome1,time1)
k2 <- temp_fun(df_interp[,2],outcome1,time1)
k3 <- temp_fun(df_interp[,3],outcome1,time1)
k4 <- temp_fun(df_interp[,4],outcome1,time1)
k5 <- temp_fun(df_interp[,5],outcome1,time1)
k6 <- temp_fun(df_interp[,6],outcome1,time1)
k7 <- temp_fun(df_interp[,7],outcome1,time1)
k8 <- temp_fun(df_interp[,8],outcome1,time1)
k9 <- temp_fun(df_interp[,9],outcome1,time1)


# vector for map (along x axis on the plot)
map1 <- as.data.frame(seq(from = 33,to = 116, length.out = 84)) ; colnames(map1) <- "MAP"


# select map_mean_base values in 40 to 90 along with the outcome
#indx <- which(data_train_map_pred$map_mean_base<90 & data_train_map_pred$map_mean_base>40)


s <- as.formula("outcome~s(MAP,k=5)") ; fm <- binomial(); fm$link <- "logit"
data_temp <- as.data.frame(cbind(data_train_map_pred$map_mean_base,outcome1)) ; colnames(data_temp) <- c("MAP","outcome")
gam_temp <- gam(formula = s,family = fm,data = data_temp) 
map_pred  <- predict.gam(object = gam_temp,newdata = map1,type = "response")  #risk precitions for map values from 40 to 80


map <- seq(from = 33,to = 116,length.out = 84)
time <- seq(from = 0,to = 500,length.out = 501)


plot_data_map <- data.frame(matrix(nrow=length(map)*length(time),ncol=5)) ; colnames(plot_data_map) <- c("map","time","risk_map","risk_time","risk_cor_map")
k=1
for(i in 1:length(map)){
  if(map[i]<42){
    temp_time_pred <- k1 
    mid <- 38 ; corr <- diff(range(map_pred[1:10]))/10   #map_range_33_42
  } else if(map[i]<51){
    temp_time_pred <- k2 
    mid <- 47 ; corr <- diff(range(map_pred[18:27]))/20   #map_range_42-51
  } else if(map[i]<61){
    temp_time_pred <- k3 
    mid <- 56 ; corr <- diff(range(map_pred[26:32]))/7   # map_range_51-61
  } else if(map[i]<71){
    temp_time_pred <- k4
    mid <- 66 ; corr <- diff(range(map_pred[31:34]))/4   # map_range_61-71
  } else if(map[i]<80){
    temp_time_pred <- k5
    mid <- 75 ; corr <- diff(range(map_pred[33:36]))/4   # map_range_71-80
  } else if(map[i]<89){
    temp_time_pred <- k6
    mid <- 84 ; corr <- diff(range(map_pred[35:42]))/8   # map_range_80-89
  } else if(map[i]<98){
    temp_time_pred <- k7
    mid <- 93 ; corr <- diff(range(map_pred[41:65]))/20   # map_range_89-98
  } else if(map[i]<107){
    temp_time_pred <- k8
    mid <- 102 ; corr <- diff(range(map_pred[63:75]))/13   # map_range_98-107
  } else if(map[i]<116){
    temp_time_pred <- k9
    mid <- 111 ; corr <- diff(range(map_pred[73:84]))/12   # map_range_107-116
  }
  
  for(j in 1:length(time)){
    plot_data_map$map[k] <- map[i] ; plot_data_map$time[k] <- time[j]
    plot_data_map$risk_map[k] <- map_pred[i] ; plot_data_map$risk_time[k] <- temp_time_pred[j]
    plot_data_map$risk_cor_map[k] <- temp_time_pred[j]+(mid-map[i])*corr
    k <- k+1
  }
}



#plot_data_map$risk <- round((plot_data_map$risk_map+plot_data_map$risk_time)/2,3)
plot_data_map$risk <- round(plot_data_map$risk_cor_map,3)


#threshold risk to [0,1]
indexx <- which(plot_data_map$risk > 1)
plot_data_map$risk[indexx] <- 1.0


plot_map <- ggplot(plot_data_map,aes(map,time,z=risk)) + geom_tile(aes(fill=risk)) + 
  scale_fill_gradientn(colours=c("green","red","red2"), na.value = "transparent") + 
  labs(x="Mean Arterial Blood Pressure (mmHG)",y="Time (mins)", title="AKI-7Day Risk over the MAP and time" ) + 
  geom_raster(aes(fill = risk), interpolate=TRUE) +
  guides(fill=guide_colorbar(barheight = 15)) #+ stat_contour(aes(x=map,y=time,z=risk),breaks=c(0.39),size=0.5) +
  #annotate("text",x=c(80,55),y=c(25,150),label=c("Low (<=0.39)","High (>0.39)"),size=5) 
print(plot_map)

ggsave(filename="Meixian/IDEALIST_IntraOp/Heatmaps/aki7day_map.png", plot=plot_map)
ggsave(filename="Meixian/IDEALIST_IntraOp/Heatmaps/aki7day_map.eps", plot=plot_map, dpi=300)


# v <- ggplot(plot_data_map,aes(x=map,y=time,z=risk))
# v + geom_density_2d()
# v + stat_density_2d(geom="raster", aes(fill=..density..),
#                 contour=FALSE)


############# testing interpolation ###############
library(akima)
library(dplyr)
interpdf <-interp2xyz(interp(x=plot_data_map$map, y=plot_data_map$time, z=plot_data_map$risk, duplicate="mean"), data.frame=TRUE)

colnames(interpdf) <-c('x','y','Risk') 
svg('MAP_AKIOverall_risk_for_2039patients_with_trueOutcome.svg')
ggplot(interpdf,aes(x,y,z=Risk)) + geom_tile(aes(fill=Risk)) + 
  scale_fill_gradientn(colours=c("green","red","red2"),na.value = "transparent") + 
  labs(x="Mean Arterial Blood Pressure (mmHG)",y="Time (mins)",title="AKI-7Day Risk over the MAP and time" ) + 
  geom_raster(aes(fill = Risk), interpolate=TRUE) +
  guides(fill=guide_colorbar(barheight = 15)) #+ stat_contour(aes(x=x,y=y,z=Risk),breaks=c(0.34),size=0.5) + 
  #annotate("text",x=c(80,55),y=c(25,150),label=c("Low (<=0.34)","High (>0.34)"),size=5) 
dev.off()
