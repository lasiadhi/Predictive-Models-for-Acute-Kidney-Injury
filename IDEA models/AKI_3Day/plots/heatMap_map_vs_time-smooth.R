# plot heatmap risk plot for MAP - smoothout version

#load libraries
library(ggplot2)
library(reshape2)
library(mgcv)

setwd("/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model")
#setwd('S:/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model')

# read feature data
data_all <- read.csv("Data/features_to_model_withPF.csv",header = TRUE)

# read predictions for train data
#data_pred <- read.csv("Results/AKI_3Day/IntraOp+PreOp_all_features/aki3Day_intraOp+preOP_prediction_from_train2039Patients.csv",header = TRUE)

# read predictions for test data
data_pred <- read.csv("Results/AKI_3Day/IntraOp+PreOp_all_features/aki3Day_intraOP+preOP_prediction_from_test874.csv",header = TRUE)
data_train_pred <- merge(data_all, data_pred, by.x='acc', by.y='acc')

## extract map range columns from the above dataset along with the outcomes
ind <- which(colnames(data_train_pred)%in%c("map_mean_base","map_range_43_51","map_range_51_67","map_range_67_75","map_range_67_75","map_range_75_91","true_AKI_outcome", "pred_from_intraOp"))
data_train_map_pred <- data_train_pred[,ind]
data_train_map_pred$Predicted_AKI_risk <-  sapply(data_train_map_pred$pred_from_intraOp, function(x) 
  ifelse (x>0.42, 1,ifelse(x<=0.42,0)))  #change the cutoff 0.42 based on the model cutoff

# time verctor  (along y-axis on the plot)
time1 <- as.data.frame(seq(from = 0,to = 180,length.out = 181)) ; colnames(time1) <- "duration"

temp_fun <- function(vect_gam,outcome,time1){
  s <- as.formula("outcome~duration") ; fm <- binomial(); fm$link <- "logit"
  data_temp <- as.data.frame(cbind(vect_gam,outcome)) ; colnames(data_temp) <- c("duration","outcome")
  gam_temp <- gam(formula = s,family = fm,data = data_temp) 
  pred <- predict.gam(object = gam_temp,newdata = time1,type = "response")
  return(pred)
}

# interpolate risk for given time for all the ranges
outcome1 <- data_train_map_pred$Predicted_AKI_risk
k1 <- temp_fun(data_train_map_pred$map_range_43_51,outcome1,time1)
k2 <- temp_fun(data_train_map_pred$map_range_51_67,outcome1,time1)
k3 <- temp_fun(data_train_map_pred$map_range_67_75,outcome1,time1)
k4 <- temp_fun(data_train_map_pred$map_range_75_91,outcome1,time1)


# vector for map (along x axis on the plot)
map1 <- as.data.frame(seq(from = 43,to = 91,length.out = 49)) ; colnames(map1) <- "MAP"

# select map_mean_base values in 40 to 90 along with the outcome
#indx <- which(data_train_map_pred$map_mean_base<90 & data_train_map_pred$map_mean_base>40)

s <- as.formula("outcome~s(MAP,k=5)") ; fm <- binomial(); fm$link <- "logit"
data_temp <- as.data.frame(cbind(data_train_map_pred$map_mean_base,outcome1)) ; colnames(data_temp) <- c("MAP","outcome")
gam_temp <- gam(formula = s,family = fm,data = data_temp) 
map_pred  <- predict.gam(object = gam_temp,newdata = map1,type = "response")  #risk precitions for map values from 40 to 80

map <- seq(from = 43,to = 91,length.out = 49)
time <- seq(from = 0,to = 180,length.out = 181)

plot_data1 <- data.frame(matrix(nrow=length(map)*length(time),ncol=5)) ; colnames(plot_data1) <- c("map","time","risk_map","risk_time","risk_cor_map")
k=1
for(i in 1:length(map)){
  if(map[i]<51){
    temp_time_pred <- k1 
    mid <- 46 ; corr <- diff(range(map_pred[1:8]))/8   #map_range_43_51
  }
  
  if(map[i]<67 & map[i]>=51){
    temp_time_pred <- k2 
    mid <- 58 ; corr <- diff(range(map_pred[9:24]))/16   #map_range_51_67
  }
  
  if(map[i]<75&map[i]>=67){
    temp_time_pred <- k3 
    mid <- 70 ; corr <- diff(range(map_pred[25:32]))/8   # map_range_67_75
  }
  
  if(map[i]<91 & map[i]>=75){
    temp_time_pred <- k4
    mid <- 82 ; corr <- diff(range(map_pred[33:48]))/16   #map_range_75_91
  }
  
  for(j in 1:length(time)){
    plot_data1$map[k] <- map[i] ; plot_data1$time[k] <- time[j]
    plot_data1$risk_map[k] <- map_pred[i] ; plot_data1$risk_time[k] <- temp_time_pred[j]
    plot_data1$risk_cor_map[k] <- temp_time_pred[j]+(mid-map[i])*corr
    k <- k+1
  }
}



#plot_data1$risk <- round((plot_data1$risk_map+plot_data1$risk_time)/2,3)
plot_data1$risk <- round(plot_data1$risk_cor_map,3)

#threshold risk to [0,1]
indexx <- which(plot_data1$risk > 1)
plot_data1$risk[indexx] <- 1.0

ggplot(plot_data1,aes(map,time,z=risk)) + geom_tile(aes(fill=risk)) + 
  scale_fill_gradientn(colours=c("green4","green","orange","red","red2"),na.value = "transparent") + 
  labs(x="Mean Arterial Blood pressure (mmHG)",y="Time (mins)",title="AKI-3Day Risk from IntraOp+PreOp Model" ) + 
  geom_raster(aes(fill = risk), interpolate=TRUE) +
  guides(fill=guide_colorbar(barheight = 15)) + stat_contour(aes(x=map,y=time,z=risk),breaks=c(0.42),size=0.5) + 
  annotate("text",x=c(80,55),y=c(25,150),label=c("Low (<=0.42)","High (>0.42)"),size=5) 


v <- ggplot(plot_data1,aes(x=map,y=time,z=risk))
v + geom_contour(aes(colour = ..level..))
v + geom_contour(bins=100, binwidth = 0.00001, aes(colour = ..level..))
v + geom_density_2d(bins=10, binwidth = 0.00001)
v + geom_raster(aes(fill = risk), interpolate=TRUE)


  
  ############# testing interpolation ###############
  library(akima)
  library(dplyr)
  interpdf <-interp2xyz(interp(x=plot_data1$map, y=plot_data1$time, z=plot_data1$risk, duplicate="mean"), data.frame=TRUE)
  
  
  ggplot(interpdf,aes(x,y,z=z)) + geom_tile(aes(fill=z)) + 
    scale_fill_gradientn(colours=c("green4","green","orange","red","red2"),na.value = "transparent") + 
    labs(x="Mean Arterial Blood pressure (mmHG)",y="Time (mins)",title="AKI-3Day Risk from IntraOp+PreOp Model" ) + 
    geom_raster(aes(fill = z), interpolate=TRUE) +
    guides(fill=guide_colorbar(barheight = 15)) + stat_contour(aes(x=x,y=y,z=z),breaks=c(0.42),size=0.5) + 
    annotate("text",x=c(80,55),y=c(25,150),label=c("Low (<=0.42)","High (>0.42)"),size=5) 
