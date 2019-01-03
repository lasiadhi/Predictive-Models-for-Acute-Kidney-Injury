# plot heatmap risk plot for MAC vs time - smoothout version - FINAL version on 11/30/2017
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


## extract mac range columns from the above dataset along with the outcomes
ind <- which(colnames(data_train_pred)%in%c("mac_mean_base","mac_range_0_1","mac_range_0_1.1","true_AKI_outcome","pred_from_intraPreOp"))
data_train_mac_pred <- data_train_pred[,ind]
# data_train_mac_pred$Predicted_AKI_risk <-  sapply(data_train_mac_pred$pred_from_intraOp, function(x) 
#   ifelse (x>0.34, 1,ifelse(x<=0.34,0)))  #change the cutoff 0.42 based on the model cutoff


# time verctor  (along y-axis on the plot)
time1 <- as.data.frame(seq(from = 0, to = 180, length.out = 181)) ; colnames(time1) <- "duration"


temp_fun <- function(vect_gam,outcome,time1){
  s <- as.formula("outcome~duration") ; fm <- binomial(); fm$link <- "logit"
  data_temp <- as.data.frame(cbind(vect_gam, outcome)) ; colnames(data_temp) <- c("duration","outcome")
  gam_temp <- gam(formula = s, family = fm, data = data_temp) 
  pred <- predict.gam(object = gam_temp, newdata = time1, type = "response")
  return(pred)
}


# create a dataframe with small bin sizes for mac values over the time:
df_mac_time <- data.frame(matrix(nrow=length(data_train_mac_pred$mac_range_0_1.1), ncol=4))
df_mac_time$X1 <- data_train_mac_pred$mac_range_0_1.1
#df_mac_time$X3 <- data_train_mac_pred$mac_range_0_1.1


#interpNA(df_mac_time, method = "linear")
#colnames(plot_data_mac) <- c("mac_43-51","interp1_43-51","interp2_43-51","mac_51-67", "interp1_51-67","interp2_51-67",)

# using timeseries
#t0 <- as.Date("2000-01-01")
#vv = na.spline(zoo(t(df_mac_time),t0+seq_len(ncol(df_mac_time))), na.rm = FALSE)
#t(coredata(vv))


# interpolate mac values accross mac_ranges: use natural cubic spline
myFun<-function(yrow){
  fmm = spline(x = 1:4, y = yrow, xout=1:4, method = "natural")
  return(fmm$y)
}
df_interp <- t(apply(df_mac_time, 1, myFun))  # 1 indicates rows


# interpolate risk for given time for all the ranges
outcome1 <- data_train_mac_pred$true_AKI_outcome
k1 <- temp_fun(df_interp[,1],outcome1,time1)
k2 <- temp_fun(df_interp[,2],outcome1,time1)
k3 <- temp_fun(df_interp[,3],outcome1,time1)
k4 <- temp_fun(df_interp[,4],outcome1,time1)
#k5 <- temp_fun(df_interp[,5],outcome1,time1)



# vector for mac (along x axis on the plot)
mac1 <- as.data.frame(seq(from = 0, to = 1.1, by=0.02)) ; colnames(mac1) <- "MAC"


# select mac_mean_base values in 40 to 90 along with the outcome
#indx <- which(data_train_mac_pred$mac_mean_base<90 & data_train_mac_pred$mac_mean_base>40)


s <- as.formula("outcome~s(MAC,k=5)") ; fm <- binomial(); fm$link <- "logit"
data_temp <- as.data.frame(cbind(data_train_mac_pred$mac_mean_base, outcome1)) ; colnames(data_temp) <- c("MAC","outcome")
gam_temp <- gam(formula = s, family = fm, data = data_temp) 
mac_pred  <- predict.gam(object = gam_temp, newdata = mac1, type = "response")  #risk precitions for mac values from 40 to 80


mac <- seq(from = 0,to = 1.1,by=0.02)
time <- seq(from = 0,to = 180,length.out = 181)


plot_data_mac <- data.frame(matrix(nrow=length(mac)*length(time),ncol=5)) ; colnames(plot_data_mac) <- c("mac","time","risk_mac","risk_time","risk_cor_mac")

k=1
for(i in 1:length(mac)){
  if(mac[i]<0.3){
    temp_time_pred <- k1 
    mid <- 0.15 ; corr <- diff(range(mac_pred[1:16]))/16   #mac_range_0_0.3
  } else if(mac[i]<0.6){
    temp_time_pred <- k2 
    mid <- 0.45 ; corr <- diff(range(mac_pred[14:29]))/16   #mac_range_0.3-0.6
  } else if(mac[i]<0.9){
    temp_time_pred <- k3 
    mid <- 0.75 ; corr <- diff(range(mac_pred[27:42]))/16   # mac_range_0.6-0.9
  } else if(mac[i]<1.1){
    temp_time_pred <- k4
    mid <- 1 ; corr <- diff(range(mac_pred[40:56]))/17   # mac_range_0.9-1.1
  } 
  
  for(j in 1:length(time)){
    plot_data_mac$mac[k] <- mac[i] ; plot_data_mac$time[k] <- time[j]
    plot_data_mac$risk_mac[k] <- mac_pred[i] ; plot_data_mac$risk_time[k] <- temp_time_pred[j]
    plot_data_mac$risk_cor_mac[k] <- temp_time_pred[j]+(mid-mac[i])*corr
    k <- k+1
  }
}



#plot_data_mac$risk <- round((plot_data_mac$risk_mac+plot_data_mac$risk_time)/2,3)
plot_data_mac$risk <- round(plot_data_mac$risk_cor_mac,3)


#threshold risk to [0,1]
indexx <- which(plot_data_mac$risk > 1)
plot_data_mac$risk[indexx] <- 1.0


plot_mac <- ggplot(plot_data_mac,aes(mac,time,z=risk)) + geom_tile(aes(fill=risk)) + 
  scale_fill_gradientn(colours=c("green","red","red2"), na.value = "transparent") + 
  labs(x="Minimum Alveolar Concentration",y="Time (mins)", title="AKI-7Day Risk over the MAC and time" ) + 
  geom_raster(aes(fill = risk), interpolate=TRUE) +
  guides(fill=guide_colorbar(barheight = 15)) #+ stat_contour(aes(x=mac,y=time,z=risk),breaks=c(0.39),size=0.5) +
  #annotate("text",x=c(80,55),y=c(25,150),label=c("Low (<=0.39)","High (>0.39)"),size=5) 
print(plot_mac)

ggsave(filename="Meixian/IDEALIST_IntraOp/Heatmaps/aki7day_mac.png", plot=plot_mac)
ggsave(filename="Meixian/IDEALIST_IntraOp/Heatmaps/aki7day_mac.eps", plot=plot_mac, dpi=300)


# v <- ggplot(plot_data_mac,aes(x=mac,y=time,z=risk))
# v + geom_density_2d()
# v + stat_density_2d(geom="raster", aes(fill=..density..),
#                 contour=FALSE)


############# testing interpolation ###############
library(akima)
library(dplyr)
interpdf <-interp2xyz(interp(x=plot_data_mac$mac, y=plot_data_mac$time, z=plot_data_mac$risk, duplicate="mean"), data.frame=TRUE)

colnames(interpdf) <-c('x','y','Risk') 
svg('MAC_AKIOverall_risk_for_2039patients_with_trueOutcome.svg')
ggplot(interpdf,aes(x,y,z=Risk)) + geom_tile(aes(fill=Risk)) + 
  scale_fill_gradientn(colours=c("green","red","red2"),na.value = "transparent") + 
  labs(x="Minimum Alveolar Concentration",y="Time (mins)",title="AKI-7Day Risk over the MAC and time" ) + 
  geom_raster(aes(fill = Risk), interpolate=TRUE) +
  guides(fill=guide_colorbar(barheight = 15)) #+ stat_contour(aes(x=x,y=y,z=Risk),breaks=c(0.34),size=0.5) + 
  #annotate("text",x=c(80,55),y=c(25,150),label=c("Low (<=0.34)","High (>0.34)"),size=5) 
dev.off()
