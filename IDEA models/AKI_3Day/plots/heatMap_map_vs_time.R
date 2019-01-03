# plot heatmap risk plot for MAP
library(ggplot2)
library(reshape2)



setwd("/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model")
#setwd('S:/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model')

# read feature data
data_all <- read.csv("Data/features_to_model_withPF.csv",header = TRUE)

# read predictions for train data
#data_pred <- read.csv("Results/AKI_3Day/IntraOp+PreOp_all_features/aki3Day_intraOp+preOP_prediction_from_train2039Patients.csv",header = TRUE)

# read predictions for test data
data_pred <- read.csv("Results/AKI_3Day/IntraOp+PreOp_all_features/aki3Day_intraOP+preOP_prediction_from_test874.csv",header = TRUE)


data_train_pred <- merge(data_all, data_pred)


## extract map range columns from the above dataset
ind <- which(colnames(data_train_pred)%in%c("map_range_43_51","map_range_51_67","map_range_67_75","map_range_67_75","map_range_75_91", "pred_from_intraOp"))

# extract map percentage columns from the above dataset
#ind <- which(colnames(data_train_pred)%in%c("map_perct_range_43_75","map_perct_range_51_67","map_perct_range_67_75","map_perct_range_67_75","map_perct_range_75_91", "pred_from_intraOp"))

data_train_map_pred <- data_train_pred[,ind]

# apply cutoff:
data_train_map_pred$risk <-  sapply(data_train_map_pred$pred_from_intraOp, function(x) 
   ifelse (x>0.42, 1,ifelse(x<=0.42,0)))

data_melt <- melt(data_train_map_pred, id.vars = c('pred_from_intraOp', 'risk'))
data_melt$risk = factor(data_melt$risk)
data_melt$variable <- factor(data_melt$variable)


#working - train
ggplot(data_melt,aes(variable,value,z=pred_from_intraOp_train)) + geom_tile(aes(fill=pred_from_intraOp_train),na.rm = TRUE, show.legend = TRUE) +
  scale_fill_gradientn(colours=c("green","orange","red"),na.value = "transparent") + 
  labs(x="Mean Arterial Blood pressure (mmHG)",y="Time (mins)",title="Risk Plot for training cohort" ) + 
  guides(fill=guide_colorbar(barheight = 10)) 
  #ylim(0, 200)

# for test
#svg('MAP_ranges_AKI3Day_Preop+IntraOpModel_874testData_zoom.svg')
ggplot(data_melt,aes(variable,value,z=pred_from_intraOp)) + geom_tile(aes(fill=pred_from_intraOp),na.rm = TRUE, show.legend = TRUE) +
  scale_fill_gradientn(colours=c("green","orange","red"),na.value = "transparent") + 
  labs(x="Mean Arterial Blood pressure (mmHG)",y="Time (mins)",title="Risk Plot for test cohort" ) + 
  guides(fill=guide_colorbar(barwidth = 1, barheight = 10)) +ylim(0,120)
#dev.off()

# for test - for risk 
ggplot(data_melt,aes(variable,value,z=risk)) + geom_tile(aes(fill=risk),na.rm = TRUE, show.legend = TRUE) +
  scale_fill_manual(values = c( "green","red")) +
  labs(x="Mean Arterial Blood pressure (mmHG)",y="Time (mins)",title="Risk Plot for test cohort" ) 




