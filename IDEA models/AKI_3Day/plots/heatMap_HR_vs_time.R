# plot heatmap risk plot for HR
library(ggplot2)
library(reshape2)

setwd('S:/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model')

# read feature data
data_all <- read.csv("Data/features_to_model_withPF.csv",header = TRUE)

# read predictions for train data
#data_pred <- read.csv("Results/AKI_3Day/IntraOp+PreOp_all_features/aki3Day_intraOp+preOP_prediction_from_train2039Patients.csv",header = TRUE)

# read predictions for test data
data_pred <- read.csv("Results/AKI_3Day/IntraOp+PreOp_all_features/aki3Day_intraOP+preOP_prediction_from_test874.csv",header = TRUE)


data_train_pred <- merge(data_all, data_pred)


# extract HR percentage columns from the above dataset
ind <- which(colnames(data_train_pred)%in%c("hr_range_93_104",'hr_range_104_127', "pred_from_intraOp"))

data_train_map_pred <- data_train_pred[,ind]

# apply cutoff:
data_train_map_pred$risk <-  sapply(data_train_map_pred$pred_from_intraOp, function(x) 
   ifelse (x>0.42, 1,ifelse(x<=0.42,0)))

data_melt <- melt(data_train_map_pred, id.vars = c('pred_from_intraOp', 'risk'))
data_melt$risk = factor(data_melt$risk)
data_melt$variable <- factor(data_melt$variable)




# for test
svg('HR_ranges_AKI3Day_Preop+IntraOpModel_874testData.svg')
ggplot(data_melt,aes(variable,value,z=pred_from_intraOp)) + geom_tile(aes(fill=pred_from_intraOp),na.rm = TRUE, show.legend = TRUE) +
  scale_fill_gradientn(colours=c("green","orange","red"),na.value = "transparent") + 
  labs(x="Heart Rate (BPM)",y="Time (mins)",title="Risk Plot for test cohort" ) + 
  guides(fill=guide_colorbar(barwidth = 1, barheight = 10)) +ylim(0,100)
dev.off()


###############################

#working - train
ggplot(data_melt,aes(variable,value,z=pred_from_intraOp_train)) + geom_tile(aes(fill=pred_from_intraOp_train),na.rm = TRUE, show.legend = TRUE) +
  scale_fill_gradientn(colours=c("green","orange","red"),na.value = "transparent") + 
  labs(x="Mean Arterial Blood pressure (mmHG)",y="Time (mins)",title="Risk Plot for training cohort" ) + 
  guides(fill=guide_colorbar(barheight = 10)) 
#ylim(0, 200)


# for test - for risk 
ggplot(data_melt,aes(variable,value,z=risk)) + geom_tile(aes(fill=risk),na.rm = TRUE, show.legend = TRUE) +
  scale_fill_manual(values = c( "green","red")) +
  labs(x="Mean Arterial Blood pressure (mmHG)",y="Time (mins)",title="Risk Plot for test cohort" ) 

