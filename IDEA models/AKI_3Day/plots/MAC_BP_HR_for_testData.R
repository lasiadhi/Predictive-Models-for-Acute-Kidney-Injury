# 3D plot: Time MAC less than 1 vs Min BP vs Max HR (for all )
# by Lasith Adhikari

#load libraries

library(mgcv)
library(scatterplot3d)
library(pROC)

setwd("/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model")
#setwd('S:/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model')


# read feature data from training cohort:
data_all <- read.csv("Data/aki3_drop_esrd1_patients/aki3Day_X_train.csv",header = TRUE)


# plot MAC v BP vs HR with true AKI outcome:
# read true outcome
outcome<- read.csv("Data/aki3_drop_esrd1_patients/aki3Day_y_train.csv",header = TRUE)

data_temp <- as.data.frame(cbind(data_all$map_Min, data_all$hr_Max, data_all$mac_range_0_1, outcome$aki3day))  # change here for the outcome
colnames(data_temp) <- c("MAP_min","HR_max","MAC_0_1","outcome")


# AKI-3day/7-day/overall - change the title accordingly
#svg('MAP_MAC_HR_AKI3day_risk_for 2913patients_with_trueOutcome_cutoff_Youden.svg')
scatterplot3d(x = data_temp$MAP_min, y = data_temp$HR_max, z= data_temp$MAC_0_1,
              color = ifelse(outcome$aki3day==1, "red", "green"),xlab="Minimum MAP (mmHG)",ylab="Maximum Heart Rate (bpm)",
              zlab="Time MAC less than 1.0 (mins)",angle=20, main="True AKI-3Day with MAC, MAP, and HR", xlim = c(0,120), zlim=c(0,1800),box=FALSE, pch = 19)
#dev.off()


######################## using test data ############################

# read feature data from testing cohort:
data_all <- read.csv("Data/aki3_drop_esrd1_patients/aki3Day_X_test.csv",header = TRUE)

# plot MAC v BP vs HR with AKI prediction:
# read predictions from our model:
prediction<- read.csv("Results/AKI_3Day/IntraOp+PreOp_probs_probs_drop_esrd/aki3Day_prediction_from_intraOP+preOP_probs_from_test873.csv",header = TRUE)
optimal_cutoff = 0.44 # CHANGE HERE


data_temp <- as.data.frame(cbind(data_all$map_Min, data_all$hr_Max, data_all$mac_range_0_1, prediction$pred_from_intraOp))  # change here for the outcome
colnames(data_temp) <- c("MAP_min","HR_max","MAC_0_1","prediction")

risk <- cut(data_temp$prediction,breaks = c(0,optimal_cutoff,1))

# AKI-3day/7-day/overall - change the title accordingly
#svg('MAP_MAC_HR_AKI3day_risk_for 2913patients_with_trueOutcome_cutoff_Youden.svg')
scatterplot3d(x = data_temp$MAP_min, y = data_temp$HR_max, z= data_temp$MAC_0_1,
              color = c("green","red")[risk],xlab="Minimum MAP (mmHG)",ylab="Maximum Heart Rate (bpm)",
              zlab="Time MAC less than 1.0 (mins)",angle=20, main="Predicted risk for AKI-3Day for testing cohort", xlim = c(0,120), zlim=c(0,1800),box=FALSE, pch = 19)
#dev.off()


