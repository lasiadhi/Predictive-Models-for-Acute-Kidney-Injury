# 3D plot: Time MAC less than 0.8 sv Min BP vs Max HR (for all )
# by Lasith Adhikari

#load libraries

library(mgcv)
library(scatterplot3d)
library(pROC)

#setwd("/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model")
setwd('S:/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model')

# read feature data
data_all <- read.csv("Data/features_to_model_withPF.csv",header = TRUE)

# read predictions for train data
#data_pred <- read.csv("Results/AKI_3Day/IntraOp+PreOp_all_features/aki3Day_intraOp+preOP_prediction_from_train2039Patients.csv",header = TRUE)

# read predictions for test data
#data_pred <- read.csv("Results/AKI_3Day/IntraOp+PreOp_all_features/aki3Day_intraOP+preOP_prediction_from_test874.csv",header = TRUE)
#data_train_pred <- merge(data_all, data_pred, by.x='acc', by.y='acc')


data_temp <- as.data.frame(cbind(data_all$map_Min, data_all$hr_Max, data_all$mac_range_0_1.1, data_all$aki7day))  # change here for the outcome
colnames(data_temp) <- c("MAP_min","HR_max","MAC_0_1.1","outcome")

s1 <- as.formula("outcome~s(MAP_min,k=5)+s(MAC_0_1.1,k=5)+s(HR_max,k=5)")
fm <- binomial(); fm$link <- "logit" 
model1 <- gam(formula = s1,family = fm,data = data_temp) 
summary(model1)

#ROC analysis to compute the cutoff
rocobj <- roc(data_temp$outcome, model1$fitted.values)
optimal_cutoff <- coords(rocobj, x="best", input="threshold", best.method="youden")[[1]] #optimal cutoff based on the Youden index

risk <- cut(model1$fitted.values,breaks = c(0,optimal_cutoff,1)) # CHANGE HERE. we can use prevalence = mean(outcome) as the cutoff as well


# AKI-3day/7-day/overall - change the title accordingly
svg('MAP_MAC_HR_AKI7day_risk_for 2913patients_with_trueOutcome_cutoff_Youden.svg')
scatterplot3d(x = model1$model$MAP_min, y = model1$model$HR_max, z=model1$model$MAC_0_1.1,
              color = c("green","red")[risk],xlab="Minimum MAP (mmHG)",ylab="Maximum Heart Rate (bpm)",
              zlab="Time MAC less than 1.1 (mins)",angle=20, main="Predicted Risk for AKI-7Day", xlim = c(0,120), zlim=c(0,1800),box=FALSE, pch = 19)
dev.off()

