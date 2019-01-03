# Lasith Adhikari
# This script is used to compute AUC with 95% CI for all the models: postop full model

library(OptimalCutpoints)

#########################################################
#setwd("S:/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith")
setwd("/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/IntraOp_model-DECLARE")

#set path to prediction scores and true values of testing cohort
preds <- read.csv("Model/Results/AKI_7Day/IntraOp+PreOp_fullData_drop_esrd/aki7Day_intraOP+preOPFulldata_prediction_from_test873.csv",header = TRUE) # data

optimal.cutpoint.Youden<-optimal.cutpoints(X = "pred_from_intraPreOp", status = "true_AKI_outcome", tag.healthy = 0, 
                                           methods = "Youden", data = preds, pop.prev = NULL, 
                                           control = control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = FALSE)

# Plot by default
plot(optimal.cutpoint.Youden)




#########################################################
# Try to compute CI for other metrics
#########################################################

library("pROC")  # for ROC curve plotting
library(boot)

#set path to prediction scores and true values of testing cohort
preds <- read.csv("Model/Results/AKI_7Day/IntraOp+PreOp_fullData_drop_esrd/aki7Day_intraOP+preOPFulldata_prediction_from_test873.csv",header = TRUE) # data


df <- data.frame(preds$true_AKI_outcome, preds$pred_from_intraPreOp)
threshold = 0.40

roc_obj <- roc(df[,1], df[,2])
# spe = 0.826
# ci.se(preds$outcome, preds$kdigo_corrpred, specificities= spe)  # default bootstrap=2000
# ci.sp(preds$outcome, preds$kdigo_corrpred, sensitivities= sen) 

### CI for sensitivity and specificity
print("CI for sensitivity and specificity:")
ci(roc_obj, of="thresholds", thresholds=threshold)

############################### CI for accuracy
metrics_acc <- function(ref_pred_data, indices){
  
  ref_pred_data <- ref_pred_data[indices,] # allows boot to select sample
  
  pred_cut = ref_pred_data[,2]>threshold
  reference = ref_pred_data[,1]
  TP <- sum(pred_cut==1 & reference==1)
  TN <- sum(pred_cut==0 & reference==0)
  #FP <- sum(pred_cut==1 & reference==0)
  #FN <- sum(pred_cut==0 & reference==1)
  deno <- length(reference)
  acc <- (TP+TN)/deno
  #PPV <- TP/(TP+FP)
  #NPV <- TN/(TN+FN)
  return (acc)
}
print("CI for accuracy:")
boot_results <- boot(data=df, statistic=metrics_acc, R=2000)
boot.ci(boot_results, type="bca")

#########################################
## CI for PPV
metrics_PPV <- function(ref_pred_data, indices){
  
  ref_pred_data <- ref_pred_data[indices,] # allows boot to select sample
  
  pred_cut = ref_pred_data[,2]>threshold
  reference = ref_pred_data[,1]
  TP <- sum(pred_cut==1 & reference==1)
  #TN <- sum(pred_cut==0 & reference==0)
  FP <- sum(pred_cut==1 & reference==0)
  #FN <- sum(pred_cut==0 & reference==1)
  #deno <- length(reference)
  #acc <- (TP+TN)/deno
  PPV <- TP/(TP+FP)
  #NPV <- TN/(TN+FN)
  return (PPV)
}
print("CI for PPV:")
boot_results <- boot(data=df, statistic=metrics_PPV, R=2000)
boot.ci(boot_results, type="bca")

#############################################
## CI for NPV
metrics_NPV <- function(ref_pred_data, indices){
  
  ref_pred_data <- ref_pred_data[indices,] # allows boot to select sample
  
  pred_cut = ref_pred_data[,2]>threshold
  reference = ref_pred_data[,1]
  #TP <- sum(pred_cut==1 & reference==1)
  TN <- sum(pred_cut==0 & reference==0)
  #FP <- sum(pred_cut==1 & reference==0)
  FN <- sum(pred_cut==0 & reference==1)
  #deno <- length(reference)
  #acc <- (TP+TN)/deno
  #PPV <- TP/(TP+FP)
  NPV <- TN/(TN+FN)
  return (NPV)
}
print("CI for NPV:")
boot_results <- boot(data=df, statistic=metrics_NPV, R=2000)
boot.ci(boot_results, type="bca")
