# This R plot generates plot for the intraOp/intraop+pre model performance

setwd("/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith")
#setwd("S:/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith")
#load packages:
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# libraries required
pkgTest("verification") # for roc.area()
pkgTest("pROC")  # for ROC curve plotting

# load the same metric evaluation R code used for preOp model:
source("./PreOp_model/Codes/Metric_Evaluation.r")

# load dataset: true outcome and predictions
data_results <- read.csv("Model/Results/AKI_Overall/IntraOp+PreOp_probs_drop_esrd/akiOverall_prediction_from_intraOP+preOP_probs_from_train2038.csv",header = TRUE) #testing data or train data


################

metric_kdigo_corr <-  calculate_metric(data_results$true_AKI_outcome, data_results$pred_from_intraPreOp)  #CHANGE HERE # generate PPV, ACC, Youden's_index, etc
cutoff1_kdigo_corr <- metric_kdigo_corr[which.max(metric_kdigo_corr[,7]),1]   # cutoff1_kdigo_corr = the threshold where the Youden's index is maximum
cutoff2_kdigo_corr <- calculate_cutoff2(metric_kdigo_corr[,1:2]) # get cutoff2 by checking the plateau of the accuracy curve
# data_new_kdigo_corr$category <- ifelse(data_new_kdigo_corr$kdigo_corrpred<=cutoff1_kdigo_corr,"low",
#                                        ifelse(data_new_kdigo_corr$kdigo_corrpred>cutoff2_kdigo_corr,"high","moderate"))

#write.csv(cbind(data_test$acc,data_new_kdigo_corr[,c('kdigo_corrpred','outcome','category')]),"aki3Day_prediction_result.csv")
write.csv(metric_kdigo_corr,"akiOverall_IntraOp+preOP_ROC_AUC_ACC_PPV_NPV_F1_trainData.csv")

######### Plot ROC curve ############
roc_obj <- roc(data_results$true_AKI_outcome, data_results$pred_from_intraPreOp)
###plot(roc_obj, col="blue", lwd=2, main="The PreOp model performance with AKI-3Day outcome")
#svg('AKI7Day_fromIntraOp+PreOP_ROC_with_874testData.svg')
plot(roc_obj, col="blue", lwd=2, xlab='1 - Specificity')
text(0.85, 1, paste("AUC = ",round(auc(roc_obj),4)))
dev.off()
auc(roc_obj)  # print Area under the curve


############################3 for testing data ###############################
data_results <- read.csv("Model/Results/AKI_Overall/IntraOp+PreOp_probs_drop_esrd/akiOverall_prediction_from_intraOP+preOP_probs_from_test873.csv",header = TRUE) #testing data


metric_kdigo_corr <-  calculate_metric(data_results$true_AKI_outcome, data_results$pred_from_intraOp)  #CHANHE HERE # generate PPV, ACC, Youden's_index, etc
#cutoff1_kdigo_corr <- metric_kdigo_corr[which.max(metric_kdigo_corr[,7]),1]   # cutoff1_kdigo_corr = the threshold where the Youden's index is maximum
#cutoff2_kdigo_corr <- calculate_cutoff2(metric_kdigo_corr[,1:2]) # get cutoff2 by checking the plateau of the accuracy curve
# data_new_kdigo_corr$category <- ifelse(data_new_kdigo_corr$kdigo_corrpred<=cutoff1_kdigo_corr,"low",
#                                        ifelse(data_new_kdigo_corr$kdigo_corrpred>cutoff2_kdigo_corr,"high","moderate"))

#write.csv(cbind(data_test$acc,data_new_kdigo_corr[,c('kdigo_corrpred','outcome','category')]),"aki3Day_prediction_result.csv")
write.csv(metric_kdigo_corr,"akiOverall_IntraOp+preOP_ROC_AUC_ACC_PPV_NPV_F1_TestData.csv")


