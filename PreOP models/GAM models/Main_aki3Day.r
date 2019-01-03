# Modified by Lasith Adhikari on 7th Dec 2017

# Modified by Lasith to compare with intraOp prediction model on 10/30/2017
# In order to compare with the intraOp model, the preOp model also has to be trained and test on the
# same data cohorts. 

############################################################################################
# Created by Paul Thottakkara : Last Updated on 17 june 2016 by Ashkan Ebadi & Shivam Mittal
# 
# This is th main file which has two sections, 1st section is the Read Data file that reads
# the user defined functions and data required. 2nd Section will pre-process the raw - data
# and generate the best GAM-Model and classify the different patients into high,medium
# & low categories by selecting the cutoff parameters for each complication
#
# Required files for running this functions are:-
#  *Data_Analysis.R
#  *PreProcessing.R
#  *Model_Build_Validation.R
#  *Metric_Evaluation.R 
# This file contains the user defined functions for processing the data and building the model
############################################################################################

#setwd("S:/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/PreOp_model")
setwd("/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/PreOp_model")

# libraries required
## check for packages
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# libraries required
pkgTest("mgcv")
pkgTest("verification")
pkgTest("VIF")
pkgTest("e1071")
#pkgTest("randomForest")
pkgTest("cvTools")
pkgTest("SDMTools")

pkgTest("pROC")  # for ROC curve plotting
pkgTest("ROCR")


# Include User-Defined Fucntions
source("Codes/PreProcessing2.R")  ### change this to PreProcessing.R, the file needs to be corrected!
source("Codes/PreProcessing1.R")  ### change this to PreProcessing.R, the file needs to be corrected!
#source("Model_Build_Validation.R")
#source("Layer1_Data_Analysis.R")
source("Codes/Metric_Evaluation.r")

source("Codes/Layer1_Model_Build_Validation.R")


# This is the Read Data file that reads the user defined functions and data required. 
######
# This reads data from three diffrent files namely:-
# "data_raw" consists of entire cohort patient information icnluding both geographical and medical features
# "features" file contains name of important features and its corresponding data type like AGE:integer
# "features_all" contains names of every feature present in the cohort database
######
# It also perform basic pre-processing of data based on different medical conditions

# read all the required data files 
data_train <- read.csv("Data_to_train/TrainData_with_aki3_2038samples_drop_esrd_patient.csv",header = TRUE) # training data
data_test <- read.csv("Data_to_test/TestData_with_aki3_drop_esrd_patient.csv",header = TRUE) #testing data
data_test$zip5 <- as.factor(data_test$zip5)     #f <- sapply(data_test, is.factor)


data_raw <- rbind(data_train, data_test) #stack train and test datasets. All columns should be in same data type

#features_all <- read.csv("Data/feature_list_all1.csv",header = TRUE)   # orginal feature set
#features_all <- read.csv("Data/feature_list_all_droped_yellow.csv",header = TRUE)   # dropped yellow colored features from feature_list_all1_colored.xlsx
#features_all <- read.csv("Data/feature_list_all_droped_yellow_green.csv",header = TRUE)   # dropped yellow+green colored features from feature_list_all1_colored.xlsx
#features_all <- read.csv("Data/feature_list_all_droped_yellow_green_purple.csv",header = TRUE)   # dropped yellow+green colored features from feature_list_all1_colored.xlsx
features_all <- read.csv("Data/feature_list_12_7_2017.csv",header = TRUE)   # considered final list from Amir


# assign admission types to each patient
t <- ifelse(data_raw$nsaids_adm==1&(data_raw$vanco_adm==1|data_raw$diuret_adm==1|data_raw$aminog_adm==1),2,
            ifelse(data_raw$nsaids_adm==1,1,0))

data_raw$nephtox_adm <- t 

# create Number of nephrotoxic medications
ind <- which(colnames(data_raw)%in%c("aminog_adm","diuret_adm","vanco_adm","ace_adm","nsaids_adm","inot_pres_adm"))
no_nephrotoxic_meds <- rowSums(data_raw[,ind],na.rm = T)
data_raw$no_nephrotoxic_meds <- no_nephrotoxic_meds

# grouping Urine Protein
t <- ifelse(data_raw$max_PROTUR_gr2==">=300",2,ifelse(data_raw$max_PROTUR_gr2=="TR-30-100",1,0))
data_raw$max_PROTUR_gr2 <- as.factor(t)

# grouping urinal hemoglobin
t <- ifelse(data_raw$max_HGBUR_gr=="Large",2,ifelse(data_raw$max_HGBUR_gr=="Missing"|data_raw$max_HGBUR_gr=="NEGATIVE",0,1))
data_raw$max_HGBUR_gr <- as.factor(t)

# grouping urinal glucose
t <- ifelse(data_raw$max_GLUURN_gr=="Large",2,ifelse(data_raw$max_GLUURN_gr=="Missing"|data_raw$max_GLUURN_gr=="NEGATIVE",0,1))
data_raw$max_GLUURN_gr <- as.factor(t)

# grouping of No of complete blood count tests
t <- ifelse(data_raw$count_HGBn==0,0,ifelse(data_raw$count_HGBn==1,1,"2 or more"))
data_raw$count_HGBn <- t

#End of Reading Data
###############################################################################################

#features_selected <- features_all[-c(38,51,52,73,74),]
features_selected <- features_all
#write.csv(data_raw, "Data_to_train/myPreOP_test.csv")







#########################################  AKI- old #############################################
# # run for kdigo_corr complication
# data_raw=data_raw[data_raw$kdigo_corr!=5,]
# proc_data <- gen_proc_data(data_raw,features_selected,data_raw$kdigo_corr)
# sel_list_vif <- vif_test(data_raw,outcome =data_raw$kdigo_corr,feature_list = features_selected )
# kfold_allModels_kdigo_corr <- gen_gam_model_kfold(proc_data,sel_list_vif)
# ## get the best model based on highes auc                        
# max_auc_model_kdigo_corr <- kfold_allModels_kdigo_corr$models[[(which.max(kfold_allModels_kdigo_corr$results[,8])-1)/99]]
# save(max_auc_model_kdigo_corr,file = "kdigo_corr.RData")
# ## run the best model on the entire dataset
# data_new_kdigo_corr <- proc_data
# data_new_kdigo_corr$kdigo_corrpred <- predict(max_auc_model_kdigo_corr,data_new_kdigo_corr,type="response") ; 
# metric_kdigo_corr <-  calculate_metric(data_new_kdigo_corr$outcome,data_new_kdigo_corr$kdigo_corrpred) 
# cutoff1_kdigo_corr <- metric_kdigo_corr[which.max(metric_kdigo_corr[,7]),1]
# cutoff2_kdigo_corr <- calculate_cutoff2(metric_kdigo_corr[,1:2])
# ##plot_roc(metric_kdigo_corr[,5],metric_kdigo_corr[,6],metric_kdigo_corr[,1],metric_kdigo_corr[1,8],cutoff1,cutoff2)
# data_new_kdigo_corr$category <- ifelse(data_new_kdigo_corr$kdigo_corrpred<=cutoff1_kdigo_corr,"low",
#                                            ifelse(data_new_kdigo_corr$kdigo_corrpred>cutoff2_kdigo_corr,"high","moderate"))
# #write.csv(metric_kdigo_corr,"kdigo_corr-nwa.csv")
# write.csv(data_new_kdigo_corr[,c('kdigo_corrpred','outcome','category')],"kdigo_corr_result.csv")



#########################################  AKI - 3 DAY -  #############################################
# run for kdigo_corr complication
#data_raw=data_raw[data_raw$aki3day!=5,]
proc_data <- gen_proc_data(data_raw,features_selected,data_raw$aki3day) #generate data for modelling


################ Training the model ###########
#clean_data_train = proc_data[1:13276,] # extract the cleaned traing dataset for modeling
clean_data_train = proc_data[1:2038,]   # extract same accounts/rows that we used in intraOp for training
#data_train <- data_raw[1:13276,] 
data_train <- data_raw[1:2038,] 

#sel_list_vif <- vif_test(data_train,outcome =data_train$aki3day,feature_list = features_selected )
# we do not do VIF selction:
sel_list_vif <- features_selected

kfold_allModels_kdigo_corr <- gen_gam_model_kfold(clean_data_train,sel_list_vif)
## get the best model based on highes auc                        
#max_auc_model_kdigo_corr <- kfold_allModels_kdigo_corr$models[[(which.max(kfold_allModels_kdigo_corr$results[,8])-1)/99]]
max_auc_model_kdigo_corr <- kfold_allModels_kdigo_corr$models[[which.max(unique(kfold_allModels_kdigo_corr$results[,8]))]]

############## Obtain cutoffs using the training dataset ###################
clean_data_train$kdigo_corrpred <- predict(max_auc_model_kdigo_corr,clean_data_train,type="response")
metric_kdigo_corr <-  calculate_metric(clean_data_train$outcome,clean_data_train$kdigo_corrpred) 
cutoff1_kdigo_corr <- metric_kdigo_corr[which.max(metric_kdigo_corr[,7]),1]   # cutoff1_kdigo_corr = the threshold where the Youden's index is maximum
cutoff2_kdigo_corr <- calculate_cutoff2(metric_kdigo_corr[,1:2]) # get cutoff2 by checking the plateau of the accuracy curve
#write.csv(metric_kdigo_corr,"aki3Day_PreOp_trainBy2038_ROC_AUC_ACC_PPV_NPV_F1_trainData_esrd_dropped.csv")



######################################### Test the best model using the test dataset from the intraOp tesing cohort (only 874 samples) #######################################


#data_new_kdigo_corr <- proc_data[13277:14150,] # extarct the cleaned test dataset (which was not used to train the model)
data_new_kdigo_corr <- proc_data[2039:2911,]  
data_new_kdigo_corr$kdigo_corrpred <- predict(max_auc_model_kdigo_corr,data_new_kdigo_corr,type="response") ; 
metric_kdigo_corr_test <-  calculate_metric(data_new_kdigo_corr$outcome,data_new_kdigo_corr$kdigo_corrpred) 
#cutoff1_kdigo_corr <- metric_kdigo_corr[which.max(metric_kdigo_corr[,7]),1]   # cutoff1_kdigo_corr = the threshold where the Youden's index is maximum
#cutoff2_kdigo_corr <- calculate_cutoff2(metric_kdigo_corr[,1:2]) # get cutoff2 by checking the plateau of the accuracy curve
data_new_kdigo_corr$category <- ifelse(data_new_kdigo_corr$kdigo_corrpred<=cutoff1_kdigo_corr,"low",
                                       ifelse(data_new_kdigo_corr$kdigo_corrpred>cutoff2_kdigo_corr,"high","moderate"))

write.csv(cbind(data_test$acc,data_new_kdigo_corr[,c('kdigo_corrpred','outcome','category')]),"aki3Day_preOp_trainBy2038_prediction_result_on874Test_esrd_dropped.csv")
write.csv(metric_kdigo_corr_test,"aki3Day_PreOp_trainBy2038_ROC_AUC_ACC_PPV_NPV_F1_874testData_esrd_dropped.csv")

######### Plot ROC curve for test results ############
# roc_obj <- roc(data_new_kdigo_corr$outcome, data_new_kdigo_corr$kdigo_corrpred)
# #plot(roc_obj, col="blue", lwd=2, main="The PreOp model performance with AKI-3Day outcome")
# svg('AKI3Day_PreOp_ROC_with_874testData.svg')
# plot(roc_obj, col="blue", lwd=2, xlab='1 - Specificity')
# text(0.85, 1, paste("AUC = ",round(auc(roc_obj),4)))
# dev.off()
# auc(roc_obj)  # print Area under the curve

roc_obj <- roc(data_new_kdigo_corr$outcome, data_new_kdigo_corr$kdigo_corrpred)
svg('AKI3Day_PreOp_trainBy2038_ROC_with_874testData_esrd_dropped.svg')
pred2 <- prediction(as.vector(data_new_kdigo_corr$kdigo_corrpred), data_new_kdigo_corr$outcome)
perf2 <- performance(pred2,"tpr","fpr")
plot(perf2, col="blue", lwd=2, xlab='1 - Specificity', ylab = 'Sensitivity')
abline(0,1,col="black", lwd=0.8, lty=2)
text(0.85, 0.0, paste("AUC = ",round(auc(roc_obj),4)))
dev.off()


##################################### Obtain predictions for the whole cohort using the best preOp model ############################################
data_all <- proc_data
data_all$aki_predScore <- predict(max_auc_model_kdigo_corr,proc_data,type="response") 
data_all$acc <- data_raw$acc
write.csv(data_all,"aki3Day_PreOp_trainBy2038_predScore_for_wholeCohort_esrd_Dropped.csv")  # write the predition on the whole cohort to input as a feature to the intraOp model
metric_aki <-  calculate_metric(data_all$outcome,data_all$aki_predScore)  #Just check values. Did not use for analysis

#save(max_auc_model_kdigo_corr,file = "aki3Day.RData")
#save.image("aki3Day_preOp_trainBy2039.RData")