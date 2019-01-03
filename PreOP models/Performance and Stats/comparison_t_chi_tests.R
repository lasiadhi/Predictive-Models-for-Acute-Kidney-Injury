# apply t-test/Wilcoxon rank-sum test and chi-square test to compare two unpaired groups
# preop data

setwd("/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith")


# read preop train data
preop_train_data <- read.csv("PreOp_model/Data_to_train/TrainData_with_akiOverall_2038samples_drop_esrd_patient.csv",header = TRUE)
# read preop test data
preop_test_data <- read.csv("PreOp_model/Data_to_test/TestData_with_akiOverall_drop_esrd_patient.csv",header = TRUE)
preop_test_data$zip5 <- as.factor(preop_test_data$zip5) 
preop_data <- rbind(preop_train_data, preop_test_data)
outcome <- preop_data$aki_overall  ## Change the outcome here

# read feature name list:
feature_names <- read.csv("PreOp_model/Data/preOp_feature_list_toGetStats_ver1.csv",header = TRUE)



# create empty data frame for the p-values
variable <- c()
p_value <- c()
p_leq_0_05 <- c()
df = data.frame(variable,p_value, p_leq_0_05) 


# ############ apply t-test for continous variables with large sample size # independent 2-group t-test with 95% CI
# num_features <- feature_names[which(feature_names$original_variable_type=='num'),]
# for(i in 1:length(num_features$variable)){
#   var <- num_features$variable[i]
#   #print(as.character(var))
#   result <- t.test(preop_data[,which(colnames(preop_data)==var)]~preop_data$aki_overall)
#   #print(result$p.value)
#   val <- result$p.value<0.05
#   df <- rbind(df, data.frame("variable" = as.character(var), "p_value"= result$p.value, "p_leq_0_05"=val))
# }

### OR

############ apply Wilcoxon rank-sum test for continous data assuming non-normal distribution
num_features <- feature_names[which(feature_names$original_variable_type=='num'),]
gp1 <- preop_data[which(outcome==1),]
gp0 <- preop_data[which(outcome==0),]
for(i in 1:length(num_features$variable)){
  var <- as.character(num_features$variable[i])
  #print(as.character(var))
  result <- wilcox.test(gp0[,var], gp1[,var], paired=FALSE)
  #print(result$p.value)
  val <- result$p.value<0.05
  df <- rbind(df, data.frame("variable" = as.character(var), "p_value"= result$p.value, "p_leq_0_05"=val))
}


############ apply chi-square for cat variables

num_features <- feature_names[which(feature_names$original_variable_type=='cat'),]
for(i in 1:length(num_features$variable)){
  var <- num_features$variable[i]
  print(as.character(var))
  result <- chisq.test(preop_data[,which(colnames(preop_data)==var)], as.factor(outcome), correct=FALSE)
  print(result$p.value)
  val <- result$p.value<0.05
  df <- rbind(df, data.frame("variable" = as.character(var), "p_value"= result$p.value, "p_leq_0_05"=val))
}

write.csv(df,"comparison_stat_results_for_preOp_akiOverall_wilcox_chi_tests.csv")
