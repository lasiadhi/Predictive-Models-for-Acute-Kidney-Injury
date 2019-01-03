# apply t-test/Wilcoxon rank-sum test and chi-square test to compare two unpaired groups


setwd("/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith")

# CHANGE HERE
# read intraop  data
intraop_data <- read.csv("Model/Data/aki7_drop_esrd1_patients/all_clean_feature_to_model_aki7.csv",header = TRUE)
outcome <- intraop_data$aki7day

# read feature name list:
feature_names <- read.csv("Model/Data/all_intraop_features.csv",header = TRUE)


# create empty data frame for the p-values
variable <- c()
p_value <- c()
p_leq_0_05 <- c()
df = data.frame(variable,p_value, p_leq_0_05) 


# ############ apply t-test for continous variables with large sample size # independent 2-group t-test with 95% CI
# num_features <- feature_names[which(feature_names$variable_type=='num'),]
# for(i in 1:length(num_features$variable)){
#   var <- as.character(num_features$variable[i])
#   #print(as.character(var))
#   result <- t.test(intraop_data[,which(colnames(intraop_data)==var)]~as.factor(outcome))
#   #print(result$p.value)
#   val <- result$p.value<0.05
#   df <- rbind(df, data.frame("variable" = as.character(var), "p_value"= result$p.value, "p_leq_0_05"=val))
# }

### OR

############ apply Wilcoxon rank-sum test for continous data assuming non-normal distribution
num_features <- feature_names[which(feature_names$variable_type=='num'),]
gp1 <- intraop_data[which(outcome==1),]
gp0 <- intraop_data[which(outcome==0),]
for(i in 1:length(num_features$variable)){
  var <- as.character(num_features$variable[i])
  #print(as.character(var))
  result <- wilcox.test(gp0[,var], gp1[,var], paired=FALSE)
  #print(result$p.value)
  val <- result$p.value<0.05
  df <- rbind(df, data.frame("variable" = as.character(var), "p_value"= result$p.value, "p_leq_0_05"=val))
}




############ apply chi-square for cat variables

num_features <- feature_names[which(feature_names$variable_type=='cat'),]
for(i in 1:length(num_features$variable)){
  var <- num_features$variable[i]
  #print(as.character(var))
  result <- chisq.test(intraop_data[,which(colnames(intraop_data)==var)], as.factor(outcome), correct=FALSE)
  #print(result$p.value)
  val <- result$p.value<0.05
  df <- rbind(df, data.frame("variable" = as.character(var), "p_value"= result$p.value, "p_leq_0_05"=val))
}

write.csv(df,"comparison_stat_results_for_intraOp_aki7Day_with_wilcoxon_rank_sum.csv")
