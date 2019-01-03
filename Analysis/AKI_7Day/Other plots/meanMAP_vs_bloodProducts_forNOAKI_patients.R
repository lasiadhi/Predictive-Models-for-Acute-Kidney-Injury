# AKI risk with Max_HR vs. Min_BP 
# Modified/written by Lasith Adhikari 

# libraries that needs to be activated
#library(verification)
#library(mgcv)


setwd("/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/IntraOp_model-DECLARE")
#setwd('S:/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith')

# read preOp only prediction test cohort
pred_preOp <- read.csv("PreOp_model/Results/AKI_7/esrd_dropped_final_features_model/aki7Day_preOp_trainBy2038_prediction_result_on873Test_esrd_dropped.csv",header = TRUE)
#pred_preOp <- read.csv("PreOp_model/Results/AKI_3/aki3Day_PreOp_trainBy2039_predScore_for_wholeCohort.csv",header = TRUE)


# read preOp+intraOp only prediction test cohort
pred_preOp_intraOp <- read.csv("Model/Results/AKI_7Day/IntraOp+PreOp_probs_drop_esrd/aki7Day_prediction_from_intraOP+preOP_probs_from_test873.csv",header = TRUE)
#pred_preOp_intraOp <- read.csv("Model/Results/AKI_3Day/IntraOp+PreOp_all_features/aki3Day_intraOp+preOP_prediction_from_train2039Patients.csv",header = TRUE)

preOp_cutoff = 0.43
intra_preOp_cutoff = 0.41


data_fnl <- merge(pred_preOp, pred_preOp_intraOp, by.x='data_test.acc', by.y='acc')

#pick the preOp misclassiffied patients 
ind <- which(data_fnl$outcome== 0 & data_fnl$kdigo_corrpred>preOp_cutoff)

# read the HR and BP features for the above ind's acc patients
data_all <- read.csv("Model/Data/features_to_model_withPF_moreClean.csv",header = TRUE)
data_all <- merge(data_all, data_fnl[ind,], by.y='data_test.acc', by.x='acc')

#select important cols for the plot along with the intra+preOp predictions from the test cohort

#plot1: hr_max vs min BP,  xlab="Maximum Heart rate (beats per minute)",ylab="Minimum MAP (mmHg)"
#plot2: mean MAP vs min MAP, xlab="Mean MAP (mmHg)",ylab="Minimum MAP (mmHg)"
test <- cbind(data_all$blood_product_ml, data_all$map_mean_base, data_all$pred_from_intraOp)
  
col <- rep("green",times = nrow(test)) ; col[which(test[,3]>intra_preOp_cutoff)] <- "red"   # label according the cutoff

#svg('HR_vs_BP_AKI3Day_risk_for_874TestCohort_with_Pred.svg')
par(mfrow=c(1,2)) 
plot(test[,1],test[,2],col=col,pch=16,cex=1.3,xlab="Blood products (ml)",ylab="Mean base MAP (mmHg)")
legend("topright",c("Low risk in postoperative stacked model","High risk in postoperative stacked model"),pch=c(16,16),col=c("green","red"))
cols <- c("green","red") ; 
for(i in 1:2){
    var_hr <- test[which(col==cols[i]),1] ; var_bp <- test[which(col==cols[i]),2]
    m_hr <- mean(var_hr) ; stder_hr <- qt(p = 0.975,df = length(var_hr)-1)*sd(var_hr)/sqrt(length(var_hr))
    m_bp <- mean(var_bp) ; stder_bp <- qt(p = 0.975,df = length(var_bp)-1)*sd(var_bp)/sqrt(length(var_bp))
    xl <- m_hr-stder_hr ; xu <- m_hr+stder_hr
    yl <- m_bp-stder_bp ; yu <- m_bp+stder_bp
    rect(xleft = xl,ybottom = yl,xright = xu,ytop = yu,col=cols[i])
}
#dev.off()

# testing whether the two samples are statistically different
test <- cbind(test, col)
green_gp_x <- as.numeric(test[which(test[,'col']=="green"),1])
red_gp_x <- as.numeric(test[which(test[,'col']=="red"),1])
print(wilcox.test(green_gp_x, red_gp_x, paired=FALSE))

green_gp_y <- as.numeric(test[which(test[,'col']=="green"),2])
red_gp_y <- as.numeric(test[which(test[,'col']=="red"),2])
print(wilcox.test(green_gp_y, red_gp_y, paired=FALSE))

############ for correctly classified data ##################


#pick the preOp classiffied patients 
ind <- which(data_fnl$outcome==0 & data_fnl$kdigo_corrpred<=preOp_cutoff)

# read the HR and BP features for the above ind's acc patients
data_all <- read.csv("Model/Data/features_to_model_withPF_moreClean.csv",header = TRUE)
data_all <- merge(data_all, data_fnl[ind,], by.y='data_test.acc', by.x='acc')

#select important cols for the plot along with the intra+preOp predictions from the test cohort

#plot1: hr_max vs min BP,  xlab="Maximum Heart rate (beats per minute)",ylab="Minimum MAP (mmHg)"
#plot2: mean MAP vs min MAP, xlab="Mean MAP (mmHg)",ylab="Minimum MAP (mmHg)"
test <- cbind(data_all$blood_product_ml, data_all$map_mean_base, data_all$pred_from_intraOp)

col <- rep("green",times = nrow(test)) ; col[which(test[,3]>intra_preOp_cutoff)] <- "red"   # label according the cutoff

#svg('HR_vs_BP_AKI3Day_risk_for_874TestCohort_with_Pred.svg')
plot(test[,1],test[,2],col=col,pch=16,cex=1.3,xlab="Blood products (ml)",ylab="Mean base MAP (mmHg)")
legend("topright",c("Low risk in postoperative stacked model","High risk in postoperative stacked model"),pch=c(16,16),col=c("green","red"))
cols <- c("green","red") ; 
for(i in 1:2){
  var_hr <- test[which(col==cols[i]),1] ; var_bp <- test[which(col==cols[i]),2]
  m_hr <- mean(var_hr) ; stder_hr <- qt(p = 0.975,df = length(var_hr)-1)*sd(var_hr)/sqrt(length(var_hr))
  m_bp <- mean(var_bp) ; stder_bp <- qt(p = 0.975,df = length(var_bp)-1)*sd(var_bp)/sqrt(length(var_bp))
  xl <- m_hr-stder_hr ; xu <- m_hr+stder_hr
  yl <- m_bp-stder_bp ; yu <- m_bp+stder_bp
  rect(xleft = xl,ybottom = yl,xright = xu,ytop = yu,col=cols[i])
}

# statistical difference between new red group and red_gp in missclassifed plot
test <- cbind(test, col)
red_gp_correct_x <- as.numeric(test[which(test[,'col']=="red"),1])
red_gp_correct_y <- as.numeric(test[which(test[,'col']=="red"),2])
print(wilcox.test(red_gp_x, red_gp_correct_x, paired=FALSE))
print(wilcox.test(red_gp_y, red_gp_correct_y, paired=FALSE))

