new_Data=read.csv("etsev_cleaned_data.csv")
new_Data1=read.csv("etiso_cleaned_data.csv")
data_intra_iso=new_Data1
data_intra_sev=new_Data
data_intra_iso$obser <- data_intra_iso$obser/1.17
data_intra_sev$obser <- data_intra_sev$obser/1.8

data_intra <- as.data.frame(rbind(data_intra_iso,data_intra_sev))
data_intra$X<-NULL
bp_data_cleaned=data_intra
mean_mac=mean(bp_data_cleaned$obser)
sd_mac=sd(bp_data_cleaned$obser)
lower_limits <- c(0) ; 
upper_limits <- c((mean_mac+(2*sd_mac)));
interval_list <- cbind(c(floor(mean_mac-(2*sd_mac)),
                         floor(mean_mac-(1*sd_mac)),
                         floor(mean_mac-(1*sd_mac)),
                         floor(mean_mac-(0.5*sd_mac)),
                         floor(mean_mac+(0.5*sd_mac)),
                         floor(mean_mac+(1*sd_mac))),
                       c(floor(mean_mac-(1*sd_mac)),
                         floor(mean_mac+(1*sd_mac)),
                         floor(mean_mac-(0.5*sd_mac)),
                         floor(mean_mac+(0.5*sd_mac)),
                         floor(mean_mac+(1*sd_mac)),
                         floor(mean_mac+(2*sd_mac))))
param_result <- fun_extract_par("systolic",bp_data_cleaned,lower_limits,upper_limits,trun_check = 60,interval_list)
write.csv(param_result$parameters,"mac_paramteres.csv")





library(caret)
control<-rfeControl(functions=rfFuncs,method = "cv",number=10)
data=read.csv("data_11_26_2015_116vars_ordered_clean1.csv")
data_final=data[,c("acc","mort_status_30d")]
merge_data=merge(x=data_final,y=param_result$parameters,by=c("acc"))
require(randomForest)
results<-rfe(merge_data[,3:24],merge_data[,2],sizes = c(1:10),rfeControl = control)
