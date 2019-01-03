new_Data=read.csv("HR_cleaned_data.csv")

data_BP_HR=new_Data
acc_nos <- unique(data_BP_HR$acc)
# blood pressure parameter generation
bp_parameters <- data.frame(matrix(nrow=length(acc_nos),ncol=21)) 
rem_accs <- array(dim=10); rem_clean_acc <- array(dim=5) ; k_acc_c <- 1 ; k_par <- 1 ; k_acc <- 1
for(j in 1:length(acc_nos)){
  index <- which(data_BP_HR$acc==acc_nos[j]) 
  # remove data with less observations 
  temp_data <- data_BP_HR[index,]
  if(check_data_HR(temp_data)){
    rem_accs[k_acc] <- acc_nos[j] ; print("acc removed due to less data"); k_acc <- k_acc + 1; next
  }
}

bp_data_cleaned=data_BP_HR[!data_BP_HR$acc %in% rem_accs,]


check_data_HR <- function(data_temp){
  count_sys <- nrow(data_temp)
  if(count_sys<10){
    return(TRUE)
  }else 
    return(FALSE)
  
}


mean_hr=mean(bp_data_cleaned$obser)
sd_hr=sd(bp_data_cleaned$obser)
lower_limits <- c(floor(mean_hr-(2*sd_hr))) ; 
upper_limits <- c(floor(mean_hr+(2*sd_hr)));
interval_list <- cbind(c(floor(mean_hr-(2*sd_hr)),
                         floor(mean_hr-(1*sd_hr)),
                         floor(mean_hr-(1*sd_hr)),
                         floor(mean_hr-(0.5*sd_hr)),
                         floor(mean_hr+(0.5*sd_hr)),
                         floor(mean_hr+(1*sd_hr))),
                       c(floor(mean_hr-(1*sd_hr)),
                         floor(mean_hr+(1*sd_hr)),
                         floor(mean_hr-(0.5*sd_hr)),
                         floor(mean_hr+(0.5*sd_hr)),
                         floor(mean_hr+(1*sd_hr)),
                         floor(mean_hr+(2*sd_hr))))
param_result <- fun_extract_par("systolic",bp_data_cleaned,lower_limits,upper_limits,trun_check = 60,interval_list)

head(param_result$parameters)
library(caret)
control<-rfeControl(functions=rfFuncs,method = "cv",number=10)

data=read.csv("data_11_26_2015_116vars_ordered_clean1.csv")
data_final=data[,c("acc","mort_status_30d")]
merge_data=a=merge(x=data_final,y=param_result$parameters,by=c("acc"))

results<-rfe(merge_data[,3:24],merge_data[,2],sizes = c(1:10),rfeControl = control)
