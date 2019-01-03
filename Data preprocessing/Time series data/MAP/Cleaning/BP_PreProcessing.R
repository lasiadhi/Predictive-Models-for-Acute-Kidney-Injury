data=read.csv("monitor_comb_all_fnl.csv")
data_pat=data[data$val>0,]
back=data_pat


vars <- c("abpd","abps","abpm","abp dia","abp sys","abp mean","nbpd","nbps","nbpm","nibp dia","nibp sys","nibp mean");
data_pat <- data_pat[which((data_pat$type %in% vars)),]

#new dataframe with with addition datetimestamp column
pat_data <- cbind(data_pat,paste(data_pat$date,data_pat$time))
patient_sur_dt=read.csv("BP_acc_Sum.csv")
bp_std_vals=read.csv("BP_std_vals.csv")
bp_data_final <- data.frame(matrix(ncol=8)) ; 
colnames(bp_data_final) <-  c("acc","time_stamp","systolic","diastolic","map","non_systolic","non_diastolic","non_map")  
colnames(pat_data)[10] <- "Timestamp"
pat_data$Timestamp <- as.POSIXlt(pat_data$Timestamp,format="%m/%d/%Y %I:%M:%S %p") 
pat_data=pat_data[,-c(3,4)]
pat_data=pat_data[,-c(1,3,5)]
pat_data_new <- data.frame(matrix(nrow=nrow(pat_data),ncol=5)) ; 
colnames(pat_data_new) <-  c("type","acc","value","ssi_op_id","Timestamp")  
pat_data_new$type=pat_data$type
pat_data$type=as.character(pat_data$type)
pat_data[pat_data$type=="abpd","type"]="diastolic"
pat_data[pat_data$type=="abps","type"]="systolic"
pat_data[pat_data$type=="abpm","type"]="map"
pat_data[pat_data$type=="abp dia","type"]="diastolic"
pat_data[pat_data$type=="abp sys","type"]="systolic"
pat_data[pat_data$type=="abp mean","type"]="map"


pat_data[pat_data$type=="nbpd","type"]="non_diastolic"
pat_data[pat_data$type=="nbps","type"]="non_systolic"
pat_data[pat_data$type=="nbpm","type"]="non_map"
pat_data[pat_data$type=="nibp dia","type"]="non_diastolic"
pat_data[pat_data$type=="nibp sys","type"]="non_systolic"
pat_data[pat_data$type=="nibp mean","type"]="non_map"

pat_data$Timestamp=as.character(pat_data$Timestamp)
mean_data=pat_data %>% group_by(type,acc,Timestamp,ssi_op_id)
mean_data1=summarize(mean_data,value=mean(value))

dia_data=mean_data1[mean_data1$type=='diastolic',]
sys_data=mean_data1[mean_data1$type=='systolic',]
map_data=mean_data1[mean_data1$type=='map',]
ndia_data=mean_data1[mean_data1$type=='non_diastolic',]
nsys_data=mean_data1[mean_data1$type=='non_systolic',]
nmap_data=mean_data1[mean_data1$type=='non_map',]

a=merge(x=dia_data,y=sys_data,by=c("acc","Timestamp","ssi_op_id"),all=TRUE)
a=merge(x=a,y=map_data,by=c("acc","Timestamp","ssi_op_id"),all=TRUE)
a=merge(x=a,y=ndia_data,by=c("acc","Timestamp","ssi_op_id"),all=TRUE)
a=merge(x=a,y=nsys_data,by=c("acc","Timestamp","ssi_op_id"),all=TRUE)
a=merge(x=a,y=nmap_data,by=c("acc","Timestamp","ssi_op_id"),all=TRUE)


colnames(a)[5]<-"diastolic"
colnames(a)[7]<-"systolic"
colnames(a)[9]<-"map"
colnames(a)[11]<-"non-diastolic"
colnames(a)[13]<-"non-systolic"
colnames(a)[15]<-"non-map"
a$type.x<-NULL
a$type.x<-NULL
a$type.x<-NULL
a$type.x<-NULL
a$type.y<-NULL
a$type.y<-NULL
a$type.x<-NULL
a$type.y<-NULL


a$diastolic[a$diastolic>200] <- NA
a$systolic[a$systolic>300] <- NA
a$map[a$map>200] <- NA
a$`non-diastolic`[a$`non-diastolic`>200] <- NA
a$`non-systolic`[a$`non-systolic`>300] <- NA
a$`non-map`[a$`non-map`>200] <- NA



b=a[(!(is.na(a$diastolic))|!(is.na(a$systolic))|!(is.na(a$map)))&(!(is.na(a$`non-diastolic`))|!(is.na(a$`non-systolic`))|!(is.na(a$`non-map`))),]
backup2=a

a$`non-diastolic`<-NULL
a$`non-systolic`<-NULL
a$`non-map`<-NULL


a=a[(!(is.na(a$diastolic))|!(is.na(a$systolic))|!(is.na(a$map))),]



new_data=a
write.csv(a,"proc.csv")
bp_data_final <- data.frame(matrix(ncol=6)) ; 
colnames(bp_data_final) <- c("acc","Timestamp","systolic","diastolic","map","ssi_op_id")
for(acc in unique(new_data$acc))
{
  new_data_acc=new_data[new_data$acc==acc,]
  ind <- which(patient_sur_dt$acc==acc)
  if(length(ind)>0){
    time_info <- patient_sur_dt[ind,] ; index <- which(time_info$ssi_op_id==min(time_info$ssi_op_id))
    start_sur <- time_info$start_time[index];
    end_sur <- time_info$end_time[index];
    time_ind <- which(as.POSIXlt(new_data_acc$Timestamp) <= as.POSIXct(end_sur) & as.POSIXlt(new_data_acc$Timestamp) >= as.POSIXct(start_sur)) ; 
    new_data_acc <- new_data_acc[time_ind,]
    bp_data_final=rbind(bp_data_final,new_data_acc)
  }else
  {
    print("acc match not found in key file")
  }
  
}
##588367987
new_data=bp_data_final
rm_temp <- array(dim=1); k<-1
for(i in 1:nrow(new_data))
{

    if(sum(is.na(new_data[i,3:5]))==3){
          rm_temp[k] <- i; k <- k+1
          next;
    }
  
  else if(sum(is.na(new_data[i,3:5]))==2){
    if(!is.na(new_data$map[i])){ # correct using map
      ref_ind <- which(abs(bp_std_vals$map-new_data$map[i])==min(abs(bp_std_vals$map-new_data$map[i])))
      new_data$systolic[i] <- bp_std_vals$systolic[ref_ind]
      new_data$diastolic[i] <- bp_std_vals$diastolic[ref_ind] ; next
    }
    if(!is.na(new_data$systolic[i])){ # correct using systolic
      ref_ind <- which(abs(bp_std_vals$systolic-new_data$systolic[i])==min(abs(bp_std_vals$systolic-new_data$systolic[i])))
      new_data$map[i] <- bp_std_vals$map[ref_ind]
      new_data$diastolic[i] <- bp_std_vals$diastolic[ref_ind] ; next
    }
    if(!is.na(new_data$diastolic[i])){ # correct using diastolic
      ref_ind <- which(abs(bp_std_vals$diastolic-new_data$diastolic[i])==min(abs(bp_std_vals$diastolic-new_data$diastolic[i])))
      new_data$map[i] <- bp_std_vals$map[ref_ind]
      new_data$systolic[i] <- bp_std_vals$systolic[ref_ind] ; next
    }
  }
  else if(sum(is.na(new_data[i,3:5]))==1){
    if(is.na(new_data$systolic[i])){
      new_data$systolic[i] <- new_data$map[i] - floor(new_data$diastolic[i]*0.66) ; next
    }
    if(is.na(new_data$diastolic[i])){
      new_data$diastolic[i] <- new_data$map[i] - floor(new_data$systolic[i]*0.33) ; next
    }
    if(is.na(new_data$map[i])){
      new_data$map[i] <- floor(new_data$diastolic[i]*.66) + floor(new_data$systolic[i]*0.33) ; next
    }
  }
}

rm_temp <- unique(rm_temp)
if(length(rm_temp)>=1&!is.na(rm_temp[1]))
  new_data <- new_data[-rm_temp,]




data_BP_HR=new_Data
acc_nos <- unique(data_BP_HR$acc)
# blood pressure parameter generation
bp_parameters <- data.frame(matrix(nrow=length(acc_nos),ncol=21)) 
rem_accs <- array(dim=10); rem_clean_acc <- array(dim=5) ; k_acc_c <- 1 ; k_par <- 1 ; k_acc <- 1
for(j in 1:length(acc_nos)){
  index <- which(data_BP_HR$acc==acc_nos[j]) 
  # remove data with less observations 
  temp_data <- data_BP_HR[index,]
  if(check_data_BP(temp_data)){
    rem_accs[k_acc] <- acc_nos[j] ; print("acc removed due to less data"); k_acc <- k_acc + 1; next
  }
}
  
bp_data_cleaned=data_BP_HR[!data_BP_HR$acc %in% rem_accs,]



mean_sytolic=mean(bp_data_cleaned$systolic)
sd_systolic=sd(bp_data_cleaned$systolic)
lower_limits <- c(floor(mean_sytolic-(2*sd_systolic))) ; 
upper_limits <- c(floor(mean_sytolic+(2*sd_systolic)));

interval_list <- cbind(c(floor(mean_sytolic-(2*sd_systolic)),
                         floor(mean_sytolic-(1*sd_systolic)),
                         floor(mean_sytolic-(1*sd_systolic)),
                         floor(mean_sytolic-(0.5*sd_systolic)),
                         floor(mean_sytolic+(0.5*sd_systolic)),
                         floor(mean_sytolic+(1*sd_systolic))),
                       c(floor(mean_sytolic-(1*sd_systolic)),
                         floor(mean_sytolic+(1*sd_systolic)),
                         floor(mean_sytolic-(0.5*sd_systolic)),
                         floor(mean_sytolic+(0.5*sd_systolic)),
                         floor(mean_sytolic+(1*sd_systolic)),
                         floor(mean_sytolic+(2*sd_systolic))))
param_result <- fun_extract_par("systolic",bp_data_cleaned,lower_limits,upper_limits,trun_check = 60,interval_list)
write.csv(param_result$parameters,"bp_systolic_parameters.csv")

mean_diastolic=mean(bp_data_cleaned$diastolic)
sd_diastolic=sd(bp_data_cleaned$diastolic)
lower_limits <- c(floor(mean_diastolic-(2*sd_diastolic))) ; 
upper_limits <- c(floor(mean_diastolic+(2*sd_diastolic)));
interval_list <- cbind(c(floor(mean_diastolic-(2*sd_diastolic)),
                         floor(mean_diastolic-(1*sd_diastolic)),
                         floor(mean_diastolic-(1*sd_diastolic)),
                         floor(mean_diastolic-(0.5*sd_diastolic)),
                         floor(mean_diastolic+(0.5*sd_diastolic)),
                         floor(mean_diastolic+(1*sd_diastolic))),
                       c(floor(mean_diastolic-(1*sd_diastolic)),
                         floor(mean_diastolic+(1*sd_diastolic)),
                         floor(mean_diastolic-(0.5*sd_diastolic)),
                         floor(mean_diastolic+(0.5*sd_diastolic)),
                         floor(mean_diastolic+(1*sd_diastolic)),
                         floor(mean_diastolic+(2*sd_diastolic))))
param_result1 <- fun_extract_par("diastolic",bp_data_cleaned,lower_limits,upper_limits,trun_check = 60,interval_list)
write.csv(param_result1$parameters,"bp_diastolic_parameters.csv")


mean_map=mean(bp_data_cleaned$map)
sd_map=sd(bp_data_cleaned$map)
lower_limits <- c(floor(mean_map-(2*sd_map))) ; 
upper_limits <- c(floor(mean_map+(2*sd_map)));
interval_list <- cbind(c(floor(mean_map-(2*sd_map)),
                         floor(mean_map-(1*sd_map)),
                         floor(mean_map-(1*sd_map)),
                         floor(mean_map-(0.5*sd_map)),
                         floor(mean_map+(0.5*sd_map)),
                         floor(mean_map+(1*sd_map))),
                       c(floor(mean_map-(1*sd_map)),
                         floor(mean_map+(1*sd_map)),
                         floor(mean_map-(0.5*sd_map)),
                         floor(mean_map+(0.5*sd_map)),
                         floor(mean_map+(1*sd_map)),
                         floor(mean_map+(2*sd_map))))
param_result2 <- fun_extract_par("systolic",bp_data_cleaned,lower_limits,upper_limits,trun_check = 60,interval_list)
write.csv(param_result2$parameters,"bp_map_parameters.csv")



check_data_BP <- function(data_temp){
  
  count_sys <- nrow(data_temp)
  if(count_sys<20){
    return(TRUE)
  }else 
    return(FALSE)
  
}