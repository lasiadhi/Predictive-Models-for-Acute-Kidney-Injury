data=read.csv("monitor_comb_all_fnl.csv")
##count-total - 92762115

##unique account-10762
##unqiue mrn- 10037

data_pat=data
vars <-c("etco2");
data_pat<- data_pat[which(data_pat$type %in% vars),]
check1=10
ll=0
ul=60 
pat_data <- cbind(data_pat,paste(data_pat$date,data_pat$time))
patient_sur_dt=read.csv("BP_acc_Sum.csv")
bp_data_final <- data.frame(matrix(ncol=3)) ; 
colnames(bp_data_final) <- c("acc","time_stamp","obser")  
colnames(pat_data)[10] <- "Timestamp"
for(acc in unique(pat_data$Account))
{
  print(acc);
  pat_data_acc=pat_data[pat_data$Account==acc,]
  pat_data_acc$value <- as.numeric(pat_data_acc$value); 
  pat_data_acc$Timestamp <- as.POSIXlt(pat_data_acc$Timestamp,format="%m/%d/%Y %I:%M:%S %p") 
  ## read file having surgery details
  ind <- which(patient_sur_dt$acc==acc)
  if(length(ind)>0){
    time_info <- patient_sur_dt[ind,] ; index <- which(time_info$ssi_op_id==min(time_info$ssi_op_id))
    start_sur <- time_info$start_time[index];
    end_sur <- time_info$end_time[index];
    time_ind <- which(pat_data_acc$Timestamp <= as.POSIXct(end_sur) & pat_data_acc$Timestamp >= as.POSIXct(start_sur)) ; 
    pat_data_acc <- pat_data_acc[time_ind,]
  }else
    print("acc match not found in key file")
  if(nrow(pat_data_acc)<1)
    next;
  order_time <- sort(unique(pat_data_acc$Timestamp))
  # order data based on time, take mean for repeating values and remove the extreme values.
  et_data <- data.frame(matrix(ncol=3,nrow=length(order_time))) ; colnames(et_data) <- c("acc","time_stamp","obser")    
  et_data$time_stamp <- order_time
  if(nrow(et_data)<10)
    return(et_data)
  for(i in 1:length(order_time)){
    temp_data <- pat_data[which(pat_data_acc$Timestamp==order_time[i]),]
    if(length(vars)==1)
      temp_ind <- which(temp_data$type==vars)
    if(length(vars)==2)
      temp_ind <- which(temp_data$type==vars[1]|temp_data$type==vars[2])
    if(length(temp_ind)>0)
      et_data$exp_gas[i] <- mean(temp_data$value[temp_ind],na.rm=TRUE)
    if(et_data$exp_gas[i]<ll|et_data$exp_gas[i]>ul)
      et_data$exp_gas[i] <- NA
  }
  # find out the missing values and if we have more than 3 consecutive missing values delete those time frames
  NA_count <- array(dim=nrow(et_data)) ; NA_count[1] <- 0;
  for(i in 2:length(NA_count)){
    NA_count[i] <- ifelse(is.na(et_data$exp_gas[i]),NA_count[i-1]+1,0)
  }
  del_ind <- which(NA_count > 3);
  if(length(del_ind)>0)
    et_data <- et_data[-del_ind,]
  if(nrow(et_data)<6)
    next;
  # replace the remaining NA values with approximated values (using moving averages)
  #lin_approx <- filter(et_data$exp_gas,rep(1/5,5),method="convolution",sides=2,circular=TRUE)
  #rep_ind <- which(is.na(et_data$exp_gas)) ; et_data$exp_gas[rep_ind] <- lin_approx[rep_ind]
  
  # instead use approximate missing values with the average of closest 5 observations
  for(i in 1:nrow(et_data)){
    if(is.na(et_data$exp_gas[i])){
      temp_ind <- sort.int(abs(i-1:nrow(et_data)),index.return=TRUE)$ix[1:5]
      et_data$exp_gas[i] <- round(mean(et_data$exp_gas[temp_ind],na.rm=TRUE),0)
    }
  }
  if(nrow(et_data)<6)
    next;
  lin_approx <- filter(et_data$exp_gas,rep(1/5,5),method="convolution",sides=2,circular=TRUE)
  diff <- abs(et_data$exp_gas-lin_approx); diff_ind <- which(diff>check1)
  if(length(diff_ind)>1)
    et_data$exp_gas[diff_ind] <- lin_approx[diff_ind]
  bp_data_final=rbind(et_data,bp_data_final);
  
}

