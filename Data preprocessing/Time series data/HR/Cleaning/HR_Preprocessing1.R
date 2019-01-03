setwd("d:/desktop")
data=read.csv("correct_monitor_datal.csv")


data_pat=data
vars <- c("hr","heart rate") ; 
data_pat <- data_pat[which((data_pat$type %in% vars)),]
data_pat=data_pat[which(data_pat %in% na.omit(data_pat$Account)),]
#new dataframe with with addition datetimestamp column
pat_data <- cbind(data_pat,paste(data_pat$date,data_pat$time))
patient_sur_dt=read.csv("BP_acc_Sum.csv")
bp_std_vals=read.csv("BP_std_vals.csv")
bp_data_final <- data.frame(matrix(ncol=5)) ; 
colnames(bp_data_final) <- c("time_stamp","hr","Acc_No")  
colnames(pat_data)[10] <- "Timestamp"
for(acc in unique(pat_data$Account))
{
  acc <- data_pat$Acc_No[1] ; data_pat <- data_pat[which(data_pat$Obser_Type %in% vars),]
  pat_data <- cbind(data_pat,paste(data_pat$Date,data_pat$Time))
  colnames(pat_data) <- c(colnames(data_pat),"Timestamp")
  pat_data$Obser_Value <- as.numeric(pat_data$Obser_Value); pat_data$Timestamp <- as.POSIXlt(pat_data$Timestamp,format="%m/%d/%Y %I:%M:%S %p") 
  
  ind <- which(patient_sur_dt$acc==acc)
  if(length(ind)>0){
    time_info <- patient_sur_dt[ind,] ; index <- which(time_info$ssi_op_id==min(time_info$ssi_op_id))
    start_sur <- as.POSIXlt(paste(time_info$st_date[index],time_info$st_time[index]),format="%m/%d/%Y %I:%M:%S %p") 
    end_sur <- as.POSIXlt(paste(time_info$end_date[index],time_info$end_time[index]),format="%m/%d/%Y %I:%M:%S %p")
    time_ind <- which(pat_data$Timestamp <= end_sur & pat_data$Timestamp >= start_sur) ; pat_data <- pat_data[time_ind,]
  }else
    print("acc match not found in key file")
  
  order_time <- sort(unique(pat_data$Timestamp))
  # order data based on time, take mean for repeating values and remove the extreme values.
  hr_data <- data.frame(matrix(ncol=2,nrow=length(order_time))) ; colnames(hr_data) <- c("time_stamp","heart_rate")  
  hr_data$time_stamp <- order_time
  if(nrow(hr_data)<20)
    return(hr_data)
  for(i in 1:length(order_time)){
    temp_data <- pat_data[which(pat_data$Timestamp==order_time[i]),]
    temp_ind <- which(temp_data$Obser_Type==vars[1]|temp_data$Obser_Type==vars[2])
    if(length(temp_ind)>0)
      hr_data$heart_rate[i] <- mean(temp_data$Obser_Value[temp_ind],na.rm=TRUE)
    if(hr_data$heart_rate[i]<1|hr_data$heart_rate[i]>219)
      hr_data$heart_rate[i] <- NA
  }
  # find out the missing values and if we have more than 3 consecutive missing values delete those time frames
  NA_count <- array(dim=nrow(hr_data)) ; NA_count[1] <- 0;
  for(i in 2:length(NA_count)){
    NA_count[i] <- ifelse(is.na(hr_data$heart_rate[i]),NA_count[i-1]+1,0)
  }
  del_ind <- which(NA_count > 3);
  if(length(del_ind)>0)
    hr_data <- hr_data[-del_ind,]
  # replace the remaining NA values with approximated values (using moving averages)
  #lin_approx <- filter(hr_data$heart_rate,rep(1/5,5),method="convolution",sides=2,circular=TRUE)
  #rep_ind <- which(is.na(hr_data$heart_rate)) ; hr_data$heart_rate[rep_ind] <- lin_approx[rep_ind]
  
  # instead use approximate missing values with the average of closest 5 observations
  for(i in 1:nrow(hr_data)){
    if(is.na(hr_data$heart_rate[i])){
      temp_ind <- sort.int(abs(i-1:nrow(hr_data)),index.return=TRUE)$ix[1:5]
      hr_data$heart_rate[i] <- round(mean(hr_data$heart_rate[temp_ind],na.rm=TRUE),0)
    }
  }
  lin_approx <- filter(hr_data$heart_rate,rep(1/5,5),method="convolution",sides=2,circular=TRUE)
  diff <- abs(hr_data$heart_rate-lin_approx); diff_ind <- which(diff>30)
  if(length(diff_ind)>1)
    hr_data$heart_rate[diff_ind] <- lin_approx[diff_ind]
  return(hr_data)
  bp_data_final=rbind(bp_data,bp_data_final);
}