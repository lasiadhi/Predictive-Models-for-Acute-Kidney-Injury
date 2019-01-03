# 3 parameters mean and var of base signal and var of residual signal
para_Saria <- function(data_vect){
  output_sar <- data.frame(matrix(nrow=1,ncol=3))
  colnames(output_sar) <- c("mean_base","var_long","var_short")
  lin_approx <- filter(data_vect,rep(1/5,5),method="convolution",sides=2,circular=TRUE)
  output_sar[1,1] <- round(mean(lin_approx,na.rm=TRUE),digits=1)
  output_sar[1,2] <- round(sqrt(var(lin_approx,na.rm=TRUE)),digits=1)
  output_sar[1,3] <- round(sqrt(var(abs(lin_approx-data_vect),na.rm=TRUE)),digits=1)
  
  #   if(mean(data_vect,na.rm=TRUE)<10){
  #     output_sar[1,1] <- round(mean(lin_approx,na.rm=TRUE),digits=2)
  #     output_sar[1,2] <- round(sqrt(var(lin_approx,na.rm=TRUE)),digits=2)
  #     output_sar[1,3] <- round(sqrt(var(abs(lin_approx-data_vect),na.rm=TRUE)),digits=2)
  #   }
  return(output_sar)
}

para_extract <- function(data_clean,check_ll,check_ul,interval_list){
  saria_res <- para_Saria(data_clean) ; #other_res <- para_Other(ex_data_clean$obser) ; 
  limit_val <- limit_param(data_clean,check_ll,check_ul,interval_list)
  low <- min(data_clean,na.rm = TRUE) ; high <- max(data_clean,na.rm = TRUE)
  dur <- length(data_clean)
  
  output <- cbind(saria_res,limit_val,low,high,dur)
  output <- as.data.frame(output) ; colnames(output) <- c(colnames(saria_res),colnames(limit_val),"Min","Max","Duration")
  
  return(output)
  
  
}

limit_param <- function(data_clean,check_ll,check_ul,interval_list){
  for(i in 1:length(check_ll)){
    if(i==1){
      res_ll <- per_time_ll(data_clean,check_ll[i])
      name_c <- paste(c("count_ll_","perct_ll_"),check_ll[i],sep="")
    }else{
      temp <- per_time_ll(data_clean,check_ll[i])
      res_ll <- cbind(res_ll,temp)
      name_c <- c(name_c,paste(c("count_ll_","perct_ll_"),check_ll[i],sep=""))
    }
  }
  res_ll <- as.data.frame(res_ll) ; colnames(res_ll) <- name_c
  
  for(i in 1:length(check_ul)){
    if(i==1){
      res_ul <- per_time_ul(data_clean,check_ul[i])
      name_c <- paste(c("count_ul_","perct_ul_"),check_ul[i],sep="")
    }else{
      temp <- per_time_ul(data_clean,check_ul[i])
      res_ul <- cbind(res_ul,temp)
      name_c <- c(name_c,paste(c("count_ul_","perct_ul_"),check_ul[i],sep=""))
    }
  }
  res_ul <- as.data.frame(res_ul) ; colnames(res_ul) <- name_c
  
  for(i in 1:nrow(interval_list)){
    if(i==1){
      res_i <- interval_check(data_clean,interval_list[i,])
      name_c <- c(paste(c("range_","perct_range_"),interval_list[i,1],"_",interval_list[i,2],sep=""))
    }else{
      temp <- interval_check(data_clean,interval_list[i,])
      res_i <- cbind(res_i,temp)
      name_c <- c(name_c,paste(c("range_","perct_range_"),interval_list[i,1],"_",interval_list[i,2],sep=""))
    }
  }
  res_i <- as.data.frame(res_i) ; colnames(res_i) <- name_c
  
  temp <- cbind(res_ll,res_ul,res_i)
  return(temp)
}

interval_check <- function(data_clean,interval){
  l <- min(interval,na.rm = TRUE) ; u <- max(interval,na.rm = TRUE)
  count_int  <- sum((as.numeric(data_clean)>=l&as.numeric(data_clean)<u),na.rm = TRUE) 
  per_count <- round(100*count_int/length(data_clean),1)
  return(cbind(count_int,per_count))
}
per_time_ll <- function(data_clean,ll){
  less_ll <- sum(as.numeric(data_clean)<ll,na.rm = TRUE) ; per_ll <- round(100*less_ll/length(data_clean),1)
  return(cbind(less_ll,per_ll))
}
per_time_ul <- function(data_clean,ul){
  grt_ul <- sum(as.numeric(data_clean)>ul,na.rm = TRUE) ; per_ul <- round(100*grt_ul/length(data_clean),1)
  return(cbind(grt_ul,per_ul))
}

fun_extract_par <- function(var,bp_data_cleaned,lower_limits,upper_limits,trun_check,interval_list){
  

  data_intra=bp_data_cleaned
  unique_accs <- unique(data_intra$acc) ; iss_ind <- 1
  acc_with_gap <- array(dim = 10) ; ind_gap <- 1 ; in_len <- 10
  issue_accs <- data.frame(matrix(nrow=in_len,ncol=3)) ;  colnames(issue_accs) <- c("acc","start_time","end_time")
  for(i in 1:length(unique_accs)){
    temp_data <- data_intra[data_intra$acc==unique_accs[i],]
    if(i%%100==0)
      print(i);
    
    # check for gap in time series
    check_gap <- truncate_fun(temp_data$Timestamp,trun_check)
    if(check_gap$check){
      print("gap_detected"); print(i)
      acc_with_gap[ind_gap] <- temp_data$acc[1]
      temp_data <- temp_data[check_gap$index,] ; ind_gap <- ind_gap+1
    }
    
    if(nrow(temp_data)<15){
      if(iss_ind>in_len){
        temp <- issue_accs ; in_len <- in_len+10
        issue_accs <- data.frame(matrix(nrow=in_len,ncol=3)) ;  colnames(issue_accs) <- c("acc","start_time","end_time")
        issue_accs[1:(in_len-10),] <- temp 
      }
      issue_accs$acc[iss_ind] <- temp_data$acc[1] ; issue_accs$start_time[iss_ind] <- as.character(temp_data$Timestamp[1])
      issue_accs$end_time[iss_ind] <- as.character(temp_data$time_stamp[nrow(temp_data)]) ; iss_ind <- iss_ind+1
      next
    }
    if(var=="systolic")
    {
    if(i==1){
      result <- para_extract(temp_data$systolic,lower_limits,upper_limits,interval_list) 
      result$acc <- unique_accs[i] ; next
    }else{
      temp_result <- para_extract(temp_data$systolic,lower_limits,upper_limits,interval_list) 
      temp_result$acc <- unique_accs[i]
    }
    }
    if(var=="diastolic")
    {
      if(i==1){
        result <- para_extract(temp_data$diastolic,lower_limits,upper_limits,interval_list) 
        result$acc <- unique_accs[i] ; next
      }else{
        temp_result <- para_extract(temp_data$diastolic,lower_limits,upper_limits,interval_list) 
        temp_result$acc <- unique_accs[i]
      }
    }
    if(var=="map")
    {
      if(i==1){
        result <- para_extract(temp_data$map,lower_limits,upper_limits,interval_list) 
        result$acc <- unique_accs[i] ; next
      }else{
        temp_result <- para_extract(temp_data$map,lower_limits,upper_limits,interval_list) 
        temp_result$acc <- unique_accs[i]
      }
    }
    result <- rbind(result,temp_result)
  }
  return(list("parameters"=result,"acc_with_gap"=acc_with_gap))
}

truncate_fun <- function(time_series,trun_check){
  len <- length(time_series) 
  temp1 <- time_series[1:(len-1)] ; temp2 <- time_series[2:len] ; tdiff <- as.numeric(difftime(temp2,temp1,units = "mins"))
  if(sum(tdiff>trun_check)!=0){
    ind <- which(tdiff>trun_check) ;
    if(length(ind)==1){
      if(ind>len/2){
        index <- 1:ind
      }else{
        index <- ind:len
      }
    }else{
      temp11 <- c(1,ind); temp22 <- c(ind,len) ; ind_diff <- temp22-temp11 ; ind_s <- which(ind_diff==max(ind_diff))
      index <- temp11[ind_s]:(temp22[ind_s]-1)
    }
    return(list("index"=index,"check"=TRUE))
    
  }else{
    index <- 1:len
    return(list("index"=index,"check"=FALSE))
  }
  
}




