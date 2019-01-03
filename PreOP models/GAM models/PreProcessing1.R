# Set of basic user defined functions 
# Created by Paul Thottakkara : Last Updated on Nov 30 2015 

# libraries required
library(mgcv) ; library(verification) ; library(VIF) ;library(e1071) ; library(randomForest)

# Perfoming analysis using univariate models
univ_analysis <- function(model_data,feat_all){
  fm<-binomial();  fm$link = "logit";
  p_ind <- which(model_data$outcome==1) ; n_ind <- which(model_data$outcome==0) 
  rest_uni <- data.frame(matrix(nrow=nrow(feat_all),ncol=4))
  colnames(rest_uni) <- c("Variable","Z_val_Log_Reg","P_val_Log","P_val_t_test")
  for(i in 1:nrow(feat_all)){
    rest_uni[i,1] <- as.character(feat_all[i,1]) ; print(i)
    ind <- which(colnames(model_data)==feat_all[i,1])
    s <- as.formula(paste("outcome~",feat_all[i,1]))
    model_temp <- glm(formula = s,family = fm,data=model_data) ; sm <- summary(model_temp)
    if(feat_all[i,2]=="num"){
      #s_g <- as.formula(paste("outcome~s(",feat_all[i,1],",k=5)",sep=""))
      #model_gam <- gam(formula = s_g,family = fm,data=model_data)
      #plot(model_gam,trans=function(x)exp(x)/(1+exp(x)),shade=T,page=1,main = paste("Risk plot: ",feat_all[i,1],sep=""))
      
      # z value and p value
      rest_uni[i,2] <- round(sm$coefficients[2,3],1) ; rest_uni[i,3] <- round(sm$coefficients[2,4],3) 
      t_test <- t.test(model_data[p_ind,ind],model_data[n_ind,ind])
      rest_uni[i,4] <- round(t_test$p.value,3)
    }else if(length(unique(model_data[,ind]))>5){
      # z value and p value
      rest_uni[i,2] <- round(sm$coefficients[2,3],1) ; rest_uni[i,3] <- round(sm$coefficients[2,4],3) 
      t_test <- t.test(model_data[p_ind,ind],model_data[n_ind,ind])
      rest_uni[i,4] <- round(t_test$p.value,3)
    }else{
      rest_uni[i,2]  <- paste(round(sm$coefficients[-1,3],3),collapse = ",")
      rest_uni[i,3]  <- paste(round(sm$coefficients[-1,4],3),collapse = ",")
    }
  }
  return(rest_uni)
}

gen_data_uni <- function(raw_data,feature_list,outcome){
  t <- rep(0,length(outcome)) ; t[outcome==1] <- 1 ; outcome <- t 
  
  # replace missing values in admission source
  raw_data$Admission_Source[raw_data$Admission_Source==""] <- "outpatient"
  
  for(i in 1:nrow(feature_list)){
    ind <- which(colnames(raw_data)==feature_list[i,1])
    if(feature_list[i,2]=="num"){
      raw_data[,ind] <- as.numeric(raw_data[,ind]) ; next
    }
    if(feature_list[i,2]=="cat"&feature_list[i,1]=="pr1c"){
      t <- raw_data[,ind] ; t <- gsub("[^0-9]","",t)
      d<-TrainProcedureFeature(t, outcome, 100)
      raw_data[,ind] <- d$procedures ; next
    }
    if(feature_list[i,2]=="cat"&length(unique(raw_data[,ind]))>5){
      d<-TrainCategoricalFeature(raw_data[,ind], outcome, 100, 2)          
      raw_data[,ind] <- d$d	; next
    }else{
      raw_data[,ind] <- as.factor(raw_data[,ind])
    }
  }
  return(raw_data)
}

# Summary of variables
#### Basic functions used for analysis
variable_sum <- function(var,type,var_name){
  output1 <- data.frame(matrix(nrow=1,ncol=5)) ; colnames(output1) <- c("var_name","var_type","sum_Stat","Ref","Missing")
  if(type=="cat"){
    if(length(unique(var))==2){
      l <- levels(factor(var)) ; output1$Ref <- l[1] ; output1$var_name <- paste(var_name,": ",l[2],sep = "") ; output1$var_type <- type ; 
      c <- sum(var==l[2]) ; p <- round(100*c/length(var),1) ; output1$sum_Stat <- paste(c," (",p,")",sep="") ; output1$Missing <- sum(is.na(var)|var==""|var=="NA")
    }
    if(length(unique(var))<=5 & length(unique(var))>2 ){
      l <- levels(factor(var)) 
      output1 <- data.frame(matrix(nrow=length(l)+1,ncol=4)) ; colnames(output1) <- c("var_name","var_type","sum_Stat","Ref")
      output1$Ref[1] <- l[1] ; output1$var_name[1] <- as.character(var_name) ; output1$var_type[1] <- as.character(type) ; output1$sum_Stat[1] <- "n(%)"
      output1$Missing[1] <- sum(is.na(var)|var==""|var=="NA")
      for(i in 1:length(l)){
        c <- sum(var==l[i]) ; p <- round(100*c/length(var),1) ; output1$sum_Stat[i+1] <- paste(c," (",p,")",sep="") ; output1$var_name[i+1] <- l[i]
      }
    }
    if(length(unique(var))>5){
      tab <- sort(table(var),decreasing = TRUE)
      output1 <- data.frame(matrix(nrow=6,ncol=4)) ; colnames(output1) <- c("var_name","var_type","sum_Stat","Ref")
      output1$var_name[1] <- as.character(var_name) ; output1$var_type[1] <- as.character(type) ; output1$Ref[1] <- ">5 levels" ; output1$sum_Stat[1] <- "n(%)"
      output1$Missing[1] <- sum(is.na(var)|var==""|var=="NA")
      for(i in 1:5){
        c <- tab[i] ; p <- round(100*c/length(var),1) ; output1$sum_Stat[i+1] <- paste(c," (",p,")",sep="") ; output1$var_name[i+1] <- names(tab[i])
      }
    }
  }
  if(type=="num"){
    rnd=1
    output1$var_name[1] <- as.character(var_name) ; output1$var_type[1] <- as.character(type) ; m <- round(median(var,na.rm = TRUE),rnd) ; 
    qt1 <- round(quantile(var,probs = 0.25,na.rm = TRUE),rnd) ; qt2 <- round(quantile(var,probs = 0.75,na.rm = TRUE),rnd)
    output1$sum_Stat[1] <- paste(m," (",qt1,",",qt2,")",sep = "")
    output1$Missing[1] <- sum(is.na(var)|var==""|var=="NA")
  }
  return(output1)
}
vect_sum <- function(var,rnd){
  m <- round(mean(var,na.rm = TRUE),rnd)
  s <- sd(var,na.rm = TRUE) ; s1 <- round(m-1.96*s,rnd) ; s2 <- round(m+1.96*s,rnd) 
  return(paste(m," (",s1,",",s2,")",sep=""))
}
create_summary_table <- function(data,features_sel){
  tab_output <- NULL ; 
  for(i in 1:nrow(features_sel)){
    print(as.character(features_sel$feature_name[i]))
    var <- data[,which(colnames(data)==features_sel$feature_name[i])] ;
    var_name <- features_sel$feature_name[i] ; type <- features_sel$feature_type[i]
    tab_output <- rbind(tab_output,variable_sum(var,type,var_name))
  }
  return(tab_output)
  
}

############
TrainCategoricalFeature <- function(a, outcome, limit, clusters){
  
  if (length(unique(a)) == 2){
    vocabulary = unique(a)
    p = c(0,1)
    d<-rep(0, length(a))
    d[a == vocabulary[2]] = 1      
    return(list("d" = d, "vocabulary" = vocabulary, "p" = p))
  }	
  cl_vocabulary<-NULL
  cl_p<-NULL	
  vocabulary1<-unique(a) 	
  count1<-rep(0, times  = length(vocabulary1))
  count2<-rep(0, times  = length(vocabulary1))	
  for (i in 1:length(vocabulary1)){
    if (is.na(vocabulary1[i])){
      count1[i]<-sum((is.na(a))*(outcome==1))
      count2[i]<-sum((is.na(a))*(outcome==0))
    } else{
      count1[i]<-sum((a[!is.na(a)] == vocabulary1[i])*(outcome[!is.na(a)] == 1))
      count2[i]<-sum((a[!is.na(a)] == vocabulary1[i])*(outcome[!is.na(a)] == 0))}
  }
  condition<-(count1+count2>=limit)
  condition[which(is.na(vocabulary1))]<-TRUE	
  index<-sum(condition)
  vocabulary<-(1:index)
  p<-rep(0, times  = length(vocabulary))
  index<-0
  for (i in (1:length(vocabulary1))){
    if (count1[i]+count2[i]>=limit || is.na(vocabulary1[i])){
      index<-index+1
      vocabulary[index]<-as.character(vocabulary1[i])
      if (count1[i] ==0 )
        p[index]<-log(1/(2*count2[i]))
      else if (count2[i] == 0)
        p[index]<-log(2*count1[i])
      else
        p[index]<-log(count1[i]/count2[i])
    }
  } 
  if (sum(condition==0)<=1){
    if (sum(condition==0) == 1){
      index<-index+1
      if (count1[which(condition==0)] ==0 )
        p[index]<-log(1/(2*count2[which(condition==0)]))
      else if (count2[which(condition==0)] == 0)
        p[index]<-log(2*count1[which(condition==0)])
      else
        p[index]<-log(count1[which(condition==0)]/count2[which(condition==0)])
      vocabulary[index]<-as.character(vocabulary1[which(condition==0)])
    }
  } else{
    cl<-min(clusters, sum(condition == 0)-1)
    residual1<-count1[condition == 0]
    residual2<-count2[condition == 0]
    s<-matrix(rep(0, times = length(residual1)), ncol = 1)
    for (i in 1:length(residual1)){
      if (residual1[i] ==0 )
        s[i]<-log(1/(2*residual2[i]))
      else if (residual2[i] == 0)
        s[i]<-log(2*residual1[i])
      else
        s[i]<-log(residual1[i]/residual2[i])
    }
    v<-kmeans(s, cl)
    for (i in (1:cl)){
      idx1<-(1:length(condition))[condition == 0]
      cl_vocabulary<-data.frame(matrix(nrow = sum(condition == 0), ncol = 2))
      cl_vocabulary[,1]<-as.character(vocabulary1[condition == 0])
      cl_vocabulary[,2]<-v$cluster			
      c1<- count1[idx1[v$cluster == i]]
      c2<- count2[idx1[v$cluster == i]]
      if (sum(c1) ==0 )
        cl_p[i]<-log(1/(2*sum(c2)))
      else if (sum(c2) == 0)
        cl_p[i]<-log(2*sum(c1))
      else
        cl_p[i]<-log(sum(c1)/sum(c2))
    }
  }
  d<-rep(0, times = length(a))
  d[is.na(a)]<-p[which(is.na(vocabulary))] 
  for (i in 1:length(vocabulary))
    d[a==vocabulary[i]]<-p[i]
  if(!is.null(cl_vocabulary)){
    for (i in 1:nrow(cl_vocabulary))
      d[a==cl_vocabulary[i,1]]<-cl_p[cl_vocabulary[i,2]]
  }
  output<-list("d" = d, "vocabulary" = vocabulary, "cl_vocabulary" = cl_vocabulary, "p" = p, "cl_p" = cl_p)
}

## used to handle the pr1 feature (the procedures followed), network pruned to generate levels that has statistical significance
TrainProcedureFeature <-function(data, outcome, limit){
  lowlim<- c(100, 101, 106, 108, 117, 118, 121, 130, 135, 140, 142, 155, 160, 165, 172, 176, 185, 187)
  procedures<-rep(0, times = length(data))
  groups<-data.frame(matrix(rep(0, times = 20000*2),ncol = 2))
  
  for (i in 1:length(data)){
    if (is.na(data[i]))
      data[i]<-"00"  
    s<-as.character(data[i])
    for (k in nchar(s):-1:2){
      j<-as.numeric(paste("1", substr(s,1,k), sep = ""))
      if (outcome[i] == 1)
        groups[j,1]<-groups[j,1]+1
      else
        groups[j,2]<-groups[j,2]+1		
    }
    num<-as.numeric(paste("1", substr(s,1,2), sep = ""))
    for (j in (length(lowlim)+1):-1:2){
      if (num >= lowlim[j-1]){
        if (outcome[i] == 1)
          groups[j,1]<-groups[j,1]+1
        else
          groups[j,2]<-groups[j,2]+1
        break
      }
    }	
  }
  
  for (i in 2:(length(lowlim)+1)){
    if (groups[i,1]+groups[i,2] < limit){
      groups[1,1] = groups[1,1]+groups[i,1];
      groups[1,2] = groups[1,2]+groups[i,2];
      groups[i,1] = 0;
      groups[i,2] = 0;
      
    }
  }
  
  for (i in 1:length(data)) {
    flag<-0
    k<-0
    s<-as.character(data[i])
    while (k <= nchar(s) - 2){
      ind<-as.numeric(paste("1", substr(s,1,(nchar(s) - k)),sep = ""))
      if (groups[ind,1]+groups[ind,2] >= limit){
        if (groups[ind,1] == 0){
          procedures[i]<-log(1/(2*groups[ind,2]))
        }
        else if (groups[ind,2] == 0){
          procedures[i]<-log(2*groups[ind,1])
        }
        else{
          procedures[i] = log(groups[ind,1]/groups[ind,2]); 
        }
        flag<-1
        break	
      }
      k<-k+1
    }
    if (flag == 0){
      q<-as.numeric(paste("1", substr(s,1,2), sep = ""))
      for (j in (length(lowlim)+1):-1:2){
        if (q >= lowlim[j-1]){
          if (groups[j,1]+groups[j,2] > 0){
            if (groups[j,1] == 0){
              procedures[i]<-log(1/(2*groups[j,2]))
            }
            else if (groups[j,2] == 0){
              procedures[i]<-log(2*groups[j,1])
            }
            else{
              procedures[i] = log(groups[j,1]/groups[j,2]); 
            }
          }
          else{
            if (groups[1,1] == 0){
              procedures[i]<-log(1/(2*groups[1,2]))
            }
            else if (groups[1,2] == 0){
              procedures[i]<-log(2*groups[1,1])
            }
            else{
              procedures[i] = log(groups[1,1]/groups[1,2]); 
            }
            
          }
          break
        }
      }
    }
  }
  output<-list("procedures" = procedures, "groups" = groups)
}
