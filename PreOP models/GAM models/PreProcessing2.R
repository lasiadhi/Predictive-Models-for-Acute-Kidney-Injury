# Set of user defined functions for model building and performance reporting
# Created by Paul Thottakkara : Last Updated on 2 December 2015 

# develop gams model and report performance
run_gams_model <- function(proc_data,feature_list){
  if("outcome"%in%colnames(proc_data)){print("outcome variable found")}else{
    print("outcome variable missing");
    return(list("model"=NULL,"testResults_rawData"=NULL,"trainResults_rawData"=NULL,"predicted_values"=NULL,"status"="FAIL"))
  }
  response <- rep(0,nrow(proc_data)) ; response[proc_data$outcome==1] <- 1
  fm <- binomial() ; fm$link <- "logit"
  s <- "outcome~"
  for(i in 1:nrow(feature_list)){
    if(feature_list[i,2]=="num"){s <- paste0(s,"s(",feature_list[i,1],",k=5)+")}else{
      s <- paste0(s,feature_list[i,1],"+")
    }
  }
  s <- substr(s,1,nchar(s)-1) ; s <- as.formula(s)
  
  test_result <- NULL ; train_result <- NULL ; thres_tune <- round(sum(proc_data$outcome==1)/nrow(proc_data),3)
  data_model <- proc_data;
  print("Running GAMs iterations to report training and test performance")
  for(i in 1:50){
      print(i) ; 
      train_ind <- sample(1:nrow(data_model),size = floor(0.7*nrow(data_model)))
      test_ind <- setdiff(1:nrow(data_model),train_ind)
      model <- bam(formula = s,family = fm,data = data_model[train_ind,])
      temp <- perf_gam(model,traindata=data_model[train_ind,],testdata=data_model[test_ind,],thres_tune)
      train_result <- rbind(train_result,temp$train_result)
      test_result <- rbind(test_result,temp$test_result)
  }
  model = bam(formula = s,family = fm,data = data_model)
  print("Model building completed")
  pred <- predict(model,data_model,type="response") ; 
  pred_val <- as.data.frame(cbind(response,pred)) ; colnames(pred_val) <- c("observed","predicted")
  pred_val$observed <- rep(0,nrow(pred_val)) ; pred_val$observed[which(response==1)] <- 1
  
  print("+++++++++++++++++  Completed +++++++++++++++++++")
  return(list("model"=model,"testResults_rawData"=test_result,"trainResults_rawData"=train_result,"predicted_values"=pred_val,"status"="PASS"))
}

run_gams_model_single_run <- function(proc_data,feature_list){
  if("outcome"%in%colnames(proc_data)){print("outcome variable found")}else{
    print("outcome variable missing");
    return(list("model"=NULL,"predicted_values"=NULL,"status"="FAIL"))
  }
  response <- rep(0,nrow(proc_data)) ; response[proc_data$outcome==1] <- 1
  fm <- binomial() ; fm$link <- "logit"
  s <- "outcome~"
  for(i in 1:nrow(feature_list)){
    if(feature_list[i,2]=="num"){s <- paste0(s,"s(",feature_list[i,1],",k=5)+")}else{
      s <- paste0(s,feature_list[i,1],"+")
    }
  }
  s <- substr(s,1,nchar(s)-1) ; s <- as.formula(s)
  
  test_result <- NULL ; train_result <- NULL ; thres_tune <- round(sum(proc_data$outcome==1)/nrow(proc_data),3)
  data_model <- proc_data;
  train_ind <- sample(1:nrow(proc_data),floor(0.7*nrow(proc_data)))
  print("Running GAMs model")
  model = bam(formula = s,family = fm,data = data_model[train_ind,])
  print("Model building completed")
  pred <- predict(model,data_model,type="response") ; 
  pred_val <- as.data.frame(cbind(response,pred)) ; colnames(pred_val) <- c("observed","predicted")
  pred_val$observed <- rep(0,nrow(pred_val)) ; pred_val$observed[which(response==1)] <- 1
  
  print("+++++++++++++++++  Completed +++++++++++++++++++")
  return(list("model"=model,"predicted_values"=pred_val,"status"="PASS"))
}

run_SVM_model_single_run <- function(proc_data,feature_list,cost,gamma){
  if("outcome"%in%colnames(proc_data)){print("outcome variable found")}else{
    print("outcome variable missing");
    return(list("model"=NULL,"predicted_values"=NULL,"status"="FAIL"))
  }
  response <- rep(0,nrow(proc_data)) ; response[proc_data$outcome==1] <- 1
  x <- proc_data[,which(colnames(proc_data)%in%feature_list[,1])];
  y <- as.factor(proc_data$outcome)
  print("Running SVM model")
  result <- NULL
  print(paste0("cost=",cost,",","gamma=",gamma)) ;
  train_ind <- sample(1:nrow(x),floor(0.7*nrow(x)),replace = T) 
  model = svm(x = x[train_ind,],y = y[train_ind],type = "C-classification", kernel ="radial",cost = cost,gamma=gamma,tolerance = 0.01,epsilon = 0.1,probability = TRUE)
  print("Model building completed")
  pred_temp <- predict(model,x,decision.values = TRUE,probability = T) ; 
  pred <- attr(pred_temp, "probabilities") ; pred <- pred[,which(colnames(pred)==1)]
  pred_val <- as.data.frame(cbind(response,pred)) ; colnames(pred_val) <- c("observed","predicted")
  pred_val$observed <- rep(0,nrow(pred_val)) ; pred_val$observed[which(response==1)] <- 1
  
  print("+++++++++++++++++  Completed +++++++++++++++++++")
  return(list("model"=model,"predicted_values"=pred_val,"status"="PASS"))
}

run_SVM_model_gridsearch_run <- function(proc_data,feature_list){
  if("outcome"%in%colnames(proc_data)){print("outcome variable found")}else{
    print("outcome variable missing");
    return(list("model"=NULL,"predicted_values"=NULL,"status"="FAIL"))
  }
  response <- rep(0,nrow(proc_data)) ; response[proc_data$outcome==1] <- 1
  x <- proc_data[,which(colnames(proc_data)%in%feature_list[,1])];
  y <- as.factor(proc_data$outcome)
  print("Running SVM model")
  cost_values <- c(10,100,1000) ; gamma_values <- c(0.001,0.01,0.005,0.0001,0.05)
  result <- NULL
  for(i in 1:length(cost_values)){
    for(j in 1:length(gamma_values)){
      cost = cost_values[i] ; gamma = gamma_values[j]
      print(paste0(i,":cost=",cost,",","gamma=",gamma)) ;
      train_ind <- sample(1:nrow(x),floor(0.6*nrow(x)),replace = T) 
      model = svm(x = x[train_ind,],y = y[train_ind],type = "C-classification", kernel ="radial",cost = cost,gamma=gamma,tolerance = 0.01,epsilon = 0.1,probability = TRUE)
      for(k in 1:5){
        test_ind <- sample(1:nrow(x),floor(0.3*nrow(x)),replace = T) 
        pred_temp <- predict(model,x[test_ind,],decision.values = TRUE,probability = T) ; 
        pred <- attr(pred_temp, "probabilities") ; pred <- pred[,which(colnames(pred)==1)]
        pred_resp <- rep(0,length(pred)) ; pred_resp[which(pred>=0.5)] <- 1 ; y1 <- y[test_ind]
        tab <- table(y1,pred_resp) ; acc <- round(100*sum(tab[c(1,4)])/sum(tab),1)
        ppv <- round(100*sum((pred_resp==1&y1==1))/sum(pred_resp==1),1)
        npv <- round(100*sum((pred_resp==0&y1==0))/sum(pred_resp==0),1)
        temp <- cbind(cost,gamma,acc,ppv,npv) ; print(temp) ; result <- rbind(result,temp)
      }
    }
  }
  
  print("Model building completed")
  pred_temp <- predict(model,x,decision.values = TRUE,probability = T) ; 
  pred <- attr(pred_temp, "probabilities") ; pred <- pred[,which(colnames(pred)==1)]
  pred_val <- as.data.frame(cbind(response,pred)) ; colnames(pred_val) <- c("observed","predicted")
  pred_val$observed <- rep(0,nrow(pred_val)) ; pred_val$observed[which(response==1)] <- 1
  
  print("+++++++++++++++++  Completed +++++++++++++++++++")
  return(list("model"=model,"tunning_result"=result,"predicted_values"=pred_val,"status"="PASS"))
}

run_RF_model_parameter_check <- function(proc_data,feature_list){
  if("outcome"%in%colnames(proc_data)){print("outcome variable found")}else{
    print("outcome variable missing");
    return(list("model"=NULL,"predicted_values"=NULL,"status"="FAIL"))
  }
  response <- rep(0,nrow(proc_data)) ; response[proc_data$outcome==1] <- 1
  x <- proc_data[,which(colnames(proc_data)%in%feature_list[,1])];
  y <- as.factor(proc_data$outcome)
  print("Running RF model")
  ntree_values <- c(200,300,400) ; nodesize_values <- c(10,20,30)
  result <- NULL
  for(i in 1:length(ntree_values)){
    for(j in 1:length(nodesize_values)){
      ntree = ntree_values[i] ; nodesize = nodesize_values[j]
      print(paste0(i,":no trees=",ntree,",","nodesizer=",nodesize)) ;
      train_ind <- sample(1:nrow(x),floor(0.7*nrow(x)),replace = T) 
      model = randomForest(x = x[train_ind,],y = as.factor(y[train_ind]),replace=TRUE,ntree=ntree,nodesize=nodesize)
      for(k in 1:5){
        test_ind <- sample(1:nrow(x),floor(0.3*nrow(x)),replace = T) 
        pred <- predict(model,x[test_ind,],type="prob") ; pred <- pred[,which(colnames(pred)==1)]
        pred_resp <- rep(0,length(pred)) ; pred_resp[which(pred>=0.5)] <- 1 ; y1 <- y[test_ind]
        tab <- table(y1,pred_resp) ; acc <- round(100*sum(tab[c(1,4)])/sum(tab),1)
        ppv <- round(100*sum((pred_resp==1&y1==1))/sum(pred_resp==1),1)
        npv <- round(100*sum((pred_resp==0&y1==0))/sum(pred_resp==0),1)
        temp <- cbind(ntree,nodesize,acc,ppv,npv) ; print(temp) ; result <- rbind(result,temp)
      }
    }
  }
  
  print("Model building completed")
  pred <- predict(model,x,type="prob") ; pred <- pred[,which(colnames(pred)==1)]
  pred_val <- as.data.frame(cbind(response,pred)) ; colnames(pred_val) <- c("observed","predicted")
  pred_val$observed <- rep(0,nrow(pred_val)) ; pred_val$observed[which(response==1)] <- 1
  
  print("+++++++++++++++++  Completed +++++++++++++++++++")
  return(list("model"=model,"tunning_result"=result,"predicted_values"=pred_val,"status"="PASS"))
}

run_RF_model_single_run <- function(proc_data,feature_list,ntree,nodesize){
  if("outcome"%in%colnames(proc_data)){print("outcome variable found")}else{
    print("outcome variable missing");
    return(list("model"=NULL,"predicted_values"=NULL,"status"="FAIL"))
  }
  response <- rep(0,nrow(proc_data)) ; response[proc_data$outcome==1] <- 1
  x <- proc_data[,which(colnames(proc_data)%in%feature_list[,1])];
  y <- as.factor(proc_data$outcome)
  print("Running RF model")
  train_ind <- sample(1:nrow(x),floor(nrow(x)*0.7))
  model = randomForest(x = x[train_ind,],y = as.factor(y[train_ind]),replace=TRUE,ntree=ntree,nodesize=nodesize)
  print("Model building completed")
  pred <- predict(model,x,type="prob") ; pred <- pred[,which(colnames(pred)==1)]
  pred_val <- as.data.frame(cbind(response,pred)) ; colnames(pred_val) <- c("observed","predicted")
  pred_val$observed <- rep(0,nrow(pred_val)) ; pred_val$observed[which(response==1)] <- 1
  
  print("+++++++++++++++++  Completed +++++++++++++++++++")
  return(list("model"=model,"predicted_values"=pred_val,"status"="PASS"))
}

run_RF_model <- function(proc_data,feature_list,ntree,nodesize){
  if("outcome"%in%colnames(proc_data)){print("outcome variable found")}else{
    print("outcome variable missing");
    return(list("model"=NULL,"testResults_rawData"=NULL,"trainResults_rawData"=NULL,"predicted_values"=NULL,"status"="FAIL"))
  }
  response <- rep(0,nrow(proc_data)) ; response[proc_data$outcome==1] <- 1
  x <- proc_data[,which(colnames(proc_data)%in%feature_list[,1])];
  y <- as.factor(proc_data$outcome)
  test_result <- NULL ; train_result <- NULL ; thres_tune <- round(sum(proc_data$outcome==1)/nrow(proc_data),3)
  data_model <- proc_data;
  print("Running RF iterations to report training and test performance")
  for(i in 1:10){
    print(i) ; 
    train_ind <- sample(1:nrow(data_model),size = floor(0.7*nrow(data_model)))
    test_ind <- setdiff(1:nrow(data_model),train_ind)
    model = randomForest(x = x[train_ind,],y = as.factor(y[train_ind]),mtry = 10,replace=TRUE,ntree=ntree,nodesize=nodesize,sampsize = floor(0.4*nrow(x)))
    temp <- perf_rf(model,traindata=data_model[train_ind,],testdata=data_model[test_ind,],thres_tune)
    train_result <- rbind(train_result,temp$train_result)
    test_result <- rbind(test_result,temp$test_result)
  }
  print("Model building completed")
  pred <- predict(model,data_model,type="prob") ; pred <- as.numeric(pred[,which(colnames(pred)==1)])
  pred_val <- as.data.frame(cbind(response,pred)) ; colnames(pred_val) <- c("observed","predicted")
  pred_val$observed <- rep(0,nrow(pred_val)) ; pred_val$observed[which(response==1)] <- 1
  
  print("+++++++++++++++++  Completed +++++++++++++++++++")
  return(list("model"=model,"testResults_rawData"=test_result,"trainResults_rawData"=train_result,"predicted_values"=pred_val,"status"="PASS"))
}

run_gams_model_auc <- function(proc_data,feature_list,cut1,cut2){
  if("outcome"%in%colnames(proc_data)){print("outcome variable found")}else{
    print("outcome variable missing");
    return(list("model"=NULL,"testResults_rawData"=NULL,"trainResults_rawData"=NULL,"predicted_values"=NULL,"status"="FAIL"))
  }
  response <- rep(0,nrow(proc_data)) ; response[proc_data$outcome==1] <- 1
  fm <- binomial() ; fm$link <- "logit"
  s <- "outcome~"
  for(i in 1:nrow(feature_list)){
    if(feature_list[i,2]=="num"){s <- paste0(s,"s(",feature_list[i,1],",k=5)+")}else{
      s <- paste0(s,feature_list[i,1],"+")
    }
  }
  s <- substr(s,1,nchar(s)-1) ; s <- as.formula(s)
  
  test_result <- NULL ; train_result <- NULL ; thres_tune <- round(sum(proc_data$outcome==1)/nrow(proc_data),3)
  data_model <- proc_data;
  print("Running GAMs iterations to report training and test performance")
  for(i in 1:30){
    print(i) ; 
    train_ind <- sample(1:nrow(data_model),size = floor(0.7*nrow(data_model)))
    test_ind <- setdiff(1:nrow(data_model),train_ind)
    model <- bam(formula = s,family = fm,data = data_model[train_ind,])
    temp <- perf_gam_auc(model,traindata=data_model[train_ind,],testdata=data_model[test_ind,],cut1,cut2)
    train_result <- rbind(train_result,temp$train_result)
    test_result <- rbind(test_result,temp$test_result)
  }
  model = bam(formula = s,family = fm,data = data_model)
  print("Model building completed")
  pred <- predict(model,data_model,type="response") ; 
  pred_val <- as.data.frame(cbind(response,pred)) ; colnames(pred_val) <- c("observed","predicted")
  pred_val$observed <- rep(0,nrow(pred_val)) ; pred_val$observed[which(response==1)] <- 1
  
  print("+++++++++++++++++  Completed +++++++++++++++++++")
  return(list("model"=model,"testResults_rawData"=test_result,"trainResults_rawData"=train_result,"predicted_values"=pred_val,"status"="PASS"))
}


run_gams_svm <- function(proc_data,feature_list,cut1,cut2,cost,gamma){
  if("outcome"%in%colnames(proc_data)){print("outcome variable found")}else{
    print("outcome variable missing");
    return(list("model"=NULL,"testResults_rawData"=NULL,"trainResults_rawData"=NULL,"predicted_values"=NULL,"status"="FAIL"))
  }
  response <- rep(0,nrow(proc_data)) ; response[proc_data$outcome==1] <- 1
  fm <- binomial() ; fm$link <- "logit"
  s <- "outcome~"
  for(i in 1:nrow(feature_list)){
    if(feature_list[i,2]=="num"){s <- paste0(s,"s(",feature_list[i,1],",k=5)+")}else{
      s <- paste0(s,feature_list[i,1],"+")
    }
  }
  s <- substr(s,1,nchar(s)-1) ; s <- as.formula(s)
  
  test_result <- NULL ; train_result <- NULL ; thres_tune <- round(sum(proc_data$outcome==1)/nrow(proc_data),3)
  data_model <- proc_data;
  print("Running GAMs + SVM iterations to report training and test performance")
  for(i in 1:5){
    print(i) ; 
    train_ind <- sample(1:nrow(data_model),size = floor(0.6*nrow(data_model)))
    test_ind <- setdiff(1:nrow(data_model),train_ind)
    train_data <- data_model[train_ind,]
    model_gam <- bam(formula = s,family = fm,data = train_data)
    pred_gam <- predict(model_gam,data=train_data,type="response") ; ind1 <- which(pred_gam<=cut2) ;
 
    proc_data_num <- gen_proc_data_PCA(data_raw,feature_list,proc_data$outcome)
    x <- proc_data_num[,which(colnames(proc_data_num)%in%feature_list[,1])];
    y <- as.factor(proc_data$outcome)
    print("Running SVM model")
    model_svm = svm(x = x[train_ind[ind1],],y = y[train_ind[ind1]],type = "C-classification", kernel ="radial",cost = cost,gamma=gamma,tolerance = 0.01,epsilon = 0.1)
    print("Model building completed")
    pred_svm <- predict(model_svm,x[train_ind[ind1],]) ; 
    pred <- rep(1,nrow(train_data)) ; obser = train_data$outcome
    pred[ind1[which(pred_svm==0)]] <- 0
    train_result <- rbind(train_result,measure_perform_stage2(pred,obser))
    
    pred_gam <- predict(model_gam,data=proc_data[test_ind,],type="response") ; ind1 <- which(pred_gam<=cut2) ;
    pred_svm <- predict(model_svm,x[test_ind[ind1],]) ; 
    pred <- rep(1,length(test_ind)) ; obser = proc_data$outcome[test_ind]
    pred[ind1[which(pred_svm==0)]] <- 0
    test_result <- rbind(test_result,measure_perform_stage2(pred,obser))
  }
  return(list("testResults"=test_result,"trainResults_rawData"=train_result,"status"="PASS"))
  
  
}


run_gams_svm_lin <- function(proc_data,feature_list,cut1,cut2,cost){
  if("outcome"%in%colnames(proc_data)){print("outcome variable found")}else{
    print("outcome variable missing");
    return(list("model"=NULL,"testResults_rawData"=NULL,"trainResults_rawData"=NULL,"predicted_values"=NULL,"status"="FAIL"))
  }
  response <- rep(0,nrow(proc_data)) ; response[proc_data$outcome==1] <- 1
  fm <- binomial() ; fm$link <- "logit"
  s <- "outcome~"
  for(i in 1:nrow(feature_list)){
    if(feature_list[i,2]=="num"){s <- paste0(s,"s(",feature_list[i,1],",k=5)+")}else{
      s <- paste0(s,feature_list[i,1],"+")
    }
  }
  s <- substr(s,1,nchar(s)-1) ; s <- as.formula(s)
  
  test_result <- NULL ; train_result <- NULL ; thres_tune <- round(sum(proc_data$outcome==1)/nrow(proc_data),3)
  data_model <- proc_data;
  print("Running GAMs + SVM iterations to report training and test performance")
  for(i in 1:5){
    print(i) ; 
    train_ind <- sample(1:nrow(data_model),size = floor(0.6*nrow(data_model)))
    test_ind <- setdiff(1:nrow(data_model),train_ind)
    train_data <- data_model[train_ind,]
    model_gam <- bam(formula = s,family = fm,data = train_data)
    pred_gam <- predict(model_gam,data=train_data,type="response") ; ind1 <- which(pred_gam<=cut2) ;
    
    proc_data_num <- gen_proc_data_PCA(data_raw,feature_list,proc_data$outcome)
    x <- proc_data_num[,which(colnames(proc_data_num)%in%feature_list[,1])];
    y <- as.factor(proc_data$outcome)
    print("Running SVM model")
    model_svm = svm(x = x[train_ind[ind1],],y = y[train_ind[ind1]],type = "C-classification", kernel ="linear",cost = cost)
    print("Model building completed")
    pred_svm <- predict(model_svm,x[train_ind[ind1],]) ; 
    pred <- rep(1,nrow(train_data)) ; obser = train_data$outcome
    pred[ind1[which(pred_svm==0)]] <- 0
    train_result <- rbind(train_result,measure_perform_stage2(pred,obser))
    
    pred_gam <- predict(model_gam,data=proc_data[test_ind,],type="response") ; ind1 <- which(pred_gam<=cut2) ;
    pred_svm <- predict(model_svm,x[test_ind[ind1],]) ; 
    pred <- rep(1,length(test_ind)) ; obser = proc_data$outcome[test_ind]
    pred[ind1[which(pred_svm==0)]] <- 0
    test_result <- rbind(test_result,measure_perform_stage2(pred,obser))
  }
  return(list("testResults"=test_result,"trainResults_rawData"=train_result,"status"="PASS"))
  
  
}

# funciton to calculate the performance of gams model
perf_gam <- function(model,traindata,testdata,thres_tune=NULL){
  if(is.null(thres_tune)){
    prev <- 0.5
  }else{
    prev <- as.numeric(thres_tune)
  }
  ind <- sample(1:nrow(traindata),floor(0.42*nrow(traindata)))
  pred <- as.numeric(predict(model,newdata = traindata[ind,],type="response"))
  obser <- rep(0,length(ind)); obser[traindata$outcome[ind]==1] <- 1
  
  train_result <- measure_perform(pred,obser,prev)
  
  pred <- as.numeric(predict(model,newdata = testdata,type="response"))
  obser <- rep(0,nrow(testdata)) ; obser[testdata$outcome==1] <- 1
  test_result <- measure_perform(pred,obser,prev)
  
  return(list("train_result"=train_result,"test_result"=test_result))
}

perf_gam_auc <- function(model,traindata,testdata,cut1,cut2){
  
  ind <- sample(1:nrow(traindata),floor(0.42*nrow(traindata)))
  pred <- as.numeric(predict(model,newdata = traindata[ind,],type="response"))
  obser <- rep(0,length(ind)); obser[traindata$outcome[ind]==1] <- 1
  train_result1 <- roc_parm_merge(obser,pred,cut1) ; 
  colnames(train_result1) <- paste0("cut1_",colnames(train_result1))
  train_result2 <- roc_parm_merge(obser,pred,cut2)  ; 
  colnames(train_result2) <- paste0("cut2_",colnames(train_result2))
  train_result <- cbind(train_result1,train_result2)
  train_result$train_auc <- round(roc.area(obser,pred)$A,4)
  
  pred <- as.numeric(predict(model,newdata = testdata,type="response"))
  obser <- rep(0,nrow(testdata)) ; obser[testdata$outcome==1] <- 1
  test_result1 <- roc_parm_merge(obser,pred,cut1) ; 
  colnames(test_result1) <- paste0("cut1_",colnames(test_result1))
  test_result2 <- roc_parm_merge(obser,pred,cut2) ; 
  colnames(test_result2) <- paste0("cut2_",colnames(test_result2))
  test_result <- cbind(test_result1,test_result2)
  test_result$test_auc <- round(roc.area(obser,pred)$A,4)
  
  return(list("train_result"=train_result,"test_result"=test_result))
}

perf_rf <- function(model,traindata,testdata,thres_tune=NULL){
  if(is.null(thres_tune)){
    prev <- 0.5
  }else{
    prev <- as.numeric(thres_tune)
  }
  ind <- sample(1:nrow(traindata),floor(0.42*nrow(traindata)))
  pred <- predict(model,traindata[ind,],type="prob") ; pred <- as.numeric(pred[,which(colnames(pred)==1)])
  obser <- rep(0,length(ind)); obser[traindata$outcome[ind]==1] <- 1
  
  train_result <- measure_perform(pred,obser,prev)
  
  pred <- predict(model,testdata,type="prob") ; pred <- as.numeric(pred[,which(colnames(pred)==1)])
  obser <- rep(0,nrow(testdata)) ; obser[testdata$outcome==1] <- 1
  test_result <- measure_perform(pred,obser,prev)
  
  return(list("train_result"=train_result,"test_result"=test_result))
}

# funcitons for performance measure
measure_perform <- function(pred,obser,prev){
  if(length(pred)!=length(obser)){print("error in predicted variable : pred > length");print(length(pred))}
  results <- data.frame(matrix(nrow=1,ncol=6));  colnames(results) <- c("Accuracy","AUC","PPV","NPV","FALSE_Negtives","HL")
  acc <- ifelse(pred>prev,1,0)
  tab <- table(acc,obser)
  
  results$Accuracy <- sum(tab[c(1,4)])/sum(tab)
  results$AUC <- roc.area(obser,pred)$A
  results$PPV <- sum(tab[c(4)])/sum(tab[c(2,4)])
  results$NPV <- sum(tab[c(1)])/sum(tab[c(1,3)])
  results$FALSE_Negtives <- sum(tab[c(3)])/sum(tab[c(3,4)]) 
  results$HL <- hosmerlem(y=obser,yhat=pred)$p
  return(results)
}
measure_perform_stage2 <- function(pred,obser){
  if(length(pred)!=length(obser)){print("error in predicted variable : pred > length");print(length(pred))}
  results <- data.frame(matrix(nrow=1,ncol=5));  colnames(results) <- c("Accuracy","PPV","NPV","Sensitivity","Specificity")
  tab <- table(pred,obser)
  
  p_ind <- which(obser==1) ; n_ind <- which(obser==0)
  TP <- sum(pred[p_ind]==1) ; FP <- sum(pred[n_ind]==1)
  TN <- sum(pred[n_ind]==0) ; FN <- sum(pred[p_ind]==0)
  acc <- (TP+TN)/length(pred)
  
  ppv <- TP/(TP+FP) ; npv <- TN/(TN+FN)
  sen <- TP/(TP+FN) ; spe <- TN/(TN+FP)
  
  results$Accuracy <- sum(tab[c(1,4)])/sum(tab)
  results$PPV <- ppv
  results$NPV <- npv
  results$Sensitivity <- sen
  results$Specificity <- spe
  

  return(results)
}
hosmerlem <- function (y, yhat, g = 10){
  id <- is.finite(y) & is.finite(yhat)
  y<-y[id]
  yhat<-yhat[id]
  n <- length(yhat);
  a = sort(yhat, decreasing = TRUE, index.return = TRUE);
  y1<-y[a$ix];
  p1<-seq(0,0,length = g);
  p2<-seq(0,0,length = g);
  for (i in 1:g)
  {
    p1[i] = mean(a$x[(as.integer((i-1)/g*n)+1):as.integer(i/g*n)]);
    p2[i]  = sum(y1[(as.integer((i-1)/g*n)+1):as.integer(i/g*n)]==1); 
  }
  s <- sum((p1*n/g - p2)^2/(n/g*p1*(1-p1)));
  #plot(p1, col = "blue"); par(new= TRUE); plot(p2, col = "red");
  list("p" = 1-pchisq(s,g-2), "xi^2" = s)     
}

# generate data for modelling
gen_proc_data <- function(raw_data,feature_list,outcome){
  t <- rep(0,length(outcome)) ; t[outcome==1] <- 1 ; outcome <- t 
  # replace missing values in admission source
  raw_data$Admission_Source[raw_data$Admission_Source==""] <- "outpatient"
  raw_data <- raw_data[,which(colnames(raw_data)%in%feature_list[,1])]
  raw_data$outcome <- t
  
  if(sum(colnames(raw_data)%in%feature_list[,1])!=nrow(feature_list)){
    print("fields mismatch in feature list and raw data, following not found in raw data")
    print(as.character(feature_list[which(!feature_list[,1]%in%colnames(raw_data)),1]))
  }
  
  for(i in 1:nrow(feature_list)){
    ind <- which(colnames(raw_data)==feature_list[i,1])
    if(length(ind)==0){print(feature_list[i,1]);next}
    if(length(ind)>1){ind <- ind[1]}
    if(feature_list[i,2]=="num"){
      temp <- outlier_detect(raw_data[,ind])
      raw_data[,ind] <- temp$data ; next
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
      
      raw_data[,ind] <- clean_categorical(raw_data[,ind])
    }
  }
  
  return(raw_data)
}
clean_categorical <- function(vect){
  ind <- which(is.na(vect)|tolower(vect)==""|toupper(vect)=="MISSING"|toupper(vect)=="NA"|toupper(vect)=="UNKNOWN"|vect=="-"|vect==" ")
  if(length(ind)>0){levels(vect) <- c(toupper(levels(vect)),"MISSING"); vect[ind]<-"MISSING"}
  vect <- as.factor(vect) ; absent_levels <- levels(vect)[which(!(levels(vect)%in%unique(vect)))]
  if(length(absent_levels)>0){vect <- droplevels(vect,absent_levels)}
  vect <- grouping_algo(vect)
  return(vect)
}
grouping_algo <- function(vect){
  
  thres <- 50
  tab <- table(vect) ; ind <- which(tab<thres)
  if(length(ind)>0){
    low_prop <- names(tab)[ind] ; levels(vect) <- c(levels(vect),"OTHERS")
    ind <- which(vect%in%low_prop) ; vect[ind] <- "OTHERS"
    ind <- which(table(vect)>0) ; sel_levels <- names(table(vect))[ind]
    #vect <- droplevels(vect,sel_levels) #commented by Lasi
    vect <- factor(vect) #added by Lasi
    
    if(("OTHERS" %in% vect) == TRUE){
      if(sum(vect=="OTHERS")<thres){
        if(length(unique(vect))==2){return(vect)}
        tab <- sort(table(vect)) ; rep_level <- names(tab)[2]
        #Change: combine the "OTHERS" group with the largest group in the data.
        #tab <- sort(table(vect),decreasing = TRUE) ; rep_level <- names(tab)[1]
        vect[which(vect==rep_level)] <- "OTHERS"
        vect <- droplevels(vect,rep_level)
        #vect <- factor(vect)  # by Lasi
      }
    }
  }
  return(vect)
}

outlier_detect <- function(vect){
  median_dev <- mad(vect,na.rm = TRUE) ; x_bar <- mean(vect,na.rm = TRUE)
  ind_l <- which(vect<quantile(vect,probs = 0.01,na.rm = T)) ; ind_u <- which(vect>quantile(vect,probs = 0.99,na.rm = T))
  ind <- which(is.na(vect)) ; if(length(ind)>0){vect[ind]<-x_bar}
  zvalue <- abs(0.6745*(vect-x_bar)) ; zvalue <- zvalue/median_dev
  ind1 <- which(zvalue>5) ; out_ind <- NULL
  ind <- intersect(ind1,ind_l) ; new_vect <- vect
  if(length(ind)>0){new_vect[ind] <- runif(length(ind),quantile(vect,probs = 0.005,na.rm = T),quantile(vect,probs = 0.05,na.rm = T)) ; out_ind <- ind} 
  ind <- intersect(ind1,ind_u) 
  if(length(ind)>0){new_vect[ind] <- runif(length(ind),quantile(vect,probs = 0.95,na.rm = T),quantile(vect,probs = 0.995,na.rm = T)) ; out_ind <- c(out_ind,ind)} 
  #print(paste0(length(ind)," outliers replaced "))
  return(list("data"=new_vect,"outlier_index"=out_ind))
}

# clean and convert data to numerical values for pca
gen_proc_data_PCA <- function(raw_data,feature_list,outcome){
  t <- rep(0,length(outcome)) ; t[outcome==1] <- 1 ; outcome <- t 
  # replace missing values in admission source
  raw_data$Admission_Source[raw_data$Admission_Source==""] <- "outpatient"
  raw_data <- raw_data[,which(colnames(raw_data)%in%feature_list[,1])]
  outcome <- t ; rm(t)
  
  if(sum(colnames(raw_data)%in%feature_list[,1])!=nrow(feature_list)){
    print("fields mismatch in feature list and raw data, following not found in raw data")
    print(as.character(feature_list[which(!feature_list[,1]%in%colnames(raw_data)),1]))
  }
  
  for(i in 1:nrow(feature_list)){
    ind <- which(colnames(raw_data)==feature_list[i,1])
    if(length(ind)==0){print(feature_list[i,1]);next}
    if(length(ind)>1){ind <- ind[1]}
    if(feature_list[i,2]=="num"){
      temp <- outlier_detect(raw_data[,ind])
      raw_data[,ind] <- as.numeric(temp$data) ; next
    }
    if(feature_list[i,2]=="cat"&feature_list[i,1]=="pr1c"){
      t <- raw_data[,ind] ; t <- gsub("[^0-9]","",t)
      d<-TrainProcedureFeature(t, outcome, 100) ; rm(t)
      raw_data[,ind] <- as.numeric(d$procedures) ; next
    }else{
      t <- clean_categorical(raw_data[,ind])
      d<-TrainCategoricalFeature(t, outcome, 100, 2)  ; rm(t)        
      raw_data[,ind] <- as.numeric(d$d)	;
    }
  }
  
  return(raw_data)
}

# generate pca from the data 
gen_data_withPC <- function(clean_data,variance_prop=NULL){
  status = "PASS"
  for(i in 1:ncol(clean_data)){
    if(sum(is.na(is.numeric(clean_data[,i])))>0){status="FAIL";print(paste0("Non-numeric entry in ",colnames(clean_data)[i]))}
  }
  if(status=="FAIL"){print("Operation terminated due to non-numeric entries for PCA") ; return(list("status"=status,"data"=NULL))}
  if(is.na(variance_prop)|variance_prop<0|variance_prop>1){variance_prop <- 0.9}
  pca_obj <- prcomp(clean_data,scale=TRUE,center = TRUE)
  pca_data <- pca_obj$x ; sm <- summary(pca_obj) ; cum_var <- sm$importance[3,]
  no_of_pca <- which(abs(variance_prop-cum_var)==min(abs(variance_prop-cum_var),na.rm = T))
  print(paste0("# PC's requried for capturing ",variance_prop," fraction of variance is ",no_of_pca))
  pca_data <- pca_data[,1:no_of_pca]
  return(list("status"=status,"data"=pca_data))
}

# develop gams model with and report performance
run_gams_model_PCA <- function(clean_data_pca,outcome,variance_prop=NULL){
  t <- rep(0,length(outcome)) ; t[outcome==1] <- 1 ; outcome <- t 
  t <- gen_data_withPC(clean_data_pca,variance_prop)
  if(t$status=="FAIL"){
    print("Stopped due to error at PC's creation");
    return(list("model"=NULL,"testResults_rawData"=NULL,"trainResults_rawData"=NULL,"predicted_values"=NULL,"status"="FAIL"))
  }
  proc_data <- as.data.frame(t$data) ; 
  colnames(proc_data) <- paste0("PC",1:ncol(proc_data))
  s <- as.formula(paste0("outcome~",paste0("s(",colnames(proc_data),",k=5)",collapse = "+")))
  proc_data$outcome <- outcome ; rm(t)
  response <- rep(0,nrow(proc_data)) ; response[proc_data$outcome==1] <- 1
  fm <- binomial() ; fm$link <- "logit"
  test_result <- NULL ; train_result <- NULL ; thres_tune <- round(sum(proc_data$outcome==1)/nrow(proc_data),3)
  data_model <- proc_data;
  print("Running GAMs iterations to report training and test performance")
  for(i in 1:10){
    print(i) ; 
    train_ind <- sample(1:nrow(data_model),size = floor(0.7*nrow(data_model)))
    test_ind <- setdiff(1:nrow(data_model),train_ind)
    model <- bam(formula = s,family = fm,data = data_model[train_ind,])
    temp <- perf_gam(model,traindata=data_model[train_ind,],testdata=data_model[test_ind,],thres_tune)
    train_result <- rbind(train_result,temp$train_result)
    test_result <- rbind(test_result,temp$test_result)
  }
  model = bam(formula = s,family = fm,data = data_model)
  print("Model building completed")
  pred <- predict(model,data_model,type="response") ; 
  pred_val <- as.data.frame(cbind(response,pred)) ; colnames(pred_val) <- c("observed","predicted")
  pred_val$observed <- rep(0,nrow(pred_val)) ; pred_val$observed[which(response==1)] <- 1
  
  print("+++++++++++++++++  Completed +++++++++++++++++++")
  return(list("model"=model,"testResults_rawData"=test_result,"trainResults_rawData"=train_result,"predicted_values"=pred_val,"status"="PASS"))
}

# perform vif test
vif_test <- function(raw_data,outcome,feature_list){
  t <- rep(0,length(outcome)) ; t[outcome==1] <- 1 ; outcome <- t 
  # replace missing values in admission source
  raw_data$Admission_Source[raw_data$Admission_Source==""] <- "outpatient"
  raw_data <- raw_data[,which(colnames(raw_data)%in%feature_list[,1])]
  
  if(sum(colnames(raw_data)%in%feature_list[,1])!=nrow(feature_list)){
    print("fields mismatch in feature list and raw data, following not found in raw data")
    print(as.character(feature_list[which(!feature_list[,1]%in%colnames(raw_data)),1]))
    print("execution terminated")
    return(NULL)
  }
  
  for(i in 1:nrow(feature_list)){
    ind <- which(colnames(raw_data)==feature_list[i,1])
    if(length(ind)==0){print(feature_list[i,1]);next}
    if(length(ind)>1){ind <- ind[1]}
    if(feature_list[i,2]=="num"){
      temp <- outlier_detect(raw_data[,ind])
      raw_data[,ind] <- as.numeric(temp$data) ; next
    }
    if(feature_list[i,2]=="cat"&feature_list[i,1]=="pr1c"){
      t <- raw_data[,ind] ; t <- gsub("[^0-9]","",t)
      d<-TrainProcedureFeature(t, outcome, 100) ; rm(t)
      raw_data[,ind] <- as.numeric(d$procedures) ; next
    }else{
      t <- clean_categorical(raw_data[,ind])
      d<-TrainCategoricalFeature(t, outcome, 100, 2)  ; rm(t)        
      raw_data[,ind] <- as.numeric(d$d)	;
    }
  }
  
  x <- raw_data[,which(colnames(raw_data)%in%feature_list[,1])]
  y <- outcome
  t <- vif(y,x,mode="dense",trace =F,subsize = round(1*length(y),0))
  selected_feat <- feature_list[t$select,]
  return(selected_feat)
  
}
