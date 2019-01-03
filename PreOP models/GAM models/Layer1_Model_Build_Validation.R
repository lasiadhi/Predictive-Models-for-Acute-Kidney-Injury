############################################################################################
# Created by Paul Thottakkara : Last Updated on 17 june 2016 by Ashkan Ebadi & Shivam Mittal
# 
# This file contains all set of functions used to generate GAM-models 
# and predict probabilities of the patient using the best model.
# 
# The Different Functions in this file are:-
#    *run_gams_model_single_run
#    *run_gams_model
#    *gen_gam_model_kfold
############################################################################################

############
# Function Input:-    1> Processed Data
#				      2> Features list containing most importance features from which
#                        model will be generated
# Function Output:-   GAM model coefficients and its coresponding predicted values
#                     for each patient in the entire cohort
#                   
# Function Used:-     None
# Function Purpose:-  Generate GAM Model using feature_list and report its prediction score
#                     on the dataset itself.
############


run_gams_model_single_run<- function(proc_data,feature_list){
  if("outcome"%in%colnames(proc_data)){print("outcome variable found")}else{
    print("outcome variable missing");
    return(NULL)
  }
  response <- rep(0,nrow(proc_data)) ; response[proc_data$outcome==1] <- 1
  fm <- binomial() ; 
  fm$link <- "logit"
  s <- "outcome~"
  for(i in 1:nrow(feature_list)){
    if(feature_list[i,2]=="num"){s <- paste0(s,"s(",feature_list[i,1],",k=5)+")}
	else{
      s <- paste0(s,feature_list[i,1],"+")
    }
  }
  s <- substr(s,1,nchar(s)-1) ; 
  s <- as.formula(s)
  print("Running GAMs iterations to report training and test performance")
  model <- bam(formula = s,family = fm,data = proc_data)
  print("Model building completed")
  pred <- predict(model,proc_data,type="response") ; 

  print("+++++++++++++++++  Completed +++++++++++++++++++")
  return(list("predicted_values"=pred,"model"=model))
}


############
# Function Input:-    1> Training Dataset
#                     2> Test Data
#				      3> Features list containing most importance features from which
#                        model will be generated
# Function Output:-   GAM model coefficients and its coresponding predicted values
#                     for each patient in the entire cohort
#                   
# Function Used:-     None
# Function Purpose:-  Generate GAM Model using feature_list and training data
#                     and report its prediction score on the test dataset.
############
run_gams_model<- function(train_data,test_data,feature_list){
  if("outcome"%in%colnames(train_data)){print("outcome variable found")}else{
    print("outcome variable missing");
    return(NULL)
  }
  response <- rep(0,nrow(train_data)) ; 
  response[train_data$outcome==1] <- 1
  fm <- binomial() ; fm$link <- "logit"
  s <- "outcome~"
  for(i in 1:nrow(feature_list)){
    #print(feature_list[i,1])
    if(feature_list[i,2]=="num"){if (length((unique(train_data[,feature_list[i,1]])))>=5){s <- paste0(s,"s(",feature_list[i,1],",k=5)+")}else {s <- paste0(s,"s(",feature_list[i,1],",k=1)+")}}else{
      s <- paste0(s,feature_list[i,1],"+")
    }
  }
  s <- substr(s,1,nchar(s)-1); 
  #print(s)
  s <- as.formula(s)
  print("Running GAMs iterations to report training and test performance")
  model = bam(formula = s,family = fm,data = train_data)
  print("Model building completed")
  pred <- predict(model,test_data,type="response") ;
  print("+++++++++++++++++  Completed +++++++++++++++++++")
  return(list("predicted_values"=pred,"model"=model))
}



############
# Function Input:-    1> Processed Dataset
#				      2> Features list containing most importance features from which
#                        model will be generated
# Function Output:-   List of all 250 GAM model generated and its corresponding metrics 
#                     values which includes Threshold,Accuracy,PPV,NPV, Sensitivity,
#                      Specifivity, Youden Index
#                   
# Function Used:-     run_gams_model(),calculate_metric()
# Function Purpose:-  Perform 5 fold validation to obtain 5 different models
#                     and evaluate metric for each model and store it. Repeat the steps
#                     50 times by shuffling the dataset in every iteration
############
gen_gam_model_kfold<-function(proc_data,features_selected)
{
  set.seed(9001) #added by Lasi
  models <- vector(mode = "list", length = 250)
  results <- list()
  count <- 1;
  for(j in 1:1){
    print(j);
    new_data <- proc_data
    new_data <- new_data[sample(nrow(new_data)),]
    k <- 5 #the number of folds
    
    folds <- cvFolds(NROW(new_data), K=k)
    new_data$holdoutpred <- rep(0,nrow(new_data))
    for(i in 1:k){
      train <- new_data[folds$subsets[folds$which != i], ] #Set the training set
      validation <- new_data[folds$subsets[folds$which == i], ] #Set the validation set
      print("here")
      newpred<- run_gams_model(train,validation,features_selected)
      print("there")
      new_data[folds$subsets[folds$which == i], ]$holdoutpred <- newpred$pred
      models[[count]]<-newpred$model
      count=count+1;
      res <-  calculate_metric(new_data[folds$subsets[folds$which == i], ]$outcome,new_data[folds$subsets[folds$which == i], ]$holdoutpred)
      results <- rbind(results,res)
    }
  }
  return(list("models"=models,"results"=results))
}

