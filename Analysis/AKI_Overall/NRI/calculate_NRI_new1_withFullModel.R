# Lasith Adhikari
# Computing NRI for AKI-AllDay: Postop full model Vs. PreOp model 

setwd("/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/IntraOp_model-DECLARE")

preds <- read.csv("Analysis/AKI_Overall/Data/categorized_results_preOp+intraPreOp.csv",header = TRUE) # data


cutoff_preOp = 0.5
#cutoff_intraPreOp = 0.41
cutoff_intraPreOp = 0.49    # full model cutoff

# ##apply cutoff:
preds$preOp_cut <-  sapply(preds$pred_from_preOp, function(x)
  ifelse (x>cutoff_preOp, 1,ifelse(x<=cutoff_preOp,0)))

preds$intraPreOp_cut <-  sapply(preds$pred_from_fullModel, function(x)
  ifelse (x>cutoff_intraPreOp, 1,ifelse(x<=cutoff_intraPreOp,0)))

# for event:
ind <- which(preds$true_AKIOverall_outcome==1)
event <-  preds[ind,]
event_up <- length(which(event$preOp_cut==0 & event$intraPreOp_cut==1))  # up: 0 -> 1
event_down <-  length(which(event$preOp_cut==1 & event$intraPreOp_cut==0))  # down: 1 -> 0
event_0_0 <- length(which(event$preOp_cut==0 & event$intraPreOp_cut==0))
event_1_1 <- length(which(event$preOp_cut==1 & event$intraPreOp_cut==1))
total_event <-length(ind)
table_event <-  data.frame(
  preOpM__newM = c('class_0','class_1'),
  class_0 = c(event_0_0, event_down ),
  class_1 = c(event_up, event_1_1))
print("Reclassification Table for events:")
print(table_event)

# for non event:
ind <- which(preds$true_AKIOverall_outcome==0)
nonevent <-  preds[ind,]
nonevent_up <- length(which(nonevent$preOp_cut==0 & nonevent$intraPreOp_cut==1))  # up: 0 -> 1
nonevent_down <-  length(which(nonevent$preOp_cut==1 & nonevent$intraPreOp_cut==0))  # down: 1 -> 0
nonevent_0_0 <- length(which(nonevent$preOp_cut==0 & nonevent$intraPreOp_cut==0))
nonevent_1_1 <- length(which(nonevent$preOp_cut==1 & nonevent$intraPreOp_cut==1))
total_nonevent <-length(ind)
table_event <-  data.frame(
  preOpM__newM = c('class_0','class_1'),
  class_0 = c(nonevent_0_0, nonevent_down ),
  class_1 = c(nonevent_up, nonevent_1_1))
print("Reclassification Table for non-events:")
print(table_event)

NRI_ <- ((event_up/total_event - event_down/total_event) - (nonevent_up/total_nonevent - nonevent_down/total_nonevent))
#print(NRI_)
sprintf("NRI is %f",NRI_)

# compute test statistic (Z) and the p-value:

Z_val <- NRI_ / sqrt((event_up/total_event + event_down/total_event)/total_event  + (nonevent_up/total_nonevent + nonevent_down/total_nonevent)/total_nonevent)
sprintf("Z-statistic is %f",Z_val)

# Asymptotically the McNemar test statistic follows a chi-squared distribution with 1 degree of freedom
#pchisq(Z_val^2, df=1,lower.tail=FALSE) 

# using two-tailed z-statistic is also same as the chi-squar value:
p_val = 2*pnorm(Z_val, lower.tail = FALSE)
sprintf("p-value is %f",p_val)

############# Bootstrap 95% CI ###################


NRI <- function(preds, indices){
  
  preds_i <- preds[indices,] # allows boot to select sample
  
  # for event:
  ind <- which(preds_i$true_AKIOverall_outcome==1)
  event <-  preds_i[ind,]
  event_up <- length(which(event$preOp_cut==0 & event$intraPreOp_cut==1))  # up: 0 -> 1
  event_down <-  length(which(event$preOp_cut==1 & event$intraPreOp_cut==0))  # down: 1 -> 0
  event_0_0 <- length(which(event$preOp_cut==0 & event$intraPreOp_cut==0))
  event_1_1 <- length(which(event$preOp_cut==1 & event$intraPreOp_cut==1))
  total_event <-length(ind)

  
  # for non event:
  ind <- which(preds_i$true_AKIOverall_outcome==0)
  nonevent <-  preds_i[ind,]
  nonevent_up <- length(which(nonevent$preOp_cut==0 & nonevent$intraPreOp_cut==1))  # up: 0 -> 1
  nonevent_down <-  length(which(nonevent$preOp_cut==1 & nonevent$intraPreOp_cut==0))  # down: 1 -> 0
  nonevent_0_0 <- length(which(nonevent$preOp_cut==0 & nonevent$intraPreOp_cut==0))
  nonevent_1_1 <- length(which(nonevent$preOp_cut==1 & nonevent$intraPreOp_cut==1))
  total_nonevent <-length(ind)

  NRI_val <- ((event_up/total_event - event_down/total_event) - (nonevent_up/total_nonevent - nonevent_down/total_nonevent)) 
  return(NRI_val)
  
}



library(boot)
# https://stats.idre.ucla.edu/r/faq/how-can-i-generate-bootstrap-statistics-in-r/
# https://www.statmethods.net/advstats/bootstrapping.html


boot_results <- boot(data=preds, statistic=NRI, R=10000)
summary(boot_results)
plot(boot_results)

boot.ci(boot_results, type="bca")

