# here we want to evaluate the performance of our classification method
# that we propose to identify "unknown" phenotypes

##########################################################
# function make_roc 
########################???
###### input
# reverse: True if confidence is high in case of "unknown" class is presented
# mc : data frame where the following cols are required
# confidence: a measure for confidence of the prediction, such as prob
# true_label: holding class_label and indicating novel class with -1
############
###### output
# df_roc : data frame with cols quantifying the performance as AUC... 

library(ROCR) # for AUC curve w/o ci
library(pROC) # for AUC confidence intervals

#d@x.name "False positive rate"
# S4 wtf!
# why did we have <=0 ??? 
make_roc = function(mc, m, reverse=TRUE) {
  d = filter(mc, method==m)
  if (reverse){
    # unknown class was coded with -1 in true_label: true_label < 0
    # make classification  "unknown" (class 1) or "known" (class 0)
    pred = prediction(d$confidence, d$true_label < 0)  
  } else {
    pred = prediction(1-d$confidence, d$true_label < 0)  
  }
  dd = performance(pred,"tpr","fpr")
  auc = performance(pred, "auc")
  print(auc@y.values)
  fpr = dd@x.values[[1]]
  tpr = dd@y.values[[1]]
  # new for CI of AUC using package pROC
  # Syntax (response, predictor):
  my_ci=ci(d$true_label < 0, 1-d$confidence)
  
  df_roc = data.frame(
    fpr = fpr, 
    tpr = tpr, #There is one more point in the ROC Curve
    method=rep(d$method[1], length(fpr)), 
    run_id = rep(d$run_id[1], length(fpr)), 
    auc.ci.lower = rep(my_ci[1],length(fpr)),
    auc = rep(auc@y.values[[1]],length(fpr)), 
    auc.ci.upper = rep(my_ci[3],length(fpr))
  )
  return (df_roc)
}
