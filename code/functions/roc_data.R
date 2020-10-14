library(pROC)
# s. http://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html

roc_data = function(true, pred){
  # true = true prediction (0,1)
  # pred = prediction for the respective observation ([0,1])
  
  # create a ROC object with package pROC:
  roc_obj = roc(true, pred, auc=TRUE, ci=TRUE)
  
  # AUC and confidence interval
  auc_ci = ci(roc_obj, of='auc')
  auc = c(auc_ci[2], auc_ci[1], auc_ci[3])
  names(auc) = c('auc', 'lower_ci', 'upper_ci')
  
  dat = data.frame(sens = roc_obj$sensitivities, 
                   spec = roc_obj$specificities)
  
  return(list(roc_curve=dat, auc = auc))
}
