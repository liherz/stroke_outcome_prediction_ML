# s. https://www.r-bloggers.com/calculating-auc-the-area-under-a-roc-curve/
auc = function(TPR, FPR){
  # inputs already sorted, best scores first
  # calculate the difference (width) between two successive measurements
  dFPR = c(diff(FPR), 0)
  dTPR = c(diff(TPR), 0)
  
  return(sum(TPR * dFPR) + sum(dTPR * dFPR)/2)
}