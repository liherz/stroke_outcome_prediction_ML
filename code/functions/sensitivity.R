sensitivity = function(true, pred){
  # true: vector of true classes
  # pred: vector of predicted classes
  conf_mat = table(true, pred)
  sens = conf_mat[2,2]/sum(conf_mat[2,])
  return(round(sens*100, 2))
}