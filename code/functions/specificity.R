specificity = function(true, pred){
  # true: vector of true classes
  # pred: vector of predicted classes
  conf_mat = table(true, pred)
  spec = conf_mat[1,1]/sum(conf_mat[1,])
  return(round(spec*100,2))
}