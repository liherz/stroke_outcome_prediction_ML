accuracy = function(true, pred){
  # true: vector of true classes
  # pred: vector of predicted classes
  conf_mat = table(true, pred)
  acc = (conf_mat[1,1]+conf_mat[2,2])/sum(conf_mat)
  return(round(acc*100,2))
}