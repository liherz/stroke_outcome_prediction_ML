# Creates partial plots in the classification setting
# for different R objects:
# randomForest
# svm
# knn
partialPlot_class = function(dat, var, model, c, step_size=1, plot=T, ymin=-5, ymax=5){
  # dat = data frame used for the random forest
  # var = variable of interest (string)
  # model = object of class randomForest
  # c = class of interest (string)
  # step_size = Step size for the range of the data
  # plot = should the results be plotted
  # min, max = ylim(min,max)
  
  # define the possible values for the variable of interest:
  # numerical: min/max within the complete dataset
  # factor: all levels within the dataset for the variable of interest
  if(is.numeric(dat[,var])){
    steps = seq(min(dat[,var]),max(dat[,var]),step_size)
  } else{
    steps = levels(dat[,var])
  }
  # define matrix to save the results
  preds = matrix(NA, nrow=nrow(dat), ncol=length(steps))
  
  # iterate over all rows (patients)
  for(i in 1:nrow(dat)){
    dat_tmp = dat[i,]
    # iterate over all possible values of the variable of interest
    for(s in 1:length(steps)){
      # Assign a new value to the variable
      if(is.numeric(dat[,var])){
        dat_tmp[,var] = steps[s]
      } else{
        dat_tmp[,var] = factor(steps[s], levels=levels(dat[,var]))
      }
      # predict the outcome with the new variable of interest while holding the remaining variables constant
      if(class(model)[2]=="randomForest"){
        prob = predict(model, newdata=dat_tmp, type="vote", norm.votes = T) # predicted probabilities for class c
      } else if(class(model)[2]=="svm"){
        prob = predict(model, newdata=dat_tmp, probability=T) # predicted probabilities for class c
        prob = attributes(prob)$probabilities
      } else if(class(model)[2]=="lm"){
        prob = predict(model, newdata=dat_tmp, type="response") # return probability for class 1
        prob = data.frame("0"=1-prob,"1"=prob, check.names = F)
      }
      # calculate the log Odds for class c
      preds[i,s] = log(prob[,c]) - 1/dim(prob)[2]*sum(log(prob))
    }
  }
  
  # decide if the data should be plotted
  if(plot){
    if(is.numeric(dat[,var])){
      plot(NULL, xlim=c(min(steps),max(steps)), ylim=c(ymin,ymax), xlab=var, ylab=paste0("Log Odds for class ", c,")"), 
           main=paste0("Partial dependency: ",var),
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
      for(i in 1:nrow(dat)){
        lines(steps, preds[i,], col="lightblue")
      }
      lines(steps, colMeans(preds), col="blue")
    } else{
      resp = c(); for(i in 1:ncol(preds)){ resp = c(resp,preds[,i])}
      traces = rep(dat[,"p_id"],length(levels(dat[,var])))
      x = factor(rep(steps, each=nrow(preds)))
      interaction.plot(x, traces, resp, type="l", legend=F, lty=1, xlab=var, ylab=paste0("Log Odds for class ", c,")"),
                       main=paste0("Partial dependency: ",var), col="lightblue",
                       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, ylim=c(ymin,ymax))
    }
  }
  return(preds)
}
