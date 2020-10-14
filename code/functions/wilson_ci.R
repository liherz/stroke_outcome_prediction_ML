# Wilson Confidence interval
library(Hmisc)

wilson_ci = function(n_successes, n_obs, alpha=0.05){
  # n_successes = vector containing the number of "successes" for binomial variates
  # n_obs = vector containing the numbers of corresponding observations
  ci = round(binconf(x=n_successes, n=n_obs, method="wilson", alpha=alpha)*100,2)
  return(c(ci[,'Lower'],ci[,'Upper']))
}