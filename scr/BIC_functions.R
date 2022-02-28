calc_BIC <- function(n, k, llh) {
  # This function calculates the BIC 
  # n = sample size
  # k = the number of parameters
  # llh = negative log likelihood
  bic <- 2 * llh + log(n)*k
  return(bic)
}

compare_BIC <- function(BIC1, BIC2){
  # This function calculates an estimate of Bayes Factor
  # comparing two models
  # BIC1 - model 1
  # BIC2 - model 2
  bij=exp(BIC1-BIC2)
  return(bij)
}
