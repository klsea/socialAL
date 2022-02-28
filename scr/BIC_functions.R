calc_BIC <- function(n, k, llh) {
  # This function calculates the BIC 
  # n = sample size
  # k = the number of parameters
  # llh = negative log likelihood
  bic <- 2 * llh + log(n)*k
  return(bic)
}
