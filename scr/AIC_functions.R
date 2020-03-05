calc_AIC <- function(n, k, llh) {
  # This function calculates the AIC 
  # n = sample size
  # k = the number of parameters
  # llh = negative log likelihood
  #aic <- ifelse(n/k < 40, 2*llh + 2*k, 2*llh + 2*k + (2*k(k+1)/(n-k-1)))
  aic <- 2*llh + 2*k + (2*k*(k+1)/(n-k-1))
  return(aic)
}

weightAIC <- function(x) {
  # This function calculates the Akaike weight given 
  # a set of AIC values for a participant (x)
  minAIC <- min(x)
  changeAICs <- x - minAIC
  likelihoods <- exp(-.5*changeAICs)
  sumlike <- sum(likelihoods)
  weights <- likelihoods / sumlike
  return(weights)
}

winningWeight <- function(id, x) {
  # This function takes table of AIC values (x) where each column represents a different model
  # and returns at data table with 
  # (1) the id, (2) the AIC weights and (3) the name of the winning model (based on the column names)
  names <- colnames(x)
  weights <- t(apply(x, 1, weightAIC))
  colnames(weights) <- paste0(names, '_weight')
  maxWeight <- apply(weights, 1, which.max)
  win <- names[maxWeight]
  data <- data.frame(id, weights, win)
  return(data)
}
