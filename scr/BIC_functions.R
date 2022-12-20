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

winningBIC <- function(x) {
  # This function takes a table where the first column is ID names and 
  # the remaining columns are  BIC values for different models
  # and returns a data table with 
  # (1) the id and (2) the name of the winning model (based on the column names)
  min.col <- function(m, ...) max.col(-m, ...)
  names <- colnames(x)[2:ncol(x)]
  id <- x[1]
  bics <- x[2:ncol(x)]
  minBIC <- apply(bics, 1, min)
  winModel <- names[min.col(bics)]
  winModel <- sub('BIC_', '', winModel)
  data <- data.frame(id, bics, winModel)
  return(data)
}

winningLLH <- function(x) {
  # This function takes a table where the first column is ID names and 
  # the remaining columns are LLH values for different models
  # and returns a data table with 
  # (1) the id and (2) the name of the winning model (based on the column names)
  min.col <- function(m, ...) max.col(-m, ...)
  names <- colnames(x)[2:ncol(x)]
  id <- x[1]
  llhs <- x[2:ncol(x)]
  minBIC <- apply(llhs, 1, min)
  winModel <- names[min.col(llhs)]
  winModel <- sub('LLH_', '', winModel)
  data <- data.frame(id, llhs, winModel)
  return(data)
}

count_trials <- function(data) {
  dt <- data %>% filter(Choice != 'None') %>%
    group_by(Subject) %>% summarize(n = n())
  return(dt)
}

count_trials2 <- function(data) {
  dt <- data %>% filter(amount_shared != 'None') %>%
    group_by(id) %>% summarize(n = n())
  return(dt)
}
  