calc_prop_best_fit <- function(sim_type, sim_model, penalty) {
  # this funciton calculates the proportion of time each model was the best-fitting model 
  # for confusion matrix
  # sim_type is string denoting how parameters were chosen for simulation
  # sim_type possible values: 'grid_search', 'part_params', 'prior_rl_values'
  # sim_model is string denoting the model used to simulate data
  # sim_model possible values: 'b', 'a1', 'a2'; OLD possible values: 'a1d', a2d', 'a2p'
  library(here)
  library(tidyverse)
  source(here('scr', 'AIC_functions.R'))
  source(here('scr', 'BIC_functions.R'))
  
  # read files
  b <- read.csv(here('output', 'simulation', sim_type, paste0('b_fit2_', sim_model, '.csv')))
  a1 <- read.csv(here('output', 'simulation', sim_type, paste0('a1_fit2_', sim_model, '.csv')))
  a2 <- read.csv(here('output', 'simulation', sim_type, paste0('a2_fit2_', sim_model, '.csv')))
  # a1d <- read.csv(here('output', 'simulation', sim_type, paste0('a1d_fit2_', sim_model, '.csv')))
  # a2d <- read.csv(here('output', 'simulation', sim_type, paste0('a2d_fit2_', sim_model, '.csv')))
  # a2p <- read.csv(here('output', 'simulation', sim_type, paste0('a2p_fit2_', sim_model, '.csv')))
  sim_model2 <- ifelse(sim_model == 'b', 'baseline', ifelse(sim_model == 'a1', '1alpha', '2alpha'))
  dt <- read.csv(here('output', 'simulation', sim_type, paste0('sim_', sim_model2, '_model_data.csv')))
  
  if(penalty == 'BIC'){
    # calculate total trials
    b <- merge(b, count_trials(dt), by = 'Subject')
    a1 <- merge(a1, count_trials(dt), by = 'Subject')
    a2 <- merge(a2, count_trials(dt), by = 'Subject')
    #a1d <- merge(a1d, count_trials(dt), by = 'Subject')
    #a2d <- merge(a2d, count_trials(dt), by = 'Subject')
    #a2p <- merge(a2p, count_trials(dt), by = 'Subject')
    
    # calculate BIC
    b$BIC_b <- calc_BIC(b$n,1,b$llh)
    a1$BIC_a1 <- calc_BIC(a1$n,2,a1$llh)
    a2$BIC_a2 <- calc_BIC(a2$n,3,a2$llh)
    # a1d$BIC <- calc_BIC(a1d$n,3,a1d$llh)
    # a2d$BIC <- calc_BIC(a2d$n,4,a2d$llh)
    # a2p$BIC <- calc_BIC(a2p$n),6,a2p$llh)
    
  } else if (penalty == 'AIC'){
    # calculate AIC
    b$AIC_b <- calc_AIC(45,1,b$llh)
    a1$AIC_a1 <- calc_AIC(45,2,a1$llh)
    a2$AIC_a2 <- calc_AIC(45,3,a2$llh)
    # a1d$AIC <- calc_AIC(45,3,a1d$llh)
    # a2d$AIC <- calc_AIC(45,4,a2d$llh)
    # a2p$AIC <- calc_AIC(45,6,a2p$llh)
  } else {
    b <- b %>% rename(llh_b = llh)
    a1 <- a1 %>% rename(llh_a1 = llh)
    a2 <- a2 %>% rename(llh_a2 = llh)
  }

  # merge into one data frame
  d <- merge(b[c(1,ncol(b))], a1[c(1,ncol(a1))], by = 'Subject')
  d4 <- merge(d,a2[c(1,ncol(a2))], by = 'Subject')
  # d1 <- merge(a1d[c(1,ncol(a1d))], a2[c(1,ncol(a2))], by = 'Subject', suffixes = c('_a1d', '_a2'))
  # d2 <- merge(a2d[c(1,ncol(a2d))], a2p[c(1,ncol(a2p))], by = 'Subject', suffixes = c('_a2d', '_a2p'))
  # d3 <- merge(d,d1)
  # d4 <- merge(d3,d2)
  # rm(b,a1,a1d,a2,a2d,a2p,d,d1,d2,d3)
  
  if(penalty == 'BIC'){
    # find minBIC
    output <- winningBIC(d4)
    output <- output %>% count(winModel) %>% mutate(freq = round(n/sum(n), 2))
    rm(d4)
  } else if (penalty == 'AIC') {
    # find minAIC
    output <- winningAIC(d4)
    output <- output %>% count(winModel) %>% mutate(freq = round(n/sum(n), 2))
    rm(d4)
  } else {
    # find min LLH
    output <- winningLLH(d4)
    output <- output %>% count(winModel) %>% mutate(freq = round(n/sum(n), 2))
    rm(d4)
  }
  
  return(output)
  }

# #test
# sim_type = 'part_params'
# sim_model = 'a2'
# penalty = 'LLH'
# calc_prop_best_fit(sim_type, sim_model, penalty)

