calc_prop_best_fit_learners <- function(sim_type, sim_model, penalty) {
  # this funciton calculates the proportion of time each model was the best-fitting model 
  # for confusion matrix
  # sim_type is string denoting how parameters were chosen for simulation
  # sim_type possible values: 'grid_search', 'part_params', 'prior_rl_values'
  # sim_model is string denoting the model used to simulate data
  # sim_model possible values: 'b', 'a1', 'a2';
  library(here)
  library(tidyverse)
  source(here('scr', 'AIC_functions.R'))
  source(here('scr', 'BIC_functions.R'))
 
  # limit to learners-only #### 
  # comment out if unwanted
  d1 <- read.csv(here('output', 'model_comparisons.csv'))
  non_learners <- d1 %>% filter(BIC_win == 'baseline')
  non_learners <- non_learners$id
  rm(d1)
  
  # read files
  b <- read.csv(here('output', 'simulation', sim_type, paste0('b_fit2_', sim_model, '.csv')))
  b$subParent <- str_sub(b$Subject, start = 1, end = 8) 
  b <- b %>% filter(!subParent %in% non_learners)
  a1 <- read.csv(here('output', 'simulation', sim_type, paste0('a1_fit2_', sim_model, '.csv')))
  a1$subParent <- str_sub(a1$Subject, start = 1, end = 8)
  a1 <- a1 %>% filter(!subParent %in% non_learners)
  a2 <- read.csv(here('output', 'simulation', sim_type, paste0('a2_fit2_', sim_model, '.csv')))
  a2$subParent <- str_sub(a2$Subject, start = 1, end = 8)
  a2 <- a2 %>% filter(!subParent %in% non_learners)
 
  sim_model2 <- ifelse(sim_model == 'b', 'baseline', ifelse(sim_model == 'a1', '1alpha', '2alpha'))
  dt <- read.csv(here('output', 'simulation', sim_type, paste0('sim_', sim_model2, '_model_data.csv')))
  
  if(penalty == 'BIC'){
    # calculate total trials
    b <- merge(b, count_trials(dt), by = 'Subject')
    a1 <- merge(a1, count_trials(dt), by = 'Subject')
    a2 <- merge(a2, count_trials(dt), by = 'Subject')
    
    # calculate BIC
    b$BIC_b <- calc_BIC(b$n,1,b$llh)
    a1$BIC_a1 <- calc_BIC(a1$n,2,a1$llh)
    a2$BIC_a2 <- calc_BIC(a2$n,3,a2$llh)

  } else if (penalty == 'AIC'){
    # calculate AIC
    b$AIC_b <- calc_AIC(45,1,b$llh)
    a1$AIC_a1 <- calc_AIC(45,2,a1$llh)
    a2$AIC_a2 <- calc_AIC(45,3,a2$llh)
  } else {
    b <- b %>% rename(llh_b = llh)
    a1 <- a1 %>% rename(llh_a1 = llh)
    a2 <- a2 %>% rename(llh_a2 = llh)
  }
  
  # merge into one data frame
  d <- merge(b[c(1,ncol(b))], a1[c(1,ncol(a1))], by = 'Subject')
  d4 <- merge(d,a2[c(1,ncol(a2))], by = 'Subject')

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

