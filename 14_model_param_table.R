# table of model parameters
# 3.29.21 KLS

# init ####
# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load source functions
source(here::here('scr', 'concat_clean.R'))
source(here::here('scr', 'AIC_functions.R'))
source(here::here('scr', 'BIC_functions.R'))

# set hard-coded variables

# read data in 
d1 <- read.csv(here::here('output', 'two_alpha_model_params.csv' ))
d2 <- read.csv(here::here('output', 'single_alpha_model_params.csv' ))
d3 <- read.csv(here::here('output', 'baseline_model_params.csv'))
o4 <- read.csv(here::here('output', 'two_alpha_with_decay_model_params_older.csv'))
y4 <- read.csv(here::here('output', 'two_alpha_with_decay_model_params_younger.csv'))
o5 <- read.csv(here::here('output', 'prior_model_params_older.csv'))
y5 <- read.csv(here::here('output', 'prior_model_params_younger.csv'))

d4 <- rbind(o4, y4); rm(o4, y4)
d5 <- rbind(o5, y5); rm(o5, y5)

resp_summary <- read.csv(here::here('output', 'responses.csv'))

# calculate parameter means by age group

# gain-loss learning ####
d1 <- merge(d1, resp_summary, by = 'id') #adds response summary and gets rid of cut participants
d1$AIC <- calc_AIC(d1$n_resp,3,d1$X.LLH) # calc AIC
d1$BIC <- calc_BIC(d1$n_resp,3,d1$X.LLH) # calc BIC

t1 <- d1 %>% group_by(agegrp) %>%
  summarise(a_gain_mean = mean(alpha_gain), 
            a_gain_sd = sd(alpha_gain), 
            a_loss_mean = mean(alpha_loss), 
            a_loss_sd = sd(alpha_loss), 
            beta_mean = mean(beta), 
            beta_sd = sd(beta), 
            AIC_mean = mean(AIC), 
            AIC_sd = sd(AIC), 
            BIC_mean = mean(BIC), 
            BIC_sd = sd(BIC))

# general learning model ####
d2 <- merge(d2, resp_summary, by = 'id')
d2$AIC <- calc_AIC(d2$n_resp, 3, d2$X.LLH)
d2$BIC <- calc_BIC(d2$n_resp, 3, d2$X.LLH)

t2 <- d2 %>% group_by(agegrp) %>%
  summarise(alpha_mean = mean(alpha), 
            alpha_sd = sd(alpha), 
            beta_mean = mean(beta), 
            beta_sd = sd(beta), 
            AIC_mean = mean(AIC), 
            AIC_sd = sd(AIC), 
            BIC_mean = mean(BIC), 
            BIC_sd = sd(BIC))


# baseline model ####
d3 <- merge(d3, resp_summary, by = 'id')
d3$AIC <- calc_AIC(d3$n_resp,3,d3$X.LLH) # calc AIC
d3$BIC <- calc_BIC(d3$n_resp,3,d3$X.LLH) # calc BIC

t3 <- d3 %>% group_by(agegrp) %>%
  summarise(beta_mean = mean(beta), 
            beta_sd = sd(beta), 
            AIC_mean = mean(AIC), 
            AIC_sd = sd(AIC),             
            BIC_mean = mean(BIC), 
            BIC_sd = sd(BIC))


# decay model ####
d4 <- merge(d4, resp_summary, by = 'id')
d4$AIC <- calc_AIC(d4$n_resp,3,d4$X.LLH) # calc AIC
d4$BIC <- calc_BIC(d4$n_resp,3,d4$X.LLH) # calc BIC

t4 <- d4 %>% group_by(agegrp) %>%
  summarise(a_gain_mean = mean(alpha_gain), 
            a_gain_sd = sd(alpha_gain), 
            a_loss_mean = mean(alpha_loss), 
            a_loss_sd = sd(alpha_loss), 
            beta_mean = mean(beta), 
            beta_sd = sd(beta), 
            decay_mean = mean(decay), 
            decay_sd = sd(decay),
            AIC_mean = mean(AIC), 
            AIC_sd = sd(AIC),
            BIC_mean = mean(BIC), 
            BIC_sd = sd(BIC))


# prior model ####
d5 <- merge(d5, resp_summary, by = 'id')
d5$AIC <- calc_AIC(d5$n_resp,3,d5$X.LLH) # calc AIC
d5$BIC <- calc_BIC(d5$n_resp,3,d5$X.LLH) # calc BIC

t5 <- d5 %>% group_by(agegrp) %>%
  summarise(a_gain_mean = mean(alpha_gain), 
            a_gain_sd = sd(alpha_gain), 
            a_loss_mean = mean(alpha_loss), 
            a_loss_sd = sd(alpha_loss), 
            beta_mean = mean(beta), 
            beta_sd = sd(beta), 
            itrust_mean = mean(iProb_trust), 
            itrust_sd = sd(iProb_trust), 
            ineut_mean = mean(iProb_neut),
            ineut_sd = sd(iProb_neut), 
            iuntrust_mean = mean(iProb_untrust),
            iuntrust_sd = sd(iProb_untrust),
            AIC_mean = mean(AIC), 
            AIC_sd = sd(AIC), 
            BIC_mean = mean(BIC), 
            BIC_sd = sd(BIC))

# age group make tables ####
# c('baseline', 'general', 'gain-loss', 'decay gain-loss', 'initial gain_loss')
y0 <- data.frame(model = character(), 
                 itrust = character(), 
                 ineut = character(), 
                 iuntrust = character(), 
                 decay = character(), 
                 alpha = character(), 
                 a_gain = character(), 
                 a_loss = character(),
                 beta = character(),
                 AIC = character(), 
                 BIC = character())
o0 <- data.frame(model = character(), 
                 itrust = character(), 
                 ineut = character(), 
                 iuntrust = character(), 
                 decay = character(), 
                 alpha = character(), 
                 a_gain = character(), 
                 a_loss = character(),
                 beta = character(),
                 AIC = character(), 
                 BIC = character())

# add baseline ####
y0[1,1] <- 'baseline'
y0[1,9] <- paste0(round(t3$beta_mean[1],2), ' (', round(t3$beta_sd[1],2), ')')
y0[1,10] <- paste0(round(t3$AIC_mean[1],2), ' (', round(t3$AIC_sd[1],2), ')')
y0[1,11] <- paste0(round(t3$BIC_mean[1],2), ' (', round(t3$BIC_sd[1],2), ')')

o0[1,1] <- 'baseline'
o0[1,9] <- paste0(round(t3$beta_mean[2],2), ' (', round(t3$beta_sd[2],2), ')')
o0[1,10] <- paste0(round(t3$AIC_mean[2],2), ' (', round(t3$AIC_sd[2],2), ')')
o0[1,11] <- paste0(round(t3$BIC_mean[2],2), ' (', round(t3$BIC_sd[2],2), ')')

# add general learning ####
y0[2,1] <- 'general'
y0[2,6] <- paste0(round(t2$alpha_mean[1],2), ' (', round(t2$alpha_sd[1],2), ')')
y0[2,9] <- paste0(round(t2$beta_mean[1],2), ' (', round(t2$beta_sd[1],2), ')')
y0[2,10] <- paste0(round(t2$AIC_mean[1],2), ' (', round(t2$AIC_sd[1],2), ')')
y0[2,11] <- paste0(round(t2$BIC_mean[1],2), ' (', round(t2$BIC_sd[1],2), ')')

o0[2,1] <- 'general'
o0[2,6] <- paste0(round(t2$alpha_mean[2],2), ' (', round(t2$alpha_sd[2],2), ')')
o0[2,9] <- paste0(round(t2$beta_mean[2],2), ' (', round(t2$beta_sd[2],2), ')')
o0[2,10] <- paste0(round(t2$AIC_mean[2],2), ' (', round(t2$AIC_sd[2],2), ')')
o0[2,11] <- paste0(round(t2$BIC_mean[2],2), ' (', round(t2$BIC_sd[2],2), ')')

# gain-loss learning ####
y0[3,1] <- 'gain-loss'
y0[3,7] <- paste0(round(t1$a_gain_mean[1],2), ' (', round(t1$a_gain_sd[1],2), ')')
y0[3,8] <- paste0(round(t1$a_loss_mean[1],2), ' (', round(t1$a_loss_sd[1],2), ')')
y0[3,9] <- paste0(round(t1$beta_mean[1],2), ' (', round(t1$beta_sd[1],2), ')')
y0[3,10] <- paste0(round(t1$AIC_mean[1],2), ' (', round(t1$AIC_sd[1],2), ')')
y0[3,11] <- paste0(round(t1$BIC_mean[1],2), ' (', round(t1$BIC_sd[1],2), ')')

o0[3,1] <- 'gain-loss'
o0[3,7] <- paste0(round(t1$a_gain_mean[2],2), ' (', round(t1$a_gain_sd[2],2), ')')
o0[3,8] <- paste0(round(t1$a_loss_mean[2],2), ' (', round(t1$a_loss_sd[2],2), ')')
o0[3,9] <- paste0(round(t1$beta_mean[2],2), ' (', round(t1$beta_sd[2],2), ')')
o0[3,10] <- paste0(round(t1$AIC_mean[2],2), ' (', round(t1$AIC_sd[2],2), ')')
o0[3,11] <- paste0(round(t1$BIC_mean[2],2), ' (', round(t1$BIC_sd[2],2), ')')

# decay gain-loss learning ####
y0[4,1] <- 'decay gain-loss'
y0[4,5] <- paste0(round(t4$decay_mean[1],2), ' (', round(t4$decay_sd[1],2), ')')
y0[4,7] <- paste0(round(t4$a_gain_mean[1],2), ' (', round(t4$a_gain_sd[1],2), ')')
y0[4,8] <- paste0(round(t4$a_loss_mean[1],2), ' (', round(t4$a_loss_sd[1],2), ')')
y0[4,9] <- paste0(round(t4$beta_mean[1],2), ' (', round(t4$beta_sd[1],2), ')')
y0[4,10] <- paste0(round(t4$AIC_mean[1],2), ' (', round(t4$AIC_sd[1],2), ')')
y0[4,11] <- paste0(round(t4$BIC_mean[1],2), ' (', round(t4$BIC_sd[1],2), ')')

o0[4,1] <- 'decay gain-loss'
o0[4,5] <- paste0(round(t4$decay_mean[2],2), ' (', round(t4$decay_sd[2],2), ')')
o0[4,7] <- paste0(round(t4$a_gain_mean[2],2), ' (', round(t4$a_gain_sd[2],2), ')')
o0[4,8] <- paste0(round(t4$a_loss_mean[2],2), ' (', round(t4$a_loss_sd[2],2), ')')
o0[4,9] <- paste0(round(t4$beta_mean[2],2), ' (', round(t4$beta_sd[2],2), ')')
o0[4,10] <- paste0(round(t4$AIC_mean[2],2), ' (', round(t4$AIC_sd[2],2), ')')
o0[4,11] <- paste0(round(t4$BIC_mean[2],2), ' (', round(t4$BIC_sd[2],2), ')')

# initial beliefs gain-loss learning ####
y0[5,1] <- 'initial beliefs gain-loss'
y0[5,2] <- paste0(round(t5$itrust_mean[1],2), ' (', round(t5$itrust_sd[1],2), ')')
y0[5,3] <- paste0(round(t5$ineut_mean[1],2), ' (', round(t5$ineut_sd[1],2), ')')
y0[5,4] <- paste0(round(t5$iuntrust_mean[1],2), ' (', round(t5$iuntrust_sd[1],2), ')')
y0[5,7] <- paste0(round(t5$a_gain_mean[1],2), ' (', round(t5$a_gain_sd[1],2), ')')
y0[5,8] <- paste0(round(t5$a_loss_mean[1],2), ' (', round(t5$a_loss_sd[1],2), ')')
y0[5,9] <- paste0(round(t5$beta_mean[1],2), ' (', round(t5$beta_sd[1],2), ')')
y0[5,11] <- paste0(round(t5$BIC_mean[1],2), ' (', round(t5$BIC_sd[1],2), ')')

o0[5,1] <- 'initial beliefs gain-loss'
o0[5,2] <- paste0(round(t5$itrust_mean[2],2), ' (', round(t5$itrust_sd[2],2), ')')
o0[5,3] <- paste0(round(t5$ineut_mean[2],2), ' (', round(t5$ineut_sd[2],2), ')')
o0[5,4] <- paste0(round(t5$iuntrust_mean[2],2), ' (', round(t5$iuntrust_sd[2],2), ')')
o0[5,7] <- paste0(round(t5$a_gain_mean[2],2), ' (', round(t5$a_gain_sd[2],2), ')')
o0[5,8] <- paste0(round(t5$a_loss_mean[2],2), ' (', round(t5$a_loss_sd[2],2), ')')
o0[5,9] <- paste0(round(t5$beta_mean[2],2), ' (', round(t5$beta_sd[2],2), ')')
o0[5,10] <- paste0(round(t5$AIC_mean[2],2), ' (', round(t5$AIC_sd[2],2), ')')
o0[5,11] <- paste0(round(t5$BIC_mean[2],2), ' (', round(t5$BIC_sd[2],2), ')')

# full table ####
table <- cbind(y0,o0[2:11])
table <- rbind(c('', 'Younger', '', '','','','','','','', '', 'Older', '','','','','','','','','', '', ''), table)
write.csv(table, here::here('figs', 'model_parameter_grpmeans.csv'))

