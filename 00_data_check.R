# Data Check
# 9.4.20 BC updated 3.11.21 KLS

# load required packages ####
library(here)
library(tidyverse)

# load source functions
source(here::here('scr', 'add_partner_trial.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'socialAL_clean_data.csv'))
dt <- dt[c(6,7,1:5,8:10)]

# add partner trial
dt <- add_partner_trial(dt)

# how many responses per person
resp_summary <- dt %>%
  group_by(id) %>%
  summarize(n_resp = sum(respond == 1), agegrp = mean(grp))

# how many per age group
resp_summary %>% group_by(agegrp) %>% summarize(count=n())

# mean number of responses
mean_resp = mean(resp_summary$n_resp)

# SD for  N responses
sd_resp = sd(resp_summary$n_resp)

# compare resposnes by age group
t.test(resp_summary$n_resp ~ resp_summary$agegrp)
