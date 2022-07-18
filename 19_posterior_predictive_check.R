# Posterior predictive checks - simple analysis
# 7.18.22 KLS

# load required packages ####
library(here)
library(stringr)
library(tidyverse)

# load source functions
source(here::here('scr', 'add_tt_number.R'))
source(here::here('scr', 'add_partner_trial.R'))

# set hard-coded variables

# load data
dt <- read.csv(here('output', 'ppc', 'sim_w_bestfit_2alpha_params.csv'))

## Prep data ####
## ---------------------
# add group label
dt$sub_parent <- str_split_fixed(dt$Subject, '-', 3)[,2]
dt$sub_child <- str_split_fixed(dt$Subject, '-', 3)[,3]
dt$agegrp <- ifelse(as.numeric(dt$sub_parent) > 2000, 'Older', 'Younger')

# rename columns
dt <- rename(dt, id = Subject, trial_number = Trial)

# recode choice and trial type
dt$amount_shared <- recode(dt$Choice, `1` = 0, `2` = 3, `3` = 6, `4` = 9)
dt$trial_type <- recode(dt$Stim_Sequence, `0` = 'Trustworthy', `1` = 'Neutral', `2` = 'Untrustworthy')

# reorder trial_type and age group factors
dt$trial_type <- factor(dt$trial_type, levels = c('Untrustworthy', 'Neutral', 'Trustworthy'))
dt$agegrp <- factor(dt$agegrp, levels = c('Younger', 'Older'))

# analysis 1 - group and condition ####
# calculate individual means 
indiv_means <- dt %>% 
  group_by(id, sub_parent, agegrp, trial_type) %>%
  summarise(avg_amount = mean(amount_shared, na.rm = TRUE))

old_part_means <- indiv_means %>% group_by(sub_parent, agegrp, trial_type )%>%
  summarise(mean_of_means = mean(avg_amount, na.rm = TRUE))

m1 <- aov(mean_of_means ~ agegrp * trial_type + Error(sub_parent/trial_type), data = old_part_means)
summary(m1)

d1 <- spread(old_part_means, trial_type, avg_amount)

# follow-up t-tests partner type
t.test(d1$Trustworthy, d1$Neutral, paired = TRUE)
t.test(d1$Trustworthy, d1$Untrustworthy, paired = TRUE)
t.test(d1$Untrustworthy, d1$Neutral, paired = TRUE)

# follow-up t-tests age group
t.test(d1$Trustworthy[d1$agegrp == 'Older'], d1$Trustworthy[d1$agegrp == 'Younger'])
t.test(d1$Neutral[d1$agegrp == 'Older'], d1$Neutral[d1$agegrp == 'Younger'])
t.test(d1$Untrustworthy[d1$agegrp == 'Older'], d1$Untrustworthy[d1$agegrp == 'Younger'])

# analysis 2 - group and condition over time ####
dt <- add_partner_trial(dt)

# calculate individual means 
indiv_means2 <- dt %>% 
  group_by(id, sub_parent, agegrp, trial_type, partner_trial_number) %>%
  summarise(avg_amount = mean(amount_shared, na.rm = TRUE))

old_part_means2 <- indiv_means2 %>% group_by(sub_parent, agegrp, trial_type, partner_trial_number)%>%
  summarise(mean_of_means = mean(avg_amount, na.rm = TRUE))

m2 <- aov(mean_of_means ~ agegrp * trial_type *partner_trial_number + Error(sub_parent/trial_type), data = old_part_means2)
summary(m2)
