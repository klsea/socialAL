# Visualization of msimulated data
# 11.17.22

# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load source functions

# set hard-coded variables
sim_type = 'part_params' # change depending on simulation type
sim_model = 'a2'

# read data in 
dt <- read.csv(here('data', 'socialAL_clean_data.csv'))
d1 <- read.csv(here('output', 'two_alpha_model_params.csv'))
sim <- read.csv(here('output', 'simulation', 'part_params', 'sim_2alpha_model_data.csv'))
dt <- merge(dt, d1, by = 'id')
rm(d1)

# graph constants ####
lg = 18 # text size
md = 16
sm = 14
custom_plot = list(theme(
  plot.title = element_text(size = lg, hjust = 0.5),
  axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
  legend.title = element_text(size = lg), legend.text = element_text(size = sm), 
  legend.position='top', strip.text.x = element_text(size=md))
)

# hist best-fit params
hist(dt$alpha_gain[which(dt$trial_number == 1)], breaks = 20)
hist(dt$alpha_loss[which(dt$trial_number == 1)], breaks = 20)
hist(dt$beta[which(dt$trial_number == 1)], breaks = 20)

# prep data ####
# reorder trial_type and age group factors
dt$trial_type <- factor(dt$trial_type, levels = c('Trustworthy', 'Neutral', 'Untrustworthy'), ordered = TRUE)
dt$agegrp <- factor(dt$agegrp, levels = c('Younger', 'Older'), ordered = TRUE)
dt$betagrp <- ifelse(dt$beta > 1, 'high', 'low')
dt$aggrp <- ifelse(dt$alpha_gain > 0.01, 'high', 'low')
dt$algrp <- ifelse(dt$alpha_loss > 0.01, 'high', 'low')

sim$trial_type <- recode_factor(as.character(sim$Stim_Sequence), 
                         '0' = 'Trustworthy', '1' = 'Neutral', '2' = 'Untrustworthy')
sim$parentSub <- substr(sim$Subject, 5,8)
sim$agegrp <- ifelse(floor(as.numeric(sim$parentSub)/1000) == 1, 'Younger', 'Older')
sim$agegrp <-  factor(sim$agegrp, levels = c('Younger', 'Older'), ordered = TRUE)
sim$parentSub <- as.factor(paste0('sub-', sim$parentSub))
sim$amount_shared <- as.integer(recode(as.character(sim$Choice), '1' = '0', '2' = '3', '3' = '6', '4' = '9'))
sim$betagrp <- ifelse(sim$Beta > 1, 'high', 'low')
sim$aggrp <- ifelse(sim$Alpha_gain > 0.01, 'high', 'low')
sim$algrp <- ifelse(sim$Alpha_loss > 0.01, 'high', 'low')

## Calculate means ####
## ---------------------
# calculate individual means 
indiv_means <- dt %>% 
  group_by(id, agegrp, betagrp, aggrp, algrp, trial_type) %>%
  summarise(avg_amount = mean(amount_shared, na.rm = TRUE))

indiv_means2 <- sim %>% 
  group_by(Subject, parentSub, agegrp, betagrp, aggrp, algrp, trial_type) %>%
  summarise(avg_amount = mean(amount_shared, na.rm = TRUE))

# calculate age group means 
se <- function(sd,n) {sd/sqrt(n())}
grpmeans <- indiv_means %>% 
  group_by(agegrp, trial_type) %>%
  summarise(mean_amount = mean(avg_amount), sd_amount = sd(avg_amount), 
            se_amount = sd(avg_amount)/sqrt(n()))

grpmeans2 <- indiv_means2 %>% 
  group_by(agegrp, trial_type) %>%
  summarise(mean_amount = mean(avg_amount), sd_amount = sd(avg_amount), 
            se_amount = sd(avg_amount)/sqrt(n()))

## Graph 1 - Group means ####
## ---------------------
# pretty graphs
beh <- ggplot() + 
  geom_point(data = indiv_means, aes(x = trial_type, y = avg_amount, colour = agegrp), 
             position = position_jitterdodge(jitter.height = .25), alpha = .5) + 
  geom_bar(data = grpmeans, aes(x = trial_type, y = mean_amount, colour = agegrp, fill = agegrp), 
           position=position_dodge(), stat= 'identity', alpha = 0.3) + 
  geom_errorbar(data = grpmeans, aes(x = trial_type, y = mean_amount, ymin = mean_amount-se_amount, ymax = mean_amount + se_amount, colour = agegrp), 
                position = position_dodge(.9), width = .2) + 
  xlab('Partner Type') + ylab('Average $ Shared') + coord_cartesian(ylim=c(0, 9)) + 
  scale_y_continuous(breaks = c(0,3, 6, 9)) +
  scale_fill_brewer(palette="Set1", name="Age Group") + theme_minimal() + 
  scale_colour_brewer(palette="Set1", name="Age Group") + custom_plot
beh

sim <- ggplot() + 
  geom_point(data = indiv_means2, aes(x = trial_type, y = avg_amount, colour = agegrp), 
             position = position_jitterdodge(jitter.height = .25), alpha = .5) + 
  geom_bar(data = grpmeans2, aes(x = trial_type, y = mean_amount, colour = agegrp, fill = agegrp), 
           position=position_dodge(), stat= 'identity', alpha = 0.3) + 
  geom_errorbar(data = grpmeans2, aes(x = trial_type, y = mean_amount, ymin = mean_amount-se_amount, ymax = mean_amount + se_amount, colour = agegrp), 
                position = position_dodge(.9), width = .2) + 
  xlab('Partner Type') + ylab('Average $ Shared') + coord_cartesian(ylim=c(0, 9)) + 
  scale_y_continuous(breaks = c(0,3, 6, 9)) +
  scale_fill_brewer(palette="Set1", name="Age Group") + theme_minimal() + 
  scale_colour_brewer(palette="Set1", name="Age Group") + custom_plot
sim

## Graph 2 - Individual means ####
## ---------------------
sub = 'sub-1004'

onesub <- ggplot() + 
  geom_point(data = indiv_means[which(indiv_means$id == sub),],
             aes(x = trial_type, y = avg_amount), shape = 15, size = 4, colour = 'blue') +
  geom_point(data = indiv_means2[which(indiv_means2$parentSub == sub),],
             aes(x = trial_type, y = avg_amount, alpha = 0.5, colour = 'red')) +
  xlab('Partner Type') + ylab('Average $ Shared') + coord_cartesian(ylim=c(0, 9)) + 
  scale_y_continuous(breaks = c(0,3, 6, 9)) + theme_minimal() + custom_plot + 
  theme(legend.position = 'none') + ggtitle(sub)
onesub

# reset graph constants ####
lg = 12 # text size
md = 10
sm = 8
custom_plot = list(theme(
  plot.title = element_text(size = lg, hjust = 0.5),
  axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
  legend.title = element_text(size = lg), legend.text = element_text(size = sm), 
  legend.position='top', strip.text.x = element_text(size=md))
)

## Graph 3 - Individual means by age group ####
# ## --------------------------------------------
# indiv_means2$id = indiv_means2$parentSub
# youngsubs <- ggplot() + 
#   geom_point(data = indiv_means[which(indiv_means$agegrp == 'Younger'),],
#              aes(x = trial_type, y = avg_amount), shape = 15, size = 4, colour = 'blue') +
#   geom_point(data = indiv_means2[which(indiv_means2$agegrp == 'Younger'),],
#              aes(x = trial_type, y = avg_amount, alpha = 0.5, colour = 'red')) +
#   xlab('Partner Type') + ylab('Average $ Shared') + coord_cartesian(ylim=c(0, 9)) + 
#   scale_y_continuous(breaks = c(0,3, 6, 9)) + theme_minimal() + custom_plot + 
#   theme(legend.position = 'none') + facet_wrap( ~ id, ncol = 3)
# youngsubs
# 
# oldersubs <- ggplot() + 
#   geom_point(data = indiv_means[which(indiv_means$agegrp == 'Older'),],
#              aes(x = trial_type, y = avg_amount), shape = 15, size = 4, colour = 'blue') +
#   geom_point(data = indiv_means2[which(indiv_means2$agegrp == 'Older'),],
#              aes(x = trial_type, y = avg_amount, alpha = 0.5, colour = 'red')) +
#   xlab('Partner Type') + ylab('Average $ Shared') + coord_cartesian(ylim=c(0, 9)) + 
#   scale_y_continuous(breaks = c(0,3, 6, 9)) + theme_minimal() + custom_plot + 
#   theme(legend.position = 'none') + facet_wrap( ~ id, ncol = 3)
# oldersubs

## Graph 4 - Individual means by beta size ####
## --------------------------------------------
indiv_means2$id = indiv_means2$parentSub
low_beta_subs <- ggplot() + 
  geom_point(data = indiv_means[which(indiv_means$betagrp == 'low'),],
             aes(x = trial_type, y = avg_amount), shape = 15, size = 4, colour = 'blue') +
  geom_point(data = indiv_means2[which(indiv_means2$betagrp == 'low'),],
             aes(x = trial_type, y = avg_amount, alpha = 0.5, colour = 'red')) +
  xlab('Partner Type') + ylab('Average $ Shared') + coord_cartesian(ylim=c(0, 9)) + 
  scale_y_continuous(breaks = c(0,3, 6, 9)) + theme_minimal() + custom_plot + 
  theme(legend.position = 'none') + facet_wrap( ~ id, ncol = 3)
low_beta_subs

high_beta_subs <- ggplot() + 
  geom_point(data = indiv_means[which(indiv_means$betagrp == 'high'),],
             aes(x = trial_type, y = avg_amount), shape = 15, size = 4, colour = 'blue') +
  geom_point(data = indiv_means2[which(indiv_means2$betagrp == 'high'),],
             aes(x = trial_type, y = avg_amount, alpha = 0.5, colour = 'red')) +
  xlab('Partner Type') + ylab('Average $ Shared') + coord_cartesian(ylim=c(0, 9)) + 
  scale_y_continuous(breaks = c(0,3, 6, 9)) + theme_minimal() + custom_plot + 
  theme(legend.position = 'none') + facet_wrap( ~ id, ncol = 3)
high_beta_subs

# ## Graph 5 - Individual means by alpha_gain size ####
# ## --------------------------------------------
# low_ag_subs <- ggplot() + 
#   geom_point(data = indiv_means[which(indiv_means$aggrp == 'low'),],
#              aes(x = trial_type, y = avg_amount), shape = 15, size = 4, colour = 'blue') +
#   geom_point(data = indiv_means2[which(indiv_means2$aggrp == 'low'),],
#              aes(x = trial_type, y = avg_amount, alpha = 0.5, colour = 'red')) +
#   xlab('Partner Type') + ylab('Average $ Shared') + coord_cartesian(ylim=c(0, 9)) + 
#   scale_y_continuous(breaks = c(0,3, 6, 9)) + theme_minimal() + custom_plot + 
#   theme(legend.position = 'none') + facet_wrap( ~ id, ncol = 3)
# low_ag_subs
# 
# high_ag_subs <- ggplot() + 
#   geom_point(data = indiv_means[which(indiv_means$aggrp == 'high'),],
#              aes(x = trial_type, y = avg_amount), shape = 15, size = 4, colour = 'blue') +
#   geom_point(data = indiv_means2[which(indiv_means2$aggrp == 'high'),],
#              aes(x = trial_type, y = avg_amount, alpha = 0.5, colour = 'red')) +
#   xlab('Partner Type') + ylab('Average $ Shared') + coord_cartesian(ylim=c(0, 9)) + 
#   scale_y_continuous(breaks = c(0,3, 6, 9)) + theme_minimal() + custom_plot + 
#   theme(legend.position = 'none') + facet_wrap( ~ id, ncol = 3)
# high_ag_subs

