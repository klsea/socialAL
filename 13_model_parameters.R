# Group differences in estimates of model parameters
# 3.2.20 updated 3.15.21 KLS

# load required packages
library(here)
library(tidyverse)
library(ggplot2)
library(car)

# load source functions
source(here::here('scr', 'concat_clean.R'))
source(here::here('scr', 'clean_single_alpha.R'))

# set hard-coded variables

# read data in ####
dt <- read.csv(here::here('output', 'two_alpha_model_params.csv' ))
d2 <- read.csv(here::here('output', 'single_alpha_model_params.csv'))
d3 <- read.csv(here::here('output', 'baseline_model_params.csv'))
o4 <- read.csv(here::here('output', 'two_alpha_with_decay_model_params_older.csv'))
y4 <- read.csv(here::here('output', 'two_alpha_with_decay_model_params_younger.csv'))
o5 <- read.csv(here::here('output', 'prior_model_params_older.csv'))
y5 <- read.csv(here::here('output', 'prior_model_params_younger.csv'))

d4 <- rbind(o4, y4); rm(o4, y4)
d5 <- rbind(o5, y5); rm(o5, y5)
d6 <- read.csv(here::here('output', 'model_comparison.csv'))[c(1,18)]

# create age group labels
dt <- clean_param(dt)
d2 <- clean_single_alpha(d2)
d3 <- clean_single_alpha(d3)
d4 <- clean_single_alpha(d4)
d5 <- clean_single_alpha(d5)

# 2 alpha model ####
# uncommenting the lines below allows analysis of 
# only participants best-fit by the double alpha model
dt <- merge(dt, d6, by='id')
dt <- dt[which(dt$win == 'double'),] 

# compare parameters of 2 alpha model
older <- dt[which(dt$agegrp=='Older'),]
younger <- dt[which(dt$agegrp=='Younger'),]

a2gain = t.test(older$alpha_gain, younger$alpha_gain)
a2loss = t.test(older$alpha_loss, younger$alpha_loss)
a2beta = t.test(older$beta, younger$beta)

# compare alphas to each other
a2alphas = t.test(dt$alpha_gain, dt$alpha_loss, paired = TRUE)
a2alphasYA = t.test(younger$alpha_gain, younger$alpha_loss)
a2alphasOA = t.test(older$alpha_gain, older$alpha_loss)

# 1 alpha model ####
# uncommenting the lines below allows analysis of 
# only participants best-fit by the single alpha model
d2 <- merge(d2, d6, by='id')
d2 <- d2[which(d2$win == 'single'),] 

# compare parameters of 1 alpha model
older <- d2[which(d2$agegrp=='Older'),]
younger <- d2[which(d2$agegrp=='Younger'),]
a1alpha = t.test(older$alpha, younger$alpha)
a1beta = t.test(older$beta, younger$beta)


# baseline model ####
# uncommenting the lines below allows analysis of 
# only participants best-fit by the baseline model
d3 <- merge(d3, d6, by='id')
d3 <- d3[which(d3$win == 'baseline'),] 

# compare parameters of baseline model ####
older <- d3[which(d3$agegrp=='Older'),]
younger <- d3[which(d3$agegrp=='Younger'),]
basebeta = t.test(older$beta, younger$beta)

# 2 alpha with decay model ####

# uncommenting the lines below allows analysis of 
# only participants best-fit by the double alpha decay model
d4 <- merge(d4, d6, by='id')
#d4 <- d4[which(d4$win == 'decay'),] 

# compare parameters of 2 alpha model
older <- d4[which(d4$agegrp=='Older'),]
younger <- d4[which(d4$agegrp=='Younger'),]

alphas <- gather(d4[c(1:3,7)], condition, alpha, alpha_gain:alpha_loss, factor_key = TRUE)
alphas$condition <- substr(as.character(alphas$condition), 7, 10)
alphas$condition <- as.factor(alphas$condition)
decayalpha = aov(alpha ~ agegrp * condition, data = alphas)

decaybeta = t.test(older$beta, younger$beta)
decaydecay = t.test(older$decay, younger$decay)
leveneTest(decay ~ agegrp, data = d4)

# 2 alpha with priors model ####

# uncommenting the lines below allows analysis of 
# only participants best-fit by the double alpha model with priors
#d5 <- merge(d5, d6, by='id')
#d5 <- d4[which(d5$win == 'prior'),] 

# compare parameters of 2 alpha model
older <- d5[which(d5$agegrp=='Older'),]
younger <- d5[which(d5$agegrp=='Younger'),]

priorgain = t.test(older$alpha_gain, younger$alpha_gain)
priorloss = t.test(older$alpha_loss, younger$alpha_loss)
priorbeta = t.test(older$beta, younger$beta)
priortrust = t.test(older$iProb_trust, younger$iProb_trust)
priorneutral = t.test(older$iProb_neut, younger$iProb_neut)
prioruntrust = t.test(older$iProb_untrust, younger$iProb_untrust)
