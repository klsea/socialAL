# Group differences in estimates of model parameters
# 3.2.20

# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load source functions
source(here::here('scr', 'concat_clean.R'))
source(here::here('scr', 'clean_single_alpha.R'))

# set hard-coded variables

# read data in 
dt <- read.csv(here::here('output', 'two_alpha_model_params.csv' ))
d2 <- read.csv(here::here('output', 'single_alpha_model_params.csv'))
d3 <- read.csv(here::here('output', 'baseline_model_params.csv'))
o4 <- read.csv(here::here('output', 'two_alpha_with_decay_model_params_older.csv'))
y4 <- read.csv(here::here('output', 'two_alpha_with_decay_model_params_younger.csv'))
d4 <- rbind(o4, y4); rm(o4, y4)
#d5 will be priors
d6 <- read.csv(here::here('output', 'model_comparison.csv'))[c(1,15)]

# create age group labels
dt <- clean_param(dt)
d2 <- clean_single_alpha(d2)
d3 <- clean_single_alpha(d3)
d4 <- clean_single_alpha(d4)

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

# compare parameters of baseline model
older <- d3[which(d3$agegrp=='Older'),]
younger <- d3[which(d3$agegrp=='Younger'),]
basebeta = t.test(older$beta, younger$beta)



# 2 alpha with decay model ####

# uncommenting the lines below allows analysis of 
# only participants best-fit by the double alpha model
d4 <- merge(d4, d6, by='id')
d4 <- d4[which(d4$win == 'decay'),] 

# compare parameters of 2 alpha model
older <- d4[which(d4$agegrp=='Older'),]
younger <- d4[which(d4$agegrp=='Younger'),]

decaygain = t.test(older$alpha_gain, younger$alpha_gain)
decayloss = t.test(older$alpha_loss, younger$alpha_loss)
decaybeta = t.test(older$beta, younger$beta)
decaydecay = t.test(older$decay, younger$decay)
