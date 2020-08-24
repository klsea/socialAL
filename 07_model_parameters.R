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
d4 <- read.csv(here::here('output', 'model_comparison.csv'))[c(1,12)]

# create age group labels
dt <- clean_param(dt)
d2 <- clean_single_alpha(d2)
d3 <- clean_single_alpha(d3)

# uncommenting the lines below allows analysis of 
# only participants best-fit by the double alpha model
dt <- merge(dt, d4, by='id')
dt <- dt[which(dt$win == 'double'),] 

# compare parameters of 2 alpha model
older <- dt[which(dt$agegrp=='Older'),]
younger <- dt[which(dt$agegrp=='Younger'),]

a2gain = t.test(older$alpha_gain, younger$alpha_gain)
a2loss = t.test(older$alpha_loss, younger$alpha_loss)
a2beta = t.test(older$beta, younger$beta)

# compare parameters of 1 alpha model
older <- d2[which(d2$agegrp=='Older'),]
younger <- d2[which(d2$agegrp=='Younger'),]
a1alpha = t.test(older$alpha, younger$alpha)
a1beta = t.test(older$beta, younger$beta)

# compare parameters of baseline model
older <- d3[which(d3$agegrp=='Older'),]
younger <- d3[which(d3$agegrp=='Younger'),]
basebeta = t.test(older$beta, younger$beta)

