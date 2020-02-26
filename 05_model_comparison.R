# model comparison
# 2.24.20

# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load source functions
source(here::here('scr', 'AIC_functions.R'))

# set hard-coded variables

# read data in 
d1 <- read.csv(here::here('output', 'two_alpha_model_params.csv' ))
d2 <- read.csv(here::here('output', 'single_alpha_model_params.csv' ))
d3 <- read.csv(here::here('output', 'baseline_model_params.csv'))

#merge data frames with each other - update once all subjects run with initial model
d4 <- merge(d1[c(1,5)], d2[c(1,4)], by = 'id')
colnames(d4) <- c('id', 'llh_double', 'llh_single')

dt <- merge(d4, d3[c(1,3)], by = 'id')
colnames(dt) <- c('id', 'llh_double', 'llh_single', 'llh_baseline')
rm(d1,d2,d3,d4)

# caluclate AIC for each model
dt$AIC_double <- calc_AIC(45, 3, dt$llh_double)
dt$AIC_single <- calc_AIC(45, 2, dt$llh_single)
dt$AIC_baseline <- calc_AIC(45, 1, dt$llh_baseline)

# calculate Weight AIC for each model and choose the winning weight
y <- winningWeight(dt[1], dt[5:7])
d5 <- merge(dt, y, by = 'id')
