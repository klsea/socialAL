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
dt <- read.csv(here::here('output', 'model_params.csv' ))
d2 <- read.csv(here::here('output', 'single_alpha_model_params.csv' ))
d3 <- read.csv(here::here('output', 'baseline_model_params.csv'))

#merge data frames with each other - update once all subjects run with initial model
d4 <- merge(d2[c(1,4)], d3[c(1,3)], by = 'id')
colnames(d4) <- c('id', 'llh_single', 'llh_baseline')

# caluclate AIC for each model
#d4$AIC_double <- calc_AIC(45, 3, d4$llh_double)
d4$AIC_single <- calc_AIC(45, 2, d4$llh_single)
d4$AIC_baseline <- calc_AIC(45, 1, d4$llh_baseline)

# calculate Weight AIC for each model and choose the winning weight
y <- winningWeight(d4[1], d4[4:5])
d5 <- merge(d4, y, by = 'id')
