# best fit by BIC confusion matrix
# 3.16.22 KLS

# init ####
# load required packages
library(here)
library(tidyverse)
#library(cogmod)
#library(ggpubr)

# load source functions
source(here::here('scr', 'concat_clean.R'))
source(here::here('scr', 'AIC_functions.R'))
source(here::here('scr', 'BIC_functions.R'))

# set hard-coded variables

# read in fits to baseline sim data ####
b <- read.csv(here::here('output', 'simulation', 'b_fit2_b.csv'))
#b <- rename(b, 'b_llh' = 'llh')
a1 <- read.csv(here::here('output', 'simulation', 'a1_fit2_b.csv'))
#a1 <- rename(a1, 'a1_llh' = 'llh')
a1d <- read.csv(here::here('output', 'simulation', 'a1d_fit2_b.csv'))
#a1d <- rename(a1d, 'a1d_llh' = 'llh')
a2 <- read.csv(here::here('output', 'simulation', 'a2_fit2_b.csv'))
#a2 <- rename(a2, 'a2_llh' = 'llh')
a2d <- read.csv(here::here('output', 'simulation', 'a2d_fit2_b.csv'))
#a2d <- rename(a2d, 'a2d_llh' = 'llh')
a2p <- read.csv(here::here('output', 'simulation', 'a2p_fit2_b.csv'))
#a2p <- rename(a2p, 'a2p_llh' = 'llh')

# calculate AIC and BIC
b$AIC <- calc_AIC(45,1,b$llh); b$BIC <- calc_BIC(45,1,b$llh)
a1$AIC <- calc_AIC(45,1,a1$llh); a1$BIC <- calc_BIC(45,1,a1$llh)
a1d$AIC <- calc_AIC(45,1,a1d$llh); a1d$BIC <- calc_BIC(45,1,a1d$llh)
a2$AIC <- calc_AIC(45,1,a2$llh); a2$BIC <- calc_BIC(45,1,a2$llh)
a2d$AIC <- calc_AIC(45,1,a2d$llh); a2d$BIC <- calc_BIC(45,1,a2d$llh)
a2p$AIC <- calc_AIC(45,1,a2p$llh); a2p$BIC <- calc_BIC(45,1,a2p$llh)


# merge llhs into one data frame
d <- merge(b[c(1,3:5)], a1[c(1,4:6)], by = 'Subject', suffixes = c('_b', '_a1'))
d1 <- merge(a1d[c(1,5:7)], a2[c(1,5:7)], by = 'Subject', suffixes = c('_a1d', '_a2'))
d2 <- merge(a2d[c(1,6:8)], a2p[c(1,8,12:13)], by = 'Subject', suffixes = c('_a2d', '_a2p'))
d3 <- merge(d,d1)
d4 <- merge(d3,d2)
rm(b,a1,a1d,a2,a2d,a2p,d,d1,d2,d3)

# calculate weights

# read in fits to single alpha sim data ####
b <- read.csv(here::here('output', 'simulation', 'b_fit2_a1.csv'))
a1 <- read.csv(here::here('output', 'simulation', 'sim_1alpha_model_fits.csv'))
a1d <- read.csv(here::here('output', 'simulation', 'a1d_fit2_a1.csv'))
a2 <- read.csv(here::here('output', 'simulation', 'a2_fit2_a1.csv'))
a2d <- read.csv(here::here('output', 'simulation', 'a2d_fit2_a1.csv'))
a2p <- read.csv(here::here('output', 'simulation', 'a2p_fit2_a1.csv'))

                   
