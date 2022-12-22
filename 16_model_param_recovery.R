# model parameter recovery
# 3.11.21 kls updated 11.3.22

# init ####
# load required packages
library(here)
library(tidyverse)
library(ggpubr)

# load source functions

# set hard-coded variables
sim_type = 'part_params' # change depending on simulation type 

# 2a model - gain loss - done####
# read in data
gld <- read.csv(here::here('output', 'simulation',  sim_type, 'sim_2alpha_model_data.csv')) # simulated data
gld <- gld[which(gld$Trial == 1), ] # get parameters from first trial
glf <- read.csv(here::here('output', 'simulation',  sim_type, 'a2_fit2_a2.csv')) # best-fit parameters to sim data

# clean up and merge
gld <- rename(gld, trueBeta = Beta, trueAlphaGain = Alpha_gain, trueAlphaLoss = Alpha_loss)
gld <- gld[, order(names(gld))]
glf <- rename(glf, estBeta = beta, estAlphaGain = a_gain, estAlphaLoss = a_loss)
gldl <- merge(gld, glf, by = 'Subject')
rm(gld, glf)

# # limit to gain-loss learners-only #### 
# comment out if unwanted
# dt <- read.csv(here('output', 'model_comparisons.csv'))
# gainloss <- dt %>% filter(BIC_win == 'double')
# gainloss <- gainloss$id
# rm(dt)
# gldl$subParent <- str_sub(gldl$Subject, start = 1, end = 8)
# gldl <- gldl %>% filter(subParent %in% gainloss)

# graph #### 
a2_alpha_gain <- ggscatter(gldl, x = 'trueAlphaGain', y = 'estAlphaGain', shape = 1, color = 'blue', 
                           add = 'reg.line', conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
                           xlab = 'Simulated Alpha Gain value', ylab = 'Recovered Alpha Gain value')
a2_alpha_gain

a2_alpha_loss <- ggscatter(gldl, x = 'trueAlphaLoss', y = 'estAlphaLoss', shape = 1, color = 'blue4', 
                           add = 'reg.line', conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
                           xlab = 'Simulated Alpha Loss value', ylab = 'Recovered Alpha Loss value')

a2_alpha_loss

a2_beta <- ggscatter(gldl, x = 'trueBeta', y = 'estBeta', shape = 1, color = 'red3', 
                     add = 'reg.line', conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(1, 3), cor.coef.size = 5,
                     xlab = 'Simulated Beta value', ylab = 'Recovered Beta value')
a2_beta

