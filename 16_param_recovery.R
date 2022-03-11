# model parameter recovery
# 3.11.21 kls

# init ####
# load required packages
library(here)
library(tidyverse)
library(ggpubr)

# load source functions

# set hard-coded variables

# read data in ####
# baseline model
bld <- read.csv(here::here('output', 'simulation', 'sim_baseline_model_data.csv'))
blf <- read.csv(here::here('output', 'simulation', 'sim_baseline_model_fits.csv'))

# 1a model
ad <- read.csv(here::here('output', 'simulation', 'sim_1alpha_model_data.csv'))
af <- read.csv(here::here('output', 'simulation', 'sim_1alpha_model_fits.csv'))

# 1a decay model
add <- read.csv(here::here('output', 'simulation', 'sim_1alpha_decay_model_data.csv'))

# 2a model - gain loss
gld <- read.csv(here::here('output', 'simulation', 'sim_2alpha_model_data.csv'))

# 2a decay model
gldd <- read.csv(here::here('output', 'simulation', 'sim_2alpha_decay_model_data.csv'))

# 2a prior model
glpd <- read.csv(here::here('output', 'simulation', 'sim_2alpha_with_priors_model_data.csv'))

# pull true params out of data files ####
bld <- bld[which(bld$Trial == 1),] # baseline model
ad <- ad[which(ad$Trial == 1), ] # 1a model
gld <- gld[which(gld$T)]

# clean up and merge ####
# baseline model
bld <- rename(bld, trueBeta = Beta)
blf <- rename(blf, estBeta = beta)
bl <- merge(bld[5:6], blf, by = 'Subject')
rm(bld, blf)

# 1a model
ad <- rename(ad, trueBeta = Beta, trueAlpha = Alpha)
af <- rename(af, estBeta = beta, estAlpha = alpha)
al <- merge(ad[5:7], af, by = 'Subject')
rm(ad, af)

# 1a decay model
# 2a model
# 2a decay model
# 2a prior model

# correlations ####
# baseline model
cor(bl$trueBeta, bl$estBeta)
ggscatter(bl, x = 'trueBeta', y = 'estBeta', shape = 1, color = 'red', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(1, 3), cor.coef.size = 5,
          xlab = 'True Beta value', ylab = 'Extimated Beta value')

# 1a model
ggscatter(al, x = 'trueAlpha', y = 'estAlpha', shape = 1, color = 'blue', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
          xlab = 'True Alpha value', ylab = 'Extimated Alpha value')

ggscatter(al, x = 'trueBeta', y = 'estBeta', shape = 1, color = 'red', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(1, 3), cor.coef.size = 5,
          xlab = 'True Beta value', ylab = 'Extimated Beta value')

# 1a decay model
ggscatter(adl, x = 'trueAlpha', y = 'estAlpha', shape = 1, color = 'blue', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
          xlab = 'True Alpha value', ylab = 'Extimated Alpha value')

ggscatter(adl, x = 'trueBeta', y = 'estBeta', shape = 1, color = 'red', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(1, 3), cor.coef.size = 5,
          xlab = 'True Beta value', ylab = 'Extimated Beta value')

ggscatter(adl, x = 'trueDecay', y = 'estDecay', shape = 1, color = 'darkgreen', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(1, 3), cor.coef.size = 5,
          xlab = 'True Decay value', ylab = 'Extimated Decay value')

# 2a model
ggscatter(gll, x = 'trueAgain', y = 'estAgain', shape = 1, color = 'blue', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
          xlab = 'True Alpha Gain value', ylab = 'Extimated Alpha Gain value')

ggscatter(gll, x = 'trueAloss', y = 'estAloss', shape = 1, color = 'blue', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
          xlab = 'True Alpha Loss value', ylab = 'Extimated Alpha Loss value')

ggscatter(gll, x = 'trueBeta', y = 'estBeta', shape = 1, color = 'red', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(1, 3), cor.coef.size = 5,
          xlab = 'True Beta value', ylab = 'Extimated Beta value')

# 2a decay model
# 2a prior model
