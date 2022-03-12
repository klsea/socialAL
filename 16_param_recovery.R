# model parameter recovery
# 3.11.21 kls

# init ####
# load required packages
library(here)
library(tidyverse)
library(ggpubr)

# load source functions

# set hard-coded variables

# baseline model - done ####
# read data in
bld <- read.csv(here::here('output', 'simulation', 'sim_baseline_model_data.csv'))
blf <- read.csv(here::here('output', 'simulation', 'sim_baseline_model_fits.csv'))
bld <- bld[which(bld$Trial == 1),] 
# clean up and merge 
bld <- rename(bld, trueBeta = Beta)
blf <- rename(blf, estBeta = beta)
bl <- merge(bld[5:6], blf, by = 'Subject')
rm(bld, blf)
# graph
ggscatter(bl, x = 'trueBeta', y = 'estBeta', shape = 1, color = 'red3', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(1, 3), cor.coef.size = 5,
          xlab = 'True Beta value', ylab = 'Extimated Beta value')


# 1a model - done ####
# read data in
ad <- read.csv(here::here('output', 'simulation', 'sim_1alpha_model_data.csv'))
af <- read.csv(here::here('output', 'simulation', 'sim_1alpha_model_fits.csv'))
ad <- ad[which(ad$Trial == 1), ] # 1a model
# clean up and merge 
ad <- rename(ad, trueBeta = Beta, trueAlpha = Alpha)
af <- rename(af, estBeta = beta, estAlpha = alpha)
a1 <- merge(ad[5:7], af, by = 'Subject')
rm(ad, af)
#graph
ggscatter(a1, x = 'trueAlpha', y = 'estAlpha', shape = 1, color = 'blue', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
          xlab = 'True Alpha value', ylab = 'Extimated Alpha value')

ggscatter(a1, x = 'trueBeta', y = 'estBeta', shape = 1, color = 'red3', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(1, 3), cor.coef.size = 5,
          xlab = 'True Beta value', ylab = 'Extimated Beta value')


# 1a decay model - done ####
# read in data
add <- read.csv(here::here('output', 'simulation', 'sim_1alpha_decay_model_data.csv'))
add <- add[which(add$Trial == 1), ] # 1a decay model
adf <- read.csv(here::here('output', 'simulation', 'sim_1alpha_decay_model_fits.csv'))
# clean up and merge 
add <- rename(add, trueBeta = Beta, trueAlpha = Alpha, trueDecay = Decay)
adf <- rename(adf, estBeta = beta, estAlpha = alpha, estDecay = decay)
a1d <- merge(add[5:8], adf, by = 'Subject')
rm(add, adf)
# graph
ggscatter(a1d, x = 'trueAlpha', y = 'estAlpha', shape = 1, color = 'blue', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
          xlab = 'True Alpha value', ylab = 'Extimated Alpha value')

ggscatter(a1d, x = 'trueBeta', y = 'estBeta', shape = 1, color = 'red', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(1, 3), cor.coef.size = 5,
          xlab = 'True Beta value', ylab = 'Extimated Beta value')

ggscatter(a1d, x = 'trueDecay', y = 'estDecay', shape = 1, color = 'darkgreen', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(0, .5), cor.coef.size = 5,
          xlab = 'True Decay value', ylab = 'Extimated Decay value')

# 2a model - gain loss - done####
# read in data
gld <- read.csv(here::here('output', 'simulation', 'sim_2alpha_model_data.csv'))
gld <- gld[which(gld$Trial == 1), ] # 2a model
glf <- read.csv(here::here('output', 'simulation', 'sim_2alpha_model_fit.csv'))
# clean up and merge
gld <- rename(gld, trueBeta = Beta, trueAlphaGain = Alpha_gain, trueAlphaLoss = Alpha_loss)
glf <- rename(glf, estBeta = beta, estAlphaGain = a_gain, estAlphaLoss = a_loss)
gll <- merge(gld[c(6, 1:3)], glf, by = 'Subject')
rm(gld, glf)

# graph
ggscatter(gll, x = 'trueAlphaGain', y = 'estAlphaGain', shape = 1, color = 'blue', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
          xlab = 'True Alpha Gain value', ylab = 'Extimated Alpha Gain value')

ggscatter(gll, x = 'trueAlphaLoss', y = 'estAlphaLoss', shape = 1, color = 'blue4', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
          xlab = 'True Alpha Loss value', ylab = 'Extimated Alpha Loss value')

ggscatter(gll, x = 'trueBeta', y = 'estBeta', shape = 1, color = 'red', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(1, 3), cor.coef.size = 5,
          xlab = 'True Beta value', ylab = 'Extimated Beta value')

# 2a decay model ####
# read in data
gldd <- read.csv(here::here('output', 'simulation', 'sim_2alpha_decay_model_data.csv'))
gldd <- gldd[which(gldd$Trial == 1),] # 2a with decay
gldf <- read.csv(here::here('output', 'simulation', 'sim_2alpha_decay_model_fit.csv'))
# clean up and merge
gldd <- rename(gldd, trueBeta = Beta, trueAlphaGain = Alpha_gain, trueAlphaLoss = Alpha_loss, trueDecay = Decay)
gldf <- rename(gldf, estBeta = beta, estAlphaGain = a_gain, estAlphaLoss = a_loss, estDecay = decay)
gldl <- merge(gldd[c(7, 1:3, 5)], gldf, by = 'Subject')
rm(gldd, gldf)
# graph
ggscatter(gldl, x = 'trueAlphaGain', y = 'estAlphaGain', shape = 1, color = 'blue', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
          xlab = 'True Alpha Gain value', ylab = 'Extimated Alpha Gain value')

ggscatter(gldl, x = 'trueAlphaLoss', y = 'estAlphaLoss', shape = 1, color = 'blue4', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
          xlab = 'True Alpha Loss value', ylab = 'Extimated Alpha Loss value')

ggscatter(gldl, x = 'trueBeta', y = 'estBeta', shape = 1, color = 'red', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(1, 3), cor.coef.size = 5,
          xlab = 'True Beta value', ylab = 'Extimated Beta value')

ggscatter(gldl, x = 'trueDecay', y = 'estDecay', shape = 1, color = 'darkgreen', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(0, .5), cor.coef.size = 5,
          xlab = 'True Decay value', ylab = 'Extimated Decay value')
# 2a prior model ####
# read in data
glpd <- read.csv(here::here('output', 'simulation', 'sim_2alpha_with_priors_model_data.csv'))
# clean up and merge
# graph

