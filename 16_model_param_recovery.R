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
blf <- read.csv(here::here('output', 'simulation', 'b_fit2_b.csv'))
bld <- bld[which(bld$Trial == 1),] 
# clean up and merge 
bld <- rename(bld, trueBeta = Beta)
blf <- rename(blf, estBeta = beta)
bl <- merge(bld[5:6], blf, by = 'Subject')
rm(bld, blf)
# graph
bl_beta <- ggscatter(bl, x = 'trueBeta', y = 'estBeta', shape = 1, color = 'red3', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(1, 3), cor.coef.size = 5,
          xlab = 'True Beta value', ylab = 'Extimated Beta value')
rm(bl)

# 1a model - done ####
# read data in
ad <- read.csv(here::here('output', 'simulation', 'sim_1alpha_model_data.csv'))
af <- read.csv(here::here('output', 'simulation', 'a1_fit2_a1.csv'))
ad <- ad[which(ad$Trial == 1), ] # 1a model
# clean up and merge 
ad <- rename(ad, trueBeta = Beta, trueAlpha = Alpha)
af <- rename(af, estBeta = beta, estAlpha = alpha)
a1 <- merge(ad[5:7], af, by = 'Subject')
rm(ad, af)
#graph
a1_alpha <- ggscatter(a1, x = 'trueAlpha', y = 'estAlpha', shape = 1, color = 'blue', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
          xlab = 'True Alpha value', ylab = 'Extimated Alpha value')

a1_beta <- ggscatter(a1, x = 'trueBeta', y = 'estBeta', shape = 1, color = 'red3', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(1, 3), cor.coef.size = 5,
          xlab = 'True Beta value', ylab = 'Extimated Beta value')
rm(a1)

# 1a decay model - done ####
# read in data
add <- read.csv(here::here('output', 'simulation', 'sim_1alpha_decay_model_data.csv'))
add <- add[which(add$Trial == 1), ] # 1a decay model
adf <- read.csv(here::here('output', 'simulation', 'a1d_fit2_a1d.csv'))
# clean up and merge 
add <- rename(add, trueBeta = Beta, trueAlpha = Alpha, trueDecay = Decay)
adf <- rename(adf, estBeta = beta, estAlpha = alpha, estDecay = decay)
a1d <- merge(add[5:8], adf, by = 'Subject')
rm(add, adf)
# graph
a1d_alpha <- ggscatter(a1d, x = 'trueAlpha', y = 'estAlpha', shape = 1, color = 'blue', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
          xlab = 'True Alpha value', ylab = 'Extimated Alpha value')

a1d_beta <- ggscatter(a1d, x = 'trueBeta', y = 'estBeta', shape = 1, color = 'red3', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(1, 3), cor.coef.size = 5,
          xlab = 'True Beta value', ylab = 'Extimated Beta value')

a1d_decay <- ggscatter(a1d, x = 'trueDecay', y = 'estDecay', shape = 1, color = 'darkgreen', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(0, .5), cor.coef.size = 5,
          xlab = 'True Decay value', ylab = 'Extimated Decay value')
rm(a1d)

# 2a model - gain loss - done####
# read in data
gld <- read.csv(here::here('output', 'simulation', 'sim_2alpha_model_data.csv'))
gld <- gld[which(gld$Trial == 1), ] # 2a model
glf <- read.csv(here::here('output', 'simulation', 'a2_fit2_a2.csv'))
# clean up and merge
gld <- rename(gld, trueBeta = Beta, trueAlphaGain = Alpha_gain, trueAlphaLoss = Alpha_loss)
glf <- rename(glf, estBeta = beta, estAlphaGain = a_gain, estAlphaLoss = a_loss)
gldl <- merge(gld[c(5:8)], glf, by = 'Subject')
rm(gld, glf)

# graph
a2_alpha_gain <- ggscatter(gldl, x = 'trueAlphaGain', y = 'estAlphaGain', shape = 1, color = 'blue', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
          xlab = 'True Alpha Gain value', ylab = 'Extimated Alpha Gain value')

a2_alpha_loss <- ggscatter(gldl, x = 'trueAlphaLoss', y = 'estAlphaLoss', shape = 1, color = 'blue4', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
          xlab = 'True Alpha Loss value', ylab = 'Extimated Alpha Loss value')

a2_beta <- ggscatter(gldl, x = 'trueBeta', y = 'estBeta', shape = 1, color = 'red3', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(1, 3), cor.coef.size = 5,
          xlab = 'True Beta value', ylab = 'Extimated Beta value')
rm(gldl)

# 2a decay model ####
# read in data
gldd <- read.csv(here::here('output', 'simulation', 'sim_2alpha_decay_model_data.csv'))
gldd <- gldd[which(gldd$Trial == 1),] # 2a with decay
gldf <- read.csv(here::here('output', 'simulation', 'a2d_fit2_a2d.csv'))
# clean up and merge
gldd <- rename(gldd, trueBeta = Beta, trueAlphaGain = Alpha_gain, trueAlphaLoss = Alpha_loss, trueDecay = Decay)
gldf <- rename(gldf, estBeta = beta, estAlphaGain = a_gain, estAlphaLoss = a_loss, estDecay = decay)
gldl <- merge(gldd[c(5:9)], gldf, by = 'Subject')
rm(gldd, gldf)
# graph
a2d_alpha_gain <- ggscatter(gldl, x = 'trueAlphaGain', y = 'estAlphaGain', shape = 1, color = 'blue', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
          xlab = 'True Alpha Gain value', ylab = 'Extimated Alpha Gain value')

a2d_alpha_loss <- ggscatter(gldl, x = 'trueAlphaLoss', y = 'estAlphaLoss', shape = 1, color = 'blue4', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
          xlab = 'True Alpha Loss value', ylab = 'Extimated Alpha Loss value')

a2d_beta <- ggscatter(gldl, x = 'trueBeta', y = 'estBeta', shape = 1, color = 'red3', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(1, 3), cor.coef.size = 5,
          xlab = 'True Beta value', ylab = 'Extimated Beta value')

a2d_decay <- ggscatter(gldl, x = 'trueDecay', y = 'estDecay', shape = 1, color = 'darkgreen', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(0, .5), cor.coef.size = 5,
          xlab = 'True Decay value', ylab = 'Extimated Decay value')

rm(gldl)

# 2a prior model ####
# read in data
glpd <- read.csv(here::here('output', 'simulation', 'sim_2alpha_with_priors_model_data.csv'))
glpd <- glpd[which(glpd$Trial == 1),] # 2a with priors
glpf <- read.csv(here::here('output', 'simulation', 'a2p_fit2_a2p.csv'))
# clean up and merge
glpd <- rename(glpd, trueBeta = Beta, trueAlphaGain = Alpha_gain, trueAlphaLoss = Alpha_loss, trueIProbA = iProbA, 
               trueIProbB = iProbB, trueIProbC = iProbC)
glpf <- rename(glpf, estBeta = beta, estAlphaGain = a_gain, estAlphaLoss = a_loss, estIProbA = iProbA, estIProbB = iProbB, 
               estIProbC = iProbC)
glpl <- merge(glpd[c(6, 1:3, 9:11)], glpf, by = 'Subject')
rm(glpd, glpf)
# graph
a2p_alpha_gain <- ggscatter(glpl, x = 'trueAlphaGain', y = 'estAlphaGain', shape = 1, color = 'blue', 
                           add = 'reg.line', conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
                           xlab = 'True Alpha Gain value', ylab = 'Extimated Alpha Gain value')

a2p_alpha_loss <- ggscatter(glpl, x = 'trueAlphaLoss', y = 'estAlphaLoss', shape = 1, color = 'blue4', 
                           add = 'reg.line', conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
                           xlab = 'True Alpha Loss value', ylab = 'Extimated Alpha Loss value')

a2p_beta <- ggscatter(glpl, x = 'trueBeta', y = 'estBeta', shape = 1, color = 'red3', 
                     add = 'reg.line', conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(1, 3), cor.coef.size = 5,
                     xlab = 'True Beta value', ylab = 'Extimated Beta value')

a2p_iprobA <- ggscatter(glpl, x = 'trueIProbA', y = 'estIProbA', shape = 1, color = 'orange', 
                        add = 'reg.line', conf.int = TRUE, 
                        cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
                        xlab = 'True initial Prob A value', ylab = 'Extimated initial Prob A value')

a2p_iprobB <- ggscatter(glpl, x = 'trueIProbB', y = 'estIProbB', shape = 1, color = 'orange', 
                        add = 'reg.line', conf.int = TRUE, 
                        cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
                        xlab = 'True initial Prob B value', ylab = 'Extimated initial Prob B value')
a2p_iprobC <- ggscatter(glpl, x = 'trueIProbC', y = 'estIProbC', shape = 1, color = 'orange', 
                        add = 'reg.line', conf.int = TRUE, 
                        cor.coef = TRUE, cor.method = 'pearson', cor.coef.coord = c(.5, .25), cor.coef.size = 5,
                        xlab = 'True initial Prob C value', ylab = 'Extimated initial Prob C value')

rm(glpl)




