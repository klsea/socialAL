# best fit by BIC confusion matrix
# 3.16.22 KLS

# init ####
# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load source functions
source(here::here('scr', 'concat_clean.R'))
source(here::here('scr', 'BIC_functions.R'))

# set hard-coded variables

# baseline data ####
# read in fits to baseline sim data
b <- read.csv(here::here('output', 'simulation', 'b_fit2_b.csv'))
a1 <- read.csv(here::here('output', 'simulation', 'a1_fit2_b.csv'))
a1d <- read.csv(here::here('output', 'simulation', 'a1d_fit2_b.csv'))
a2 <- read.csv(here::here('output', 'simulation', 'a2_fit2_b.csv'))
a2d <- read.csv(here::here('output', 'simulation', 'a2d_fit2_b.csv'))
a2p <- read.csv(here::here('output', 'simulation', 'a2p_fit2_b.csv'))

# calculate BIC
b$BIC <- calc_BIC(45,1,b$llh)
a1$BIC <- calc_BIC(45,2,a1$llh)
a1d$BIC <- calc_BIC(45,3,a1d$llh)
a2$BIC <- calc_BIC(45,3,a2$llh)
a2d$BIC <- calc_BIC(45,4,a2d$llh)
a2p$BIC <- calc_BIC(45,6,a2p$llh)

# merge llhs into one data frame
d <- merge(b[c(1,3:4)], a1[c(1,4:5)], by = 'Subject', suffixes = c('_b', '_a1'))
d1 <- merge(a1d[c(1,5:6)], a2[c(1,5:6)], by = 'Subject', suffixes = c('_a1d', '_a2'))
d2 <- merge(a2d[c(1,6:7)], a2p[c(1,8,12)], by = 'Subject', suffixes = c('_a2d', '_a2p'))
d3 <- merge(d,d1)
d4 <- merge(d3,d2)
rm(b,a1,a1d,a2,a2d,a2p,d,d1,d2,d3)

# find minBIC
d5 <- d4[c(1, grep('BIC', colnames(d4)))]
baseline <- winningBIC(d5)
baseline <- baseline %>% count(winModel) %>% mutate(freq = round(n/sum(n), 2))
rm(d4, d5)

# single alpha data ####
# read in fits to single alpha sim data 
b <- read.csv(here::here('output', 'simulation', 'b_fit2_a1.csv'))
a1 <- read.csv(here::here('output', 'simulation', 'a1_fit2_a1.csv'))
a1d <- read.csv(here::here('output', 'simulation', 'a1d_fit2_a1.csv'))
a2 <- read.csv(here::here('output', 'simulation', 'a2_fit2_a1.csv'))
a2d <- read.csv(here::here('output', 'simulation', 'a2d_fit2_a1.csv'))
a2p <- read.csv(here::here('output', 'simulation', 'a2p_fit2_a1.csv'))

# calculate BIC
b$BIC <- calc_BIC(45,1,b$llh)
a1$BIC <- calc_BIC(45,2,a1$llh)
a1d$BIC <- calc_BIC(45,3,a1d$llh)
a2$BIC <- calc_BIC(45,3,a2$llh)
a2d$BIC <- calc_BIC(45,4,a2d$llh)
a2p$BIC <- calc_BIC(45,6,a2p$llh)

# merge llhs into one data frame
d <- merge(b[c(1,3:4)], a1[c(1,4:5)], by = 'Subject', suffixes = c('_b', '_a1'))
d1 <- merge(a1d[c(1,5:6)], a2[c(1,5:6)], by = 'Subject', suffixes = c('_a1d', '_a2'))
d2 <- merge(a2d[c(1,6:7)], a2p[c(1,8,12)], by = 'Subject', suffixes = c('_a2d', '_a2p'))
d3 <- merge(d,d1)
d4 <- merge(d3,d2)
rm(b,a1,a1d,a2,a2d,a2p,d,d1,d2,d3)

# find minBIC
d5 <- d4[c(1, grep('BIC', colnames(d4)))]
alpha1 <- winningBIC(d5)
alpha1 <- alpha1 %>% count(winModel) %>% mutate(freq = round(n/sum(n), 2))
rm(d4, d5)

# single alpha with decay data ####
# read in fits to single alpha with decay sim data 
b <- read.csv(here::here('output', 'simulation', 'b_fit2_a1d.csv'))
a1 <- read.csv(here::here('output', 'simulation', 'a1_fit2_a1d.csv'))
a1d <- read.csv(here::here('output', 'simulation', 'a1d_fit2_a1d.csv'))
a2 <- read.csv(here::here('output', 'simulation', 'a2_fit2_a1d.csv'))
a2d <- read.csv(here::here('output', 'simulation', 'a2d_fit2_a1d.csv'))
a2p <- read.csv(here::here('output', 'simulation', 'a2p_fit2_a1d.csv'))

# calculate BIC
b$BIC <- calc_BIC(45,1,b$llh)
a1$BIC <- calc_BIC(45,2,a1$llh)
a1d$BIC <- calc_BIC(45,3,a1d$llh)
a2$BIC <- calc_BIC(45,3,a2$llh)
a2d$BIC <- calc_BIC(45,4,a2d$llh)
a2p$BIC <- calc_BIC(45,6,a2p$llh)

# merge llhs into one data frame
d <- merge(b[c(1,3:4)], a1[c(1,4:5)], by = 'Subject', suffixes = c('_b', '_a1'))
d1 <- merge(a1d[c(1,5:6)], a2[c(1,5:6)], by = 'Subject', suffixes = c('_a1d', '_a2'))
d2 <- merge(a2d[c(1,6:7)], a2p[c(1,8,12)], by = 'Subject', suffixes = c('_a2d', '_a2p'))
d3 <- merge(d,d1)
d4 <- merge(d3,d2)
rm(b,a1,a1d,a2,a2d,a2p,d,d1,d2,d3)

# find minBIC
d5 <- d4[c(1, grep('BIC', colnames(d4)))]
alpha1decay <- winningBIC(d5)
alpha1decay <- alpha1decay %>% count(winModel) %>% mutate(freq = round(n/sum(n),2))
rm(d4, d5)

# double alpha data ####
# read in fits to double alpha sim data 
b <- read.csv(here::here('output', 'simulation', 'b_fit2_a2.csv'))
a1 <- read.csv(here::here('output', 'simulation', 'a1_fit2_a2.csv'))
a1d <- read.csv(here::here('output', 'simulation', 'a1d_fit2_a2.csv'))
a2 <- read.csv(here::here('output', 'simulation', 'a2_fit2_a2.csv'))
a2d <- read.csv(here::here('output', 'simulation', 'a2d_fit2_a2.csv'))
a2p <- read.csv(here::here('output', 'simulation', 'a2p_fit2_a2.csv'))

# calculate BIC
b$BIC <- calc_BIC(45,1,b$llh)
a1$BIC <- calc_BIC(45,2,a1$llh)
a1d$BIC <- calc_BIC(45,3,a1d$llh)
a2$BIC <- calc_BIC(45,3,a2$llh)
a2d$BIC <- calc_BIC(45,4,a2d$llh)
a2p$BIC <- calc_BIC(45,6,a2p$llh)

# merge llhs into one data frame
d <- merge(b[c(1,3:4)], a1[c(1,4:5)], by = 'Subject', suffixes = c('_b', '_a1'))
d1 <- merge(a1d[c(1,5:6)], a2[c(1,5:6)], by = 'Subject', suffixes = c('_a1d', '_a2'))
d2 <- merge(a2d[c(1,6:7)], a2p[c(1,8,12)], by = 'Subject', suffixes = c('_a2d', '_a2p'))
d3 <- merge(d,d1)
d4 <- merge(d3,d2)
rm(b,a1,a1d,a2,a2d,a2p,d,d1,d2,d3)

# find minBIC
d5 <- d4[c(1, grep('BIC', colnames(d4)))]
alpha2 <- winningBIC(d5)
alpha2 <- alpha2 %>% count(winModel) %>% mutate(freq = round(n/sum(n),2))
rm(d4, d5)

# double alpha with decay data ####
# read in fits to double alpha with decay sim data 
b <- read.csv(here::here('output', 'simulation', 'b_fit2_a2d.csv'))
a1 <- read.csv(here::here('output', 'simulation', 'a1_fit2_a2d.csv'))
a1d <- read.csv(here::here('output', 'simulation', 'a1d_fit2_a2d.csv'))
a2 <- read.csv(here::here('output', 'simulation', 'a2_fit2_a2d.csv'))
a2d <- read.csv(here::here('output', 'simulation', 'a2d_fit2_a2d.csv'))
a2p <- read.csv(here::here('output', 'simulation', 'a2p_fit2_a2d.csv'))

# calculate BIC
b$BIC <- calc_BIC(45,1,b$llh)
a1$BIC <- calc_BIC(45,2,a1$llh)
a1d$BIC <- calc_BIC(45,3,a1d$llh)
a2$BIC <- calc_BIC(45,3,a2$llh)
a2d$BIC <- calc_BIC(45,4,a2d$llh)
a2p$BIC <- calc_BIC(45,6,a2p$llh)

# merge llhs into one data frame
d <- merge(b[c(1,3:4)], a1[c(1,4:5)], by = 'Subject', suffixes = c('_b', '_a1'))
d1 <- merge(a1d[c(1,5:6)], a2[c(1,5:6)], by = 'Subject', suffixes = c('_a1d', '_a2'))
d2 <- merge(a2d[c(1,6:7)], a2p[c(1,8,12)], by = 'Subject', suffixes = c('_a2d', '_a2p'))
d3 <- merge(d,d1)
d4 <- merge(d3,d2)
rm(b,a1,a1d,a2,a2d,a2p,d,d1,d2,d3)

# find minBIC
d5 <- d4[c(1, grep('BIC', colnames(d4)))]
alpha2decay <- winningBIC(d5)
alpha2decay <- alpha2decay %>% count(winModel) %>% mutate(freq = round(n/sum(n),2))
rm(d4, d5)

# double alpha with priors data ####
# read in fits to double alpha with priors sim data
b <- read.csv(here::here('output', 'simulation', 'b_fit2_a2p.csv'))
a1 <- read.csv(here::here('output', 'simulation', 'a1_fit2_a2p.csv'))
a1d <- read.csv(here::here('output', 'simulation', 'a1d_fit2_a2p.csv'))
a2 <- read.csv(here::here('output', 'simulation', 'a2_fit2_a2p.csv'))
a2d <- read.csv(here::here('output', 'simulation', 'a2d_fit2_a2p.csv'))
a2p <- read.csv(here::here('output', 'simulation', 'a2p_fit2_a2p.csv'))

# calculate BIC
b$BIC <- calc_BIC(45,1,b$llh)
a1$BIC <- calc_BIC(45,2,a1$llh)
a1d$BIC <- calc_BIC(45,3,a1d$llh)
a2$BIC <- calc_BIC(45,3,a2$llh)
a2d$BIC <- calc_BIC(45,4,a2d$llh)
a2p$BIC <- calc_BIC(45,6,a2p$llh)

# merge llhs into one data frame
d <- merge(b[c(1,3:4)], a1[c(1,4:5)], by = 'Subject', suffixes = c('_b', '_a1'))
d1 <- merge(a1d[c(1,5:6)], a2[c(1,5:6)], by = 'Subject', suffixes = c('_a1d', '_a2'))
d2 <- merge(a2d[c(1,6:7)], a2p[c(1,8,12)], by = 'Subject', suffixes = c('_a2d', '_a2p'))
d3 <- merge(d,d1)
d4 <- merge(d3,d2)
rm(b,a1,a1d,a2,a2d,a2p,d,d1,d2,d3)

# find minBIC
d5 <- d4[c(1, grep('BIC', colnames(d4)))]
alpha2prior <- winningBIC(d5)
alpha2prior <- alpha2prior %>% count(winModel) %>% mutate(freq = round(n/sum(n),2))
rm(d4, d5)

# create and populate confusion matrix ####
# rows = fit model
# columns = simulated model
cm1 <- merge(baseline[c(1,3)], alpha1[c(1,3)], by = 'winModel', suffixes = c('_b', '_a1'), all = TRUE)
cm2 <- merge(alpha1decay[c(1,3)], alpha2[c(1,3)], by = 'winModel', suffixes = c('_a1d', '_a2'), all = TRUE)
cm3 <- merge(alpha2decay[c(1,3)], alpha2prior[c(1,3)], by = 'winModel', suffixes = c('_a2d', '_a2p'), all = TRUE)
cm4 <- merge(cm1, cm2, all = TRUE)
cm5 <- merge(cm4, cm3, all = TRUE)
rm(baseline, alpha1, alpha1decay, alpha2, alpha2decay, alpha2prior, cm1, cm2, cm3, cm4)

# reorder rows
cm5 <- cm5[c(6,1:5),]

# change row and column names
cm5 <- rename(cm5, '1' = 'freq_b', '2' = 'freq_a1', '3' = 'freq_a1d', '4' = 'freq_a2', '5' = 'freq_a2d', '6' = 'freq_a2p')
cm5$winModel <- recode(cm5$winModel, b = '1', a1 = '2', a1d = '3', a2 = '4', a2d = '5', a2p = '6')

# transpose to match Wilson & Collins, 2019
# rows = simulated model
# columns = fit model
confusionMatrix = setNames(data.frame(t(cm5[,-1])), cm5[,1])
confusionMatrix[is.na(confusionMatrix)] <- 0

# make pretty
cm <- tibble::rownames_to_column(confusionMatrix, "sm")
cm <- pivot_longer(cm, 2:7, names_to = 'fm')

prettycm <- ggplot(cm, aes(fm, sm, fill = value)) + geom_tile() + coord_fixed() + 
  scale_y_discrete(lim=rev) + scale_x_discrete(position = "top") + 
  geom_text(aes(label = value), color = 'white', size = 4) + 
  xlab('fit model') + ylab('simulated model') + 
  theme(legend.position = 'none', axis.text = element_text(size = 12), axis.title=element_text(size = 15)) #+
  #scale_fill_gradient2(low = 'dark blue',  mid = 'green', high = 'yellow')
prettycm
# create and populate inversion matrix ####
