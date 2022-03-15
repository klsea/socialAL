# visualize simulated data
# 3.15.22 KLS

# init ####
# load required packages
library(here)
library(tidyverse)
library(ggpubr)

# load source functions
add_tt_number <- function(data) {
  # creates a new column for sequence of trials within a condition
  subs <- unique(data$Subject)
  d1 <- data.frame()
  for (s in subs) {
    d2 <- data[which(data$Subject == s),]
    d2 <- d2[order(d2$Trial),]
    d2$tt_number <- NA
    d2[which(d2$Partner == 0),]$tt_number <- seq(1,15)
    d2[which(d2$Partner == 1),]$tt_number <- seq(1,15)
    d2[which(d2$Partner == 2),]$tt_number <- seq(1,15)
    d1 <- rbind(d1,d2)
  }
  return(d1)
}
# set hard-coded variables
#read in data ####

# plot baseline model sim ####
bld <- read.csv(here::here('output', 'simulation', 'sim_baseline_model_data.csv'))
bld$Partner <- factor(bld$Stim_Sequence)
bld <- add_tt_number(bld)
levels(bld$Partner) <- c('Trustworthy', 'Neutral', 'Untrustworthy')
bld$Choice <- (bld$Choice - 1) * 3

#graph
bldplot <- ggplot(bld, aes(tt_number, Choice, colour = Partner, fill = Partner)) + theme_bw() +  xlab('Trial') + 
  geom_smooth(method = lm) + scale_x_continuous(breaks = c(5,10,15)) + ylim(0,9) #+ scale_y_continuous(breaks = seq(0,9,3))

# plot 1 alpha model sim ####
ad <- read.csv(here::here('output', 'simulation', 'sim_1alpha_model_data.csv'))
ad$Partner <- factor(ad$Stim_Sequence)
ad <- add_tt_number(ad)
levels(ad$Partner) <- c('Trustworthy', 'Neutral', 'Untrustworthy')
ad$Choice <- (ad$Choice - 1) * 3

adplot <- ggplot(ad, aes(tt_number, Choice, colour = Partner, fill = Partner)) + theme_bw() + xlab('Trial') + 
  geom_smooth(method = lm) + scale_x_continuous(breaks = c(5,10,15)) + scale_y_continuous(breaks = seq(0,9,3)) + ylim(0,9) #

# plot 1 alpha with decay model sim ####
add <- read.csv(here::here('output', 'simulation', 'sim_1alpha_decay_model_data.csv'))
add$Partner <- factor(add$Stim_Sequence)
add <- add_tt_number(add)
levels(add$Partner) <- c('Trustworthy', 'Neutral', 'Untrustworthy')
add$Choice <- (add$Choice - 1) * 3

addplot <- ggplot(add, aes(tt_number, Choice, colour = Partner, fill = Partner)) + theme_bw() +  xlab('Trial') + 
  geom_smooth(method = lm) + scale_x_continuous(breaks = c(5,10,15)) + scale_y_continuous(breaks = seq(0,9,3)) + ylim(0,9) #

# plot 2 alpha model sim ###
gld <- read.csv(here::here('output', 'simulation', 'sim_2alpha_model_data.csv'))
gld$Partner <- factor(gld$Stim_Sequence)
gld <- add_tt_number(gld)
levels(gld$Partner) <- c('Trustworthy', 'Neutral', 'Untrustworthy')
gld$Choice <- (gld$Choice - 1) * 3

gldplot <- ggplot(gld, aes(tt_number, Choice, colour = Partner, fill = Partner)) + theme_bw() +  xlab('Trial') + 
  geom_smooth(method = lm) + scale_x_continuous(breaks = c(5,10,15)) + scale_y_continuous(breaks = seq(0,9,3)) + ylim(0,9) #

# plot 2 alpha with decay model sim ###
gldd <- read.csv(here::here('output', 'simulation', 'sim_2alpha_decay_model_data.csv'))
gldd$Partner <- factor(gldd$Stim_Sequence)
gldd <- add_tt_number(gldd)
levels(gldd$Partner) <- c('Trustworthy', 'Neutral', 'Untrustworthy')
gldd$Choice <- (gldd$Choice - 1) * 3

glddplot <- ggplot(gldd, aes(tt_number, Choice, colour = Partner, fill = Partner)) + theme_bw() +  xlab('Trial') + 
  geom_smooth(method = lm) + scale_x_continuous(breaks = c(5,10,15)) + scale_y_continuous(breaks = seq(0,9,3)) + ylim(0,9) #

# plot 2 alpha with prior model sim ###
glpd <- read.csv(here::here('output', 'simulation', 'sim_2alpha_with_priors_model_data.csv'))
glpd$Partner <- factor(glpd$Stim_Sequence)
glpd <- add_tt_number(glpd)
levels(glpd$Partner) <- c('Trustworthy', 'Neutral', 'Untrustworthy')
glpd$Choice <- (glpd$Choice - 1) * 3

glpdplot <- ggplot(glpd, aes(tt_number, Choice, colour = Partner, fill = Partner)) + theme_bw() +  xlab('Trial') + 
  geom_smooth(method = lm) + scale_x_continuous(breaks = c(5,10,15)) + scale_y_continuous(breaks = seq(0,9,3)) + ylim(0,9) #