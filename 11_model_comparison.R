# model comparison
# 2.24.20 updated 3.15.21 KLS

# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load source functions
source(here::here('scr', 'concat_clean.R'))
source(here::here('scr', 'AIC_functions.R'))
source(here::here('scr', 'BIC_functions.R'))

# set hard-coded variables

# read data in 
d1 <- read.csv(here::here('output', 'two_alpha_model_params.csv' ))
d2 <- read.csv(here::here('output', 'single_alpha_model_params.csv' ))
d3 <- read.csv(here::here('output', 'baseline_model_params.csv'))
o4 <- read.csv(here::here('output', 'two_alpha_with_decay_model_params_older.csv'))
y4 <- read.csv(here::here('output', 'two_alpha_with_decay_model_params_younger.csv'))
o5 <- read.csv(here::here('output', 'prior_model_params_older.csv'))
y5 <- read.csv(here::here('output', 'prior_model_params_younger.csv'))

d4 <- rbind(o4, y4); rm(o4, y4)
d5 <- rbind(o5, y5); rm(o5, y5)

d1 <- clean_param(d1)

#merge data frames with each other - update once all subjects run with initial model
d6 <- merge(d1[c(1:2,6)], d2[c(1,4)], by = 'id')
colnames(d6) <- c('id', 'agegrp', 'llh_double', 'llh_single')

d7 <- merge(d6, d3[c(1,3)], by = 'id')
colnames(d7) <- c('id', 'agegrp', 'llh_double', 'llh_single', 'llh_baseline')

d8 <- merge(d7, d4[c(1,6)], by = 'id')
colnames(d8) <- c('id', 'agegrp', 'llh_double', 'llh_single', 'llh_baseline', 'llh_decay')

dt <- merge(d8, d5[c(1,8)], by = 'id')
colnames(dt) <- c('id', 'agegrp', 'llh_double', 'llh_single', 'llh_baseline', 'llh_decay', 'llh_prior')

rm(d1,d2,d3,d4,d5,d6,d7,d8)

# remove excluded participants
cut <- read.csv(here::here('output', 'socialAL_cut.csv'), header = F)$V1
cut <- c(as.character(cut), 'sub-2040') #sub-2040 does not exist, sub-2039 was accidentally copied 2x
for (c in cut) {
  dt <- dt[which(dt$id != c), ]
}

# calculate AIC for each model
dt$AIC_double <- calc_AIC(45, 3, dt$llh_double)
dt$AIC_single <- calc_AIC(45, 2, dt$llh_single)
dt$AIC_baseline <- calc_AIC(45, 1, dt$llh_baseline)
dt$AIC_decay <- calc_AIC(45, 4, dt$llh_decay)
dt$AIC_prior <- calc_AIC(45, 6, dt$llh_prior)

# calculate BIC for each model
dt$BIC_double <- calc_BIC(45, 3, dt$llh_double)
dt$BIC_single <- calc_BIC(45, 2, dt$llh_single)
dt$BIC_baseline <- calc_BIC(45, 1, dt$llh_baseline)
dt$BIC_decay <- calc_BIC(45, 4, dt$llh_decay)
dt$BIC_prior <- calc_BIC(45, 6, dt$llh_prior)

# calculate Weight AIC for each model and choose the winning weight
y <- winningWeight(dt$id, dt[grep('AIC_double', colnames(dt)):grep('AIC_prior', colnames(dt))])
d9 <- merge(dt, y, by = 'id')
d9$win <- gsub('AIC_', '', d9$win)
write.csv(d9, here::here('output', 'model_comparison.csv'), row.names = FALSE)

# proportions
table(d9$agegrp, d9$win)
d10 <- as.data.frame(table(d9$agegrp, d9$win))
colnames(d10) <- c("Age", "Model", "Frequency")
d10$Model <- factor(d10$Model, levels = c('baseline', 'single', 'double', 'decay', 'prior'))

# graph
# graph constants
lg = 26 # text size
md = 20
sm = 14
custom_plot = list(theme(
  plot.title = element_text(size = lg, hjust = 0.5),
  axis.title.x = element_text(size = md), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = md), axis.text.y = element_text(size = sm), 
  legend.title = element_text(size = md), legend.text = element_text(size = sm), 
  legend.position='top', strip.text.x = element_text(size=md))
)

ggplot(d10, aes(Model, Frequency, fill = Age)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_brewer(palette="Set1") + theme_minimal() + ggtitle('Best-Fitting Model') +
  custom_plot

ggsave(here('figs', 'best-fit_model.png'))
