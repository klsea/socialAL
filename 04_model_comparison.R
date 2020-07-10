# model comparison
# 2.24.20

# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load source functions
source(here::here('scr', 'concat_clean.R'))
source(here::here('scr', 'AIC_functions.R'))

# set hard-coded variables

# read data in 
d1 <- read.csv(here::here('output', 'two_alpha_model_params.csv' ))
d2 <- read.csv(here::here('output', 'single_alpha_model_params.csv' ))
d3 <- read.csv(here::here('output', 'baseline_model_params.csv'))

d1 <- clean_param(d1)

#merge data frames with each other - update once all subjects run with initial model
d4 <- merge(d1[c(1:2,6)], d2[c(1,4)], by = 'id')
colnames(d4) <- c('id', 'agegrp', 'llh_double', 'llh_single')

dt <- merge(d4, d3[c(1,3)], by = 'id')
colnames(dt) <- c('id', 'agegrp', 'llh_double', 'llh_single', 'llh_baseline')
rm(d1,d2,d3,d4)

# caluclate AIC for each model
dt$AIC_double <- calc_AIC(45, 3, dt$llh_double)
dt$AIC_single <- calc_AIC(45, 2, dt$llh_single)
dt$AIC_baseline <- calc_AIC(45, 1, dt$llh_baseline)

# calculate Weight AIC for each model and choose the winning weight
y <- winningWeight(dt$id, dt[grep('AIC_double', colnames(dt)):grep('AIC_baseline', colnames(dt))])
d5 <- merge(dt, y, by = 'id')
d5$win <- gsub('AIC_', '', d5$win)
write.csv(d5, here::here('output', 'model_comparison.csv'), row.names = FALSE)

# proportions
table(d5$agegrp, d5$win)
d6 <- as.data.frame(table(d5$agegrp, d5$win))
colnames(d6) <- c("Age", "Model", "Frequency")
d6$Model <- factor(d6$Model, levels = c('baseline', 'single', 'double'))

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

ggplot(d6, aes(Model, Frequency, fill = Age)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_brewer(palette="Set1") + theme_minimal() + ggtitle('Best-Fitting Model') +
  custom_plot

ggsave(here('figs', 'best-fit_model.png'))
