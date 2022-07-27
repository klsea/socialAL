# Graphs of neuroimaging cluster means
# 12.16.21 updated 7.27.22

# load required packages ####
library(here)
library(tidyverse)

# load source functions

# set hard-coded variables

# load data
#brain <- read.csv(here::here('data', 'SocialAL_cluster_activity.csv'))
brain <- read.csv(here::here('data', 'socialAL_OA_learner-nonlearner_ROIs.csv'))

# rename age groups
brain$Group[which(brain$Group == 'learn')] <- 'OA Learner'
brain$Group[which(brain$Group == 'nonlearn')] <- 'OA Non-learner'

# graph constants ####
lg = 18 # text size
sm = 14
custom_plot = list(theme(
  plot.title = element_text(size = 24),
  axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
  legend.title = element_text(size = lg), legend.text = element_text(size = sm))
)

# create summary tables by age group ####
d1 <- brain %>% pivot_longer(cols = R_Angular_Gyrus:R_para_hipp, names_to = 'region', values_to = 'activation')

d2 <- d1 %>% 
  group_by(Group, region) %>%
  dplyr::summarise(mean = mean(activation), sd = sd(activation), 
                   se= sd(activation)/sqrt(n()))

# decision - rAG ####
rAGmeans <- d2[which(d2$region == 'R_Angular_Gyrus'),]
rAG <- ggplot() +
  geom_point(data = brain, aes(x = Group, y = R_Angular_Gyrus, colour = Group), 
             position = position_jitterdodge(jitter.height = .25), alpha = .5) +
  geom_bar(data = rAGmeans, aes(x = Group, y = mean, colour = Group, fill = Group), 
           stat='identity', alpha = 0.3, position=position_dodge()) +
  geom_errorbar(data = rAGmeans, aes(x = Group, y = mean, ymin=mean - se, 
                                     ymax = mean + se, colour = Group), 
                width = .2, position=position_dodge(.9)) + 
  xlab('Learner Group') + ylab('Reputation Signal') +
  scale_fill_brewer(palette="Set2", name=" Learner Group") + theme_minimal() + 
  scale_colour_brewer(palette="Set2", name="Learner Group") + custom_plot
saveRDS(rAG, here::here('figs', 'rAG.RDS'))

# decision - rphipp ####
rPHCmeans <- d2[which(d2$region == 'R_para_hipp'),]
rPHC <- ggplot() +
  geom_point(data = brain, aes(x = Group, y = R_para_hipp, colour = Group), 
             position = position_jitterdodge(jitter.height = .25), alpha = .5) +
  geom_bar(data = rPHCmeans, aes(x = Group, y = mean, colour = Group, fill = Group), 
           stat='identity', alpha = 0.3, position=position_dodge()) +
  geom_errorbar(data = rPHCmeans, aes(x = Group, y = mean, ymin=mean - se, 
                                        ymax = mean + se, colour = Group), 
                width = .2, position=position_dodge(.9)) + 
  xlab('Group') + ylab('Reputation Signal') +
  scale_fill_brewer(palette="Set2", name="Group") + theme_minimal() + 
  scale_colour_brewer(palette="Set2", name="Group") + custom_plot
saveRDS(rPHC, here::here('figs', 'rPHC.RDS'))



