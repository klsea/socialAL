# Graphs of neuroimaging cluster means
# 12.16.21

# load required packages ####
library(here)
library(tidyverse)

# load source functions

# set hard-coded variables

# load data
brain <- read.csv(here::here('data', 'SocialAL_cluster_activity.csv'))

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
d1 <- brain %>% pivot_longer(cols = feedback_rTPJ:decision_ifg, names_to = 'region', values_to = 'activation')

d2 <- d1 %>% 
  group_by(AgeGroup, region) %>%
  dplyr::summarise(mean = mean(activation), sd = sd(activation), 
                   se= sd(activation)/sqrt(n()))

# decision - IFG ####
ifgmeans <- d2[which(d2$region == 'decision_ifg'),]
ifg <- ggplot() +
  geom_point(data = brain, aes(x = AgeGroup, y = decision_ifg, colour = AgeGroup), 
             position = position_jitterdodge(jitter.height = .25), alpha = .5) +
  geom_bar(data = ifgmeans, aes(x = AgeGroup, y = mean, colour = AgeGroup, fill = AgeGroup), 
           stat='identity', alpha = 0.3, position=position_dodge()) +
  geom_errorbar(data = ifgmeans, aes(x = AgeGroup, y = mean, ymin=mean - se, ymax = mean + se, colour = AgeGroup), 
                width = .2, position=position_dodge(.9)) + 
  xlab('Age Group') + ylab('Reputation Signal in IFG') +
  scale_fill_brewer(palette="Set1", name="Age Group") + theme_minimal() + 
  scale_colour_brewer(palette="Set1", name="Age Group") + custom_plot
saveRDS(ifg, here::here('figs', 'ifg.RDS'))

# feedback - TPJ ####
tpjmeans <- d2[which(d2$region == 'feedback_rTPJ'),]
tpj <- ggplot() +
  geom_point(data = brain, aes(x = AgeGroup, y = feedback_rTPJ, colour = AgeGroup), 
             position = position_jitterdodge(jitter.height = .25), alpha = .5) +
  geom_bar(data = tpjmeans, aes(x = AgeGroup, y = mean, colour = AgeGroup, fill = AgeGroup), 
           stat='identity', alpha = 0.3, position=position_dodge()) +
  geom_errorbar(data = tpjmeans, aes(x = AgeGroup, y = mean, ymin=mean - se, ymax = mean + se, colour = AgeGroup), 
                width = .2, position=position_dodge(.9)) + 
  xlab('Age Group') + ylab('RPE Signal in Angular Gyrus') +
  scale_fill_brewer(palette="Set1", name="Age Group") + theme_minimal() + 
  scale_colour_brewer(palette="Set1", name="Age Group") + custom_plot
saveRDS(tpj, here::here('figs', 'tpj.RDS'))

# feedback - Visual ####
vismeans <- d2[which(d2$region == 'feedback_visual'),]
vis <- ggplot() +
  geom_point(data = brain, aes(x = AgeGroup, y = feedback_visual, colour = AgeGroup), 
             position = position_jitterdodge(jitter.height = .25), alpha = .5) +
  geom_bar(data = vismeans, aes(x = AgeGroup, y = mean, colour = AgeGroup, fill = AgeGroup), 
           stat='identity', alpha = 0.3, position=position_dodge()) +
  geom_errorbar(data = vismeans, aes(x = AgeGroup, y = mean, ymin=mean - se, ymax = mean + se, colour = AgeGroup), 
                width = .2, position=position_dodge(.9)) + 
  xlab('Age Group') + ylab('RPE Signal in Visual Cortex') +
  scale_fill_brewer(palette="Set1", name="Age Group") + theme_minimal() + 
  scale_colour_brewer(palette="Set1", name="Age Group") + custom_plot
saveRDS(vis, here::here('figs', 'vis.RDS'))