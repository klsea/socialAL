# Visualization of model parameters
# 9.23.19 updated 2.17.20

# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load source functions
source(here::here('scr', 'concat_clean.R'))
source(here::here('scr', 'clean_single_alpha.R'))

# set hard-coded variables

# read data in 
dt <- read.csv(here::here('output', 'two_alpha_model_params.csv' ))
d2 <- read.csv(here::here('output', 'single_alpha_model_params.csv' ))
d3 <- read.csv(here::here('output', 'baseline_model_params.csv'))
o4 <- read.csv(here::here('output', 'two_alpha_with_decay_model_params_older.csv'))
y4 <- read.csv(here::here('output', 'two_alpha_with_decay_model_params_younger.csv'))
o5 <- read.csv(here::here('output', 'prior_model_params_older.csv'))
y5 <- read.csv(here::here('output', 'prior_model_params_younger.csv'))
d4 <- rbind(o4, y4); rm(o4, y4)
d5 <- rbind(o5, y5); rm(o5, y5)
d6 <- read.csv(here::here('output', 'model_comparison.csv'))[c(1,18)]

# clear model names

# graph constants ####
lg = 18 # text size
md = 16
sm = 14
custom_plot = list(theme(
  plot.title = element_text(size = lg, hjust = 0.5),
  axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
  legend.title = element_text(size = lg), legend.text = element_text(size = sm), 
  legend.position='top', strip.text.x = element_text(size=md))
)

# double alpha with decay model ####
d7 <- clean_single_alpha(d4)
d7 <- merge(d7, d6, by='id')
d7 <- gather(d7, parameter, estimate, alpha_gain:decay)

#d7 <- d7[which(d7$win == 'decay'),] 
# uncommenting the line above allows graphing of 
# only participants best-fit by the decay + double alpha model

decaygrpmeans <- d7 %>% 
  dplyr::group_by(agegrp, parameter) %>%
  summarise(mean = mean(estimate), sd = sd(estimate), 
            se= sd(estimate)/sqrt(n()))
alphas <- decaygrpmeans[which(decaygrpmeans$parameter != 'beta'),]
beta <- decaygrpmeans[which(decaygrpmeans$parameter == 'beta'),]
decay <- decaygrpmeans[which(decaygrpmeans$parameter == 'decay'),]

# Graph 1 - bar graph group means
ggplot(alphas, aes(parameter, mean, fill = agegrp)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin=mean - se, ymax = mean + se), 
                width = .2, position=position_dodge(.9)) + 
  scale_fill_brewer(palette="Set1", name="Age Group") + 
  scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() +custom_plot
ggsave(here('figs', 'decay_alpha_age_grp_means.png'))

ggplot(beta, aes(parameter, mean, fill = agegrp)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin=mean - se, ymax = mean + se), 
                width = .2, position=position_dodge(.9)) + 
  scale_fill_brewer(palette="Set1", name="Age Group") + 
  scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() +custom_plot
ggsave(here('figs', 'decay_beta_age_grp_means.png'), width = 5.5)

#pretty plot
decay <- ggplot() + 
  geom_point(data = d7[which(d7$parameter == 'decay'),], aes(x = agegrp, y = estimate, colour = agegrp), 
             position = position_jitterdodge(jitter.height = .25), alpha = .5) + 
  geom_bar(data = decay, aes(x = agegrp, y = mean, fill= agegrp, colour = agegrp), 
           stat='identity', alpha = 0.3, position=position_dodge()) + 
  geom_errorbar(data = decay, aes(x = agegrp, y = mean, fill= agegrp, ymin=mean - se, ymax = mean + se, colour = agegrp), 
                width = .2, position=position_dodge(.9)) + 
  scale_fill_brewer(palette="Set1", name="Age Group") + 
  scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() + custom_plot + 
  xlab('Age Group') + ylab('Decay estimate') + coord_cartesian(ylim=c(-0.2,1))
saveRDS(decay, here::here('figs', 'decay.RDS'))

# # double alpha model ####
# 
# # create age group labels
# dt <- clean_param(dt)
# dt <- merge(dt, d6, by='id')
# dt <- gather(dt, parameter, estimate, alpha_gain:beta)
# #dt <- dt[which(dt$win == 'double'),] 
# # uncommenting the line above allows graphing of 
# # only participants best-fit by the double alpha model
# 
# se <- function(sd,n) {sd/sqrt(n())}
# 
# # remove excluded participants
# cut <- read.csv(here::here('output', 'socialAL_cut.csv'), header = F)$V1
# cut <- c(as.character(cut), 'sub-2040') #sub-2040 does not exist, sub-2039 was accidently copied 2x
# for (c in cut) {
#   dt <- dt[which(dt$id != c), ]
# }
# 
# grpmeans <- dt %>% 
#   dplyr::group_by(agegrp, parameter) %>%
#   summarise(mean = mean(estimate), sd = sd(estimate), 
#             se= sd(estimate)/sqrt(n()))
# alphas <- grpmeans[which(grpmeans$parameter != 'beta'),]
# beta <- grpmeans[which(grpmeans$parameter == 'beta'),]
# 
# # Graph - bar graph group means
# ggplot(alphas, aes(parameter, mean, fill = agegrp)) + 
#   geom_bar(stat='identity', position=position_dodge()) + 
#   geom_errorbar(aes(ymin=mean - se, ymax = mean + se), 
#                 width = .2, position=position_dodge(.9)) + 
#   scale_fill_brewer(palette="Set1", name="Age Group") + 
#   scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() +custom_plot
# ggsave(here('figs', '2alpha_alpha_age_grp_means.png'))
# 
# ggplot(beta, aes(parameter, mean, fill = agegrp)) + 
#   geom_bar(stat='identity', position=position_dodge()) + 
#   geom_errorbar(aes(ymin=mean - se, ymax = mean + se), 
#                 width = .2, position=position_dodge(.9)) + 
#   scale_fill_brewer(palette="Set1", name="Age Group") + 
#   scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() +custom_plot
# ggsave(here('figs', '2alpha_beta_age_grp_means.png'), width = 5.5)
# 
# # single alpha model ####
# d1 <- clean_single_alpha(d2)
# d1 <- merge(d1, d6, by='id')
# d1 <- gather(d1, parameter, estimate, alpha:beta)
# 
# #d1 <- d1[which(d1$win == 'single'),] 
# # uncommenting the line above allows graphing of 
# # only participants best-fit by the single alpha model
# 
# singlegrpmeans <- d1 %>% 
#   dplyr::group_by(agegrp, parameter) %>%
#   summarise(mean = mean(estimate), sd = sd(estimate), 
#             se= sd(estimate)/sqrt(n()))
# 
# # double alpha with prior model ####
# d8 <- clean_single_alpha(d5)
# d8 <- merge(d8, d6, by='id')
# d8 <- gather(d8, parameter, estimate, alpha_gain:iProb_untrust)
# 
# #d8 <- d8[which(d8$win == 'decay'),] 
# # uncommenting the line above allows graphing of 
# # only participants best-fit by the decay + double alpha model
# 
# priorgrpmeans <- d8 %>% 
#   dplyr::group_by(agegrp, parameter) %>%
#   summarise(mean = mean(estimate), sd = sd(estimate), 
#             se= sd(estimate)/sqrt(n()))
# alphas <- priorgrpmeans[which(priorgrpmeans$parameter != 'beta'),]
# beta <- priorgrpmeans[which(decaygrpmeans$parameter == 'beta'),]
# 
# ggplot(alphas, aes(parameter, mean, fill = agegrp)) + 
#   geom_bar(stat='identity', position=position_dodge()) + 
#   geom_errorbar(aes(ymin=mean - se, ymax = mean + se), 
#                 width = .2, position=position_dodge(.9)) + 
#   scale_fill_brewer(palette="Set1", name="Age Group") + 
#   scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() +custom_plot
# ggsave(here('figs', 'prior_alpha_age_grp_means.png'))
# 
# ggplot(beta, aes(parameter, mean, fill = agegrp)) + 
#   geom_bar(stat='identity', position=position_dodge()) + 
#   geom_errorbar(aes(ymin=mean - se, ymax = mean + se), 
#                 width = .2, position=position_dodge(.9)) + 
#   scale_fill_brewer(palette="Set1", name="Age Group") + 
#   scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() +custom_plot
# ggsave(here('figs', 'prior_beta_age_grp_means.png'), width = 5.5)

