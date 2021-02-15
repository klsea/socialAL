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
d4 <- rbind(o4, y4); rm(o4, y4)
d5 <- read.csv(here::here('output', 'model_comparison.csv'))[c(1,15)]

# clear model names

# graph constants
lg = 26 # text size
md = 20
sm = 14
custom_plot = list(theme(
  plot.title = element_text(size = lg, hjust = 0.5),
  axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
  legend.title = element_text(size = lg), legend.text = element_text(size = sm), 
  legend.position='top', strip.text.x = element_text(size=md))
)


# double alpha model ####

# create age group labels
dt <- clean_param(dt)
dt <- merge(dt, d5, by='id')
dt <- gather(dt, parameter, estimate, alpha_gain:beta)
#dt <- dt[which(dt$win == 'double'),] 
# uncommenting the line above allows graphing of 
# only participants best-fit by the double alpha model

se <- function(sd,n) {sd/sqrt(n())}

# remove excluded participants
cut <- read.csv(here::here('output', 'socialAL_cut.csv'), header = F)$V1
cut <- c(as.character(cut), 'sub-2040') #sub-2040 does not exist, sub-2039 was accidently copied 2x
for (c in cut) {
  dt <- dt[which(dt$id != c), ]
}

grpmeans <- dt %>% 
  dplyr::group_by(agegrp, parameter) %>%
  summarise(mean = mean(estimate), sd = sd(estimate), 
            se= sd(estimate)/sqrt(n()))
alphas <- grpmeans[which(grpmeans$parameter != 'beta'),]
beta <- grpmeans[which(grpmeans$parameter == 'beta'),]

# Graph 1 - bar graph group means
ggplot(grpmeans, aes(parameter, mean, fill = agegrp)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin=mean - se, ymax = mean + se), 
                                            width = .2, position=position_dodge(.9)) + 
  scale_fill_brewer(palette="Set1", name="Age Group") + 
  scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() + custom_plot

ggplot(alphas, aes(parameter, mean, fill = agegrp)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin=mean - se, ymax = mean + se), 
                width = .2, position=position_dodge(.9)) + 
  scale_fill_brewer(palette="Set1", name="Age Group") + 
  scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() +custom_plot
ggsave(here('figs', '2alpha_alpha_age_grp_means.png'))

ggplot(beta, aes(parameter, mean, fill = agegrp)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin=mean - se, ymax = mean + se), 
                width = .2, position=position_dodge(.9)) + 
  scale_fill_brewer(palette="Set1", name="Age Group") + 
  scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() +custom_plot
ggsave(here('figs', '2alpha_beta_age_grp_means.png'), width = 5.5)

# Graph 2 - violin plots group means
# parameters <- ggplot(dt, aes(parameter, estimate, colour = agegrp)) +
#   scale_fill_brewer(palette="Set1", name="Age Group") + 
#   scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() + custom_plot
# 
# parameters+ geom_violin(trim= FALSE) + geom_boxplot(width = 0.1, position = position_dodge(.9))
# parameters + geom_violin(trim= FALSE) + geom_dotplot(binaxis='y', stackdir='center', dotsize=1, position = position_dodge(.9), aes(fill = agegrp))

# single alpha model ####
d1 <- clean_single_alpha(d2)
d1 <- gather(d1, parameter, estimate, alpha:beta)

singlegrpmeans <- d1 %>% 
  dplyr::group_by(agegrp, parameter) %>%
  summarise(mean = mean(estimate), sd = sd(estimate), 
            se= sd(estimate)/sqrt(n()))

# Graph 1 - bar graph group means
ggplot(singlegrpmeans, aes(parameter, mean, fill = agegrp)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin=mean - se, ymax = mean + se), 
                width = .2, position=position_dodge(.9)) + 
  scale_fill_brewer(palette="Set1", name="Age Group") + 
  scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() + custom_plot

ggsave(here('figs', '1alpha_age_grp_means.png'))

# Graph 2 - violin plots group means
parameters <- ggplot(d1, aes(parameter, estimate, colour = agegrp)) +
  scale_fill_brewer(palette="Set1", name="Age Group") + 
  scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() + custom_plot

parameters+ geom_violin(trim= FALSE) + geom_boxplot(width = 0.1, position = position_dodge(.9))
parameters + geom_violin(trim= FALSE) + geom_dotplot(binaxis='y', stackdir='center', dotsize=1, position = position_dodge(.9), aes(fill = agegrp))


# double alpha with decay model ####
d7 <- clean_single_alpha(d4)
d7 <- gather(d7, parameter, estimate, alpha_gain:decay)

decaygrpmeans <- d7 %>% 
  dplyr::group_by(agegrp, parameter) %>%
  summarise(mean = mean(estimate), sd = sd(estimate), 
            se= sd(estimate)/sqrt(n()))
alphas <- decaygrpmeans[which(decaygrpmeans$parameter != 'beta'),]
beta <- decaygrpmeans[which(decaygrpmeans$parameter == 'beta'),]

# Graph 1 - bar graph group means
ggplot(decaygrpmeans, aes(parameter, mean, fill = agegrp)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin=mean - se, ymax = mean + se), 
                width = .2, position=position_dodge(.9)) + 
  scale_fill_brewer(palette="Set1", name="Age Group") + 
  scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() + custom_plot

ggsave(here('figs', 'decay_age_grp_means.png'))

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
