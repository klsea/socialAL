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
dt <- read.csv(here::here('output', 'model_params.csv' ))
d2 <- read.csv(here::here('output', 'single_alpha_model_params.csv' ))
d3 <- read.csv(here::here('output', 'baseline_model_params.csv'))

# create age group labels
dt <- clean_param(dt)

se <- function(sd,n) {sd/sqrt(n())}
grpmeans <- dt %>% 
  dplyr::group_by(agegrp, parameter) %>%
  summarise(mean = mean(estimate), sd = sd(estimate), 
            se= sd(estimate)/sqrt(n()))

# graph constants
lg = 18 # text size
sm = 14
custom_plot = list(theme(
  axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
  legend.title = element_text(size = lg), legend.text = element_text(size = sm))
)
# Graph 1 - bar graph group means
ggplot(grpmeans, aes(parameter, mean, fill = agegrp)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin=mean - se, ymax = mean + se), 
                                            width = .2, position=position_dodge(.9)) + 
  scale_fill_brewer(palette="Set1", name="Age Group") + 
  scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() 

# Graph 2 - violin plots group means
parameters <- ggplot(dt, aes(parameter, estimate, colour = agegrp)) +
  scale_fill_brewer(palette="Set1", name="Age Group") + 
  scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() + custom_plot

parameters+ geom_violin(trim= FALSE) + geom_boxplot(width = 0.1, position = position_dodge(.9))
parameters + geom_violin(trim= FALSE) + geom_dotplot(binaxis='y', stackdir='center', dotsize=1, position = position_dodge(.9), aes(fill = agegrp))

# single alpha model
d1 <- clean_single_alpha(d2)

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
  scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() 

# Graph 2 - violin plots group means
parameters <- ggplot(d1, aes(parameter, estimate, colour = agegrp)) +
  scale_fill_brewer(palette="Set1", name="Age Group") + 
  scale_colour_brewer(palette="Set1", name="Age Group") + theme_minimal() + custom_plot

parameters+ geom_violin(trim= FALSE) + geom_boxplot(width = 0.1, position = position_dodge(.9))
parameters + geom_violin(trim= FALSE) + geom_dotplot(binaxis='y', stackdir='center', dotsize=1, position = position_dodge(.9), aes(fill = agegrp))
