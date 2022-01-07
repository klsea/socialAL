# Compare model parameters to behavior
# 9.23.19 KLS

# load required packages
library(here)
library(tidyverse)

# load source functions
source(here('scr', 'add_tt_number.R'))
source(here('scr', 'concat_clean.R'))

# set hard-coded variables

# read data in and 
# concatenate data for group analyses
files <- list.files(here('data', 'modeling'), pattern = ".csv")
d1 <- concat_clean(files)

d2 <- read.csv(here('output', 'model_params.csv' ))
d2 <- clean_param(d2)

# calculate individual means summary table
indiv_means <- d1 %>% 
  dplyr::group_by(id, agegrp, trial_type) %>%
  summarize(avg_amount = mean(amount_shared, na.rm = TRUE))

# Correlation between alpha_gain and trustworthy amount
# subset trustworthy
trust <- indiv_means[which(indiv_means$trial_type == 'Trustworthy'),]

#subset alpha_gain
gain <- d2[which(d2$parameter== 'alpha_gain'),]

# merge 
d3 <- merge(trust, gain, by = c('id', 'agegrp'))

ggplot(d3, aes(estimate, avg_amount)) + geom_point() + geom_smooth(method = 'lm') + 
  facet_wrap(~ agegrp)

r <- cor(d3$avg_amount, d3$estimate)
cor.test(d3$avg_amount, d3$estimate)

# test by age grp
# YA
ya1 <- d3[which(d3$agegrp == 'Younger'),]
cor(ya1$avg_amount, ya1$estimate)
cor.test(ya1$avg_amount, ya1$estimate)
# OA
oa1 <- d3[which(d3$agegrp == 'Older'),]
cor(oa1$avg_amount, oa1$estimate)
cor.test(oa1$avg_amount, oa1$estimate)

# Correlation between alpha_loss and untrustworthy amount
# subset untrustworthy
notrust <- indiv_means[which(indiv_means$trial_type == 'Untrustworthy'),]

#subset alpha_gain
loss <- d2[which(d2$parameter== 'alpha_loss'),]

# merge 
d4 <- merge(notrust, loss, by = c('id', 'agegrp'))

ggplot(d4, aes(estimate, avg_amount)) + geom_point() + geom_smooth(method = 'lm') + 
  facet_wrap(~agegrp)
r <- cor(d4$avg_amount, d4$estimate)
cor.test(d4$avg_amount, d4$estimate)

# test by age grp
# YA
ya2 <- d4[which(d4$agegrp == 'Younger'),]
cor(ya2$avg_amount, ya2$estimate)
cor.test(ya2$avg_amount, ya2$estimate)
# OA
oa2 <- d4[which(d4$agegrp == 'Older'),]
cor(oa2$avg_amount, oa2$estimate)
cor.test(oa2$avg_amount, oa2$estimate)
