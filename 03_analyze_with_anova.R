# Group-level analyese
# 9.16.19 KLS

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
dt <- concat_clean(files)

# Model 1 - Age Group * Condition (trial_type) ANOVA
# calculate individual means summary table
indiv_means <- dt %>% 
  dplyr::group_by(id, agegrp, trial_type) %>%
  summarize(avg_amount = mean(amount_shared, na.rm = TRUE))

m1 <- aov(avg_amount ~ agegrp * trial_type + Error(id/trial_type), data = indiv_means)
summary(m1)
saveRDS(m1, here('output', 'age*tt_model.rds'))

# Model 2 - Age Group * Condition * Stage (5 trials) ANOVA
# create stage variable
dt <- add_stage(dt)

# calculate individual mean summary table by stage
indiv_means_stage <- dt %>% 
  dplyr::group_by(id, agegrp, trial_type, stage) %>%
  summarize(avg_amount = mean(amount_shared, na.rm = TRUE))

m2 <- aov(avg_amount ~ agegrp * trial_type * stage + Error(id/(trial_type*stage)) + agegrp, data = indiv_means_stage) 
summary(m2)
saveRDS(m2, here('output', 'age*tt*stage_model.rds'))
