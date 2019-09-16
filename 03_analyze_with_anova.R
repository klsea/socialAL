# Group-level analyese
# 9.16.19 KLS

# load required packages
library(here)
library(tidyverse)

# load source functions
source(here('scr', 'add_tt_number.R'))

# set hard-coded variables

# read data in and 
# concatenate data for group analyses
files <- list.files(here('data', 'modeling'), pattern = ".csv")

dt <- data.frame()
for (f in files) {
  dt1 <- read.csv(here('data', 'modeling', f))
  sub <- strsplit(f, '[.]')[[1]][1] # pulls sub number out of file name
  dt1$id <- sub
  dt1$grp <- floor(as.numeric(strsplit(sub, '[-]')[[1]][2])/1000) #1 is YA, 2 is OA
  dt <- rbind(dt,dt1)
}
rm(dt1, sub, files, f)

# prep data
# Convert response_key to $$ shared
dt <- dt %>% 
  mutate(amount_shared = (as.numeric(response_key) - 1) * 3)

# Convert group # to meaningful label
dt <- dt %>% 
  mutate(agegrp = ifelse(grp == 1, 'Younger', 'Older'))
dt$agegrp <- factor(dt$agegrp)

# make subjective dientifier a factor
dt$id <- factor(dt$id)

# Model 1 - Age Group * Condition (trial_type) ANOVA
# calculate individual means summary table
indiv_means <- dt %>% 
  dplyr::group_by(id, agegrp, trial_type) %>%
  summarize(avg_amount = mean(amount_shared, na.rm = TRUE))

m1 <- aov(avg_amount ~ agegrp * trial_type + Error(id/trial_type), data = indiv_means)
summary(m1)

# Model 2 