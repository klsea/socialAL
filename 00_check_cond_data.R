# Group-level visualizations
# 3.4.20 KLS

# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load source functions
source(here::here('scr', 'add_tt_number.R'))
source(here::here('scr', 'concat_clean.R'))
source(here::here('scr', 'summarySE.R'))

# set hard-coded variables

# read data in and 
# concatenate data for group visualization
files <- list.files(here::here('data', 'modeling'), pattern = ".csv")
dt <- concat_clean(files)

# check for missing trials
dt$respond<- ifelse(dt$response_key == 'None', 0, 1)
mt <- summarySE(dt, 'trial_earnings', groupvars=c('id', 'respond'))[1:3]
mt <- mt[which(mt$respond == 0),]
mt$percMissed <- round(mt$N/45, 2)
mt$exclude <- ifelse(mt$percMissed > .10, 1, 0)
mt <- mt[which(mt$exclude == 1),]
# 3 participants missed > 10% of trials

# check for same response
st <- summarySE(dt, 'trial_earnings', groupvars=c('id', 'response_key'))[1:3]
st <- st[which(st$response_key != 'None'),]
st$perChoice <- round(st$N/45,2)
st$exclude <- ifelse(st$perChoice < .90, 0, 1)
st <- st[which(st$exclude == 1),]
# 4 participants made the same response on >90% of trials

# Look for extreme reaction times
rt <- summarySE(dt, 'response_time', groupvars = c('id', 'agegrp'), na.rm = TRUE)[1:4]
rt <- rt %>% group_by(agegrp) %>%  mutate(scaledRT = scale(response_time))
#rt$scaledRT <- scale(rt$response_time)
rt$excess <- ifelse(rt$scaledRT > 2, 1, 0)
rt$slow <- ifelse(rt$scaledRT < -2, 1, 0)
rt <- rt[which(rt$slow == 1 | rt$excess == 1),]
# 1 person responded too slowly for age group, 1 person responded too fast for age group

cut <- c(as.character(st$id), as.character(rt$id), as.character(mt$id))
write.csv(cut, here::here('output', 'socialAL_cut.csv'), row.names = FALSE)
