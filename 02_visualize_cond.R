# Group-level visualizations
# 9.13.19 KLS

# load required packages
library(here)
library(tidyverse)

# load source functions

# set hard-coded variables

# read data in and 
# concatenate data for group visualization
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

# Convert response_key to $$ shared
dt <- dt %>% 
  mutate(amount_shared = (as.numeric(response_key) - 1) * 3)

# Convert group # to meaningful label
dt <- dt %>% 
  mutate(agegrp = ifelse(grp == 1, 'Younger', 'Older'))

# calculate individual means 
dt2 <- dt %>% 
  dplyr::group_by(id, agegrp, trial_type) %>%
  summarize(mean_amount=mean(amount_shared, na.rm = TRUE))

# calculate age group means 
# group sd not working
grpmeans <- dt2 %>% 
  dplyr::group_by(agegrp, trial_type) %>%
  summarise(mean_amount = mean(mean_amount), sd_amount = sd(mean_amount))

# graph group means


