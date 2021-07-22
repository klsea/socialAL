# overall means decay model
# 7.22.21 KLS

# load required packages
library(here)
library(tidyverse)

# load source functions

# set hard-coded variables

# read data in 
o4 <- read.csv(here::here('output', 'two_alpha_with_decay_model_params_older.csv'))
y4 <- read.csv(here::here('output', 'two_alpha_with_decay_model_params_younger.csv'))
dt <- rbind(o4, y4); rm(o4, y4)
rm(o4, y4)

# overall means
means <- as.data.frame(t(colMeans(dt[2:6])))
d1 <- merge(dt$id, means)


# save
write.csv(d1, here::here('output', 'model_parameter_means.csv'), row.names = FALSE)
