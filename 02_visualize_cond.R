# Group-level visualizations
# 9.13.19 KLS

# load required packages
library(here)
library(tidyverse)
library(ggplot2)
library(Hmisc)

# load source functions
source(here('scr', 'add_tt_number.R'))

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

# reorder trial_type and agegrp factors
dt$trial_type <- factor(dt$trial_type, levels = c("Untrustworthy", "Neutral", "Trustworthy"))
dt$agegrp <- factor(dt$agegrp, levels = c("Younger", "Older"))


## Graph 1 - Group means
## ---------------------
# calculate individual means 
indiv_means <- dt %>% 
  dplyr::group_by(id, agegrp, trial_type) %>%
  summarize(avg_amount=mean(amount_shared, na.rm = TRUE))

# calculate age group means 
se <- function(sd,n) {sd/sqrt(n())}
grpmeans <- indiv_means %>% 
  dplyr::group_by(agegrp, trial_type) %>%
  summarise(mean_amount = mean(avg_amount), sd_amount = sd(avg_amount), 
            se_amount = sd(avg_amount)/sqrt(n()))

# graph group means
age_grp_means <- ggplot(grpmeans, aes(trial_type, mean_amount, colour = agegrp, fill= agegrp)) + 
  geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=mean_amount - se_amount, ymax = mean_amount + se_amount), 
                width = .2, position=position_dodge(.9)) + theme_bw() + 
  scale_fill_brewer(palette="Set1") + xlab('Trial Type') + ylab('Average Amount Shared')
ggsave(here('figs', 'age_grp_means.pdf'))

# make it a violin plot
violin <- ggplot(indiv_means, aes(trial_type, avg_amount, fill = agegrp)) + 
  geom_violin() + geom_boxplot(width = 0.1, position = position_dodge(.9)) + 
  theme_minimal() + scale_fill_brewer(palette="Set1") + xlab('Trial Type') + 
  ylab('Average Amount Shared')
violin
ggsave(here('figs', 'age_grp_means_violin.pdf'))

## Graph 2 - Change over time
## --------------------------
dt <- add_tt_number(dt)

# calculate age group x trial_type x trial means
d3 <- dt %>% 
  dplyr::group_by(agegrp, trial_type, tt_number) %>%
  summarise(mean_amount = mean(amount_shared, na.rm = TRUE), 
            sd_amount = sd(amount_shared, na.rm = TRUE), 
            se_amount = sd(amount_shared, na.rm = TRUE)/sqrt(n()))

# graph trial_type over time
trial_type_by_time <- ggplot(d3, aes(tt_number, mean_amount, colour = trial_type, fill = trial_type)) + 
  geom_point() + geom_smooth(method=lm) + facet_grid(. ~ agegrp) + 
  xlab('Trial') + ylab('Amount shared') + theme_minimal() + 
  scale_fill_brewer(palette="Dark2") +  scale_colour_brewer(palette="Dark2")
ggsave(here('figs', 'grp_means_over_time.pdf'))


# graph raw data
ggplot(dt, aes(tt_number, amount_shared, colour = trial_type, fill = trial_type)) + 
  geom_point() + geom_smooth(method=lm) + facet_grid(. ~ agegrp) +
  xlab('Trial') + ylab('Amount shared')
