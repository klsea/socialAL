# Group-level visualizations
# 9.13.19 KLS

# load required packages
library(here)
library(tidyverse)
library(ggplot2)

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
  summarize(avg_amount = mean(amount_shared, na.rm = TRUE))

# calculate age group means 
se <- function(sd,n) {sd/sqrt(n())}
grpmeans <- indiv_means %>% 
  dplyr::group_by(agegrp, trial_type) %>%
  summarise(mean_amount = mean(avg_amount), sd_amount = sd(avg_amount), 
            se_amount = sd(avg_amount)/sqrt(n()))

# graph group means
lg = 18
sm = 14
age_grp_means <- ggplot(grpmeans, aes(trial_type, mean_amount, colour = agegrp, fill= agegrp)) + 
  geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=mean_amount - se_amount, ymax = mean_amount + se_amount), 
                width = .2, position=position_dodge(.9)) + theme_minimal() + 
  scale_fill_brewer(palette="Set1", name="Age Group") + 
  scale_colour_brewer(palette="Set1", name="Age Group") +
  xlab('Trial Type') + ylab('Average $ Shared') + 
  coord_cartesian(ylim=c(0, 9)) + scale_y_continuous(breaks = c(3, 6, 9)) + theme(
  axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
  legend.title = element_text(size = lg), legend.text = element_text(size = sm)
)
#ggsave(here('figs', 'age_grp_means.pdf'))
ggsave(here('figs', 'age_grp_means.png'))

# make it a violin plot
library(Hmisc)
violin <- ggplot(indiv_means, aes(trial_type, avg_amount, color = agegrp)) + 
  geom_violin(trim= FALSE) + geom_boxplot(width = 0.1, position = position_dodge(.9)) + 
  scale_color_brewer(palette="Set1", name="Age Group") + 
  scale_fill_brewer(palette="Set1", name="Age Groupn") + 
  xlab('Trial Type') + ylab('Average $ Shared') + 
  scale_y_continuous(breaks = c(3, 6, 9)) + theme_minimal() + theme(
    axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
    axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
    legend.title = element_text(size = lg), legend.text = element_text(size = sm)
    )
violin
#ggsave(here('figs', 'age_grp_means_violin.pdf'))
ggsave(here('figs', 'age_grp_means_violin.png'))

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
  xlab('Trial') + ylab('Average $ Shared') + theme_minimal() + 
  scale_fill_brewer(palette="Dark2", name="Condition") +
  scale_colour_brewer(palette="Dark2", name="Condition") +
  scale_x_continuous(breaks = c(3, 6, 9, 12, 15)) + 
  coord_cartesian(ylim=c(0, 9)) + scale_y_continuous(breaks = c(3, 6, 9)) + theme(
    axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
    axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
    legend.title = element_text(size = lg), legend.text = element_text(size = sm), 
    strip.text.x = element_text(size = lg))
#ggsave(here('figs', 'grp_means_over_time.pdf'))
ggsave(here('figs', 'grp_means_over_time.png'))

# graph raw data
# need to add something for overplotting
#ggplot(dt, aes(tt_number, amount_shared, colour = trial_type, fill = trial_type)) + 
#  geom_point() + geom_smooth(method=lm) + facet_grid(. ~ agegrp) +
#  xlab('Trial') + ylab('Amount $ Shared')

## Graph 3 - Change over stage
dt <- add_stage(dt)

# calculate individual mean summary table by stage
indiv_means_stage <- dt %>% 
  dplyr::group_by(id, agegrp, trial_type, stage) %>%
  summarize(avg_amount = mean(amount_shared, na.rm = TRUE))

grp_means_stage <- indiv_means_stage %>% 
  dplyr::group_by(agegrp, trial_type, stage) %>%
  summarize(sd_amount = sd(avg_amount), se_amount = sd(avg_amount)/sqrt(n()), 
            avg_amount = mean(avg_amount))

# graph trial_type over stage
ggplot(grp_means_stage, aes(stage, avg_amount, colour = trial_type)) + 
         geom_point() + geom_line() + 
  geom_errorbar(aes(ymin = avg_amount - se_amount, ymax =avg_amount + se_amount, width=.1)) + 
  facet_grid(.~agegrp) +   xlab('Trial') + ylab('Average $ Shared') + theme_minimal() + 
  scale_fill_brewer(palette="Dark2", name="Condition") +
  scale_colour_brewer(palette="Dark2", name="Condition") +
  scale_x_continuous(breaks = c(1,2,3)) + 
  coord_cartesian(ylim=c(0, 9)) + scale_y_continuous(breaks = c(0, 3, 6, 9)) + theme(
    axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
    axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
    legend.title = element_text(size = lg), legend.text = element_text(size = sm), 
    strip.text.x = element_text(size = lg))
ggsave(here('figs', 'grp_means_over_stage.png'))

       
       