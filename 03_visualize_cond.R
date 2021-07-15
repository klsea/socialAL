# Group-level visualizations
# 9.13.19 KLS update 8.17.20

# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load source functions
source(here::here('scr', 'add_tt_number.R'))

# set hard-coded variables

# read in and concatenate data for group visualization
dt <- read.csv(here::here('data', 'socialAL_clean_data.csv'))
d1 <- read.csv(here::here('output', 'model_comparison.csv'))[c(1,12)]
dt <- merge(dt, d1, by = 'id')
rm(d1)

# reorder trial_type and age group factors
dt$trial_type <- factor(dt$trial_type, levels = c('Untrustworthy', 'Neutral', 'Trustworthy'))
dt$agegrp <- factor(dt$agegrp, levels = c('Younger', 'Older'))

# use the following to isolate subsets of data
#dt <- dt[which(dt$win == 'double'),]
#dt <- dt[which(dt$win == 'single'),]
#dt <- dt[which(dt$win == 'baseline'),]
#dt <- dt[which(dt$win != 'baseline'),]

## Graph 1 - Group means ####
## ---------------------
# calculate individual means 
indiv_means <- dt %>% 
  group_by(id, agegrp, trial_type) %>%
  summarise(avg_amount = mean(amount_shared, na.rm = TRUE))

# calculate age group means 
se <- function(sd,n) {sd/sqrt(n())}
grpmeans <- indiv_means %>% 
  group_by(agegrp, trial_type) %>%
  summarise(mean_amount = mean(avg_amount), sd_amount = sd(avg_amount), 
            se_amount = sd(avg_amount)/sqrt(n()))

# graph constants
lg = 18 # text size
sm = 14
custom_plot = list(theme(
  plot.title = element_text(size = 24),
  axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
  legend.title = element_text(size = lg), legend.text = element_text(size = sm))
)

# graph group means
age_grp_means <- ggplot(grpmeans, aes(trial_type, mean_amount, colour = agegrp, fill= agegrp)) + 
  geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=mean_amount - se_amount, ymax = mean_amount + se_amount), 
                width = .2, position=position_dodge(.9)) + theme_minimal() + 
  scale_fill_brewer(palette="Set1", name="Age Group") + 
  scale_colour_brewer(palette="Set1", name="Age Group") +
  xlab('Trial Type') + ylab('Average $ Shared') + coord_cartesian(ylim=c(0, 9)) + 
  scale_y_continuous(breaks = c(0,3, 6, 9)) + custom_plot #+ 
  #ggtitle('Baseline Model') + theme(plot.title = element_text(hjust = 0.5))
age_grp_means
#ggsave(here::here('figs', 'age_grp_means.png'))
ggsave(here::here('figs', 'baseline_beh_age_grp_means.png'))

# pretty graph
beh <- ggplot() + 
  geom_point(data = indiv_means, aes(x = trial_type, y = avg_amount, colour = agegrp), 
             position = position_jitterdodge(jitter.height = .25), alpha = .5) + 
  geom_bar(data = grpmeans, aes(x = trial_type, y = mean_amount, colour = agegrp, fill = agegrp), 
           position=position_dodge(), stat= 'identity', alpha = 0.3) + 
  geom_errorbar(data = grpmeans, aes(x = trial_type, y = mean_amount, ymin = mean_amount-se_amount, ymax = mean_amount + se_amount, colour = agegrp), 
                position = position_dodge(.9), width = .2) + 
  xlab('Trial Type') + ylab('Average $ Shared') + coord_cartesian(ylim=c(0, 9)) + 
  scale_y_continuous(breaks = c(0,3, 6, 9)) +
  scale_fill_brewer(palette="Set1", name="Age Group") + theme_minimal() + 
  scale_colour_brewer(palette="Set1", name="Age Group") + custom_plot
saveRDS(beh, here::here('figs', 'bbg.RDS'))

## Graph 2 - Change over time ####
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
  coord_cartesian(ylim=c(0, 9)) + scale_y_continuous(breaks = c(3, 6, 9)) + custom_plot + 
  theme(strip.text.x = element_text(size=lg), legend.position="bottom") 
trial_type_by_time
#ggsave(here::here('figs', 'grp_means_over_time.png'))

# graph raw data
# need to add something for overplotting
behxtime <- ggplot(dt, aes(tt_number, amount_shared, colour = trial_type, fill = trial_type)) + 
  geom_smooth(method=lm) + facet_grid(. ~ agegrp) +
  xlab('Trial') + ylab('Amount $ Shared') + geom_jitter(size=1, alpha=0.2, width=0.3) + 
  scale_fill_brewer(palette="Dark2", name="Condition") +
  scale_colour_brewer(palette="Dark2", name="Condition") +
  scale_x_continuous(breaks = c(3, 6, 9, 12, 15)) + 
  coord_cartesian(ylim=c(-1, 10)) + scale_y_continuous(breaks = c(0,3, 6, 9)) + 
  theme_minimal() + custom_plot + theme(strip.text.x = element_text(size=lg)) + 
  theme(legend.position = 'bottom')
#ggsave(here::here('figs', 'all_data_over_time_no_legend.png'))
ggsave(here::here('figs', 'baseline_all_beh_over_time.png'))
saveRDS(behxtime, here::here('figs', 'bot.RDS'))

## Graph 3 - Change over stage ####
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
  coord_cartesian(ylim=c(0, 9)) + scale_y_continuous(breaks = c(0, 3, 6, 9)) + custom_plot
#ggsave(here::here('figs', 'grp_means_over_stage.png'))

# Graph 4 - Difference scores (by Age Group) ####
library(tidyr)
dw <- pivot_wider(indiv_means, id_cols = c(id, agegrp), names_from = trial_type, values_from = avg_amount)
dw$tudiff <- dw$Trustworthy - dw$Untrustworthy
dw$tndiff <- dw$Trustworthy - dw$Neutral

ggplot(dw, aes(agegrp, tudiff, color = agegrp)) + geom_violin(trim= FALSE) + geom_point(alpha = .5) + 
  scale_color_brewer(palette="Set1") +  theme_minimal() +
  xlab('Age Group') + ylab('Difference in Trust \n(Trustworthy - Untrustworthy)') + 
  geom_hline(aes(yintercept = 0)) + theme(legend.position = 'none') + custom_plot 
#ggsave(here::here('figs', 'grp_means_tu_diff_score.png'))
#ggsave(here::here('figs', 'baseline_grp_means_tu_diff_score.png'))

ggplot(dw, aes(agegrp, tndiff, color = agegrp)) + geom_violin(trim= FALSE) + geom_point(alpha = .5) + 
  scale_color_brewer(palette="Set1") + theme_minimal() + 
  xlab('Age Group') + ylab('Difference in Trust \n(Trustworthy - Neutral)') + 
  geom_hline(aes(yintercept = 0)) + theme(legend.position = 'none') + custom_plot
       