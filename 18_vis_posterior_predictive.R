# Posterior predictive checks - graph
# 7.18.22 KLS

# load required packages ####
library(here)
library(stringr)
library(tidyverse)

# load source functions
source(here::here('scr', 'add_tt_number.R'))

# set hard-coded variables

# load data
dt <- read.csv(here('output', 'ppc', 'sim_w_bestfit_2alpha_params.csv'))

## Prep data ####
## ---------------------
# add group label
dt$sub_parent <- str_split_fixed(dt$Subject, '-', 3)[,2]
dt$sub_child <- str_split_fixed(dt$Subject, '-', 3)[,3]
dt$agegrp <- ifelse(as.numeric(dt$sub_parent) > 2000, 'Older', 'Younger')

# rename columns
dt <- rename(dt, id = Subject, trial_number = Trial)

# recode choice and trial type
dt$amount_shared <- recode(dt$Choice, `1` = 0, `2` = 3, `3` = 6, `4` = 9)
dt$trial_type <- recode(dt$Stim_Sequence, `0` = 'Trustworthy', `1` = 'Neutral', `2` = 'Untrustworthy')

# reorder trial_type and age group factors
dt$trial_type <- factor(dt$trial_type, levels = c('Untrustworthy', 'Neutral', 'Trustworthy'))
dt$agegrp <- factor(dt$agegrp, levels = c('Younger', 'Older'))

# graphs ####
# graph constants
lg = 18 # text size
sm = 14
custom_plot = list(theme(
  plot.title = element_text(size = 24),
  axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
  legend.title = element_text(size = lg), legend.text = element_text(size = sm))
)

# calculate individual means 
indiv_means <- dt %>% 
  group_by(id, sub_parent, agegrp, trial_type) %>%
  summarise(avg_amount = mean(amount_shared, na.rm = TRUE))

old_part_means <- dt %>% group_by(sub_parent, agegrp, trial_type )%>%
  summarise(avg_amount = mean(amount_shared, na.rm = TRUE))

# calculate age group means 
se <- function(sd,n) {sd/sqrt(n())}
grpmeans <- old_part_means %>% 
  group_by(agegrp, trial_type) %>%
  summarise(mean_amount = mean(avg_amount), sd_amount = sd(avg_amount), 
            se_amount = sd(avg_amount)/sqrt(n()))
# pretty graph
beh <- ggplot() + 
  geom_point(data = old_part_means, aes(x = trial_type, y = avg_amount, colour = agegrp), 
  position = position_jitterdodge(jitter.height = .25), alpha = .5) + 
  geom_bar(data = grpmeans, aes(x = trial_type, y = mean_amount, colour = agegrp, fill = agegrp), 
           position=position_dodge(), stat= 'identity', alpha = 0.3) + 
  geom_errorbar(data = grpmeans, aes(x = trial_type, y = mean_amount, ymin = mean_amount-se_amount, ymax = mean_amount + se_amount, colour = agegrp), 
                position = position_dodge(.9), width = .2) + 
  xlab('Partner Type') + ylab('Average $ Shared') + coord_cartesian(ylim=c(0, 9)) + 
  scale_y_continuous(breaks = c(0, 3, 6, 9)) +
  scale_fill_brewer(palette="Set1", name="Age Group") + theme_minimal() + 
  scale_colour_brewer(palette="Set1", name="Age Group") + custom_plot
beh
saveRDS(beh, here::here('figs', 'ppc-bbg.RDS'))

# Graph 2 - Change over time ####
## --------------------------
dt <- add_tt_number(dt)

# calculate average behavior
d2 <- dt %>% 
  dplyr::group_by(sub_parent, agegrp, trial_type, tt_number) %>%
  summarise(mean_amount = mean(amount_shared, na.rm = TRUE))

# calculate age group x trial_type x trial means
d3 <- d2 %>% 
  dplyr::group_by(agegrp, trial_type, tt_number) %>%
  summarise(mean_of_means_amount = mean(mean_amount, na.rm = TRUE), 
            sd_amount = sd(mean_amount, na.rm = TRUE), 
            se_amount = sd(mean_amount, na.rm = TRUE)/sqrt(n()))

behxtime <- ggplot(d2, aes(tt_number, mean_amount, colour = trial_type, fill = trial_type)) + 
  geom_smooth(method=lm) + facet_grid(. ~ agegrp) +
  xlab('Trial') + ylab('Amount $ Shared') + #geom_jitter(size=1, alpha=0.2, width=0.3) + 
  scale_fill_brewer(palette="Dark2", name="Partner Type") +
  scale_colour_brewer(palette="Dark2", name="Partner Type") +
  scale_x_continuous(breaks = c(3, 6, 9, 12, 15)) + 
  coord_cartesian(ylim=c(-1, 10)) + scale_y_continuous(breaks = c(0,3, 6, 9)) + 
  theme_minimal() + custom_plot + theme(strip.text.x = element_text(size=lg)) + 
  theme(legend.position = 'bottom')
behxtime
saveRDS(behxtime, here::here('figs', 'ppc-bot.RDS'))
