# Graph Linear models
# 9.4.20 BC updated 3.12.21 KLS

# load required packages ####
library(here)
library(tidyverse)
library(emmeans)
library(ggplot2)


# load source functions
source(here::here('scr', 'add_partner_trial.R'))

# set hard-coded variables

# load data
m3 <- readRDS(here::here('output', 'model3.rds'))
dt <- read.csv(here::here('data', 'socialAL_clean_data.csv'))
dt <- dt[c(6,7,1:5,8:10)]

# remove trials where Ps did not respond
dt <- na.omit(dt)

# add partner trial
dt <- add_partner_trial(dt)

# prepare data ####
# make categorical variables factors and reorder
dt$trial_type <- factor(dt$trial_type, levels = c("Trustworthy", "Neutral", "Untrustworthy")) #trustworthy is reference group
dt$agegrp <- factor(dt$agegrp, levels = c("Younger", "Older")) # young is reference group

# plot 1 ####
#Making a plot that is going from -1.5 SD below the mean trial # to 1.5 SD above the mean trial 
#If mean trial is 8, -1.5 SD corresponds to trial 1.55 and +1.5 SD corresponds to trial 14.44, 
# so pretty much at the beginning and end trials.

mylist <- list(z_partner_trial_number=seq(-1.5,1.5,by=3), agegrp=c("Younger","Older"), 
                trial_type=c("Untrustworthy","Neutral", "Trustworthy"))

d1 <- summary(emmeans(m3, ~agegrp*trial_type*z_partner_trial_number, at = mylist))

d1$lower <- d1$emmean - d1$SE
d1$upper <- d1$emmean + d1$SE

p1 <- ggplot(data=d1, 
            aes(x=z_partner_trial_number, y=emmean, group=interaction(trial_type, agegrp))) + 
  facet_grid(.~agegrp) + 
  geom_line(aes(linetype = trial_type)) +
  geom_pointrange(aes(ymin=lower, ymax=upper, shape=20, fill="black")) +
  scale_shape_identity() + theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1,"lines")) +
  scale_y_continuous(limits=c(-0.2,9), breaks=seq(0, 9, 3), expand = c(0, 0)) +
  scale_x_continuous(breaks=c(-1.5,1.5), labels=c("Earlier", "Later")) +
  xlab("\nPartner Experience (time)") + 
  ylab("Amount Shared (predicted)\n") + 
  labs(linetype="Partner") +
  guides(fill=FALSE)
p1 

# plot 2 ####
# plot actual YA and OA means per trial and per partner

d2 <- dt %>%
  group_by(agegrp, trial_type, partner_trial_number) %>%
  summarize(m = mean(amount_shared), sd = sd(amount_shared), n = length(amount_shared), se = sd/sqrt(n), 
            upper = m + se, lower = m - se)

# graph YA and OA means per trial and per partner.
p2 <- ggplot(data = d2,
             aes(x = partner_trial_number, y = m, fill = trial_type)) +
  geom_line(aes(linetype = trial_type)) +
  geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(0.05)) +
  facet_wrap(~agegrp) + scale_shape_identity() + theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1,"lines")) + coord_cartesian(xlim=c(1,15), ylim=c(0,9)) +          
  scale_y_continuous(breaks=seq(0, 9, 3)) +
  scale_x_continuous(breaks=seq(1, 15, 2)) +
  xlab("\nPartner Experience (trial number)") + 
  ylab("Amount Shared (actual)\n") + 
  labs(linetype="Partner") +
  guides(fill=FALSE)
p2


# plot 3 ####
# bar graph of age x trial_type interaction

d3 <- summary(emmeans(m3, ~agegrp*trial_type, at = mylist))
d3$lower <- d3$emmean - d3$SE
d3$upper <- d3$emmean + d3$SE

p3 <- ggplot(d3, aes(x = agegrp, y = emmean, fill = trial_type)) + 
  geom_bar(stat = "identity", position=position_dodge(), width = .5, size = .5, color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), position=position_dodge(width=.5), 
                width = .1, size = .6, color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 9),  breaks = seq(0, 9, 3)) +
  scale_fill_grey(start = 1, end = .4) +
  labs(fill = "Partner", title = "Interaction between Age Group and Partner Trustworthiness", x = "Age Group",
       y = "Amount Shared (predicted)") +
  theme(
    plot.title = element_text(hjust = 0.5, size=14, face='bold'),
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    axis.title.x=element_text(size=14, margin = margin(8, 0, 0, 0)),
    axis.title.y=element_text(size=14, margin = margin(0, 8, 0, 0)),
    axis.text.x=element_text(size=12, color="black"),
    axis.text.y=element_text(size=10, color="black"),
    legend.title=element_text(size = 12),
    legend.text=element_text(size = 12),
  )
p3


