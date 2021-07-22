# Liking Ratings
# 10.5.20

# load required packages ####
library(here)
library(tidyverse)
library(ggplot2)
library(rstatix)

# load source functions
source('~/Dropbox (Personal)/Functions/SummarySE.R')

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'SocialAL_Liking_Ratings.csv'))
d2 <- read.csv(here::here('data', 'SocialAL_demo_question_data.csv'))

# remove unused people
dt <- merge(dt, d2[c(2,4)], by = 'ID')
rm(d2)

# recode liking ratings based on trustworthy, neutral, and untrustworth partners ####
decode <- function(img, x) {
  if(img == "A1f01") {
    y = dt$A1f01_LikeRating[x]
  } else if (img == 'B1f01') {
    y = dt$B1f01_LikeRating[x]
  } else {
    y = dt$C1f01_LikeRating[x]
  }
  return(y)
}

for (x in 1:length(dt$Trustworthy_IMG)) {
  dt$trustworthy_like[x] <- decode(dt$Trustworthy_IMG[x], x)
}

for (x in 1:length(dt$Neutral_IMG)) {
  dt$neutral_like[x] <- decode(dt$Neutral_IMG[x], x)
}

for (x in 1:length(dt$Untrustworthy_IMG)) {
  dt$untrustworthy_like[x] <- decode(dt$Untrustworthy_IMG[x], x)
}

# make long
d3 <- gather(dt[c(1,3, 18:20)], Partner, Rating, trustworthy_like:untrustworthy_like, factor_key = TRUE)
d3$AgeGroup <- as.factor(recode(as.character(d3$AgeGroup_YA1_OA2), '1' = 'Younger', '2' = 'Older'))
d3$AgeGroup <- ordered(d3$AgeGroup, levels = c('Younger', 'Older'))
d3$Partner <- recode(d3$Partner, 'trustworthy_like' = 'Trustworthy', 'neutral_like' = 'Neutral', 'untrustworthy_like' = 'Untrustworthy')
d3$Partner <- ordered(d3$Partner, levels = c('Untrustworthy', 'Neutral', 'Trustworthy'))

# age group graph ####

# summarize and futz with order of factors
d4 <- summarySE(d3, 'Rating', c('AgeGroup', 'Partner'))
#d4$AgeGroup <- as.factor(recode(as.character(d4$AgeGroup_YA1_OA2), '1' = 'Younger', '2' = 'Older'))
#d4$AgeGroup <- ordered(d4$AgeGroup, levels = c('Younger', 'Older'))

# graph constants
lg = 18 # text size
sm = 14
custom_plot = list(theme(
  plot.title = element_text(size = 24),
  axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
  legend.title = element_text(size = lg), legend.text = element_text(size = sm))
)

ggplot(d4, aes(Partner, Rating, fill = AgeGroup)) + 
  geom_bar(position=position_dodge(), stat= 'identity') + 
  geom_errorbar(aes(ymin = Rating-se, ymax = Rating + se), width = .2, position = position_dodge(.9)) + 
  coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) + 
  scale_fill_brewer(palette="Set1", name="Age Group") + theme_minimal() + 
  scale_colour_brewer(palette="Set1", name="Age Group") + custom_plot

# pretty plot?
ratings<- ggplot() + 
  geom_point(data = d3, aes(x = Partner, y = Rating, colour = AgeGroup), 
             position = position_jitterdodge(jitter.height = .25), alpha = .5) + 
  geom_bar(data = d4, aes(x = Partner, y = Rating, colour = AgeGroup, fill = AgeGroup), 
           position=position_dodge(), stat= 'identity', alpha = 0.3) + 
  geom_errorbar(data = d4, aes(x = Partner, y = Rating, ymin = Rating-se, ymax = Rating + se, colour = AgeGroup), 
                position = position_dodge(.9), width = .2) + 
  coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) + 
  scale_fill_brewer(palette="Set1", name="Age Group") + theme_minimal() + 
  scale_colour_brewer(palette="Set1", name="Age Group") + custom_plot
#ggsave(here::here('figs', 'post-experiment_ratings.png'))
#saveRDS(ratings, here::here('figs', 'per.RDS'))

# stats
liking_aov = anova_test(data = d3, dv = Rating, wid = ID, between = AgeGroup, within = Partner)
get_anova_table(liking_aov)

# breakdown
one.way <- d3 %>% group_by(AgeGroup) %>% anova_test(dv = Rating, wid = ID, within = Partner) %>% 
  get_anova_table() %>% adjust_pvalue(method = 'bonferroni')

pwc <- d3 %>% group_by(AgeGroup) %>% pairwise_t_test(Rating ~ Partner, p.adjust.method = 'bonferroni')
