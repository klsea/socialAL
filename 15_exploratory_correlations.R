# Exploratory correlations
# 11.30.21 updated 12.16.21

# load required packages ####
library(here)
library(tidyverse)
library(cocor)

# load source functions
source(here::here('scr', 'decode_img.R'))

# set hard-coded variables

# load data
demo <- read.csv(here::here('data', 'SocialAL_demo_question_data.csv'))
#liking <- read.csv(here::here('data', 'SocialAL_Liking_Ratings.csv'))
behavior <- read.csv(here::here('data', 'socialAL_clean_data.csv'))
brain <- read.csv(here::here('data', 'SocialAL_cluster_activity.csv'))
oa <- read.csv(here::here('output', 'two_alpha_with_decay_model_params_older.csv'))
ya <- read.csv(here::here('output', 'two_alpha_with_decay_model_params_younger.csv'))
modeling <- rbind(oa, ya)
rm(oa,ya)

# clean data ####

# _clean demo data_ #
# remove unused people
demo <- demo[which(demo$Use_in_Analyses == 1),]
demo <- demo[4:29]
demo$AgeGroup_YA1OA2 <- factor(demo$AgeGroup_YA1OA2)
subs = demo$ID
demo$ID <- paste0('sub-', demo$ID)
demo$ID[which(demo$ID == 'sub-2040')] <- 'sub-2039'

# make liking
liking <- demo[c(1,22:24)] %>% pivot_longer(Trustworthy_LikeRating:Untrustworthy_LikeRating, names_to = c('trial_type', 'delete'), 
                      names_sep = '_', values_to = 'rating')
liking$delete <- NULL

# _clean behavior_ # 
#calculate average recip rate by partner type
behavior$ID <- behavior$id
behavior_indiv_means <- behavior %>% dplyr::group_by(ID, agegrp, trial_type) %>%
  summarize(avg_amount = mean(amount_shared, na.rm = TRUE))
rm(behavior)

# _clean modeling_ # 
modeling$ID <- modeling$id
modeling$id <- NULL

# Correlations ####
# Behavior and Liking Ratings
dt <- merge(behavior_indiv_means, liking, by = c('ID', 'trial_type'))

c1 <- dt %>% group_by(agegrp) %>% summarize(
    df = cor.test(avg_amount, rating)$parameter, 
    correlation = cor.test(avg_amount, rating)$estimate,
    pvalue = cor.test(avg_amount, rating)$p.value
)
c1

# Fishers r to z
younger <- dt %>% filter(agegrp == 'Younger')
older <- dt %>% filter(agegrp == 'Older')
bl_dat <- list(younger = as.data.frame(younger), older = as.data.frame(older))
cocor(~avg_amount + rating | avg_amount + rating, data = bl_dat)

# graph
ggplot(dt, aes(avg_amount, rating, colour = agegrp)) + 
  geom_jitter() + geom_smooth(method='lm') + theme_minimal() + scale_colour_viridis_d(option = "E")

#Digit Span and Decay Parameter
d1 <- merge(demo, modeling, by = 'ID')

d1 %>% group_by(AgeGroup_YA1OA2) %>% summarize(
  df <- cor.test(Digit.Span.Backward, decay)$parameter,
  correlation <- cor.test(Digit.Span.Backward, decay)$estimate,
  pvalue <- cor.test(Digit.Span.Backward, decay)$p.value
)   

# likeabiity and forgetting
liking$trating <- (liking$rating - 1) * 1.5
d2 <- merge(behavior_indiv_means, liking, by = c('ID', 'trial_type'))
d2 <- merge(d2, modeling, by = 'ID')
d2$amt_rating_diff <- abs(d2$avg_amount - d2$trating)

d2 <- d2 %>% group_by(ID, agegrp, decay) %>% summarize(
  mean(amt_rating_diff)
)
d2$avg_amt_rating_diff <- d2$`mean(amt_rating_diff)`

d2 %>% group_by(agegrp) %>% summarize(
  df <- cor.test(avg_amt_rating_diff, decay)$parameter,
  correlation <- cor.test(avg_amt_rating_diff, decay)$estimate,
  pvalue <- cor.test(avg_amt_rating_diff, decay)$p.value
)

# IFG and digit span
brain$ID <- paste0('sub-', brain$ID)
d3 <- merge(demo, brain, by = 'ID')
  
d3 %>% group_by(AgeGroup) %>% summarize(
  df <- cor.test(Digit.Span.Backward, decision_ifg)$parameter,
  correlation <- cor.test(Digit.Span.Backward, decision_ifg)$estimate,
  pvalue <- cor.test(Digit.Span.Backward, decision_ifg)$p.value
)            

# learning and forgetting parameters
modeling %>% summarize(
  df <- cor.test(alpha_gain, decay)$parameter, 
  correlation <- cor.test(alpha_gain, decay)$estimate, 
  pvalue <- cor.test(alpha_gain, decay)$p.value
)

modeling %>% summarize(
  df <- cor.test(alpha_loss, decay)$parameter, 
  correlation <- cor.test(alpha_loss, decay)$estimate, 
  pvalue <- cor.test(alpha_loss, decay)$p.value
)

d1 %>% group_by(AgeGroup_YA1OA2) %>% summarize(
  df <- cor.test(alpha_gain, decay)$parameter,
  correlation <- cor.test(alpha_gain, decay)$estimate,
  pvalue <- cor.test(alpha_gain, decay)$p.value
)   

d1 %>% group_by(AgeGroup_YA1OA2) %>% summarize(
  df <- cor.test(alpha_loss, decay)$parameter,
  correlation <- cor.test(alpha_loss, decay)$estimate,
  pvalue <- cor.test(alpha_loss, decay)$p.value
)  

# graph
ggplot(d1, aes(alpha_gain, decay, colour = AgeGroup_YA1OA2, fill = AgeGroup_YA1OA2)) + 
  geom_jitter() + geom_smooth(method='lm') + theme_minimal() + 
  scale_colour_viridis_d(option = "E") + scale_fill_viridis_d(option = "E")

ggplot(d1, aes(alpha_loss, decay, colour = AgeGroup_YA1OA2, fill = AgeGroup_YA1OA2)) + 
  geom_jitter() + geom_smooth(method='lm') + theme_minimal() + 
  scale_colour_viridis_d(option = "E") + scale_fill_viridis_d(option = "E")

# Fishers r to z
younger <- d1 %>% filter(AgeGroup_YA1OA2 == 1)
older <- d1 %>% filter(AgeGroup_YA1OA2 == 2)
mo_dat <- list(younger = as.data.frame(younger), older = as.data.frame(older))
cocor(~alpha_gain + decay | alpha_gain + decay, data = mo_dat)
cocor(~alpha_loss + decay | alpha_loss + decay, data = mo_dat)

