# Eploratory correlations
# 11.30.21

# load required packages ####
library(here)
library(tidyverse)

# load source functions
source(here::here('scr', 'decode_img.R'))

# set hard-coded variables

# load data
demo <- read.csv(here::here('data', 'SocialAL_demo_question_data.csv'))
#liking <- read.csv(here::here('data', 'SocialAL_Liking_Ratings.csv'))
behavior <- read.csv(here::here('data', 'socialAL_clean_data.csv'))
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

dt %>% group_by(agegrp) %>% summarize(
    df <- cor.test(avg_amount, rating)$parameter, 
    correlation <- cor.test(avg_amount, rating)$estimate,
    pvalue <- cor.test(avg_amount, rating)$p.value
)

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

  
            