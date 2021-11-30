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
liking <- read.csv(here::here('data', 'SocialAL_Liking_Ratings.csv'))
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

# _clean liking_ #
# remove unused people
liking <- liking %>% filter(ID %in% subs)

# rename liking data
for (x in 1:length(liking$Trustworthy_IMG)) {
  liking$trustworthy_like[x] <- decode_img(liking, liking$Trustworthy_IMG[x], x)
}

for (x in 1:length(liking$Neutral_IMG)) {
  liking$neutral_like[x] <- decode_img(liking, liking$Neutral_IMG[x], x)
}

for (x in 1:length(liking$Untrustworthy_IMG)) {
  liking$untrustworthy_like[x] <- decode_img(liking, liking$Untrustworthy_IMG[x], x)
}

# make long
liking <- liking[c(2,17:19)] %>% pivot_longer(c(trustworthy_like, neutral_like, untrustworthy_like), names_to = c('trial_type','like'),
                                    names_sep = "_", values_to = 'rating')
liking$like <- NULL
liking$ID <- paste0('sub-', liking$ID)
liking$trial_type <- str_to_title(liking$trial_type)

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
dt <- merge(behavior_indiv_means, liking, by = c('ID', 'trial'))

dt %>% group_by(agegrp) %>% summarize(
    correlation <- cor.test(avg_amount, rating)$estimate,
    pvalue <- cor.test(avg_amount, rating)$p.value
)

#Digit Span and Decay Parameter
d1 <- merge(demo, modeling, by = 'ID')

d1 %>% group_by(AgeGroup_YA1OA2) %>% summarize(
  correlation <- cor.test(Digit.Span.Backward, decay)$estimate,
  pvalue <- cor.test(Digit.Span.Backward, decay)$p.value
)   

# likeabiity and forgetting
d1$like_diff <- d1$Trustworthy_LikeRating - d1$Untrustworthy_LikeRating

d1 %>% group_by(AgeGroup_YA1OA2) %>% summarize(
  correlation <- cor.test(Trustworthy_LikeRating, decay)$estimate,
  pvalue <- cor.test(Trustworthy_LikeRating, decay)$p.value
)  

d1 %>% group_by(AgeGroup_YA1OA2) %>% summarize(
  correlation <- cor.test(Neutral_LikeRating, decay)$estimate,
  pvalue <- cor.test(Neutral_LikeRating, decay)$p.value
)  

d1 %>% group_by(AgeGroup_YA1OA2) %>% summarize(
  correlation <- cor.test(Untrustworthy_LikeRating, decay)$estimate,
  pvalue <- cor.test(Untrustworthy_LikeRating, decay)$p.value
)  

d1 %>% group_by(AgeGroup_YA1OA2) %>% summarize(
  correlation <- cor.test(like_diff, decay)$estimate,
  pvalue <- cor.test(like_diff, decay)$p.value
)  
            