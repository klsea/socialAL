# Make data dictionary for task data
# 1.5.22 KLS

# load required packages
library(here)

# load source functions
source(here::here('scr', 'create_dd.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'socialAL_demo_question_data.csv'))

# create data dictionary
dd <- create_data_dictionary(dt)

# populate Variable column ####
dd$Variable[which(dd$`Variable Name` == 'Number')] <- 'Cumulative number of participants in age group'
dd$Variable[which(dd$`Variable Name` == 'Use_in_Analyses')] <- 'Is this participant used in analyses'
dd$Variable[which(dd$`Variable Name` == 'Was_Scanned')] <- 'Was this participant scanned?'
dd$Variable[which(dd$`Variable Name` == 'ID')] <- 'Participant ID Number'
dd$Variable[which(dd$`Variable Name` == 'AgeGroup_YA1OA2')] <- 'Age group of participant (number)'
dd$Variable[which(dd$`Variable Name` == 'Age')] <- 'Age of participant (in years)'
dd$Variable[which(dd$`Variable Name` == 'Gender_0M1F')] <- 'Gender of participant (number)'
dd$Variable[which(dd$`Variable Name` == 'Ethnicity_0NH1H')] <- 'Ethnicity of participant (number)'
dd$Variable[which(dd$`Variable Name` == 'Race_5W4B2A3PI6M')] <- 'Race of participant (number)'
dd$Variable[which(dd$`Variable Name` == 'YearsEdu')] <- 'Years of Education'
dd$Variable[which(dd$`Variable Name` == 'Political.ideology_Econoomic')] <- 'Political ideology around economic issues' 
dd$Variable[which(dd$`Variable Name` == 'Political.ideology_Social')] <- 'Political ideology around social issues' 
dd$Variable[which(dd$`Variable Name` == 'Political.ideology_ForeignPolicy')] <- 'Political ideology around foreign policy issues' 
dd$Variable[which(dd$`Variable Name` == 'SES_USA')] <- 'MacArthur SSS Scale for United States'
dd$Variable[which(dd$`Variable Name` == 'SES_Community')] <- 'MacArthur SSS Scale for Community'
dd$Variable[which(dd$`Variable Name` == 'MoCA')] <- 'Score on the Montreal Cognitive Assessment test'
dd$Variable[which(dd$`Variable Name` == 'Digit.Comparison.Correct')] <- 'Digit Comparison - correct answers' 
dd$Variable[which(dd$`Variable Name` == 'Digit.Comparison.Error')] <- 'Digit Comparison - errors' 
dd$Variable[which(dd$`Variable Name` == 'Digit.Span.Forward')] <- 'Digit Span Forward' 
dd$Variable[which(dd$`Variable Name` == 'Digit.Span.Backward')] <- 'Digit Span Backward' 
dd$Variable[which(dd$`Variable Name` == 'Shipley')] <- 'Score on the Shipley-2 Vocabulary Test'
dd$Variable[which(dd$`Variable Name` == 'SMQ_IS')] <- 'Social Motivation Questionnaire - Information Seeking Score'
dd$Variable[which(dd$`Variable Name` == 'SMQ_ER')] <- 'Social Motivation Questionnaire - Emotional Regulatory Score'
dd$Variable[which(dd$`Variable Name` == 'Payout')] <- 'Amount of money paid based on task performance'
dd$Variable[which(dd$`Variable Name` == 'Trustworthy_LikeRating')] <- 'Likeability rating of trustworthy partner'
dd$Variable[which(dd$`Variable Name` == 'Neutral_LikeRating')] <- 'Likeability rating of neutral partner'
dd$Variable[which(dd$`Variable Name` == 'Untrustworthy_LikeRating')] <- 'Likeability rating of untrustworthy partner'

# create and populate Measurement Units column ####
dd$`Measurement Units` <- sapply(dt, class)

# create and populate Allowed Values column ####
dd$`Allowed Values` <- NA
dd$`Allowed Values`[which(dd$`Varlable Name` == 'Number')] <- '1-35 or 999'
dd$`Allowed Values`[which(dd$`Variable Name` == 'Use_in_Analyses')] <- "0 = don't use, 1 = use"
dd$`Allowed Values`[which(dd$`Variable Name` == 'Was_Scanned')] <- '0 = was not scanned, 1 = was scanned'
dd$`Allowed Values`[which(dd$`Variable Name` == 'ID')] <- '1000-2040'
dd$`Allowed Values`[which(dd$`Variable Name` == 'AgeGroup_YA1OA2')] <- '1 = Younger, 2 = Older'
dd$`Allowed Values`[which(dd$`Variable Name` == 'Age')] <- 'Positive integer'
dd$`Allowed Values`[which(dd$`Variable Name` == 'Gender_0M1F')] <- '0 = Male, 1 = Female'
dd$`Allowed Values`[which(dd$`Variable Name` == 'Ethnicity_0NH1H')] <- '0 = Non-hispanic, 1 = hispanic'
dd$`Allowed Values`[which(dd$`Variable Name` == 'Race_5W4B2A3PI6M')] <- '2 = Asian, 3 = Asian Pacific Islander, 4 = Black, 5 = White, 6 = Multiracial'
dd$`Allowed Values`[which(dd$`Variable Name` == 'YearsEdu')] <- 'Positive integers; 12 = high school, 16 = college'
dd$`Allowed Values`[which(dd$`Variable Name` == 'Political.ideology_Econoomic')] <- '1 = extremely liberal, 5 = moderate, 9 = extremely conservative' 
dd$`Allowed Values`[which(dd$`Variable Name` == 'Political.ideology_Social')] <- '1 = extremely liberal, 5 = moderate, 9 = extremely conservative' 
dd$`Allowed Values`[which(dd$`Variable Name` == 'Political.ideology_ForeignPolicy')] <- '1 = extremely liberal, 5 = moderate, 9 = extremely conservative' 
dd$`Allowed Values`[which(dd$`Variable Name` == 'SES_USA')] <- '1-10'
dd$`Allowed Values`[which(dd$`Variable Name` == 'SES_Community')] <- '1-10'
dd$`Allowed Values`[which(dd$`Variable Name` == 'MoCA')] <- '0-30'
dd$`Allowed Values`[which(dd$`Variable Name` == 'Digit.Comparison.Correct')] <- '0-192' 
dd$`Allowed Values`[which(dd$`Variable Name` == 'Digit.Comparison.Error')] <- '0-192' 
dd$`Allowed Values`[which(dd$`Variable Name` == 'Digit.Span.Forward')] <- '0-14' 
dd$`Allowed Values`[which(dd$`Variable Name` == 'Digit.Span.Backward')] <- '0-14' 
dd$`Allowed Values`[which(dd$`Variable Name` == 'Shipley')] <- '0-40'
dd$`Allowed Values`[which(dd$`Variable Name` == 'SMQ_IS')] <- '4-28'
dd$`Allowed Values`[which(dd$`Variable Name` == 'SMQ_ER')] <- '4-28'
dd$`Allowed Values`[which(dd$`Variable Name` == 'Payout')] <- '$0-$18'
dd$`Allowed Values`[which(dd$`Variable Name` == 'Trustworthy_LikeRating')] <- '1-7; 1 = extremely dislike, 7 = extremely like'
dd$`Allowed Values`[which(dd$`Variable Name` == 'Neutral_LikeRating')] <- '1-7; 1 = extremely dislike, 7 = extremely like'
dd$`Allowed Values`[which(dd$`Variable Name` == 'Untrustworthy_LikeRating')] <- '1-7; 1 = extremely dislike, 7 = extremely like'

# create and populate Description column ####
dd$Description <- NA
dd$Description[which(dd$`Varlable Name` == 'Number')] <- '999 means participant was not used'
dd$Description[which(dd$`Variable Name` == 'Use_in_Analyses')] <- ""
dd$Description[which(dd$`Variable Name` == 'Was_Scanned')] <- ''
dd$Description[which(dd$`Variable Name` == 'ID')] <- ''
dd$Description[which(dd$`Variable Name` == 'AgeGroup_YA1OA2')] <-''
dd$Description[which(dd$`Variable Name` == 'Age')] <- ''
dd$Description[which(dd$`Variable Name` == 'Gender_0M1F')] <- ''
dd$Description[which(dd$`Variable Name` == 'Ethnicity_0NH1H')] <- ''
dd$Description[which(dd$`Variable Name` == 'Race_5W4B2A3PI6M')] <- ''
dd$Description[which(dd$`Variable Name` == 'YearsEdu')] <- ''
dd$Description[which(dd$`Variable Name` == 'Political.ideology_Econoomic')] <- '' 
dd$Description[which(dd$`Variable Name` == 'Political.ideology_Social')] <- ''
dd$Description[which(dd$`Variable Name` == 'Political.ideology_ForeignPolicy')] <- ''
dd$Description[which(dd$`Variable Name` == 'SES_USA')] <- 'At the top of the ladder are the people who are the best off, those who have the most money, most education, and best jobs. At the bottom are the people who are the worst off, those who have the least money, least education, worst jobs, or no job. Please place an ‘X’ on the rung that best represents where you think you stand on the ladder.'
dd$Description[which(dd$`Variable Name` == 'SES_Community')] <- 'At the top of the ladder are the people who are the best off, those who have the most money, most education, and best jobs. At the bottom are the people who are the worst off, those who have the least money, least education, worst jobs, or no job. Please place an ‘X’ on the rung that best represents where you think you stand on the ladder.'
dd$Description[which(dd$`Variable Name` == 'MoCA')] <- 'Screening test for cognitive impariment'
dd$Description[which(dd$`Variable Name` == 'Digit.Comparison.Correct')] <- '' 
dd$Description[which(dd$`Variable Name` == 'Digit.Comparison.Error')] <- '' 
dd$Description[which(dd$`Variable Name` == 'Digit.Span.Forward')] <- '' 
dd$Description[which(dd$`Variable Name` == 'Digit.Span.Backward')] <- '' 
dd$Description[which(dd$`Variable Name` == 'Shipley')] <- 'Higher values mean better vocabulary'
dd$Description[which(dd$`Variable Name` == 'SMQ_IS')] <- 'Higher values mean more information seeking focus; Gong et al., 2019'
dd$Description[which(dd$`Variable Name` == 'SMQ_ER')] <- 'Higher values mean more emotion regulation focus; Gong et al., 2019'
dd$Description[which(dd$`Variable Name` == 'Payout')] <- '$0-$18'
dd$Description[which(dd$`Variable Name` == 'Trustworthy_LikeRating')] <- 'How much do you like this person?'
dd$Description[which(dd$`Variable Name` == 'Neutral_LikeRating')] <- 'How much do you like this person?'
dd$Description[which(dd$`Variable Name` == 'Untrustworthy_LikeRating')] <- 'How much do you like this person?'

# save 
write.csv(dd, here::here('data', 'socialAL_demo_question_data_dictionary.csv'))
