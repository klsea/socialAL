# Make data dictionary for task data
# 1.5.22 KLS

# THIS IS INCOMPLETE!

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
dd$Variable[which(dd$`Variable Name` == 'Trustworthy_Name')] <- 'Name of trustworthy partner'
dd$Variable[which(dd$`Variable Name` == 'ID')] <- 'Participant ID Number'
dd$Variable[which(dd$`Variable Name` == 'AgeGroup_YA1OA2')] <- 'Age group of participant (number)'
dd$Variable[which(dd$`Variable Name` == 'Age')] <- 'Age of participant (in years)'
dd$Variable[which(dd$`Variable Name` == 'Gender_0M1F')] <- 'Gender of participant (number)'
dd$Variable[which(dd$`Variable Name` == 'Ethnicity_0NH1H')] <- 'Ethnicity of participant (number)'
dd$Variable[which(dd$`Variable Name` == 'Race_5W4B2A3PI6M')] <- 'Race of participant (number)'
dd$Variable[which(dd$`Variable Name` == 'YearsEdu')] <- 'Years of Education'
dd$Variable[which(dd$`Variable Name` == 'Political.ideology_Econoomic')] <- '' 
dd$Variable[which(dd$`Variable Name` == 'Political.ideology_Social')] <- ''
dd$Variable[which(dd$`Variable Name` == 'Political.ideology_ForeignPolicy')] <- ''
dd$Variable[which(dd$`Variable Name` == 'SES_USA')] <- 'MacArthur SSS Scale for United States'
dd$Variable[which(dd$`Variable Name` == 'SES_Community')] <- 'MacArthur SSS Scale for Community'
dd$Variable[which(dd$`Variable Name` == 'MoCA')] <- 'Score on the Montreal Cognitive Assessment test'
dd$Variable[which(dd$`Variable Name` == 'Digit.Comparison.Correct')] <- '' 
dd$Variable[which(dd$`Variable Name` == 'Digit.Comparison.Error')] <- '' 
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

# create and populate Allowed Values column
dd$`Allowed Values` <- NA

# create and populate Description column
dd$Description <- NA

# save 
write.csv(dd, here::here('data', 'socialAL_demo_question_data_dictionary.csv'))
