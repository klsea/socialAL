# Make data dictionary for task data
# 1.5.22 KLS

# load required packages
library(here)

# load source functions
source(here::here('scr', 'create_dd.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'socialAL_Liking_Ratings.csv'))

# create data dictionary
dd <- create_data_dictionary(dt)

# populate Variable column ####
dd$Variable[which(dd$`Variable Name` == 'number')] <- 'Cumulative number of participants in age group'
dd$Variable[which(dd$`Variable Name` == 'ID')] <- 'Participant ID Number'
dd$Variable[which(dd$`Variable Name` == 'AgeGroup_YA1_OA2')] <- 'Age group of participant (number)'
dd$Variable[which(dd$`Variable Name` == 'Trustworthy_Name')] <- 'Name of trustworthy partner'
dd$Variable[which(dd$`Variable Name` == 'Neutral_Name')] <- 'Name of neutral partner'
dd$Variable[which(dd$`Variable Name` == 'Untrustworthy_Name')] <- 'Name of untrustworthy partner'
dd$Variable[which(dd$`Variable Name` == 'Trustworthy_IMG')] <- 'Image of trustworthy partner'
dd$Variable[which(dd$`Variable Name` == 'Neutral_IMG')] <- 'Image of neutral partner'
dd$Variable[which(dd$`Variable Name` == 'Untrustworthy_IMG')] <- 'Image of untrustworthy partner'
dd$Variable[which(dd$`Variable Name` == 'Trustworthy_Share')] <- 'Average amount shared with trustworthy partner'
dd$Variable[which(dd$`Variable Name` == 'Neutral_Share')] <- 'Average amount shared with neutral partner'
dd$Variable[which(dd$`Variable Name` == 'Untrustworthy_Share')] <- 'Average amount shared with untrustworthy partner'
dd$Variable[which(dd$`Variable Name` == 'N_Missed_Trials')] <- 'Number of missed trials'
dd$Variable[which(dd$`Variable Name` == 'A1f01_LikeRating')] <- 'Participant rating of image A1f01.png'
dd$Variable[which(dd$`Variable Name` == 'B1f01_LikeRating')] <- 'Participant rating of image B1f01.png'
dd$Variable[which(dd$`Variable Name` == 'C1f01_LikeRating')] <- 'Participant rating of image C1f01.png'

# create and populate Measurement Units column ####
dd$`Measurement Units` <- sapply(dt, class)

# create and populate Allowed Values column
dd$`Allowed Values` <- NA
dd$`Allowed Values`[which(dd$`Variable Name` == 'number')] <- '1-35'
dd$`Allowed Values`[which(dd$`Variable Name` == 'ID')] <- 'alphanumeric code'
dd$`Allowed Values`[which(dd$`Variable Name` == 'AgeGroup_YA1_OA2')] <- '1= younger or 2 = older'
dd$`Allowed Values`[which(dd$`Variable Name` == 'Trustworthy_Name')] <- 'Adam, Matt or Jeff'
dd$`Allowed Values`[which(dd$`Variable Name` == 'Neutral_Name')] <- 'Adam, Matt or Jeff'
dd$`Allowed Values`[which(dd$`Variable Name` == 'Untrustworthy_Name')] <- 'Adam, Matt or Jeff'
dd$`Allowed Values`[which(dd$`Variable Name` == 'Trustworthy_IMG')] <- "A1f01, B1f01, C1f01"
dd$`Allowed Values`[which(dd$`Variable Name` == 'Neutral_IMG')] <- "A1f01, B1f01, C1f01"
dd$`Allowed Values`[which(dd$`Variable Name` == 'Untrustworthy_IMG')] <- "A1f01, B1f01, C1f01"
dd$`Allowed Values`[which(dd$`Variable Name` == 'Trustworthy_Share')] <- '$0-$9'
dd$`Allowed Values`[which(dd$`Variable Name` == 'Neutral_Share')] <- '$0-$9'
dd$`Allowed Values`[which(dd$`Variable Name` == 'Untrustworthy_Share')] <- '$0-$9'
dd$`Allowed Values`[which(dd$`Variable Name` == 'N_Missed_Trials')] <- '0-45'
dd$`Allowed Values`[which(dd$`Variable Name` == 'A1f01_LikeRating')] <- '1-7; 1 = extremely dislike, 7 = extremely like'
dd$`Allowed Values`[which(dd$`Variable Name` == 'B1f01_LikeRating')] <- '1-7; 1 = extremely dislike, 7 = extremely like'
dd$`Allowed Values`[which(dd$`Variable Name` == 'C1f01_LikeRating')] <- '1-7; 1 = extremely dislike, 7 = extremely like'

# create and populate Description column
dd$Description <- NA
dd$Description[which(dd$`Variable Name` == 'number')] <- 'Number of participants in age group; sequentially assigned'
dd$Description[which(dd$`Variable Name` == 'ID')] <- 'Participant code; beings with sub-1000 and assigned sequentially'
dd$Description[which(dd$`Variable Name` == 'AgeGroup_YA1_OA2')] <- 'Age group of participant in numbers'
dd$Description[which(dd$`Variable Name` == 'Trustworthy_Name')] <- 'Name displayed on screen for trustworthy partner'
dd$Description[which(dd$`Variable Name` == 'Neutral_Name')] <-'Name displayed on screen for neutral partner'
dd$Description[which(dd$`Variable Name` == 'Untrustworthy_Name')] <- 'Name displayed on screen for untrustworthy partner'
dd$Description[which(dd$`Variable Name` == 'Trustworthy_IMG')] <- 'Name of image file displayed on screen for trustworthy partner'
dd$Description[which(dd$`Variable Name` == 'Neutral_IMG')] <- 'Name of image file displayed on screen for trustworthy partner'
dd$Description[which(dd$`Variable Name` == 'Untrustworthy_IMG')] <- 'Name of image file displayed on screen for trustworthy partner'
dd$Description[which(dd$`Variable Name` == 'Trustworthy_Share')] <- 'Average amount of money shared with trustworthy participants over 15 trials'
dd$Description[which(dd$`Variable Name` == 'Neutral_Share')] <-  'Average amount of money shared with neutral participants over 15 trials'
dd$Description[which(dd$`Variable Name` == 'Untrustworthy_Share')] <-  'Average amount of money shared with untrustworthy participants over 15 trials'
dd$Description[which(dd$`Variable Name` == 'N_Missed_Trials')] <- 'Number of trials missed'
dd$Description[which(dd$`Variable Name` == 'A1f01_LikeRating')] <- 'How much do you like this person?'
dd$Description[which(dd$`Variable Name` == 'B1f01_LikeRating')] <- 'How much do you like this person?'
dd$Description[which(dd$`Variable Name` == 'C1f01_LikeRating')] <- 'How much do you like this person?'

# save 
write.csv(dd, here::here('data', 'socialAL_Liking_Ratings_data_dictionary.csv'))
