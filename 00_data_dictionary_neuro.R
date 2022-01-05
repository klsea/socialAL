# Make data dictionary for task data
# 1.5.22 KLS

# load required packages
library(here)

# load source functions
source(here::here('scr', 'create_dd.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'socialAL_cluster_activity.csv'))

# create data dictionary
dd <- create_data_dictionary(dt)

# populate Variable column ####
dd$Variable[which(dd$`Variable Name` == 'feedback_rTPJ')] <- 'Average activity in rTPJ cluster during feedback'
dd$Variable[which(dd$`Variable Name` == 'feedback_visual')] <- 'Average activity in visual cluster during feedback'
dd$Variable[which(dd$`Variable Name` == 'decision_ifg')] <- 'Average activity in ifg during decision'
dd$Variable[which(dd$`Variable Name` == 'ID')] <- 'Participant ID Number'
dd$Variable[which(dd$`Variable Name` == 'AgeGroup')] <- 'Age group of participant (number)'

# create and populate Measurement Units column ####
dd$`Measurement Units` <- sapply(dt, class)
dd$`Measurement Units`[which(dd$`Variable Name` == 'feedback_rTPJ')] <- 'arbitrary units'
dd$`Measurement Units`[which(dd$`Variable Name` == 'feedback_visual')] <- 'arbitrary units'
dd$`Measurement Units`[which(dd$`Variable Name` == 'decision_ifg')] <- 'arbitrary units'

# create and populate Allowed Values column
dd$`Allowed Values` <- NA
dd$`Allowed Values`[which(dd$`Variable Name` == 'feedback_rTPJ')] <- 'decimals'
dd$`Allowed Values`[which(dd$`Variable Name` == 'feedback_visual')] <- 'decimals'
dd$`Allowed Values`[which(dd$`Variable Name` == 'decision_ifg')] <- 'decimals'
dd$`Allowed Values`[which(dd$`Variable Name` == 'ID')] <- 'Integers over 1000'
dd$`Allowed Values`[which(dd$`Variable Name` == 'AgeGroup')] <- 'Age Group; YA = Younger, OA = Older'

# create and populate Description column
dd$Description <- NA
dd$Description[which(dd$`Variable Name` == 'feedback_rTPJ')] <- 'Average activity correlated with prediction error in the right angular gyrus (BA 39'
dd$Description[which(dd$`Variable Name` == 'feedback_visual')] <- 'Average activity correlated with prediction error in the bilateral visual cortex (BA 19/18'
dd$Description[which(dd$`Variable Name` == 'decision_ifg')] <- 'Average activity correlated with reputation signal in the right inferior frontal gyrus (BA 45/46'
dd$Description[which(dd$`Variable Name` == 'ID')] <- 'Participant code; beings with 1000 and assigned sequentially'
dd$Description[which(dd$`Variable Name` == 'AgeGroup')] <- 'Age group of participant'

# save 
write.csv(dd, here::here('data', 'socialAL_cluster_activity_data_dictionary.csv'))
