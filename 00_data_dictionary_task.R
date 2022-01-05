# Make data dictionary for task data
# 1.5.22 KLS

# load required packages
library(here)

# load source functions
source(here::here('scr', 'create_dd.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'socialAL_clean_data.csv'))

# create data dictionary
dd <- create_data_dictionary(dt)

# populate Variable column
dd$Variable[which(dd$`Variable Name` == 'trial_number')] <- 'Trial Number'
dd$Variable[which(dd$`Variable Name` == 'trial_type')] <- 'Partner Type'
dd$Variable[which(dd$`Variable Name` == 'response_key')] <- 'Key pressed by participant'
dd$Variable[which(dd$`Variable Name` == 'response_time')] <- 'Response time'
dd$Variable[which(dd$`Variable Name` == 'trial_earnings')] <- 'Amount of money earned'
dd$Variable[which(dd$`Variable Name` == 'id')] <- 'Participant ID Number'
dd$Variable[which(dd$`Variable Name` == 'grp')] <- 'Age group of participant (number)'
dd$Variable[which(dd$`Variable Name` == 'amount_shared')] <- 'Amount of money shared'
dd$Variable[which(dd$`Variable Name` == 'agegrp')] <- 'Age group of participant (words)'
dd$Variable[which(dd$`Variable Name` == 'respond')] <- 'Response or no response'

# create and populate Measurement Units column
dd$`Measurement Units` <- sapply(dt, class)
dd$`Measurement Units`[which(dd$Variable.Name == 'response_time')] <- 'seconds'
dd$`Measurement Units`[which(dd$Variable.Name == 'trial_earnings')] <- 'dollars'
dd$`Measurement Units`[which(dd$`Variable Name` == 'amount_shared')] <- 'dollars'

# create and populate Allowed Values column
dd$`Allowed Values` <- NA
dd$`Allowed Values`[which(dd$`Variable Name` == 'trial_number')] <- '1-45'
dd$`Allowed Values`[which(dd$`Variable Name` == 'trial_type')] <- "Neutral, Trustworthy, Untrustworthy"
dd$`Allowed Values`[which(dd$`Variable Name` == 'response_key')] <- '1 = $0, 2 = $3, 3 = $6, 4 = $9 or None = no response'
dd$`Allowed Values`[which(dd$`Variable Name` == 'response_time')] <- 'positive number'
dd$`Allowed Values`[which(dd$`Variable Name` == 'trial_earnings')] <- "0, 3, 6, 9, 12, 15, or 18"
dd$`Allowed Values`[which(dd$`Variable Name` == 'id')] <- 'alphanumeric code'
dd$`Allowed Values`[which(dd$`Variable Name` == 'grp')] <- '1 = younger, 2 = older'
dd$`Allowed Values`[which(dd$`Variable Name` == 'amount_shared')] <- '0, 3, 6 or 9'
dd$`Allowed Values`[which(dd$`Variable Name` == 'agegrp')] <- 'Younger or Older'
dd$`Allowed Values`[which(dd$`Variable Name` == 'respond')] <- '1 = response, 0 = no response'

# create and populate Description column
dd$Description <- NA
dd$Description[which(dd$`Variable Name` == 'trial_number')] <- 'Trial number during task'
dd$Description[which(dd$`Variable Name` == 'trial_type')] <- "Partner type; Neutral reciprocates 60% of time, Trustworthy reciprocates 93% of time, Untrustworthy reciprocates 7% of time"
dd$Description[which(dd$`Variable Name` == 'response_key')] <- 'Key chosen by participant'
dd$Description[which(dd$`Variable Name` == 'response_time')] <- 'Time it takes to make decision'
dd$Description[which(dd$`Variable Name` == 'trial_earnings')] <- "Amount of money earned on trial (amount kept + amount reciprocated)"
dd$Description[which(dd$`Variable Name` == 'id')] <- 'Participant code; beings with sub-1000 and assigned sequentially'
dd$Description[which(dd$`Variable Name` == 'grp')] <- 'Age group of participant in numbers'
dd$Description[which(dd$`Variable Name` == 'amount_shared')] <- 'Amount of money shared by the participant (out of $9)'
dd$Description[which(dd$`Variable Name` == 'agegrp')] <- 'Age group of participant in words'
dd$Description[which(dd$`Variable Name` == 'respond')] <- 'If the participant responded or not'

# save 
#write.csv(dd, here::here('data', 'socialAL_clean_data_dictionary.csv'))
