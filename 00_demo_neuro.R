# Demo and questionnaire Data
# 9.14.20

# load required packages ####
library(here)
library(tidyverse)
library(rstatix)
library(ggpubr)

# load source functions

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'SocialAL_demo_question_data.csv'))

# remove unused people
dt <- dt[which(dt$Use_in_Analyses == 1),]
dt <- dt[4:29]
dt$AgeGroup_YA1OA2 <- factor(dt$AgeGroup_YA1OA2)

# calc digit span total
dt$Digit.Total = dt$Digit.Span.Forward + dt$Digit.Span.Backward

# numbers in each age group
table(dt$AgeGroup_YA1OA2)

# numbers in each age group
breakdown <- table(dt$AgeGroup_YA1OA2, dt$Gender_0M1F)

# age mean, range by group for text
dt %>% group_by(AgeGroup_YA1OA2) %>% summarize(age_min = min(Age), age_max = max(Age))

# group means ####
means <- dt %>% group_by(AgeGroup_YA1OA2) %>% 
  summarize(
    Age = mean(Age), 
    Education= mean(YearsEdu), 
    SES_USA = mean(SES_USA), 
    SES_Community= mean(SES_Community), 
    PI_Economic = mean(Political.ideology_Economic), 
    PI_Social = mean(Political.ideology_Social), 
    PI_Foreign_Policy = mean(Political.ideology_ForeignPolicy),
    MoCA = mean(MoCA), 
    Digit_Comparison_Correct = mean(Digit.Comparison.Correct), 
    Digit_Comparison_Error = mean(Digit.Comparison.Error),
    Digit_Total = mean(Digit.Total), 
    Shipley = mean(Shipley), 
    SMQ_IS = mean(SMQ_IS), 
    SMQ_ER = mean(SMQ_ER)
  )
rownames(means) <- c("Younger", "Older")
means <- as.data.frame(t(means))
means$Younger <- as.numeric(means$Younger)
means$Older <- as.numeric(means$Older)

# group standard devs ####
sds <- dt %>% group_by(AgeGroup_YA1OA2) %>% 
  summarize(
    Age = sd(Age),
    Education= sd(YearsEdu), 
    SES_USA = sd(SES_USA), 
    SES_Community= sd(SES_Community), 
    PI_Economic = sd(Political.ideology_Economic), 
    PI_Social = sd(Political.ideology_Social), 
    PI_Foreign_Policy = sd(Political.ideology_ForeignPolicy),
    MoCA = sd(MoCA), 
    Digit_Comparison_Correct = sd(Digit.Comparison.Correct), 
    Digit_Comparison_Error = sd(Digit.Comparison.Error),
    Digit_Total = sd(Digit.Total), 
    Shipley = sd(Shipley), 
    SMQ_IS = sd(SMQ_IS), 
    SMQ_ER = sd(SMQ_ER)
  )
rownames(sds) <- c("Younger", "Older")
sds <- as.data.frame(t(sds))
sds$Younger <- as.numeric(sds$Younger)
sds$Older <- as.numeric(sds$Older)

# create table ####
table <- matrix(nrow= nrow(means), ncol = ncol(means) + 3)
for (x in 1:nrow(means)){
  for (y in 1:ncol(means)){
    table[x,y+1] <- paste0(round(means[x,y], 2), ' (', round(sds[x,y], 2), ')')
  }
}
colnames(table) <- c("Variable", "Younger", "Older","t value", "p value")
table <- as.data.frame(table)
table$Variable <- rownames(means)

#### Functions to compare group means ####
# Group comparisons - https://www.datanovia.com/en/lessons/t-test-in-r/

graph <- function(data, variable, variname) {
  # variable and variname need to be in quotes
  ggboxplot(data, x = 'AgeGroup_YA1OA2', y = variable, ylab = variname, xlab = 'Groups', add = 'jitter')
}

compare <- function(data, formula) {
  # formula is the comparison in quotes - e.g. 'age ~ weight'
  ltest <- dt %>% levene_test(as.formula(formula))
  ifelse(ltest$p > 0.05, 
                 ttest <- dt %>% t_test(as.formula(formula), var.equal = TRUE) %>% add_significance(),
                 ttest <- dt %>%  t_test(as.formula(formula), var.equal = FALSE) %>% add_significance()
  )
  return(ttest)
}

add_to_table <- function(table, row, ttest) {
  table[row,4] <- round(ttest$statistic, 2)
  ifelse(ttest$p.signif == 'ns', 
    table[row,5] <- round(ttest$p, 3), 
    table[row,5] <- paste0(round(ttest$p, 3), '*')
  )
  return(table)
}

#### Using functions to add demo comparisons to table ####
# Age
graph(dt, 'Age', 'Age in Years')
age <- compare(dt, 'Age ~ AgeGroup_YA1OA2')
table <- add_to_table(table, 2, age)
rm(age)

# Eduation
graph(dt, 'YearsEdu', 'Years of Education')
ed <- compare(dt, 'YearsEdu ~ AgeGroup_YA1OA2')
table <- add_to_table(table, 3, ed)
rm(ed)

# SES US
graph(dt, 'SES_USA', 'SES compared to USA')
ses1 <- compare(dt, 'SES_USA ~ AgeGroup_YA1OA2')
table <- add_to_table(table, 4, ses1)
rm(ses1)

# SES Community
graph(dt, 'SES_Community', 'SES compared to own community')
ses2 <- compare(dt, 'SES_Community ~ AgeGroup_YA1OA2')
table <- add_to_table(table, 5, ses1)
rm(ses2)

#Political Ideology Economic
graph(dt, 'Political.ideology_Economic', 'Economic Political Ideology')
pi1 <- compare(dt, 'Political.ideology_Economic ~ AgeGroup_YA1OA2')
table <- add_to_table(table, 6, pi1)
rm(pi1)

#Political Ideology Social
graph(dt, 'Political.ideology_Social', 'Social Political Ideology')
pi2 <- compare(dt, 'Political.ideology_Social ~ AgeGroup_YA1OA2')
table <- add_to_table(table, 7, pi2)
rm(pi2)

#Political Ideology Foreign Policy
graph(dt, 'Political.ideology_ForeignPolicy', 'Foreign Policy Political Ideology')
pi3 <- compare(dt, 'Political.ideology_ForeignPolicy ~ AgeGroup_YA1OA2')
table <- add_to_table(table, 8, pi3)
rm(pi3)

#### Using functions to add neuro comparisons to table ####
# MoCA
graph(dt, 'MoCA', 'MoCA Score')
moca <- compare(dt, 'MoCA ~ AgeGroup_YA1OA2')
table <- add_to_table(table, 9, moca)
rm(moca)

# Digit Comparison Accuracy
graph(dt, 'Digit.Comparison.Correct', 'Digit Comparison Accuracy')
dc1 <- compare(dt, 'Digit.Comparison.Correct ~ AgeGroup_YA1OA2')
table <- add_to_table(table, 10, dc1)
rm(dc1)

# Digit Comparison Errors
graph(dt, 'Digit.Comparison.Error', 'Digit Comparison Errors')
dc2 <- compare(dt, 'Digit.Comparison.Error ~ AgeGroup_YA1OA2')
table <- add_to_table(table, 11, dc2)
rm(dc2)

# Digit Span Total
graph(dt, 'Digit.Total', 'Digit Span Total')
ds <- compare(dt, 'Digit.Total ~ AgeGroup_YA1OA2')
table <- add_to_table(table, 12, ds)
rm(ds)

# Shipley
graph(dt, 'Shipley', 'Shipley Vocabulary')
vocab <- compare(dt, 'Shipley ~ AgeGroup_YA1OA2')
table <- add_to_table(table, 13, vocab)
rm(vocab)

# SMQ-IS
graph(dt, 'SMQ_IS', 'Information Seeking Social Motive')
smq1 <- compare(dt, 'SMQ_IS ~ AgeGroup_YA1OA2')
table <- add_to_table(table, 14, smq1)
rm(smq1)

# SMQ-ER
graph(dt, 'SMQ_ER', 'Emotion Regulation Social Motive')
smq2 <- compare(dt, 'SMQ_ER ~ AgeGroup_YA1OA2')
table <- add_to_table(table, 15, smq2)
rm(smq2)

#### Clean up table ####
table[1,1] <- 'N'
table[1,2] <- paste0(breakdown[1,1], 'M/', breakdown[1,2], 'F')
table[1,3] <- paste0(breakdown[2,1], 'M/', breakdown[2,2], 'F')
table[1,4] <- round(chisq_test(breakdown)$statistic, 2)
table[1,5] <- chisq_test(breakdown)$p
