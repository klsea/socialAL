# Demo and questionnaire Data
# 9.14.20

# load required packages
library(here)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(abind)

#library(ggplot2)

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
table(dt$AgeGroup_YA1OA2, dt$Gender_0M1F)

# age mean, range by group
dt %>% group_by(AgeGroup_YA1OA2) %>% 
  summarize(
  age_mean = mean(Age), 
  age_min = min(Age), 
  age_max = max(Age), 
  edu_mean = mean(YearsEdu)
)

# other group means
means <- dt %>% group_by(AgeGroup_YA1OA2) %>% 
  summarize(
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

sds <- dt %>% group_by(AgeGroup_YA1OA2) %>% 
  summarize(
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

#table <- abind(means, sds, along = 0)
table <- matrix(nrow= nrow(means), ncol = ncol(means))
for (x in 1:nrow(means)){
  for (y in 1:ncol(means)){
    table[x,y] <- paste0(round(means[x,y], 2), ' (', round(sds[x,y], 2), ')')
  }
}
colnames(table) <- c("Younger", "Older")
rownames(table) <- rownames(means)
#table$tvalue <- NA
#table$significance<- NA

# Group comparisons - https://www.datanovia.com/en/lessons/t-test-in-r/
#### Make all this junk below a function!!!###

# eduation
dt %>% group_by(AgeGroup_YA1OA2) %>% get_summary_stats(YearsEdu, type = "mean_sd") 

# ed <- ggboxplot(
# dt, x = "AgeGroup_YA1OA2", y = "YearsEdu", 
#   ylab = "YearsEdu", xlab = "Groups", add = "jitter"
# )
# ed

dt %>% levene_test(YearsEdu ~ AgeGroup_YA1OA2)

edcomp <- dt %>%  t_test(YearsEdu ~ AgeGroup_YA1OA2, var.equal = TRUE) %>% add_significance()
demotable[2,3] <- edcomp$statistic
demotable[2,4] <- edcomp$p
rm(edcomp)

# MoCA
dt %>% group_by(AgeGroup_YA1OA2) %>%  get_summary_stats(MoCA, type = "mean_sd") 

# moca <- ggboxplot(
#   dt, x = "AgeGroup_YA1OA2", y = "MoCA", 
#   ylab = "MoCA", xlab = "Groups", add = "jitter"
# )
# moca

dt %>% levene_test(MoCA ~ AgeGroup_YA1OA2)

moca<- dt %>% t_test(MoCA ~ AgeGroup_YA1OA2, var.equal = TRUE) %>% add_significance()
mocacomp <- dt %>%  t_test(YearsEdu ~ AgeGroup_YA1OA2, var.equal = TRUE) %>% add_significance()
demotable[8,3] <- mocacomp$statistic
demotable[8,4] <- mocacomp$p
rm(mocacomp)

# Digit comparison
# dt %>% group_by(AgeGroup_YA1OA2) %>%  get_summary_stats() 
# 
# dcompare <- ggboxplot(
#   dt, x = "AgeGroup_YA1OA2", y = "Digit.Total", 
#   ylab = "Digit.Total", xlab = "Groups", add = "jitter"
# )
# dcompare
# 
# dt %>% levene_test(Digit.Total ~ AgeGroup_YA1OA2)
# 
# dt %>% t_test(Digit.Total ~ AgeGroup_YA1OA2, var.equal = TRUE) %>% add_significance()

# Digit Span Total
dt %>% group_by(AgeGroup_YA1OA2) %>% get_summary_stats(Digit.Total, type = "mean_sd")

# dstotal <- ggboxplot(
#   dt, x = "AgeGroup_YA1OA2", y = "Digit.Total", 
#   ylab = "Digit.Total", xlab = "Groups", add = "jitter"
# )
# dstotal

dt %>% levene_test(Digit.Total ~ AgeGroup_YA1OA2)

dt %>% t_test(Digit.Total ~ AgeGroup_YA1OA2, var.equal = TRUE) %>% add_significance()

#Shipley
dt %>% group_by(AgeGroup_YA1OA2) %>%  get_summary_stats(Shipley, type = "mean_sd") 

# shipley <- ggboxplot(
#   dt, x = "AgeGroup_YA1OA2", y = "Shipley", 
#   ylab = "Shipley", xlab = "Groups", add = "jitter"
# )
# shipley

dt %>% levene_test(Shipley ~ AgeGroup_YA1OA2)

dt %>% t_test(Shipley ~ AgeGroup_YA1OA2, var.equal = TRUE) %>% add_significance()

#SMQ_IS
dt %>% group_by(AgeGroup_YA1OA2) %>%  get_summary_stats(SMQ_IS, type = "mean_sd") 

# SMQ_IS <- ggboxplot(
#   dt, x = "AgeGroup_YA1OA2", y = "SMQ_IS", 
#   ylab = "SMQ_IS", xlab = "Groups", add = "jitter"
# )
# SMQ_IS

dt %>% levene_test(SMQ_IS ~ AgeGroup_YA1OA2)

dt %>% t_test(SMQ_IS ~ AgeGroup_YA1OA2, var.equal = TRUE) %>% add_significance()

#SMQ_ER
dt %>% group_by(AgeGroup_YA1OA2) %>%  get_summary_stats(SMQ_ER, type = "mean_sd") 

# SMQ_ER <- ggboxplot(
#   dt, x = "AgeGroup_YA1OA2", y = "SMQ_ER", 
#   ylab = "SMQ_ER", xlab = "Groups", add = "jitter"
# )
# SMQ_ER

dt %>% levene_test(SMQ_ER ~ AgeGroup_YA1OA2)

dt %>% t_test(SMQ_ER ~ AgeGroup_YA1OA2, var.equal = TRUE) %>% add_significance()
