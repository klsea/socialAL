# Linear models
# 9.4.20 BC updated 3.12.21 KLS

# load required packages ####
library(here)
library(tidyverse)
library(lme4) 
library(MuMIn) #get r squared values
library(sjPlot)
library(emmeans)
library(lmerTest)

# load source functions
source(here::here('scr', 'add_partner_trial.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'socialAL_clean_data.csv'))
dt <- dt[c(6,7,1:5,8:10)]

# remove trials where Ps did not respond
dt <- na.omit(dt)

# add partner trial
dt <- add_partner_trial(dt)

# prepare data ####
# make categorical variables factors and reorder
dt$trial_type <- factor(dt$trial_type, levels = c("Trustworthy", "Neutral", "Untrustworthy")) #trustworthy is reference group
dt$agegrp <- factor(dt$agegrp, levels = c("Younger", "Older")) # young is reference group
dt$amount_shared <- as.numeric(dt$amount_shared)
dt$partner_trial_number <- as.numeric(dt$partner_trial_number)

#effects coding (reduces multicollinearity in the model)
contrasts(dt$agegrp)<-c(1,-1) #Younger = 1, Older = -1
contrasts(dt$trial_type)[,1]<-c(-1/3,2/3,-1/3)
contrasts(dt$trial_type)[,2]<-c(-1/3,-1/3,2/3)

# run models ####
m1 = lmer(amount_shared ~ agegrp * trial_type * z_partner_trial_number + (1 | id), 
          data=dt, REML = TRUE, control=lmerControl(optimizer="bobyqa"))

m2 = lmer(amount_shared ~ agegrp * trial_type * z_partner_trial_number + (1 + trial_type + z_partner_trial_number | id), 
          data=dt, REML = TRUE, control=lmerControl(optimizer="bobyqa"))

m3 = lmer(amount_shared ~ agegrp * trial_type * z_partner_trial_number + (1 + trial_type*z_partner_trial_number | id), 
          data=dt, REML = TRUE, control=lmerControl(optimizer="bobyqa"))

# model comparison ####
AIC(m1, m3)
anova(m1, m3, test="Chisq")

AIC(m2, m3)
anova(m2, m3, test="Chisq")

# save model 3
#saveRDS(m3, file = here::here('output', 'model3.rds'))

# summary model 3 ####
summary(m3)
confint(m3, oldNames=FALSE)
r.squaredGLMM(m3)

#the first value, R2m, is the 'marginal' R2 value, i.e. the amount of variance explained by the fixed effects alone
#the second value, R2c., is the 'conditional' value, i.e. the amount explained by both the fixed and random effects 
#together--in other words, the total amount of variance explained by the model when including what you are able to 
#explain about individual differences (e.g., participant means from (1|ID)

# anova table for model 3 ####
anova(m3)
#ANOVA results from the model Significant main effects of age group and trial_type. 
#Significant interactions between age group and trial type, and trial type and time. Significant three-way interaction.

# regression table for model 3 ####
sjPlot::tab_model(m3)


# means for interpretation ####
# define a list for the breakdown of effects in emmeans
mylist <- list(z_partner_trial_number = seq(-1.5,1.5,by = 3), agegrp=c("Younger","Older"), 
               trial_type=c("Untrustworthy","Neutral", "Trustworthy"))

# characterize age effect
em_agegrp <- emmeans (m3, ~agegrp, at = mylist)
em_agegrp
em_agegrp.c <- contrast(em_agegrp, "pairwise")
em_agegrp.c 
confint(em_agegrp.c)

# characterize trial_type effect 
em_type <- emmeans (m3, ~trial_type, at = mylist)
em_type
em_type.c <- contrast(em_type, "pairwise")
em_type.c 
confint(em_type.c)

# characterize age group x trial type interaction 
em_agetype <- emmeans (m3, ~agegrp*trial_type, at = mylist)
em_agetype
em_agetype.c <- contrast(em_agetype, "pairwise")
em_agetype.c 
confint(em_agetype.c)

# characterize trial type x time interaction 
em_typetime <- emmeans (m3, ~z_partner_trial_number*trial_type, at = mylist)
em_typetime
em_typetime.c <- contrast(em_typetime, "pairwise")
em_typetime.c 
confint(em_typetime.c)

# alternative breakdown comparing time slopes by trial_type
emtrends(m3, pairwise ~trial_type, var = "z_partner_trial_number")
test(emtrends(m3, pairwise ~ trial_type, var = "z_partner_trial_number"))

# characterize three way interaction
em_agetypetime <- emmeans(m3, ~agegrp*trial_type*z_partner_trial_number, at = mylist)
em_agetypetime
em_agetypetime.c <- contrast(em_agetypetime, "pairwise")
em_agetypetime.c 
confint(em_agetypetime.c)

#Characterize using emtrends to compare slopes
emtrends(m3, pairwise ~ agegrp*trial_type, var = "z_partner_trial_number")
test(emtrends(m3, pairwise ~ agegrp*trial_type, var = "z_partner_trial_number"))

