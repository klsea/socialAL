# best fit by BIC confusion matrix - alt
# 11.15.22 KLS

# init ####
# load required packages
library(here)
library(tidyverse)
library(ggplot2)

# load source functions
source(here::here('scr', 'concat_clean.R'))
source(here::here('scr', 'BIC_functions.R'))
source(here('scr', 'calc_prop_best_fit.R'))
source(here('scr', 'calc_prop_best_fit_learners.R'))

# set hard-coded variables
sim_type = 'part_params' # change depending on simulation type
penalty = 'BIC' # change depending on penalty type
name = paste0('Confusion Matrix built from ', penalty)

# baseline data ####
#baseline <- calc_prop_best_fit(sim_type, 'b', penalty)
baseline <- calc_prop_best_fit_gllearners(sim_type, 'b', penalty)

# single alpha data ####
#alpha1 <- calc_prop_best_fit(sim_type, 'a1', penalty)
alpha1 <- calc_prop_best_fit_gllearners(sim_type, 'a1', penalty)

# # single alpha with decay data ####
# alpha1decay <- calc_prop_best_fit(sim_type, 'a1d')

# double alpha data ####
#alpha2 <- calc_prop_best_fit(sim_type, 'a2', penalty)
alpha2 <- calc_prop_best_fit_gllearners(sim_type, 'a2', penalty)
alpha2 <- alpha2 %>% rename(freq_a2 = freq)

# # double alpha with decay data ####
# alpha2decay <- calc_prop_best_fit(sim_type, 'a2d')
# 
# # double alpha with priors data ####
# alpha2prior <- calc_prop_best_fit(sim_type, 'a2p')

# create and populate confusion matrix ####
# rows = fit model
# columns = simulated model
cm1 <- merge(baseline[c(1,3)], alpha1[c(1,3)], by = 'winModel', suffixes = c('_b', '_a1'), all = TRUE)
cm5 <- merge(cm1, alpha2[c(1,3)], by = 'winModel', all = TRUE)

# add missing rows
models <- c('b', 'a1', 'a2')
missingmodels <- models[!(models %in% cm5$winModel)]
missingrows <- 3 - nrow(cm5) 
if (missingrows > 0) {
  for (x in 1:missingrows) {
    model <- missingmodels[x]
    row <- (6 - missingrows) + x
    print(row)
    cm5[row,] <- c(model, NA, NA, NA)
  }
}

# change row and column names and reorder rows
cm5 <- rename(cm5, '1' = 'freq_b', '2' = 'freq_a1', '3' = 'freq_a2')
if(penalty == 'BIC') {
  cm5$winModel <- recode(cm5$winModel, b = '1', a1 = '2', a2 = '3')
} else if (penalty == 'AIC') {
  cm5$winModel <- recode(cm5$winModel, AIC_b = '1', AIC_a1 = '2', AIC_a2 = '3')
} else {
  cm5$winModel <- recode(cm5$winModel, llh_b = '1', llh_a1 = '2', llh_a2 = '3')
}
cm5 <- cm5 %>% arrange(winModel)

# transpose to match Wilson & Collins, 2019
# rows = simulated model
# columns = fit model
confusionMatrix = setNames(data.frame(t(cm5[,-1])), cm5[,1])
confusionMatrix[is.na(confusionMatrix)] <- 0

# make pretty
cm <- tibble::rownames_to_column(confusionMatrix, "sm")
cm <- pivot_longer(cm, 2:4, names_to = 'fm')
cm$value <- as.numeric(cm$value)

prettycm <- ggplot(cm, aes(fm, sm, fill = value)) + geom_tile() + coord_fixed() + 
  scale_y_discrete(lim=rev) + scale_x_discrete(position = "top") + 
  geom_text(aes(label = value), color = 'white', size = 4) + 
  xlab('fit model') + ylab('simulated model') + 
  theme(legend.position = 'none', axis.text = element_text(size = 12), 
        axis.title=element_text(size = 15), plot.title = element_text(size = 18)) + ggtitle(name)
# scale_fill_gradient2(low = 'dark blue', mid = 'green', high = 'yellow')
prettycm

# dep code with all 6 models ####
# create and populate confusion matrix ####
# rows = fit model
# columns = simulated model
# cm1 <- merge(baseline[c(1,3)], alpha1[c(1,3)], by = 'winModel', suffixes = c('_b', '_a1'), all = TRUE)
# cm2 <- merge(alpha1decay[c(1,3)], alpha2[c(1,3)], by = 'winModel', suffixes = c('_a1d', '_a2'), all = TRUE)
# cm3 <- merge(alpha2decay[c(1,3)], alpha2prior[c(1,3)], by = 'winModel', suffixes = c('_a2d', '_a2p'), all = TRUE)
# cm4 <- merge(cm1, cm2, all = TRUE)
# cm5 <- merge(cm4, cm3, all = TRUE)
# rm(baseline, alpha1, alpha1decay, alpha2, alpha2decay, alpha2prior, cm1, cm2) #cm3, cm4)

# # add missing rows
# models <- c('b', 'a1', 'a1d', 'a2', 'a2d', 'a2p')
# missingmodels <- models[!(models %in% cm5$winModel)]
# missingrows <- 6 - nrow(cm5) 
# if (missingrows > 0) {
#   for (x in 1:missingrows) {
#     model <- missingmodels[x]
#     row <- (6 - missingrows) + x
#     print(row)
#     cm5[row,] <- c(model, NA, NA, NA, NA, NA, NA)
#   }
# }
# # change row and column names and reorder rows
# cm5 <- rename(cm5, '1' = 'freq_b', '2' = 'freq_a1', '3' = 'freq_a1d', '4' = 'freq_a2', '5' = 'freq_a2d', '6' = 'freq_a2p')
# cm5$winModel <- recode(cm5$winModel, b = '1', a1 = '2', a1d = '3', a2 = '4', a2d = '5', a2p = '6')
# cm5 <- cm5 %>% arrange(winModel)

# # transpose to match Wilson & Collins, 2019
# # rows = simulated model
# # columns = fit model
# confusionMatrix = setNames(data.frame(t(cm5[,-1])), cm5[,1])
# confusionMatrix[is.na(confusionMatrix)] <- 0

# # make pretty
# cm <- tibble::rownames_to_column(confusionMatrix, "sm")
# cm <- pivot_longer(cm, 2:7, names_to = 'fm')
# cm$value <- as.numeric(cm$value)