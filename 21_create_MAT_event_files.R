# Create event files for SPM analysis
# 4.14.21 KLS
# modified from script by AC

# load required packages
library(here)
library(R.matlab)
library(matlab)

# load source functions
source(here::here('scr', 'convert.R'))
source(here::here('scr', 'pull_onsets.R'))

# set hard-coded variables
bids <- '~/Box/SocialAL/ScanningData/BIDS/' # change this path to the path on your local machine

# create directories - only need to run this code once
#dir.create(here::here('output', 'eventfiles'))
#dir.create(here::here('output', 'eventfiles', 'glm'))
#dir.create(here::here('output', 'eventfiles', 'pmod'))

# create participant list
part <- list.files(bids)[grep("sub", list.files(bids))]

# loop to create participant files
for(i in 1:length(part)) {
  
  # create participant folders
  dir.create(here::here('output', 'eventfiles', 'glm', part[i]))
  dir.create(here::here('output', 'eventfiles', 'pmod', part[i]))
  
  files <- list.files(paste0(bids, '/', part[i], '/func'))
  tsv <- files[grep('.tsv', files)]; cond <- tsv[grep('cond', tsv)]; rm(tsv)
  
  # loop thru event files
  for(j in 1:length(cond)) {
    if (j == 1) {
      target <- read.csv(paste0(bids, part[i], '/func/', cond[j]), header = TRUE, sep="\t")
      target <- convert(target)
    } else {
      a <- ifelse(i<= 18, 348, 350)  # 348 for sub-1004 through sub-1022, 350 for all others
      add <- read.csv(paste0(bids, part[i], '/func/', cond[j]), header = TRUE, sep="\t")
      add <- convert(add)
      add$onset <- add$onset + a
      target <- rbind(target, add)
      rm(add)
      return(target)
    }
  }
  
  # Adjust onset values to start with 0
  target$onset <- target$onset - target$onset[1]

  ## Make GLM .mat files ####
  names <- matrix(c("trust","untrust","neutral"),ncol = 3) 
  
  durations <- matrix(c("0","0","0"),ncol = 3)
  
  # create "Feedback" files
  onsets <- pull_onsets(target, 'Feedback')
  #onsets <- array(onsets, dim = c(15, 1, 3))

  # Save "Feedback" .mat file
  writeMat(here::here('output', 'eventfiles', 'glm', part[i], paste0(part[i], '_feedback.mat', sep='')), 
           #file = file)           
           names = names, durations = durations, onsets = onsets)
  
  # create "Decision" files # this is the decision phase where they are not allowed to respond
  onsets <- pull_onsets(target, "Decision")
  
  # Save "Decision" .mat file
  writeMat(here::here('output', 'eventfiles', 'glm', part[i], paste0(part[i], '_decision.mat', sep='')), 
           names = names, durations = durations, onsets = onsets)
  
  # create "Decision start" files # this is when they are allowed to start responding (response = time of response)
  ds <- subset(target, event == 'Decision' )
  ds$onset <- ds$onset + ds$duration 
  
  onsets <- pull_onsets(ds, "Decision")

  # Save "Decision start" .mat file
  writeMat(here::here('output', 'eventfiles', 'glm', part[i], paste0(part[i], '_decision_start.mat', sep='')), 
           names = names, durations = durations, onsets = onsets)
  ## Make PMOD .mat files ####
  
  names <- matrix(c("trust","untrust","neutral","noresp"),ncol = 4)
  
  durations <- matrix(c(0,0,0,0),ncol = 4)  
}
