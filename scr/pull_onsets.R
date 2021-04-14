pull_onsets <- function(file, event_type) {
  
  # subset file
  dt <- subset(file, event == event_type)[-4]
  
  # create table for .mat file
  onsets <- matrix(c(as.numeric(as.character(subset(dt, trial_type == "Trustworthy")$onset)),
                     as.numeric(as.character(subset(dt, trial_type == "Untrustworthy")$onset)),
                     as.numeric(as.character(subset(dt, trial_type == "Neutral")$onset))),
                     ncol = 3,byrow = FALSE)
  return(onsets)
}

