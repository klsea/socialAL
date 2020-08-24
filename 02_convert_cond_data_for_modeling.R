# Conert raw socialAL behavior into format for modeling
# 5.30.19 KLS

# load required packages
library(here)

# load source functions
source("scr/isolate_run.R")
source("scr/convert_to_wide.R")

# set hard-coded variables
path_to_data <- "~/Box/SocialAL/task/data/"

# read data in
files <- list.files(path_to_data, pattern = ".csv")
#files <- files[- (grep("button", files))]

for (f in files) {
  dt <- read.csv(paste0(path_to_data, f))

  # isolate conditioning runs and convert to bids
  c_run_1 <- isolate_run(dt, "cond", 1)
  b_c_run_1 <- convert_to_wide(c_run_1)
  rm(c_run_1)
  c_run_2 <- isolate_run(dt, "cond", 2)
  b_c_run_2 <- convert_to_wide(c_run_2); rm(c_run_2)

  # concatenate runs
  b_c_run_2$trial_number <- b_c_run_2$trial_number + nrow(b_c_run_1)
  c_dt <- rbind(b_c_run_1, b_c_run_2)  

  # save in modeling folder
  name <- dt$participant[1]
  dir.create(here("data", "modeling"))
  
  fname <- here("data", "modeling", paste0("sub-", name, ".csv"))
  write.csv(c_dt, fname, row.names = FALSE)
}

