# Convert raw socialAL behavior into format for modeling
# 5.30.19 KLS updated 6.2.22 with boxr 

# setup ####
# load required packages
library(here)
library(tidyverse)
library(boxr)

# load source functions
source("scr/isolate_run.R")
source("scr/convert_to_wide.R")

# set hard-coded variables
#path_to_data <- "~/Box/SocialAL/task/data/"
dir_id = 69604452980

# authorize box
box_auth()

# read data in ####
#files <- list.files(path_to_data, pattern = ".csv")
files <- box_search_files('_socialALscanner_', ancestor_folder_ids = dir_id, file_extensions = 'csv')
files <- files[-1]
files <- files[-1]
#files <- files[- (grep("button", files))]


for (i in 1:length(files)) {
  f = files[i]
  fn <- f[[1]]$name
  sub <- str_split(fn, '_')[[1]][1]
  print(sub)
  dt <- box_read_csv(file_id = f[[1]]$id) # read in data

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
  #dir.create(here("data", "modeling"))
  
  fname <- here("data", "modeling", paste0("sub-", name, ".csv"))
  write.csv(c_dt, fname, row.names = FALSE)
}

