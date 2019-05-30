# Conert raw socialAL behavior into BIDS format
# 5.9.19 KLS

# load required packages
library(here)

# load source functions
source("scr/org_cond_phase.R")
source("scr/org_gen_phase.R")
source("scr/isolate_run.R")
source("scr/convert_to_bids.R")

# set hard-coded variables
path_to_data <- "~/Box/SocialAL/task/data/"

# read data in
files <- list.files(path_to_data, pattern = ".csv")
files <- files[- (grep("button", files))]

for (f in files) {
  dt <- read.csv(paste0(path_to_data, f))

  # isolate conditioning runs and convert to bids
  c_run_1 <- isolate_run(dt, "cond", 1)
  b_c_run_1 <- convert_to_bids(c_run_1, "cond"); rm(c_run_1)
  c_run_2 <- isolate_run(dt, "cond", 2)
  b_c_run_2 <- convert_to_bids(c_run_2, "cond"); rm(c_run_2)

  # isolate generalization trials and remove empty rows
  g_run_1 <- isolate_run(dt, "gen", 1)
  b_g_run_1 <- convert_to_bids(g_run_1, "gen"); rm(g_run_1)
  g_run_2 <- isolate_run(dt, "gen", 2)
  b_g_run_2 <- convert_to_bids(g_run_2, "gen"); rm(g_run_2)
  g_run_3 <- isolate_run(dt, "gen", 3)
  b_g_run_3 <- convert_to_bids(g_run_3, "gen"); rm(g_run_3)
  g_run_4 <- isolate_run(dt, "gen", 4)
  b_g_run_4 <- convert_to_bids(g_run_4, "gen"); rm(g_run_4)

  # save with BIDS filenames
  name <- dt$participant[1]
  dir.create(here("data", "BIDS", name))
  dfnames <- list(b_c_run_1, b_c_run_2, b_g_run_1, b_g_run_2,
                  b_g_run_3, b_g_run_4)

  for (x in 1:2) {
    fname <- here("data", "BIDS", name,
                  paste0("sub-", name, "_task-cond_run-0", x, "_events.tsv"))
    vname <- dfnames[x]
    write.table(vname, fname, sep = "\t", row.names = FALSE)
  }

  for (x in 1:4) {
    fname <- here("data", "BIDS", name,
                  paste0("sub-", name, "_task-gen_run-0", x, "_events.tsv"))
    vname <- dfnames[x + 2]
    write.table(vname, fname, sep = "\t", row.names = FALSE)
  }
  rm(f, b_c_run_1, b_c_run_2, b_g_run_1, b_g_run_2, b_g_run_3, b_g_run_4)
}
