# Conert raw socialAL behavior into BIDS format
# 5.9.19 KLS

# load required packages
#library()
# load source functions
source('scr/cond_phase_org.R')
source('scr/gen_phase_org.R')
source('scr/isolate_run.R')
source('scr/convert_to_bids.R')
# set hard-coded variables

# read data in 
dt <- read.csv('~/Box/SocialAL/task/data/1004_socialALscanner_FINAL_v2_2019_Apr_15_0936.csv')[5:73]

# isolate conditioning runs and convert to bids
c_run_1 <- isolate_run(dt, 'cond', 1); b_c_run_1 <-convert_to_bids(c_run_1, 'cond'); rm(c_run_1)
c_run_2 <- isolate_run(dt, 'cond', 2); b_c_run_2 <-convert_to_bids(c_run_2, 'cond'); rm(c_run_2)

# isolate generalization trials and remove empty rows 
g_run_1 <- isolate_run(dt, 'gen',1); b_g_run_1 <- convert_to_bids(g_run_1, 'gen'); rm(g_run_1)
g_run_2 <- isolate_run(dt, 'gen',2); b_g_run_2 <- convert_to_bids(g_run_2, 'gen'); rm(g_run_2)
g_run_3 <- isolate_run(dt, 'gen',3); b_g_run_3 <- convert_to_bids(g_run_3, 'gen'); rm(g_run_3)
g_run_4 <- isolate_run(dt, 'gen',4); b_g_run_4 <- convert_to_bids(g_run_4, 'gen'); rm(g_run_4)

# save with BIDS filenames
name <- dt$participant[1]
for (x in 1:2) {
  fname <- paste0('output/BIDS/sub-', name, '_task-cond_run-0', x, '_bold.tsv')
  vname <- paste0('c_run_', x)
  write.csv(vname, fname, row.names = FALSE)
}
for (x in 1:4) {
  fname <- paste0('output/BIDS/sub-', name, '_task-gen_run-0', x, '_bold.tsv')
  vname <- paste0('g_run_', x)
  write.csv(vname, fname, row.names = FALSE)
}

