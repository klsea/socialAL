# Conert raw socialAL behavior into BIDS format
# 5.9.19 KLS

# load required packages
library(here)

# load source functions
source("./scr/org_cond_phase.R")
source("./scr/org_gen_phase.R")
source("./scr/isolate_run.R")
source("./scr/convert_to_bids.R")

# set hard-coded variables
path_to_data <- "./task/data/"

# read data in
files <- list.files(path_to_data, pattern = ".csv")
#files <- files[- (grep("button", files))]

for (f in files[1:2]) {
  dt <- read.csv(paste0(path_to_data, f))
  
  # isolate conditioning runs and convert to bids
  c_run_1 <- isolate_run(dt, "cond", 1)
  b_c_run_1 <- convert_to_bids(c_run_1, "cond"); rm(c_run_1)
  c_run_2 <- isolate_run(dt, "cond", 2)
  b_c_run_2 <- convert_to_bids(c_run_2, "cond"); rm(c_run_2)
  
  # isolate generalization trials and remove empty rows
  
  # removes tasks that were not completed (e.g., 1036)
  miss <- function(...){tryCatch(..., warn=FALSE, error=function(e) return(NA))}
  
  g_run_1 <- miss(isolate_run(dt, "gen", 1))
  b_g_run_1 <- miss(convert_to_bids(g_run_1, "gen")); rm(g_run_1)
  g_run_2 <- miss(isolate_run(dt, "gen", 2))
  b_g_run_2 <- miss(convert_to_bids(g_run_2, "gen")); rm(g_run_2)
  g_run_3 <- miss(isolate_run(dt, "gen", 3))
  b_g_run_3 <-miss(convert_to_bids(g_run_3, "gen")); rm(g_run_3)
  g_run_4 <- miss(isolate_run(dt, "gen", 4))
  b_g_run_4 <- miss(convert_to_bids(g_run_4, "gen")); rm(g_run_4)
  
  # save with BIDS filenames
  name <- dt$participant[1]
  #dir.create(here("data", "BIDS", name))
  dfnames <- list(b_c_run_1, b_c_run_2, b_g_run_1, b_g_run_2,
                  b_g_run_3, b_g_run_4)
  
  dfnames <- dfnames[which(!is.na(dfnames))]
  
  # convert NA to n/a
  na.convert <- function(vname)
  {
    # enter list
    res <- vname[[1]]
    
    if(any(is.na(res)))
    {
      # remove factors and white space
      res <- trimws(apply(res,2,as.character))
      
      # NA indicies
      ind <- which(is.na(res), arr.ind = TRUE)
      
      # loop through
      for(i in 1:nrow(ind))
      {res[ind[i,1],ind[i,2]] <- "n/a"}
    }
    
    return(list(res))
  }
  
  for (x in 1:2) {
    fname <- here("ScanningData", "BIDS", paste("sub-",name,sep=""), "func",
                  paste0("sub-", name, "_task-cond_run-0", x, "_events.tsv"))
    vname <- dfnames[x]
    write.table(na.convert(vname), fname, sep = "\t", row.names = FALSE, quote = FALSE)
  }
  
  for (x in 1:(length(dfnames)-2)) {
    fname <- here("ScanningData", "BIDS", paste("sub-",name,sep=""), "func",
                  paste0("sub-", name, "_task-gen_run-0", x, "_events.tsv"))
    vname <- dfnames[x + 2]
    write.table(na.convert(vname), fname, sep = "\t", row.names = FALSE, quote = FALSE)
  }
  rm(f, b_c_run_1, b_c_run_2, b_g_run_1, b_g_run_2, b_g_run_3, b_g_run_4)
}
