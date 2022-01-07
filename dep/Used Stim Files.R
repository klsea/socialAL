# set working directory and path
path <- "D:/Box/SocialAL/"
setwd(path)

# List files
files <- list.files("./ScanningData/BIDS")

# grab only subjects
subs <- files[grep("sub",files)]

# initialize results list
res <- vector("list", length = length(subs))

# loop through subs
for(i in subs)
{
  # set path
  setwd(paste(path,"ScanningData/BIDS/",i,"/func/",sep=""))
  
  # functional files
  func.files <- list.files()
  
  # grab only cond and gen events
  cond <- func.files[grep("cond",func.files)]
  cond <- cond[grep("events",cond)]
  gen <- func.files[grep("gen",func.files)]
  gen <- gen[grep("events",gen)]
  
  # stim files
  stim <- vector("list", length = 6)
  
  # initialize count
  count <- 0
  
  # loop through cond files
  for(j in 1:length(cond))
  {
    count <- count + 1
    
    stim[[count]] <- as.character(read.table(cond[j], header = TRUE, sep = "\t")[,"stim_file"])
  }
  
  # loop through gen files
  for(j in 1:length(gen))
  {
    count <- count + 1
    
    left <- as.character(read.table(gen[j], header = TRUE, sep = "\t")[,"stim_file_left"])
    right <- as.character(read.table(gen[j], header = TRUE, sep = "\t")[,"stim_file_right"])
    
    stim[[count]] <- c(left,right)
  }
  
  # grab only unique files
  res[[i]] <- unique(unlist(stim))
  
}

# grab only unique files across all participants
used <- unique(unlist(res))

# create new directory for stimuli in BIDS
dir.create("./ScanningData/BIDS/stimuli/images")

# loop through to copy and paste only the used
# files into the images folder
for(i in 1:length(used))
{file.copy(from = paste("./task/",used[i],sep=""), to = "./images")}


