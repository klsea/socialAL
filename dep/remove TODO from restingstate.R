# Remove TODO column from resting-state files

# grab files
files <- list.files("./ScanningData/BIDS")

# grab only subject files
subs <- files[grep("sub",files)]

# loop through subjects
for(i in subs)
{
  # set working directory
  setwd("D:/Box/SocialAL/")
  
  # change directory
  setwd(paste("./ScanningData/Bids/",i,"/func/",sep=""))
  
  # grab event --> rest files
  func.files <- list.files()
  event <- func.files[grep("events.tsv",func.files)]
  rest <- event[grep("rest",event)]
  
  # loop through resting state files
  for(j in 1:length(rest))
  {
    new.file <- suppressWarnings(t(as.matrix(colnames(read.csv(rest[j], sep="\t")[-6]))))
    
    write.table(new.file, file = rest[j], sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
}
