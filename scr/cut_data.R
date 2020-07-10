cut_data <- function (data, cut) {
  ### Inputs:
  ### a data set, a vector of subject numbers, and a data set with age
  ### Outputs: 
  ### new data set without "cut" subjects 
  d1 <- data[which(! data$id %in% cut),]
  return(d1)
}
