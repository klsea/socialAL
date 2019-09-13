add_tt_number <- function(data) {
  # creates a new column 
  subs <- unique(data$id)
  d1 <- data.frame()
  for (s in subs) {
    d2 <- data[which(data$id == s),]
    d2 <- d2[order(d2$trial_number),]
    d2$tt_number <- NA
    d2[which(d2$trial_type == 'Trustworthy'),]$tt_number <- seq(1,15)
    d2[which(d2$trial_type == 'Neutral'),]$tt_number <- seq(1,15)
    d2[which(d2$trial_type == 'Untrustworthy'),]$tt_number <- seq(1,15)
    d1 <- rbind(d1,d2)
  }
  return(d1)
}
