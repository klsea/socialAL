add_tt_number <- function(data) {
  # creates a new column for sequence of trials within a condition
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

add_stage <- function(data) {
  # creates a new column for stage within a condition
  subs <- unique(data$id)
  d1 <- data.frame()
  for (s in subs) {
    d2 <- data[which(data$id == s),]
    d2 <- d2[order(d2$trial_number),]
    d2$stage <- NA
    d2[which(d2$trial_type == 'Trustworthy'),]$stage <- c(rep(1,5), rep(2,5), rep(3,5))
    d2[which(d2$trial_type == 'Neutral'),]$stage <- c(rep(1,5), rep(2,5), rep(3,5))
    d2[which(d2$trial_type == 'Untrustworthy'),]$stage <- c(rep(1,5), rep(2,5), rep(3,5))
    d1 <- rbind(d1,d2)
  }
  return(d1)
}