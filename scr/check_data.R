check_data <- function(data) {
  # this function checks to make sure participants are not pressing the same buttons
  # returns a data frame with one value for each participant in the repeater column
  # 1 = all the same response
  # 0 = different responses
  same <- data.frame()
  for (s in 1:length(unique(data$id))) {
    d1 <- data[which(data$id == unique(data$id)[s]), ]
    same[s,1] <- unique(data$id)[s]
    same[s,2] <- ifelse(all(d1$response_key == lead(d1$response_key)), 1,0)
  }
  colnames(same) <- c('Participant', 'Repeater')
  return(same)
}


