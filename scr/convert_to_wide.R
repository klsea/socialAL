convert_to_wide <- function(data) {
  # Takes condition phase data and puts it in minimum format
  names <- c("trial_number", "trial_type", "response_key", "response_time", "trial_earnings")
  nt <- seq(1,nrow(data))
  tt <- data$condition
  resp <- data$cTrialResponse.keys
  rt <- data$cTrialResponse.rt
  te <- data$trialEarnings
  dt <- data.frame(nt, tt, resp, rt, te); colnames(dt) <- names
  return(dt)
}
