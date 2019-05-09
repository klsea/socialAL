cond_phase_org <- function(data, event) {
  # Takes condition phase data and puts it in BIDS format
  names <- c('onset', 'duration', 'event', 'trial_type', 'response_time', 'response_key', 'stim_file', 'trial_earnings')
  st <- data$triggerCondStartTime[1]
  tt <- data$condition
  sf <- data$stim
  te <- data$trialEarnings
  rt = data$cTrialResponse.rt
  resp = data$cTrialResponse.keys
  if (event == 'decision') {
    e <- list(o = data$cTrialStartTime - st, d = data$cTrialResponse.rt,  mc = 'Decision')
  } else if (event == 'response') {
    e <- list(o = data$cResponseStartTime - st, d = data$cResponseDuration, mc = 'Response')
  } else if (event == 'thinking') {
    e <- list(o = data$cThinkStartTime - st, d = data$cISIJitter, mc = 'Thinking')
  } else if (event == 'feedback') {
    e <- list(o = data$cFeedbackStartTime - st, d = 2, mc = 'Feedback')
  } else {
    e <- NULL
  }
  dt <- data.frame(e$o, e$d, e$mc, tt, rt, resp, sf, te); colnames(dt) <- names
  return(dt)
}