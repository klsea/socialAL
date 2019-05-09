gen_phase_org <- function(data, event) {
  st <- data$triggerGenStartTime[1]
  tt <- data$condition
  sfl <- data$leftStim
  sfr <- data$rightStim
  rt <- data$gTrialResponse.rt
  resp <- data$gTrialResponse.keys
  if (event == 'view') {
    e <- list(o = data$gTrialStartTime - st, d = 2, mc = 'View')
  } else if (event == 'decision') {
    e <- list(o = (data$gTrialStartTime - st) + 2, d = 4, mc = 'Decision')
  } else {
    e <- NULL
  }
  names <- c('onset', 'duration', 'event', 'trial_type', 'response_time', 'response_key','stim_file_left', 'stim_file_right')
  dt <- data.frame(e$o, e$d, e$mc, tt, rt, resp, sfl,sfr); colnames(dt) <- names
  return(dt)
}