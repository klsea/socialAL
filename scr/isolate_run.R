isolate_run <- function(data, phase, run) {
  if (phase == 'cond') {
    if (run == 1) {
      ntrial = 21 
    } else {
      ntrial = 22 }
    row <- grep(ntrial, dt$cNumTrials); full_run <- dt[row:(row+ntrial), ]
    return(full_run)
  } else if (phase == 'gen') {
    ntrial = 27
    row <- grep(run, dt$gRunNum); full_run <- dt[row:(row+ntrial), ]
    return(full_run)
  }
}