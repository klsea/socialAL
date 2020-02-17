clean_single_alpha <- function(data) {
  data$jac <- gsub('\\[', '', data$jac)
  data$jac <- gsub('\\]', '', data$jac)
  data$jac <- trimws(data$jac)
  param = strsplit(as.character(data$jac), ' ')
  param <- param[length(param[]) > 0 ]
  
  for (sub in 1:length(param)) {
    #print(paste0('Sub is: ', sub))
    l <- length(param[sub][[1]])
    #print(paste0('Length is: ', l))
    if(l > 2) {
      #print('This participant is too long!')
      param[sub][[1]] <- param[sub][[1]][ param[sub][[1]] != ""]
    }
  }
  
  data$alpha <- as.numeric(t(as.data.frame(param))[,1])
  data$beta <- as.numeric(t(as.data.frame(param))[,2])
  data$LLH <- data$fun
  data$subnum <- t(as.data.frame(strsplit(as.character(data$id), '-')))[,2]
  data$agegrp <- ifelse(as.numeric(data$subnum) > 2000, 'Older', 'Younger')
  d3 <- data[c(1,8, 4:6)]
  d3 <- gather(d3, parameter, estimate, alpha:beta)
  return(d3)
}

