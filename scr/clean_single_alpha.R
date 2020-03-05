clean_single_alpha <- function(data) {
  data$subnum <- t(as.data.frame(strsplit(as.character(data$id), '-')))[,2]
  data$agegrp <- ifelse(as.numeric(data$subnum) < 2000, 'Younger', 'Older')
  data$agegrp <- factor(data$agegrp)
  data$agegrp <- factor(data$agegrp, levels = c('Younger', 'Older'))
  data$subnum <- NULL
  #data <- gather(data, parameter, estimate, alpha:beta)
  return(data)
}

