create_data_dictionary <- function(dt) {
  dd <- t(dt[1,])
  dd <- cbind(rownames(dd), data.frame(dd, row.names = NULL))
  colnames(dd) <- c('Variable Name', 'Variable')
  dd$Variable <- NA
  return(dd)
}

