# Conversion function (from Alex)
convert <- function (data, sub)
{
  data <- data[,c("trial_type","duration","onset","event","response_key")]
  data <- apply(data,2,as.character)
  data[,c("duration","onset")] <- suppressWarnings(as.numeric(data[,c("duration","onset")]))
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data$duration <- as.numeric(data$duration)
  data$onset <- as.numeric(data$onset)
  
  return(data)
}
