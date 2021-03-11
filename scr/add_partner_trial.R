add_partner_trial <- function(data) {
# function that takes a data frame (data) 
  # adds a column to track partner interactions and
  # standardizes that column
  nrow(data)/45 # verify all participants have complete data set

  d1 <- data[order(data$id, data$trial_type, data$trial_number),]
  d1 <- d1 %>%
    group_by(id, trial_type) %>%
    mutate(partner_trial_number = seq(1:15))
  d1$z_partner_trial_number <- scale(d1$partner_trial_number, scale = TRUE) # standardize partner trial #
  return(d1)
}