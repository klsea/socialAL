concat_clean <- function(file_list) {
  library(here)
  library(tidyverse)
  dt <- data.frame()
  for (f in file_list) {
    dt1 <- read.csv(here::here('data', 'modeling', f))
    sub <- strsplit(f, '[.]')[[1]][1] # pulls sub number out of file name
    dt1$id <- sub
    dt1$grp <- floor(as.numeric(strsplit(sub, '[-]')[[1]][2])/1000) #1 is YA, 2 is OA
    dt <- rbind(dt,dt1)
  }

  # Convert response_key to $$ shared
  dt <- dt %>% 
    mutate(amount_shared = (as.numeric(response_key) - 1) * 3)
  
  # Convert group # to meaningful label
  dt <- dt %>% 
    mutate(agegrp = ifelse(grp == 1, 'Younger', 'Older'))
  dt$agegrp <- factor(dt$agegrp)
  
  # make subjective dientifier a factor
  dt$id <- factor(dt$id)
  
  # reorder trial_type and agegrp factors
  dt$trial_type <- factor(dt$trial_type, levels = c("Untrustworthy", "Neutral", "Trustworthy"))
  dt$agegrp <- factor(dt$agegrp, levels = c("Younger", "Older"))
  
  return (dt)
}

clean_param <- function(data) {
  data$sub <- as.character(data$id)
  data <- separate(data, into = c('A', 'n'), col = sub, sep = '[-]')
  data <- data %>% 
    mutate(grp = floor(as.numeric(n)/1000)) %>%
    mutate(agegrp = ifelse(grp == 1, 'Younger', 'Older'))
  data$agegrp <- factor(data$agegrp)
  data$agegrp <- factor(data$agegrp, levels = c('Younger', 'Older'))
  data$A <- NULL; data$n <- NULL; data$grp <- NULL
  data <- data[c(1, ncol(data), 2:(ncol(data)-1))]
  return(data)
}
