convert_to_bids <- function(data,phase) {
  if (phase == 'cond') {
    d <- cond_phase_org(data, 'decision')
    r <- cond_phase_org(data, 'response')
    t <- cond_phase_org(data, 'thinking')
    f <- cond_phase_org(data, 'feedback')
    df <- rbind(d,r,t,f); df <- df[order(df$onset),]
    return(df)
  } else {
    v <- gen_phase_org(data, 'view')
    d <- gen_phase_org(data, 'decision')
    df <- rbind(v,d); df <- df[order(df$onset),]
    return(df)
  }
}