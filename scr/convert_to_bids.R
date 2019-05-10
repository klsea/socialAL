convert_to_bids <- function(data, phase) {
  source("scr/org_cond_phase.R")
  source("scr/org_gen_phase.R")
  if (phase == "cond") {
    d <- org_cond_phase(data, "decision")
    r <- org_cond_phase(data, "response")
    t <- org_cond_phase(data, "thinking")
    f <- org_cond_phase(data, "feedback")
    df <- rbind(d, r, t, f); df <- df[order(df$onset), ]
    return(df)
  } else {
    v <- org_gen_phase(data, "view")
    d <- org_gen_phase(data, "decision")
    df <- rbind(v, d); df <- df[order(df$onset), ]
    return(df)
  }
}
