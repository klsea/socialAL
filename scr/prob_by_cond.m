function [probs] = prob_by_cond(data)
     trust = subset_by_tt(data, 'Trustworthy').prob_recip;
     untrust = subset_by_tt(data, 'Untrustworthy').prob_recip;
     neutral = subset_by_tt(data, 'Neutral').prob_recip;
     probs = {trust, untrust, neutral}
end