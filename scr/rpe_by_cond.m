function [rpes] = rpe_by_cond(data)
     trust = subset_by_tt(data, 'Trustworthy').pe;
     untrust = subset_by_tt(data, 'Untrustworthy').pe;
     neutral = subset_by_tt(data, 'Neutral').pe;
     rpes = {trust, untrust, neutral}
end