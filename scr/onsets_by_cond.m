function [onsets] = onsets_by_cond(data)
     trust = subset_by_tt(data, 'Trustworthy').onset;
     untrust = subset_by_tt(data, 'Untrustworthy').onset;
     neutral = subset_by_tt(data, 'Neutral').onset;
     onsets = {trust, untrust, neutral}
end 
