function [onsets] = responses_by_cond(data)
     trust = subset_by_tt(data, 'Trustworthy').response;
     untrust = subset_by_tt(data, 'Untrustworthy').response;
     neutral = subset_by_tt(data, 'Neutral').response;
     onsets = {trust, untrust, neutral}
end