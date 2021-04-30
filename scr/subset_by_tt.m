function [new] = subset_by_tt(data, tt)
    sub_index = data.trial_type == tt;
    new = data(sub_index,:);
end