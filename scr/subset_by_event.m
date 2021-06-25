function [new] = subset_by_event(data, ename)
    sub_index = data.event == ename;
    new = data(sub_index,:);
end