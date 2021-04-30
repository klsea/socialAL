
function [data] = replace_data(data)
    data(:,2) = fillmissing(data(:,2), 'constant', '-99');
    data.duration = str2num(data.duration);
    data(:,5) = fillmissing(data(:,5), 'constant', '-99');
    data.response_time = str2num(data.response_time);
    data(:,6) = fillmissing(data(:,6), 'constant', '-99');
    data.response_key = str2num(data.response_key);
    data = standardizeMissing(data,-99);
end


