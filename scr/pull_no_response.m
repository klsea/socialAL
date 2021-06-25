function[data, noresp] = pull_no_response(data)
     if any(isnan(data.response_key), 'all')
         disp('No response')
         drop = isnan(data.response_key); %identify no response trials
         noresp = data(drop, :).onset + 2; % create no response matrix
         data(drop,:) = []; % pull out no response trials
     else
         disp('All responses recorded')
         if i <= 16 % I have no idea why this code is here. Ask Brittany/Alex to explain
             noresp = data.onset(3) + 2;
         else
             noresp = data.onset(5) + 2;
         end   
     end

end