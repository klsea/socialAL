function [lik] = FitSocialAL(etaG,etaL,beta)

global data  

% Find the log likelihood of the choice data under model adapted from
% Fareri et al, 2012 
% Lik = - log likelihood of the data

lik = 0;   % log likelihood

%0 3 6 9 
V_A = [9, 9 ,9 ,9];  % initial values of sharing each amount given 50% belief of reciprocation 
V_B = [9, 9 ,9 ,9];
V_C = [9, 9 ,9 ,9];
beliefA = .5; %initialize each belief of reciprocation at .5 
beliefB = .5; 
beliefC = .5; 

for t = 1:length(data)  % for all of the trials 
  trialtype = data(t, 2); %trialtype read in as 1 2 or 3 
  c = data(t, 3); %choice 1-4 
  outcome = data(t, 4); %what they got  
 if c > 0 %if they made a choice, excluding the trials where they timed out 
  if outcome > 9 %if parter reciprocated 
    y = 1; 
  else
     y = 0; 
  end 
     if trialtype == 1 
      %update log likelihood based on their choice, using values for trial
      %type 1 
      lik = lik + (V_A(c)*beta) - logsumexp(V_A*beta,2); 
      %note that this could be replaced with calculation of each choice probability 
      %%using the softmax function and if statements.  
      
      %update belief if they shared, will be used to calculate softmax on the next trial of this type  
      if c > 1 %if they shared-- if they did not share, expectations to not need to be updated 
      gain = [(y-beliefA), 0]; 
      loss = [(y-beliefA), 0]; 
      beliefA = beliefA + (etaG*max(gain)) + (etaL*min(loss)); 
      end 
      %update values
      V_A(1) = 9; 
      V_A(2) = 6 + (beliefA*6); 
      V_A(3) = 3 + (beliefA*12); 
      V_A(4) = 0 + (beliefA*18); 
       
      
  elseif trialtype == 2 
      %update log likelihood 
      lik = lik + (V_B(c)*beta) - logsumexp(V_B*beta,2); 
      %update probability 
      if c > 1 %if they shared-- if they did not share, expectations to not need to be updated 
      gain = [(y-beliefB), 0]; 
      loss = [(y-beliefB), 0]; 
      beliefB = beliefB + (etaG*max(gain)) + (etaL*min(loss)); 
      end 
      %update values
      V_B(1) = 9; 
      V_B(2) = 6 + (beliefB*6); 
      V_B(3) = 3 + (beliefB*12); 
      V_B(4) = 0 + (beliefB*18); 
      
  elseif trialtype == 3 
       %update log likelihood 
      lik = lik + (V_C(c)*beta) - logsumexp(V_C*beta,2); 
      %update probability 
      if c > 1 %if they shared-- if they did not share, expectations to not need to be updated 
          gain = [(y-beliefC), 0]; 
          loss = [(y-beliefC), 0]; 
          beliefC = beliefC + (etaG*max(gain)) + (etaL*min(loss)); 
      end 
      %update values
      V_C(1) = 9; 
      V_C(2) = 6 + (beliefC*6); 
      V_C(3) = 3 + (beliefC*12); 
      V_C(4) = 0 + (beliefC*18); 
      
     end
 end 
end 

lik = -lik;  % so we can minimize the function rather than maximize