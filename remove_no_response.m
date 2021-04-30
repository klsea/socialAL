% create pmod .mat files for SPM
% KLS 4.29.21

% add path to functions
addpath('scr/')

% set hard-coded variables
socialAL = pwd; % set current directory
addpath(socialAL)

% participant list
cd('output/eventfiles/glm/')
files = dir('sub*');
part = cell(1,72);
[part{:}] = files(1:72).name;
clear files
cd(socialAL)

for i = 1:3%length(part)
     fprintf('Now on ')
     fprintf(part{i})
     fprintf('\n')
     
     %make new directory (if needed)
     cd('output/eventfiles/pmod/')
     if 7~=exist([part{i}], 'dir')
         fprintf('make new folder!')
         fprintf('\n')
         mkdir(part{i});
     end
     cd(socialAL)
     
     % read in data
     combined = readtable(join([socialAL, '/output/eventfiles/glm/', part{i}, '/', part{i}, '_combined.txt']));
     combined.event = categorical(cellstr(combined.event));
     combined.trial_type = categorical(cellstr(combined.trial_type));
     
     % make feedback file with no responses
     load(join(['output/eventfiles/glm/', part{i}, '/', part{i}, '_feedback.mat']));

%      names{1,4} = 'noresp';
%      durations{1,4} = 0;
     
%      fdb = subset_by_event(combined, 'Feedback');
%      [fdb, noresp] = pull_no_response(fdb); % function that pulls out no response trials
%      
%      trust = subset_by_tt(fdb, 'Trustworthy').onset;
%      untrust = subset_by_tt(fdb, 'Untrustworthy').onset;
%      neutral = subset_by_tt(fdb, 'Neutral').onset;
    
     % save pmod files for feedback
%      fname = join([socialAL, '/output/eventfiles/pmod/', part{i}, '/', part{i}, '_feedback.mat']);
%      save(fname, 'names', 'durations', 'trust', 'untrust', 'neutral', 'noresp')
%      clear fdb fname names durations onsets noresp trust untrust neutral
    
     % make decision file with no responses
     load(join(['output/eventfiles/glm/', part{i}, '/', part{i}, '_decision.mat']));
     names{1,4} = 'noresp';
     durations{1,4} = 0;
     
%      dec = subset_by_event(combined, 'Decision');
%      [dec, noresp] = pull_no_response(dec); % function that pulls out no response trials
%      
%      trust = subset_by_tt(dec, 'Trustworthy').onset;
%      untrust = subset_by_tt(dec, 'Untrustworthy').onset;
%      neutral = subset_by_tt(dec, 'Neutral').onset;
     
     % save pmod files for feedback
%      fname = join([socialAL, '/output/eventfiles/pmod/', part{i}, '/', part{i}, '_decision.mat']);
%      save(fname, 'names', 'durations', 'trust', 'untrust', 'neutral', 'noresp')
%      clear dec fname names durations onsets noresp trust untrust neutral
     
     % make decision start file with no responses
     
     load(join(['output/eventfiles/glm/', part{i}, '/', part{i}, '_decision_start.mat']));
     names{1,4} = 'noresp';
     durations{1,4} = 0;      
     ds = subset_by_event(combined, 'Decision');
     
%      ds.response = ds.onset + ds.duration;
%      [ds, noresp] = pull_no_response(ds); % function that pulls out no response trials - 
%      % does it matter that onsets are from feedback file? Ask Alex/Brittany
%       
%      trust = subset_by_tt(ds, 'Trustworthy').response;
%      untrust = subset_by_tt(ds, 'Untrustworthy').response;
%      neutral = subset_by_tt(ds, 'Neutral').response;
     
     % save pmod files for feedback
%      fname = join([socialAL, '/output/eventfiles/pmod/', part{i}, '/', part{i}, '_decision_start.mat']);
%      save(fname, 'names', 'durations', 'trust', 'untrust', 'neutral', 'noresp')
%      clear ds fname names durations onsets noresp trust untrust neutral combined
end
