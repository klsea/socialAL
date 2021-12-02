% create pmod .mat files for SPM
% KLS 5.4.21

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
% remove participants cut for poor performance
part(find(strcmp(part, 'sub-1027'))) = [];
part(find(strcmp(part, 'sub-1031'))) = [];
part(find(strcmp(part, 'sub-1040'))) = [];
part(find(strcmp(part, 'sub-2008'))) = [];
part(find(strcmp(part, 'sub-2014'))) = [];
part(find(strcmp(part, 'sub-2015'))) = [];
part(find(strcmp(part, 'sub-2016'))) = [];
part(find(strcmp(part, 'sub-2029'))) = [];
part(find(strcmp(part, 'sub-2032'))) = [];

for i = 1:length(part)
     fprintf('Now on ')
     fprintf(part{i})
     fprintf('\n')
     
     %make new directory (if needed)
     cd('output/eventfiles/rl2/')
     if 7~=exist([part{i}], 'dir')
         fprintf('make new folder!')
         fprintf('\n')
         mkdir(part{i});
     end
     cd(socialAL)
     
     % make durations
     durations = {0}
     
     % read in data
     combined = readtable(join([socialAL, '/output/eventfiles/glm/', part{i}, '/', part{i}, '_combined.txt']));
     combined.event = categorical(cellstr(combined.event));
     combined.trial_type = categorical(cellstr(combined.trial_type));
     fdb = subset_by_event(combined, 'Feedback');
     dec = subset_by_event(combined, 'Decision'); 
     
     % read in rl model estimates
     fname = join([socialAL, '/output/trial_estimates/', part{i}, '.csv']);
     rl = readtable(fname);
     
     % create pmod structure for PE
     pmod(1).name = {'rpe'}
     names = {'rpe'}
     onsets = {fdb.onset}

     % make feedback file with rpe
     pmod(1).param = cellfun(@transpose, {rl.pe}, 'UniformOutput', false);
     pmod(1).poly = {[1]};
     fname = join([socialAL, '/output/eventfiles/rl2/', part{i}, '/', part{i}, '_feedback_rpe.mat']);
     save(fname, 'names', 'durations', 'onsets', 'pmod');
     clear fname names onsets pmod

     % create pmod structure for probability of reciprocation
     pmod(1).name = {'prob'}
     names = {'prob'}
     onsets = {dec.onset}
     
     % make decision file with prob
     pmod(1).param = cellfun(@transpose, {rl.prob_recip}, 'UniformOutput', false);   
     pmod(1).poly = {[1]};
     
     fname = join([socialAL, '/output/eventfiles/rl2/', part{i}, '/', part{i}, '_decision_prob.mat']);
     save(fname, 'names', 'durations', 'onsets', 'pmod');
     clear fname names durations onsets
end
