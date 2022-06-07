% create pmod .mat files for SPM
% KLS 6.1.22

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
     cd('output/eventfiles/gl/')
     if 7~=exist([part{i}], 'dir')
         fprintf('make new folder!')
         fprintf('\n')
         mkdir(part{i});
     end
     cd(socialAL)
     
     % read in rl model estimates
     fname = join([socialAL, '/output/gain_loss_trial_estimates/', part{i}, '.csv']);
     rl = readtable(fname);
     
     % read in data
     combined = readtable(join([socialAL, '/output/eventfiles/glm/', part{i}, '/', part{i}, '_combined.txt']));
     combined.event = categorical(cellstr(combined.event));
     combined.trial_type = categorical(cellstr(combined.trial_type));
     fdb = subset_by_event(combined, 'Feedback');
     
     % merge rl estimates with table
     table = [fdb, rl];
     clear combined fdb fname rl
     probs = prob_by_cond(table);
     rpes = rpe_by_cond(table);
     
     % create pmod structure
     pmod(1).name = {'trust_order'};
     pmod(2).name = {'untrust_order'};
     pmod(3).name = {'neutral_order'};
     
     % make feedback file with rpe
     load(join(['output/eventfiles/glm/', part{i}, '/', part{i}, '_feedback.mat']));
     for j = 1:3
         pmod(j).param = cellfun(@transpose, rpes(j), 'UniformOutput', false);
         pmod(j).poly = {[1]};
     end
     clear j
     fname = join([socialAL, '/output/eventfiles/gl/', part{i}, '/', part{i}, '_feedback_rpe.mat']);
     save(fname, 'names', 'durations', 'onsets', 'pmod');
     clear fname names durations onsets

     % make decision file with prob
     load(join(['output/eventfiles/glm/', part{i}, '/', part{i}, '_decision.mat']));  
     for j = 1:3
         pmod(j).param = cellfun(@transpose, probs(j), 'UniformOutput', false);   
         pmod(j).poly = {[1]};
     end
     fname = join([socialAL, '/output/eventfiles/gl/', part{i}, '/', part{i}, '_decision_prob.mat']);
     save(fname, 'names', 'durations', 'onsets', 'pmod');
     clear fname names durations onsets
end
