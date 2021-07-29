% create pmod .mat files for SPM
% KLS 7.29.21

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
     cd('output/eventfiles/rl4/')
     if 7~=exist([part{i}], 'dir')
         fprintf('make new folder!')
         fprintf('\n')
         mkdir(part{i});
     end
     cd(socialAL)

     % read in data
     combined = readtable(join([socialAL, '/output/eventfiles/glm/', part{i}, '/', part{i}, '_combined.txt']));
     combined.event = categorical(cellstr(combined.event));
     subset = subset_by_event(combined, 'Feedback');
    
     % create trust/untrust columns
     trust = find(strcmp(subset.trial_type, 'Trustworthy'))
     subset.trust(trust) = 1
     untrust = find(strcmp(subset.trial_type, 'Untrustworthy'))
     subset.untrust(untrust) = 1
     clear combined trust untrust
     
     % make feedback file with rpe
     % load old pmod file
     load(join(['output/eventfiles/rl2/', part{i}, '/', part{i}, '_feedback_rpe.mat']));
     oldpmod = pmod
     
     % replace the pmod 
     pmod = struct('name', {''}, 'param', {}, 'poly',{});
     pmod(1).name{1} = ['rpe'];
     pmod(1).param{1} = oldpmod.param{1,1};
     pmod(1).poly{1} = 1;
     pmod(1).name{2} = ['trust'];
     pmod(1).param{2} = transpose(subset.trust);
     pmod(1).poly{2} = 1;
     pmod(1).name{3} = ['untrust'];
     pmod(1).param{3} = transpose(subset.untrust);
     pmod(1).poly{3} = 1;
     clear oldpmod 
     
     fname = join([socialAL, '/output/eventfiles/rl4/', part{i}, '/', part{i}, '_feedback_rep.mat']);
     save(fname, 'names', 'durations', 'onsets', 'pmod');
     clear fname names durations onsets pmod

     % create pmod structure for probability of reciprocation
     % load old pmod file
     load(join(['output/eventfiles/rl2/', part{i}, '/', part{i}, '_decision_prob.mat']));
     oldpmod = pmod
     
     % replace the pmod 
     pmod = struct('name', {''}, 'param', {}, 'poly',{});
     pmod(1).name{1} = ['prob'];
     pmod(1).param{1} = oldpmod.param{1,1};
     pmod(1).poly{1} = 1;
     pmod(1).name{2} = ['trust'];
     pmod(1).param{2} = transpose(subset.trust);
     pmod(1).poly{2} = 1;
     pmod(1).name{3} = ['untrust'];
     pmod(1).param{3} = transpose(subset.untrust);
     pmod(1).poly{3} = 1;
     clear oldpmod subset
     
     fname = join([socialAL, '/output/eventfiles/rl4/', part{i}, '/', part{i}, '_decision_prob.mat']);
     save(fname, 'names', 'durations', 'onsets', 'pmod');
     clear fname names durations onsets pmod subset
end
