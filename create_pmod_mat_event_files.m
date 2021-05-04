% create pmod .mat files for SPM
% KLS 4.30.21

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

for i = 1:length(part)
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
     
     % create pmod structure
     pmod(1).name = {'trust_order'}
     pmod(2).name = {'untrust_order'}
     pmod(3).name = {'neutral_order'}
     for j = 1:3
         pmod(j).param = {[1:15]}
         pmod(j).poly = {[1]}
     end
     clear j
     
     % make feedback file with no responses
     load(join(['output/eventfiles/glm/', part{i}, '/', part{i}, '_feedback.mat']));
     fname = join([socialAL, '/output/eventfiles/pmod/', part{i}, '/', part{i}, '_feedback.mat']);
     save(fname, 'names', 'durations', 'onsets', 'pmod')
     clear fname names durations onsets

     % make decision file with no responses
     load(join(['output/eventfiles/glm/', part{i}, '/', part{i}, '_decision.mat']));     
     fname = join([socialAL, '/output/eventfiles/pmod/', part{i}, '/', part{i}, '_decision.mat']);
     save(fname, 'names', 'durations', 'onsets', 'pmod')
     clear fname names durations onsets
end
