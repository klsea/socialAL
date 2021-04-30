% create glm .mat files for SPM
% KLS 4.27.21

% add path to functions
addpath('scr/')

% set hard-coded variables
socialAL = pwd; % set current directory
addpath(socialAL)

% grab files
bids = '../../Box/SocialAL/ScanningData/BIDS/'; %path to scanning data
addpath(bids)
cd(bids)
files = dir('sub*');

% participant list
cd(socialAL)
part = cell(1,72);
[part{:}] = files(1:72).name;
clear files
%part(find(strcmp(part, 'sub-2032'))) = [];

% .mat files
mat = {'run-01', 'run-02'};

% loop for each participant 

for i = 1:length(part)
     fprintf('Now on ')
     fprintf(part{i})
     fprintf('\n')
     cd('output/eventfiles/glm/')
     if 7~=exist([part{i}], 'dir')
         fprintf('make new folder!')
         fprintf('\n')
         mkdir(part{i});
     end
    cd(socialAL)

    for j = 1:2 % loop for each scan run
        path = join([bids, part{i}, '/func/', part{i}, '_task-cond_', mat{j}, '_events.tsv']);
        if j == 1
            event = struct2table(tdfread(path, 'tab'));
            % deal with missing values
            if any(ismissing(event.duration), 'all')
                disp('Missing data in file 1')
                event = standardizeMissing(event, {'n/a', 'None'});
                event = replace_data(event);
            end

        else
        % add 348 to onset for sub-1004 through sub-1022, 350 for all others
            add = struct2table(tdfread(path, 'tab'));
            % deal with missing values
            if any(ismissing(add.duration), 'all')
                disp('Missing data in file 2')
                add = standardizeMissing(add, {'n/a', 'None'});
                add = replace_data(add);
            end
             if i <= 18 
                 a = 348; 
             else
                 a = 350; 
             end
            add.onset = add.onset + a;
            combined = [event; add];
         end
    end
    clear a add event
    
    %adjust onset values to start with 0 
    combined.onset = combined.onset - combined.onset(1);
    combined.event = categorical(cellstr(combined.event));
    
    % save combined file
    fname = join([socialAL, '/output/eventfiles/glm/', part{i}, '/', part{i}, '_combined.txt']);
    writetable(combined, fname)
    
    % begin making glm .mat files
    names = {'trust', 'untrust', 'neutral'};
    durations = {0,0,0};
 
    % feedback % make this a function???
    new = subset_by_event(combined, 'Feedback');
    onsets = num2cell(reshape(new.onset, [], 3), 1);
    fname = join([socialAL, '/output/eventfiles/glm/', part{i}, '/', part{i}, '_feedback.mat']);
    save(fname, 'names', 'durations', 'onsets')
    clear new fname onsets
 
    % view - this is the decision phase where they are not allowed to
    % respond?
    new = subset_by_event(combined, 'Decision');
    onsets = num2cell(reshape(new.onset, [], 3), 1);
    fname = join([socialAL, '/output/eventfiles/glm/', part{i}, '/', part{i}, '_decision.mat']);
    save(fname, 'names', 'durations', 'onsets')
    clear fname onsets
    
    % decision start - this is when they are allowed to start responding? (response = time of response)
    % this uses the same subsetted data as the decision phase
    new.response = new.onset + new.duration;
    onsets = num2cell(reshape(new.response, [], 3), 1);
    fname = join([socialAL, '/output/eventfiles/glm/', part{i}, '/', part{i}, '_decision_start.mat']);
    save(fname, 'names', 'durations', 'onsets')
    clear new fname onsets
    
    
end
