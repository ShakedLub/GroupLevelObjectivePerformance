%% B13 - data clean up and exclusions script
% Written by Dan Biderman in April 2018.
% The scripts analyzes Experiments 1a, 1b and 2 in Biderman & Mudrik
% (2018).
% We clean trials, analyze post-test performance, and then exclude subjects
% accordingly.
% the script exports a csv file with clean data - that is, only valid
% subjects and their valid trials.
% The exported data includes trials at all visibility ratings.

clear;
clc;
addpath('Functions'); % for summary statistics and exclusion rates

% set parameters
RT_exc_std_sub = 3; % number of standard deviations for subject exclusion based on overall RT
RT_exc_std_trial = 3; % number of standard deviations for trial exclusion based on RT in each cell
RT_exc_low = 0.25; % absolute low RT cuttof in seconds
RT_exc_high = 4; % absolute high RT cuttof in seconds
d_prime_cutoff = 1.5; % d' for subject exclusion, we want -1.5 < d' < 1.5
acc_cutoff = 0.65; % % acc for subject exclusion (proportion correct in PT)
export_csv = 1; % insert 1 to export a csv with clean data
remove_subs = 1; % 1 to export a csv file after subjects removal; o= all subjects, clean trials
exp = '1b'; % choose experiment number

if strcmp(exp,'1a') || strcmp(exp,'1b')
    too_few_trials_main = 12; % main: exclude subjects with <12 clean trials in each cell
    too_few_trials_PT = 12; % PT: " " " ...
    if strcmp(exp,'1a')
        is_conscious = 1;
    else
        is_conscious = 0;
    end
else % exp 2 % B13 replication or exp6
    too_few_trials_main = 20;
    too_few_trials_PT = 20;
    is_conscious = 0;
end

% check the contents of the data folder, show only .mat files
DataList = dir(sprintf('E%s/%s/%s', exp, 'Main', '*.mat'));
%% load data files and unify
RawDataAllSubs = [];
for i = 1:size(DataList,1)
    filename = DataList(i).name;
    DataStruct = load(sprintf('E%s/%s/%s',exp, 'Main', filename));
    %DataStruct = load(fullfile(sprintf('E%s/%s/%s',exp, 'Main', filename)));
    SubNum(i) = DataStruct.subData(1,1);
    RawDataAllSubs = [RawDataAllSubs ; DataStruct.subData]; % add it to the general mat.
end

% Remove unnecessary columns
RawDataAllSubs = RawDataAllSubs(:,[1,3,16,18,20]);
% keep columns:
% subnum, context condition, RT, classification (1/2), visibility (1-4)

% organize data (distance to 1:6, context to 1,2,3)
% create a context vec with values: 1 = let, 2 = num, 3 = signs
newContext_vec = zeros(size(RawDataAllSubs,1),1);
newContext_vec(RawDataAllSubs(:,2)<10) = 1; % letters (was 1-6)
newContext_vec(RawDataAllSubs(:,2)>10 &...
    RawDataAllSubs(:,2)<20) = 2; % numbers (was 11-16)
newContext_vec(RawDataAllSubs(:,2)>20) = 3; % signs (was 21-26)

% create a distance vec with values: 1:6
new_dist_vec = mod(RawDataAllSubs(:,2),10); % 1-6

RawDataAllSubsOld = RawDataAllSubs;
RawDataAllSubs(:,2:3) = [newContext_vec, new_dist_vec];
RawDataAllSubs(:,4:6) = RawDataAllSubsOld(:,3:5);

% description of the mat:
% 1. subnum; 2. context (1-let;2-num;3-sig); 3. distance (1-6); 4. RT;
% 5. classification (1/2); 6. visibility (1-4)

% define columns
sub_col=1;
context_col=2;
distance_col=3;
rt_col = 4;
resp_col = 5;
vis_col = 6;

% Exp 2 (Replication) and Exp 6:
% Remove sign trials
if strcmp(exp,'2') || strcmp(exp,'6')
    RawDataAllSubs(RawDataAllSubs(:,context_col)==3,:)=[];
end

entire_data = RawDataAllSubs; % keep: data before any sub/trial exclusion

% remove wrong resps (we code wrong key presses as 9999)
WrongKeyLogical = RawDataAllSubs(:,rt_col) == 9999 |...
    RawDataAllSubs(:,resp_col) == 9999 | RawDataAllSubs(:,vis_col) == 9999;
RawDataAllSubs(WrongKeyLogical,:)=[];

% arrange response to be: 1= 13; 0= b
RawDataAllSubs(:,resp_col) = 2-RawDataAllSubs(:,resp_col);

% Exp 1b (UC 2015) only
% flip sub 821 (L.I) responses - reported performing the opposite key assignment

if strcmp(exp,'1b')
    RawDataAllSubs(RawDataAllSubs(:,sub_col)==821,resp_col)...
        = abs(1-(RawDataAllSubs(RawDataAllSubs...
        (:,sub_col)==821,resp_col)));
end

RawDataAllSubs(:,rt_col) = RawDataAllSubs(:,rt_col)+0.183; % add 183 ms to the RT. subjects have 183 ms to prepare response after stim onset

% calculate mean RT per sub
for s = 1:length(SubNum)
    MeanRTSub(s) = nanmean(RawDataAllSubs(RawDataAllSubs(:,sub_col)==SubNum(s),rt_col));
end
Bad_Overall_RT_Sub_Nums = SubNum(abs(zscore(MeanRTSub))>RT_exc_std_sub);

% Remove trials with absolute slow or quick resps
RawDataAllSubs(RawDataAllSubs(:,rt_col) < RT_exc_low |...
    RawDataAllSubs(:,rt_col) > RT_exc_high,:)=[];

% Clean RT by STD
CleanDataAfterRT = RTCleanUp2vars(RawDataAllSubs, rt_col, sub_col,...
    RT_exc_std_trial, context_col, distance_col); % keep: mat after excluding RT and wrong resps, all subs

% calculate average visibility ratings and
% then remove trials according to visibility
[Visibility, VisAllSubs] = vis_calc_remove(CleanDataAfterRT,sub_col, vis_col, 1, is_conscious);

% count trials per cell and identify bad subjecsts
[~ , TrialsPerSubCell, ~] = CreateMeansMat2vars(VisAllSubs, resp_col,...
    sub_col, context_col, distance_col);

% check for bad subjects
NoTrials_Main_SubNum = SubNum(sum(TrialsPerSubCell<too_few_trials_main,2)>0);

%% Unconscious PT

if strcmp(exp,'2') || strcmp(exp,'1b')
    
    % get all matlab files in the relevant PostTest folder
    DataListPT = dir(sprintf('E%s/%s/%s', exp, 'PostTest', '*.mat'));
    
    % load data and unify
    RawDataAllSubsPT = [];
    for i = 1:size(DataListPT,1)
        filename = DataListPT(i).name;
        DataStruct = load(sprintf('E%s/%s/%s',exp, 'PostTest', filename));
        SubNumPT(i) = DataStruct.subData(1,1);
        RawDataAllSubsPT = [RawDataAllSubsPT ; DataStruct.subData]; % add it to the general mat.
    end
    
    % Remove unnecessary columns
    RawDataAllSubsPT = RawDataAllSubsPT(:,[1,3,16,18,20]);
    
    entire_data_PT = RawDataAllSubsPT; % keep: PT pre exclusion.
    
    % keep columns: subnum, context + dist cond, rt, resp (1/2), vis(1-4)
    % same as in main.
    
    % organize context column.
    % create a context vec with values: 1 = let, 2 = num, 3 = sig
    newContext_vec = zeros(size(RawDataAllSubsPT,1),1);
    newContext_vec(RawDataAllSubsPT(:,2)<10) = 1; % let
    newContext_vec(RawDataAllSubsPT(:,2)>10 &...
        RawDataAllSubsPT(:,2)<20) = 2; % num
    newContext_vec(RawDataAllSubsPT(:,2)>20) = 3; % sig
    
    % create a context vec with values: 1 = let, 2 = num, 3 = sig
    new_dist_vec = mod(RawDataAllSubsPT(:,2),10); % 1-6
    
    RawDataAllSubsOldPT = RawDataAllSubsPT;
    RawDataAllSubsPT(:,2:3) = [newContext_vec, new_dist_vec];
    RawDataAllSubsPT(:,4:6) = RawDataAllSubsOldPT(:,3:5);
    
    % remove sign-trials (catch trials), as subjects performed 2-AFC task
    RawDataAllSubsPT(RawDataAllSubsPT(:,2)==3,:) = [];
    
    % for d' function: arrange context to be: 1-nums; 0-let
    RawDataAllSubsPT(:,2) = RawDataAllSubsPT(:,2)-1;
    
    % description of the mat: 1. subnum; 2. context (1-nums;0-lets); 3. distance(1-6); 4. RT;
    % 5. context classification (1-nums; 2-lets); 6. visibility (1-4)
    
    RawDataAllSubsPT(:,rt_col) = RawDataAllSubsPT(:,rt_col)+0.183; % add 183 ms to the RT. subjects have 183 ms to prepare response after stim onset
    
    % remove wrong resps
    WrongKeyLogicalPT = RawDataAllSubsPT(:,rt_col) == 9999 |...
        RawDataAllSubsPT(:,resp_col) == 9999 | RawDataAllSubsPT(:,vis_col) == 9999;
    RawDataAllSubsPT(WrongKeyLogicalPT,:)=[];
    
    % arrange response to be: 1 -nums; 0-let
    RawDataAllSubsPT(:,resp_col) = 2-RawDataAllSubsPT(:,resp_col);
    
    CleanDataPTAllVis = RawDataAllSubsPT; % save mat for later export
    
    % matrix with only 1-vis trials
    CleanVisDataPT = RawDataAllSubsPT(RawDataAllSubsPT(:,vis_col)==1,:);
    
    % count which subjects didn't have enough trials.
    % first, get the unique sub nums again, as some subjects may have pressed
    % 4 visibility all the way and will be completely removed from CleanVisDataPT
    
    SubNumUpdated_PT = unique(CleanVisDataPT(:,sub_col));
    contexts = unique(CleanVisDataPT(:,context_col));
    
    for s = 1:length(SubNumUpdated_PT)
        for context = 1:length(contexts)
            TrialsPerCellPT(s,context) = sum(CleanVisDataPT(:,sub_col)==...
                SubNumUpdated_PT(s) & CleanVisDataPT(:,context_col)==...
                contexts(context));
        end
    end
    
    % check for bad subjects
    NoTrials_PT_SubNum = SubNumUpdated_PT(sum(TrialsPerCellPT<too_few_trials_PT,2)>0);
    
    %% d'
    
    [~,~, hits_context,fa_context ] = calculate_d_prime( CleanVisDataPT, sub_col,...
        context_col, resp_col, 1, 0 );
    [dprime_context,~,lnB_context,~]=PAL_SDT_1AFC_PHFtoDP([hits_context',fa_context']);
    
    % bad d' subjects for later removal:
    highD_SubNum = SubNumUpdated_PT(abs(dprime_context)>1.5);
    
    % Accuracy analysis
    for i = 1:length(SubNumUpdated_PT)
        sub_mat = CleanVisDataPT(CleanVisDataPT(:,sub_col)==SubNumUpdated_PT(i),:);
        Acc(i) = mean(sub_mat(:,context_col)==sub_mat(:,resp_col));
    end
    
    % bad accuracy (>0.65) subjects for later removal:
    high_Acc_SubNum = SubNumUpdated_PT(Acc>acc_cutoff);
    
    
    % clean main exp and PT mats
    % first collect all the bad subjects
    AllBadSubs = unique([NoTrials_PT_SubNum(:); NoTrials_Main_SubNum(:);...
        highD_SubNum(:); high_Acc_SubNum(:), Bad_Overall_RT_Sub_Nums(:)]);
    
    % subject exclusion logical
    sub_exclusion_logical = ismember(SubNum, AllBadSubs);
    
    % main
    VisAllSubs(ismember(VisAllSubs(:,1), AllBadSubs),:) =[];
    CleanDataAfterRTAllSubs = CleanDataAfterRT; % keep
    CleanDataAfterRT(ismember(CleanDataAfterRT(:,1),AllBadSubs),:)=[];
    SubNum(sub_exclusion_logical)=[];
    
    % PT
    CleanVisDataPT(ismember(CleanVisDataPT(:,sub_col), AllBadSubs),:)=[];
    SubNumUpdated_PT(ismember(SubNumUpdated_PT, AllBadSubs))=[];
    %CleanDataPTAllVis(ismember(CleanDataPTAllVis(:,sub_col), AllBadSubs),:)=[];
    
    % clean d'
    dprime_context_clean = dprime_context(~sub_exclusion_logical);
    
    % clean acc
    clean_acc = Acc(~sub_exclusion_logical);
    
end % strcmp(exp,'2') || strcmp(exp,'1b')

% both in exp 1a (conscious) and exp 6 (UC B13 inverted inducers) we do not
% perform post test analyses.
if strcmp(exp,'1a') || strcmp(exp,'6')
    AllBadSubs = unique([NoTrials_Main_SubNum, Bad_Overall_RT_Sub_Nums]);
    sub_exclusion_logical = ismember(SubNum, AllBadSubs);
    SubNum(sub_exclusion_logical)=[];
    % remove subs with too few trials in the main
    VisAllSubs(ismember(VisAllSubs(:,1),AllBadSubs),:)=[];
    sub_exclusion_logical = ismember(SubNum, NoTrials_Main_SubNum);
    CleanDataAfterRTAllSubs = CleanDataAfterRT; % keep
    CleanDataAfterRT(ismember(CleanDataAfterRT(:,1),AllBadSubs),:)=[];
end

%% Subjects' info
% Reported in the Participants section of the corresponding experiment.

InfoList = dir(sprintf('E%s/%s/%s', exp, 'SubInfo',  '*.mat'));
FileCounter = 0;
for i = 1:size(InfoList,1)
    filename = InfoList(i).name;
    FileCounter= FileCounter+1;
    InfoStruct = load(sprintf('E%s/%s/%s', exp, 'SubInfo',filename)); % field name: subData
    Sub_info(FileCounter,:) = InfoStruct.subInfo([2,4,5,6],2);
end

% check for repeating subnums
SubNumInfo = cell2mat(Sub_info(:,1));
firstVec = SubNumInfo(1:end-1);
secondVec = SubNumInfo(2:end);
row_to_remove = find(firstVec==secondVec); % the first time in which the file appears
Sub_info(row_to_remove,:)=[];
SubNumInfo(row_to_remove) = [];

% info stats:
Sub_info(sub_exclusion_logical,:)=[];
age = str2double(Sub_info(:,2));
mean_age = nanmean(age); std_age = nanstd(age); max_age = max(age); min_age = min(age);
gender = Sub_info(:,3); num_male = sum(ismember(gender,'m') | ismember(gender,'M'));
hand = Sub_info(:,4); num_right = sum(ismember(hand,'r') | ismember(hand,'R'));
msgbox(sprintf('Mean age is %.2f (SD = %.2f).\n\n %d Male, %d right-handed',...
    mean_age, std_age, num_male, num_right), 'Subject Info')

%% Count excluded subjects and for which reasons
% this is reported in the Participants section of Experiments 1b and 2.
if strcmp(exp, '1b') || strcmp(exp, '2')
    unique_exc_RT = length(Bad_Overall_RT_Sub_Nums);
    unique_exc_trials_main = sum(~ismember(NoTrials_Main_SubNum,Bad_Overall_RT_Sub_Nums));
    unique_exc_trials_PT = sum(~ismember(NoTrials_PT_SubNum,NoTrials_Main_SubNum));
    unique_exc_d_prime_PT = sum(~ismember(highD_SubNum, [NoTrials_PT_SubNum;NoTrials_Main_SubNum']));
    unique_exc_acc_PT = sum(~ismember(high_Acc_SubNum, [highD_SubNum; NoTrials_PT_SubNum;NoTrials_Main_SubNum']));
    sprintf(...
        'unique exclusions:\n overall RT: %d \n trials main: %d \n trials PT: %d \n dprime: %d \n acc: %d',...
        unique_exc_RT,unique_exc_trials_main, unique_exc_trials_PT,...
        unique_exc_d_prime_PT, unique_exc_acc_PT)
end

if strcmp(exp, '1a') || strcmp(exp, '6')
    unique_exc_RT = length(Bad_Overall_RT_Sub_Nums);
    unique_exc_trials_main = sum(~ismember(NoTrials_Main_SubNum,Bad_Overall_RT_Sub_Nums));
    sprintf(...
        'unique exclusions:\n overall RT: %d \n trials main: %d \n',...
        unique_exc_RT,unique_exc_trials_main)
end

%% percentage of trials  removed (for included subjects)
% for all experiments, we report the following data in
% Experiment 1->Analysis->Trials Exclusion Criteria.

AllDataIncludedSubs = entire_data(~ismember(entire_data(:,1), AllBadSubs),:);

% clean RT + all vis / all RT+ all vis
percent_removed_rt = round(100*(1-size(CleanDataAfterRT,1)/...
    size(AllDataIncludedSubs,1)),2);

% percentage of trials removed due to vis =
% clean RT + clean vis / clean RT + all vis
percent_PAS_removed = round(100*(1-size(VisAllSubs,1)/...
    size(CleanDataAfterRT,1)),2);

if strcmp(exp, '1a') || strcmp(exp, '6')
    % print
    sprintf('For included subs, %0.2f percent were removed due to RT and wrong responses,\n\n%0.2f percent were removed due to irrelevant visibility (out of the remaining trials)\n',...
        percent_removed_rt,percent_PAS_removed)
    
else % strcmp(exp, '1b') || strcmp(exp, '2') , including PT
    percent_PAS_removed_PT = round(100*(1-size(CleanVisDataPT,1)/...
        size(CleanDataPTAllVis,1)),2);
    % print
    sprintf('For included subs, %0.2f percent were removed due to RT and wrong responses,\n\n%0.2f percent were removed due to irrelevant visibility (out of the remaining trials)\n\nand %0.2f percent were removed in PT due to visibility',...
        percent_removed_rt,percent_PAS_removed,percent_PAS_removed_PT)
end

%% export main experiment data to a csv file
% includes valid subjects, their valid trials, and all visibility ratings.

if export_csv
    
    if remove_subs==0
        CleanDataAfterRT = CleanDataAfterRTAllSubs;
    end
    
    SubjectsVector = unique(CleanDataAfterRT(:,sub_col)); % length = n
    subs_export = CleanDataAfterRT(:,sub_col);
    for i = 1:length(SubjectsVector)
        subs_export(subs_export==SubjectsVector(i))=i;
    end
    RespVec = CleanDataAfterRT(:,resp_col); % is classed as 13
    DistanceVec = CleanDataAfterRT(:,distance_col); % distances 1:6
    RTVec = CleanDataAfterRT(:,rt_col); % rt column
    VisVec = CleanDataAfterRT(:,vis_col); % vis column
    % we define the context variable different in each experiment
    
    filename = fullfile('Summary_Matrices',sprintf('E%s_Main.csv', exp));
    
    if remove_subs==0
    filename = fullfile('Summary_Matrices',sprintf('E%s_Main_All_Subs.csv', exp));
    end
        
    if strcmp(exp, '1a') || strcmp(exp, '1b')
        
        IsLetVec = CleanDataAfterRT(:,context_col)==1; % dummy variable for let
        IsNumVec = CleanDataAfterRT(:,context_col)==2; % same for num
        
        mat = [subs_export, DistanceVec, IsLetVec, IsNumVec, RespVec, RTVec, VisVec];
        headers = {'Subject','Distance', 'Is_Let', 'Is_Num',...
            'Resp13', 'RT', 'Vis'};
        
    else % we are in exp 2 or 6(context vec is defined differently)
        
        ContextVec =  CleanDataAfterRT(:,2)-1;
        mat = [subs_export, DistanceVec, ContextVec, RespVec, RTVec, VisVec];
        headers = {'Subject','Distance', 'Context',...
            'Resp13', 'RT', 'Vis'};
        
    end
    %csvwrite_with_headers(filename,mat, headers)
    
end  % export csv
%% export csv file with PostTest data
% includes valid subjects, their valid trials, and all visibility ratings.

if export_csv
    if strcmp(exp,'1b') || strcmp(exp,'2') % UC exps
        SubjectsVector = unique(CleanDataPTAllVis(:,1)); % length = n
        subs_export = CleanDataPTAllVis(:,1);
%         for i = 1:length(SubjectsVector)
%             subs_export(subs_export==SubjectsVector(i))=i;
%         end
        RespVecPT = CleanDataPTAllVis(:,resp_col); % is classed as reference
        DistanceVecPT = CleanDataPTAllVis(:,distance_col); % distances 1:6
        RTVecPT = CleanDataPTAllVis(:,rt_col); % rt column
        VisVecPT = CleanDataPTAllVis(:,vis_col); % vis column
        ContextVecPT = CleanDataPTAllVis(:,context_col); % the responses are organized already as 1/0
        
        filename = fullfile('Summary_Matrices',sprintf('E%s_Post.csv', exp));
        
        mat = [subs_export, DistanceVecPT, ContextVecPT, RespVecPT, RTVecPT, VisVecPT];
        headers = {'Subject','Distance', 'Context',...
            'Resp13', 'RT', 'Vis'};
        
        %csvwrite_with_headers(filename,mat, headers)
        
    end % UC exps
end % export_csv
%% export PostTest d' and accuracy
% shorter data file with 24 rows (=N subjects) and 2 columns (d_prime and
% acc).
if strcmp(exp, '1b') || strcmp(exp, '2')
    if export_csv
        acc_dprime = [clean_acc', dprime_context_clean];
        filename = fullfile('Summary_Matrices/accuracy_dprime',sprintf('E%s_acc_dprime.csv', exp));
        %csvwrite_with_headers(filename,acc_dprime, {'acc','d_prime'});
    end
end

%% descriptive results
if strcmp(exp, '2') || strcmp(exp, '6')
    vis1dat = CleanDataAfterRT(CleanDataAfterRT(:, vis_col) == 1, :);
    % use a function to compute cell means
    means = CreateMeansMat2vars(vis1dat, resp_col, sub_col, distance_col, context_col);
    
    % seperate output into cong and incong contexts
    % cong
    cong_mat = means(:,2:2:end);
    cong_mean = mean(cong_mat);
    cong_std = std(cong_mat)/sqrt(length(SubNum));
    % incong
    incong_mat = means(:, 1:2:(end-1));
    incong_mean = mean(incong_mat);
    incong_std = std(incong_mat)/sqrt(length(SubNum));
    
    f = figure();
    hold on;
    errorbar(cong_mean, cong_std, 'b', 'LineWidth', 1.5);
    errorbar(incong_mean, incong_std, 'r', 'LineWidth', 1.5);
    scatter(1:6,cong_mean, 'b', 'LineWidth', 1.5);
    scatter(1:6,incong_mean, 'r', 'LineWidth', 1.5);
    legend('numbers', 'letters','Location','southeast')
    
    xlabel('distance')
    ylabel('prop. class. 13.')
    ylim([0,1])
    hold off;
    
    saveas(f, sprintf('E%s_desc_plot.png', exp))

end