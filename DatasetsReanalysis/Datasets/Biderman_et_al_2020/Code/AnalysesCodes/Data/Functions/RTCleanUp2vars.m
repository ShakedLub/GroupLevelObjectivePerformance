function [ CleanRTData, NumRTOutliers ] = RTCleanUp2vars(Data, RTColumn, SubColumn, STDCutOff, ColumnVar1 , ColumnVar2 )
%RTCleanUp cleans RT outlier trials that deviate more than absolute cutoff.
%   Input:
% Data = the data matrix you are working with. this function is written to
% a mat that includes all subjects.
% STDCutOff = high and low cut off.
% SubColumn = what column includes the number of subject?
% ColumnVar1-ColumnVar3  = number of column variables you want to analyze by.
%   Outputs:
% CleanRTData = the data matrix without RT outliers
% NumRTOutliers = how many trials were excluded

OriginalTrialNum = size(Data,1); % count num of trials before cleaning

SubjectsVector = unique(Data(:,SubColumn)); % all subject numbers as they were coded originally
Var1 = unique(Data(:,ColumnVar1)); % all distinct values of var1 to var3
Var2 = unique(Data(:,ColumnVar2));

for s = 1:length(SubjectsVector)
    for v1 = 1:length(Var1)
        for v2 = 1:length(Var2)
            
            % logical vectors (in the length of the data mat) that signify trials of
            % each condition. the conjuction of all vectors includes only the trials of
            % a specific condition.
            CurrSubTrials = (Data(:,SubColumn)==SubjectsVector(s));
            CurrV1Trials = (Data(:,ColumnVar1)==Var1(v1));
            CurrV2Trials = (Data(:,ColumnVar2)==Var2(v2));
            
            % initiate the cleaning
            CurrentTrialNumbers = find(CurrSubTrials & CurrV1Trials & CurrV2Trials); % the indices of the current trials from the original data matrix
            CurrentRTData = Data(CurrSubTrials & CurrV1Trials & CurrV2Trials , RTColumn); % a vector of RTs that are relevant to this condition
            CurrentZScore = zscore(CurrentRTData); % Zscore of each trial in this condition
            OutlierTrials = abs(CurrentZScore)>STDCutOff; % logical vector, 1 signifies trials which are considered outliers, 0 - are valid trials
            OutlierTrialNumbers = CurrentTrialNumbers(OutlierTrials); % what is the trial number of these bad trials
            Data(OutlierTrialNumbers,:) = []; % delete rows that include outlier RTs, this variable is internal to the function, and is always overwritten (we do not save which trials are considered outliers)
            
        end
    end
end
CleanRTData = Data; % the final mat

NumRTOutliers = OriginalTrialNum - size(CleanRTData,1); % calculate the amount of invalid trials that were found in this function.

end

