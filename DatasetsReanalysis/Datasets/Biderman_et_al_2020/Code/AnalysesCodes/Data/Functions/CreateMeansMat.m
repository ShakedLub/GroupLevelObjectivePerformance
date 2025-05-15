function [ MeanMatIndividualData TrialCountPerCondition ] = CreateMeansMat( Data, DependentVariableColumn, SubColumn, ColumnVar1 , ColumnVar2, ColumnVar3 )
% CreateMeansMat creates a matrix of means according to the variables we
%insert. this is a 2-D matrix that contains number of rows as number of subjects and
%number of columns as number of conditions.
%   Input:
% Data = the data matrix you are working with. this function is written to
% a mat that includes all subjects. 
% DependentVariableColumn = RT or accuracy columns.
% SubColumn = what column includes the number of subject?
% ColumnVar1-ColumnVar3  = number of column variables you want to analyze by. 
%   Outputs:
% MeanMat = the new means matrix

SubjectsVector = unique(Data(:,SubColumn)); % all subject numbers as they were coded originally
Var1 = unique(Data(:,ColumnVar1)); % all distinct values of var1 to var3
Var2 = unique(Data(:,ColumnVar2)); 
Var3 = unique(Data(:,ColumnVar3)); 
NumOfConditions = length(Var1)*length(Var2)*length(Var3);
MeanMatIndividualData = nan(length(SubjectsVector),NumOfConditions); % save room for the final matrix. #rows = #subjects, #columns = #all conditions 
TrialCountPerCondition = MeanMatIndividualData; % counts trials per condition. added by Dan in 10/11/16.
for s = 1:length(SubjectsVector)
    counter = 1; % in order to assign value into the correct columns
    for v1 = 1:length(Var1)
        for v2 = 1:length(Var2)
            for v3 = 1:length(Var3)

% logical vectors (in the length of the data mat) that signify trials of
% each condition. the conjuction of all vectors includes only the trials of
% a specific condition.
CurrSubTrials = (Data(:,SubColumn)==SubjectsVector(s)); 
CurrV1Trials = (Data(:,ColumnVar1)==Var1(v1));
CurrV2Trials = (Data(:,ColumnVar2)==Var2(v2));
CurrV3Trials = (Data(:,ColumnVar3)==Var3(v3));

% the critical calculation
CurrentTrialNumbers = find(CurrSubTrials & CurrV1Trials & CurrV2Trials & CurrV3Trials); % the indices of the current trials from the original data matrix 
CurrentCriticalData = Data(CurrSubTrials & CurrV1Trials & CurrV2Trials & CurrV3Trials , DependentVariableColumn); % a vector of RTs or accu that are relevant to this condition 
MeanMatIndividualData(s,counter) = mean(CurrentCriticalData);
TrialCountPerCondition(s,counter) = length(CurrentCriticalData); % counts trials per condition. added by Dan in 10/11/16.
counter = counter + 1;

            end %v3          
        end % v2
    end % v1
end % s


end % function

