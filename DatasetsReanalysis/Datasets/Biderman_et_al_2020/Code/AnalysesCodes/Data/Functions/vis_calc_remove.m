function [ Visibility, CleanMat ] = vis_calc_remove( DataMat, sub_col, vis_col, remove_trials, is_conscious)
% vis_calc_remove receives a matrix of data and calculates visibility rates and removes trials
% input:
% DataMat = a matrix of data, I usually use it after RT cleanup
% sub_col = column which includes subject number 
% vis_col = column which includes the PAS ratings (1:4)
% remove_trials = should the function also remove trials or only calculate
% visibility? 1= remove, 0 = don't
% is_conscious = 1 = conscious exp (removing 1 visibility trials), 0 = unconscious exp (removing 2-4 visibility). 

SubNum = unique(DataMat(:,sub_col));
for s = 1:length(SubNum)
    CurrentSubVec = DataMat(DataMat(:,sub_col)==SubNum(s) ,vis_col);
    VisTabulate = [ ([1 2 3 4]'), zeros(length(1:4),2)];
    CurrentTabulate = tabulate(CurrentSubVec);
    if size(CurrentTabulate,1)<size(VisTabulate,1)
        for i = 1:size(CurrentTabulate,1)
            if CurrentTabulate(i,1)==VisTabulate(i,1)
                VisTabulate(i,:)=CurrentTabulate(i,:);
            end
        end
    else % current tabulate does not miss any rows.
        VisTabulate = CurrentTabulate;
    end
    Visibility(:,:,s) = VisTabulate;
end

% clean trials according to visibility (conscious - remove 1-vis trials, unconscious - remove >1 trials)
% total number of trials before exclusion:
TotalTrials = size(DataMat,1);

if remove_trials == 1
    
    if is_conscious ==1 % we are in a conscious experiment
        DataMat(DataMat(:,vis_col)==1,:)=[]; % remove
    else % we are in an unconscious experiment
        DataMat(DataMat(:,vis_col)>1,:)=[]; % remove
    end
    
    CleanMat = DataMat;
    
end % remove_trials

end

