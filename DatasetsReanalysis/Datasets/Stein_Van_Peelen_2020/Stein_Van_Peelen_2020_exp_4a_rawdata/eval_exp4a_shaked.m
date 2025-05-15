clear all;

%---output
%data:
%1- subj number
%2- presentation time (1-4)
%3- valid(1), invalid(2)
%4- localization correct
%5- discrimination correct 

%data_per_subj:
%1- subj number
%2- presentation time
%4- SR
%5- ntrials

data=[];
data_per_subj=[];
for cSub = 1:45
    clear data_per_subj_one_subj data_one_subj
    load(['exp4a_' num2str(cSub) '.mat']);
    
    resAge(cSub,1) = age;
    resGender(cSub,1) = gender;
    
    %variable expDes: every row is a trial, every column specifices an
    %experimental variable
    %columns:
    %1-target left(1), target right(2)
    %2-presentation time (1-4)
    %3-valid(1), invalid(2)
    %4-target category car, cat, chair, cup, guitar, hammer, person, shoe
    %5-target exemplar
    
    %loc_accu is localization accuracy (0 or 1)
    %discr_accu is valid/invalid discrimination (0 or 1)
    
    %create data one subj
    %1- subj number
    %2- presentation time (1-4)
    %3- valid(1), invalid(2)
    %4- localization correct
    %5- discrimination correct
    subjNum=ones(1,length(loc_accu))*cSub;
    data_one_subj=[subjNum',expDes(:,2),expDes(:,3),loc_accu,discr_accu];
    
    ii=0;
    for cPres = 1:4 %presentation time
        clear data_cPres
        ii=ii+1;
        data_cPres=data_one_subj(data_one_subj(:,2)==cPres,:);
        
        %Hits are valid trials with valid response
        hit_rate_discrim(cSub,cPres) = (numel(loc_accu(expDes(:,3)==1 & expDes(:,2)==cPres & discr_accu==1))) / ...
            (numel(loc_accu(expDes(:,3)==1 & expDes(:,2)==cPres)));
        
        if hit_rate_discrim(cSub,cPres) == 1; hit_rate_discrim(cSub,cPres) = 1-(1/(2*(numel(loc_accu(expDes(:,3)==1 & expDes(:,2)==cPres))))); end
        if hit_rate_discrim(cSub,cPres) == 0; hit_rate_discrim(cSub,cPres) = (1/(2*(numel(loc_accu(expDes(:,3)==1 & expDes(:,2)==cPres))))); end
        
        %FAs are invalid trials with valid response
        fa_rate_discrim(cSub,cPres) =(numel(loc_accu(expDes(:,3)==2 & expDes(:,2)==cPres & discr_accu==0))) / ...
            (numel(loc_accu(expDes(:,3)==2 & expDes(:,2)==cPres)));
        
        if fa_rate_discrim(cSub,cPres) == 1; fa_rate_discrim(cSub,cPres) = 1-(1/(2*(numel(loc_accu(expDes(:,3)==2 & expDes(:,2)==cPres))))); end
        if fa_rate_discrim(cSub,cPres) == 0; fa_rate_discrim(cSub,cPres) = (1/(2*(numel(loc_accu(expDes(:,3)==2 & expDes(:,2)==cPres ))))); end
        
        %Dprime discrimination
        dprime_discrim(cSub,cPres) = norminv(hit_rate_discrim(cSub,cPres))-norminv(fa_rate_discrim(cSub,cPres));
        
        %calculation dprime from data_one_subj
        %Hits are valid trials with valid response
        Hits=sum(data_cPres(:,3)==1 & data_cPres(:,5)==1)/sum(data_cPres(:,3)==1);
        if Hits == 1; Hits =NaN; end
        if Hits == 0; Hits =NaN; end
        %FAs are invalid trials with valid response
        FA=sum(data_cPres(:,3)==2 & data_cPres(:,5)==0)/sum(data_cPres(:,3)==2);
        if FA == 1; FA=NaN; end
        if FA == 0; FA=NaN; end
        
        dprime = norminv(Hits)-norminv(FA);
        
        %check
        if ~isnan(dprime)
            if dprime_discrim(cSub,cPres)~=dprime
                cSub
                cPres
                error('The dataset is different than in the paper')
            end
        end
        
        %accuray in discrimination
        SR=mean(data_cPres(:,5));
        
        ntrials=size(data_cPres,1);
        
        %create data_per_subj one subj
        data_per_subj_one_subj(ii,:)=[cSub,cPres,SR,ntrials];
    end
    data=[data;data_one_subj];
    data_per_subj=[data_per_subj;data_per_subj_one_subj];
end
xlswrite('SVP4a_data.xlsx',data)
xlswrite('SVP4a_data_per_subj.xlsx',data_per_subj)