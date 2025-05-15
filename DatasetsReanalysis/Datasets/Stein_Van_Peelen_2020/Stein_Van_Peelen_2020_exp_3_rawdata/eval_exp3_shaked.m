clear all;

%---output
%data:
%1- subj number
%2- presentation time (1-5)
%3- target present(1), target absent(2)
%4- upright(1), inverted(2)
%5- localization correct
%6- discrimination correct 
%7- PAS 

%data_per_subj:
%1- subj number
%2- presentation time
%4- SR
%5- ntrials

data=[];
data_per_subj=[];
for cSub = 1:94
    clear data_per_subj_one_subj data_one_subj
    load(['exp3_' num2str(cSub) '.mat']);
    
    resAge(cSub,1) = age;
    resGender(cSub,1) = gender;
    
    %variable expDes: every row is a trial, every column specifices an
    %experimental variable
    %columns:
    %1-target present(1), target absent(2)
    %2-target left(1), target right(2)
    %3-presentation time (1-5)
    %4-upright(1), inverted(2)
    %5-target exemplar
    
    %loc_accu is localization accuracy (0 or 1)
    %pas is response on perceptual awareness scale (1-4)
    %discr_accu is upright/inverted discrimination (0 or 1)
    
    %create data one subj
    %1- subj number
    %2- presentation time (1-5)
    %3- target present(1), target absent(2)
    %4- upright(1), inverted(2)
    %5- localization correct
    %6- discrimination correct
    %7- PAS
    subjNum=ones(1,length(loc_accu))*cSub;
    data_one_subj=[subjNum',expDes(:,3),expDes(:,1),expDes(:,4),loc_accu,discr_accu,pas];
    
    %exclude trials in which the target isn't presented
    data_one_subj=data_one_subj(data_one_subj(:,3)==1,:);
    
    ii=0;
    for cPres=1:5 %presentation times
        clear data_cPres
        ii=ii+1;
        data_cPres=data_one_subj(data_one_subj(:,2)==cPres,:);
        
        %Hits are upright stimuli with upright response
        hit_rate_discrim(cSub,cPres) = (numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==1 & discr_accu==1))) / ...
            (numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==1)));
        
        if hit_rate_discrim(cSub,cPres) == 1; hit_rate_discrim(cSub,cPres) = 1-(1/(2*(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==1))))); end
        if hit_rate_discrim(cSub,cPres) == 0; hit_rate_discrim(cSub,cPres) = (1/(2*(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==1))))); end
        
        %FAs are inverted stimuli with upright response
        fa_rate_discrim(cSub,cPres) =(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==2 & discr_accu==0))) / ...
            (numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==2)));
        
        if fa_rate_discrim(cSub,cPres) == 1; fa_rate_discrim(cSub,cPres) = 1-(1/(2*(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==2))))); end
        if fa_rate_discrim(cSub,cPres) == 0; fa_rate_discrim(cSub,cPres) = (1/(2*(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==2))))); end
        
        %Dprime discrimination
        dprime_discrim(cSub,cPres) = norminv(hit_rate_discrim(cSub,cPres))-norminv(fa_rate_discrim(cSub,cPres));
        
        %calculation dprime from data_one_subj
        %Hits are upright stimuli with upright response
        Hits=sum(data_cPres(:,4)==1 & data_cPres(:,6)==1)/sum(data_cPres(:,4)==1);
        if Hits == 1; Hits =NaN; end
        if Hits == 0; Hits =NaN; end
        %FAs are inverted stimuli with upright response
        FA=sum(data_cPres(:,4)==2 & data_cPres(:,6)==0)/sum(data_cPres(:,4)==2);
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
        SR=mean(data_cPres(:,6));
        
        ntrials=size(data_cPres,1);
        
        %create data_per_subj one subj
        data_per_subj_one_subj(ii,:)=[cSub,cPres,SR,ntrials];
    end
    data=[data;data_one_subj];
    data_per_subj=[data_per_subj;data_per_subj_one_subj];
end
%save data
xlswrite('SVP3_data.xlsx',data)
xlswrite('SVP3_data_per_subj.xlsx',data_per_subj)