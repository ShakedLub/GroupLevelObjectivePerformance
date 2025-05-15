function [ dPrimePerSub, CriterionPerSub, HitsProportion, FAProportion ] = calculate_d_prime( data, sub_column, objective_column, response_column, signal_response, noise_response )
% calculate d prime and criterion and if using another function to do so, it calculates Hits and FA rates.
% before doint that, you need to define which columns are relvant for HITs
% and FAs calculation:
% objective_column = the objective appearance of the signal
% response_column = the response regarding that signal
% signal_response = what is encoded in the two rows for signal (insert a scalar)
% noise_response = what is encoded in the rows for noise (insert a scalar)
% NOTE that the responses should be encoded like the actual signal
% If the Hits and/or FA rates are 1 or 0, we change them following
% Macmillan & Kaplan,1985, as presented in Stanislaw, H., & Todorov, N. (1999). Calculation of signal detection theory measures. Behavior research methods, instruments, & computers, 31(1), 137-149.?

TotalSubs = unique(data(:,sub_column));
dPrimePerSub = nan(1,length(TotalSubs));
CriterionPerSub = nan(1,length(TotalSubs));

for sub = 1:length(TotalSubs)
    
    Hits = (data(data(:,sub_column)==TotalSubs(sub),objective_column)==signal_response &...
        data(data(:,sub_column)==TotalSubs(sub),response_column)==signal_response) + 0;
    
    HitsProportion(sub) = sum(Hits)/sum(data(data(:,sub_column)==TotalSubs(sub),objective_column)==signal_response);
    
    FA = (data(data(:,sub_column)==TotalSubs(sub),objective_column)==noise_response &...
        data(data(:,sub_column)==TotalSubs(sub),response_column)==signal_response) + 0;
    
    FAProportion(sub) = sum(FA)/sum(data(data(:,sub_column)==TotalSubs(sub),objective_column)==noise_response);
    
    % change FA/Hits rates if they equal to 0 or 1
    
    if HitsProportion(sub)==0 % change to 0.5/n where n is number of signal trials
        HitsProportion(sub)= 0.5/sum(data(data(:,sub_column)==TotalSubs(sub),objective_column)==signal_response);
    elseif HitsProportion(sub)==1 % change to n-0.5/n where n is number of signal trials
        HitsProportion(sub)= (sum(data(data(:,sub_column)==TotalSubs(sub),objective_column)==signal_response)-0.5)/sum(data(data(:,sub_column)==TotalSubs(sub),objective_column)==signal_response);
    end
    if FAProportion(sub)==0 % change to 0.5/n where n is number of noise trials
        FAProportion(sub) = 0.5/sum(data(data(:,sub_column)==TotalSubs(sub),objective_column)==noise_response);
    elseif FAProportion(sub)==1 % change to n-0.5/n where n is number of noise trials
        FAProportion(sub) = (sum(data(data(:,sub_column)==TotalSubs(sub),objective_column)==noise_response)-0.5)/sum(data(data(:,sub_column)==TotalSubs(sub),objective_column)==noise_response);      
    end
    
    Zhit = norminv(HitsProportion(sub)); Zfa = norminv(FAProportion(sub));
    
    dPrimePerSub(sub) = Zhit - Zfa;
    CriterionPerSub(sub) = -0.5*(Zhit + Zfa);
    
end
% dPrimePerSub(isinf(dPrimePerSub))=nan;
% CriterionPerSub(isinf(CriterionPerSub))=nan;

end

