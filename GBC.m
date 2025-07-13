function [h,pval]=GBC(R,N,chance,alpha,tail)
	%Implemented by: Shaked Lublinsky and Itay Yaron
	%Preprint: https://osf.io/preprints/psyarxiv/b967v_v1

	%input
	%R: number of correct reponses for each participant
	%N: number of trials for each particiapnt
	%chance: chance prefomance of task
	%alpha: alpha of statistical test 
    %tail: type of alternative hypothesis to evaluate 'both', 'right',
    %'left'

	%output
	%h: 1 test significant, 0 test none significant
	%pval: p-value of the GBC test 


	% checks on inputs
	if size(R,1) >= size(R,2)
		error('R should be a row vector')
	end
	if size(N,1) >= size(N,2)
		error('N should be a row vector')
    end
    if all(size(N) ~= size(R))
        error('N and R should be the same size')
    end
    switch tail
        case 'both'
        case 'right'
        case 'left'
        otherwise 
            error('tail should be assigned a char with the word: both, right or left')
    end

	% calcualte success ratio (SR) from number of correct responses (R) and number of trials (N) per participant
	SR = R ./ N;

	%% Chi square for goodness of fit
	% Correct responses per participant
	A=R';
	% Number of "failures" per participant
	F=N'-R';
	% Expected number of success / failures = number of trials / 2 under the null hypothesis
	E=N'/2;

	% Compute chi-squared statistic: X2 = sum((A - E)^2 / E) + sum((F - E)^2/E),
	% where A indicates number of correct awareness responses, E indicates the
	% expected number of correct responses, and F indicates the number of incorrect responses
	% under the null hyptohesis (50%)
	X2=sum((A-E).^2 ./ E) + sum((F-E).^2 ./ E);

	%Under H0, X2 ~ chisq(k), where degrees of freedom = number of participants
	pvalChi=1 - chi2cdf(X2,length(A));

	%% GB test
	% calculate average standard deviation across all subjects for the sample under the null hyptohesis
	chanceVec=ones(length(A),1)*chance;
	varSample=mean((chanceVec.*(1-chanceVec))./N');
	sigmaSample=sqrt(varSample);
	% z test (note that it is one-sided - 'right' tail)
	[~,pvalGB]=ztest(SR',chance,sigmaSample,'Tail',tail);

	%% GBC test
	%get minimal pvalue between the two tests
	pval=min(pvalChi,pvalGB);
	%we correct for two comparisons so multiply p-value by 2 before
	%comparing with alpha
	pval=pval*2;

	if pval <= alpha
		h=1;
	else
		h=0;
	end
end