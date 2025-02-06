#########################     Awareness Tests     ########################## 
# the script includes implementations of all awareness tests and the rules
# governing significance for frequentist and Bayesian tests

#########################    Significance Rules   ########################## 
# The functions define significance for given test results

# For Bayesian tests, use the fixed parameter of BF threshold
calc_sig_bayes <- function(result_test, fixed_params) {
  H1_res <- result_test > fixed_params@BF_threshold
  H0_res <- result_test < 1/fixed_params@BF_threshold
  return(list(H1 = H1_res, H0 = H0_res))
}
# For frequntist tests, use the fixed parameter of alpha
calc_sig_freq <- function(result_test, fixed_params) {
  H1_res <- result_test < fixed_params@alpha
  H0_res <- rep(NA, length(result_test))
  return(list(H1 = H1_res, H0 = H0_res))
}

#####################     Awareness Tests CLASS     ####################### 
# Define the awareness test class: the class which defines an awareness test.
# To add a new test, add an instance of the  'awareness_test_class' object 
# implementing the test (see below 'run_test' function) and significance 
# rule (see below 'get_percent_significant' function).
awareness_test_class <- 'awareness_test_type'
# Class fields
# test_name - a label for the test (e.g, 'Chi').
# run_test - a function implementing the statistical test, getting the observed
# data (a list with matrices of the #trials (trials_mat), %correct (as_mat), 
# and #correct (a_mat) per participant) and fixed parameters (see Definitions.R)
# as input, and returning the a vector of the results for all iterations.
# get_percent_significant - a function that returns significance according to the
# test (differs between frequentist tests and Bayesian ones)
setClass(awareness_test_class, slots=list(test_name="character",
                                          run_test = "function",
                                          get_percent_significant = "function"),
         prototype = list(get_percent_significant = calc_sig_freq))

## We define an X_imp instance of the awareness test class to define a specific
# implementation of an awareness test

## T-test
# Defines a one tailed t-test (test_name = 'T'), returns p-values vector
t_test_f <- function(obs_data, fixed_params) {
  # we run multiple tests at once, returning results for all iterations
  result_test <- col_t_onesample(obs_data$as_mat,alternative="greater",
                                 null=fixed_params@chance_as)
  return(result_test$pvalue)
}
t_test_imp <- new(awareness_test_class, test_name="T", run_test=t_test_f)

## Bayesian T-test
# Defines a Bayesian t-test (test_name = 'TBayes'), returns BFs vector
tbayes_test_f <- function(obs_data, fixed_params) {
  # run the test for each iteration
  result_test <- sapply(1:fixed_params@n_iterations, function(ind)
    exp(ttestBF(x=obs_data$as_mat[,ind],mu=fixed_params@chance_as)@bayesFactor$bf))
  return(result_test)
}
tbayes_test_imp <- new(awareness_test_class, test_name="TBayes",
                       run_test=tbayes_test_f, get_percent_significant=calc_sig_bayes)

## Chi-Square test
# Defines a Chi-square test (test_name = 'Chi'), returns a p-values vector
chisq_test_f <- function(obs_data, fixed_params) {
  # we use the raw data matrix and number of trials, creating frequency tables
  # Number of "failures" per participant
  failures_mat <- obs_data$trials_mat - obs_data$a_mat
  # Expected number of success / failures = number of trials / 2
  expected_mat <- obs_data$trials_mat / 2

  # Compute observed chi-squared X2 = sum((A - E)^2 / E) + sum((F - E)^2/E),
  # where A indicates number of correct awareness responses, E indicates the
  # expected number of correct responses under the null hyptohesis (50%), and
  # F indicates the number of incorrect responses
  X2 <- colSums((obs_data$a_mat - expected_mat)^2 / expected_mat) +
    colSums((failures_mat - expected_mat)^2 / expected_mat)

  ## Under H0, X2 ~ chisq(k), where k = number of participants
  # get the p-value for each iteration
  result_test <- 1 - pchisq(X2, df = nrow(obs_data$a_mat))

  return(result_test)
}
chisq_test_imp <- new(awareness_test_class, test_name="Chi",
                       run_test = chisq_test_f)
## GB test
# Defines a GB test (test_name = 'GB'), returns a p-values vector
gb_test_f <- function(obs_data, fixed_params) {
  #calculate average standard deviation across all subjects for the sample
  p_chance_mat=matrix(fixed_params@chance_as,nrow=dim(obs_data$trials_mat)[1],ncol=dim(obs_data$trials_mat)[2])
  var_samp=p_chance_mat*(1-p_chance_mat)/obs_data$trials_mat
  var_samp=colMeans(var_samp)
  sigma_samp=sqrt(var_samp)

  # run a z.test according to the sigma_samp and observed results in each iteration
  result_test <- sapply(1:fixed_params@n_iterations, function(ind)
    z.test(x=obs_data$as_mat[,ind],alternative = "greater", mu=fixed_params@chance_as, sigma.x=sigma_samp[ind] )$p.value)
  return(result_test)
}
gb_test_imp <- new(awareness_test_class, test_name="GB",
                   run_test = gb_test_f)

## GB or Chi test
# Defines a GB or Chi test (test_name = 'GBorChi'), returns a p-values vector
gborchi_test_f <- function(obs_data, fixed_params) {
  #run chi square test for each iteration
  result_test_chi= chisq_test_f(obs_data, fixed_params)
  #run GB test for each iteration
  result_test_gb = gb_test_f(obs_data, fixed_params)
  #get minimal pvalue between the two tests
  result_test = pmin(result_test_chi,result_test_gb)
  # we correct for two comparisons so multiple p-value by 2 before
  # comparing with alpha
  result_test = result_test * 2
  return(result_test)
}
gborchi_test_imp <- new(awareness_test_class, test_name="GBorChi",
                        run_test = gborchi_test_f, get_percent_significant=calc_sig_freq)

# GBF test
# Defines a GBF test (test_name = 'GB_Bayes'), returns a BF vector
gbf_test_f <- function(obs_data, fixed_params) {
  # run the test for each iteration
  generate_GB_BF <- function(accuracy, n_trials, chance_level = .5,
                             low_bound = .5,
                             theta_mu_prior = .55, theta_sig_prior = .1,
                             sigma_mu_prior = .025, sigma_sig_prior = .05,
                             n_chains = 2, burining_period = 1500, iterations_per_chain = 5000) {
    # group together all parameters feeding the model
    model_data <- list(a = accuracy, n_t = n_trials, chance_p = chance_level,
                       theta_low_bound = low_bound,
                       theta_mu_prior = theta_mu_prior,
                       theta_sig_prior = theta_sig_prior,
                       sigma_mu_prior = sigma_mu_prior,
                       sigma_sig_prior = sigma_sig_prior)
    # the parameters to monitor
    monitored_params <-c("M", "delta", "theta")
    # create the model
    model <- jags.model( textConnection(GB_MODEL), model_data, n.chains = n_chains)
    # burn the first samples for MCMC
    update(model, burining_period)
    # generate mcmc samples
    mcmc_samples <- coda.samples(model, monitored_params,  n.iter=iterations_per_chain)
    # calculate the posterior probability for H1 relying on the 'M' parameter that
    # arbitrates between H0 and H1 (average across all chains and iterations)
    probability_H1 <- do.call(rbind.data.frame, mcmc_samples) %>%
      dplyr::pull(M) %>%
      mean()
    # calculate the Bayes Factor based on the probability for H1:
    # BF10 = (p(H1)/(1-p(H1))) = (p(H1)/p(H0))
    BF_10 <- probability_H1 / (1 - probability_H1)
    return(list(mcmc_samples = mcmc_samples, BF = BF_10))
  }


  # The model
  GB_MODEL <- "model {
    # M parameter, with an uniformative prior,
    # this helps us estiamte the BF comparing the two models:
    # if M == 0, choose the H0 model (p = chance for all participants)
    # if M == 1, choose the H1 model (heirarchical model, with a parameter
    # 'delta' for each participant indicating if p is sampled from
    # a 'conscious' awareness scores distribution (p != .5), otherwise
    # p is set to chance
    M ~ dbern(.5)
    ## set the prior on the conscious group's mu:
    # calculate percision for the given sd prior:
    # JAGS uses precision to parametrize a normal distribution (percision = sd^-2)
    theta_percision = pow(theta_sig_prior,-2)
    # set the prior: theta ~ TN(mu, mu_percision, chance-level prformance, 1) -
    # we use the T(,) JAGS function to truncate below chance and above 1 values
    theta ~ dnorm(theta_mu_prior, theta_percision) T (theta_low_bound,1)

    ## set the prior on the conscious group's sd (individual differences
    ## between conscious participants):
    # calculate percision for the given sd prior:
    sigma_sig_percision = pow(sigma_sig_prior,-2)
    # set the prior: sigma ~ TN(sd_mu, sd_percision, 0, 1) -
    # we use the T(,) JAGS function to truncate below 0 and above 1 values
    sigma ~ dnorm(sigma_mu_prior, sigma_sig_percision) T(0, 1)
    # get the percision for the sampled sd
    sigma_percision = pow(sigma,-2)

    # go over all participants in a loop
    for(i in 1:length(a) ) {
      # conscious participant's p distributes normally around the conscious group mu,
      # with percision according to the level of individual differences
      mu[i] ~ dnorm(theta, sigma_percision)
      # the prior of each participant's probability for being unaware (perform
      # at chance level) or aware with equal probabilities
      delta[i] ~ dbern(.5)
      # For H1 model, set the participants P(correct) (= p) to either chance or
      # the conscious participant p, according to the 'delta' parameter.
      H1_participant_p[i] = ifelse(delta[i] == 0, chance_p, mu[i])
      # Set the participants P(correct) according to the 'M' parameter, deciding
      # between the models
      participant_p[i] = ifelse(M == 0, chance_p, H1_participant_p[i])
      # model the data as distributed binomally according to the participant P(correct),
      # and the number of trials (n_t) of the ith participant
      a[i] ~ dbin(participant_p[i], n_t[i])
    }
  }"

  # result_test <- GB_MODEL
  result_test <- sapply(1:fixed_params@n_iterations, function(ind)
    generate_GB_BF(obs_data$a_mat[,ind],obs_data$trials_mat[,ind])$BF)
  return(result_test)
}
gbf_test_imp <- new(awareness_test_class, test_name="GB_Bayes",
                    run_test = gbf_test_f, get_percent_significant=calc_sig_bayes)

# Uninformative GBF test
# Defines an uninformative GBF test (test_name = 'GB_Bayes_Uninformative'), returns a BF vector
gbf_uninformative_test_f <- function(obs_data, fixed_params) {
  # run the test for each iteration
  generate_GB_UNINF_BF <- function(accuracy, n_trials, chance_level = .5,
                             low_bound = 0,
                             n_chains = 2,
                             burining_period = 15000,
                             iterations_per_chain = 30000) {
    # group together all parameters feeding the model
    model_data <- list(a = accuracy, n_t = n_trials, chance_p = chance_level,
                       theta_low_bound = low_bound)
    # the parameters to monitor
    monitored_params <-c("M", "delta", "theta")
    # create the model
    model <- jags.model( textConnection(GB_UNINF_MODEL), model_data, n.chains = n_chains)
    # burn the first samples for MCMC
    update(model, burining_period)
    # generate mcmc samples
    mcmc_samples <- coda.samples(model, monitored_params,  n.iter=iterations_per_chain)
    # calculate the posterior probability for H1 relying on the 'M' parameter that
    # arbitrates between H0 and H1 (average across all chains and iterations)
    probability_H1 <- do.call(rbind.data.frame, mcmc_samples) %>%
      dplyr::pull(M) %>%
      mean()
    # calculate the Bayes Factor based on the probability for H1:
    # BF10 = (p(H1)/(1-p(H1))) = (p(H1)/p(H0))
    BF_10 <- probability_H1 / (1 - probability_H1)
    return(list(mcmc_samples = mcmc_samples, BF = BF_10))
  }


  # The model
  GB_UNINF_MODEL <- "model {
    # M parameter, with an uniformative prior,
    # this helps us estiamte the BF comparing the two models:
    # if M == 0, choose the H0 model (p = chance for all participants)
    # if M == 1, choose the H1 model (heirarchical model, with a parameter
    # 'delta' for each participant indicating if p is sampled from
    # a 'conscious' awareness scores distribution (p != .5), otherwise
    # p is set to chance
    M ~ dbern(.5)
    ## set the uninformative prior on the conscious group's mu:
    theta ~ dunif(theta_low_bound,1)

    ## set the prior on the conscious group's sd (individual differences
    ## between conscious participants): (upper limit is set to 0.5, the
    ## maximal value for sigma)
    sigma ~ dunif(0, .5)
    # get the percision for the sampled sd
    sigma_percision = pow(sigma,-2)

    # go over all participants in a loop
    for(i in 1:length(a) ) {
      # conscious participant's p distributes normally around the conscious group mu,
      # with percision according to the level of individual differences
      mu[i] ~ dnorm(theta, sigma_percision)
      # the prior of each participant's probability for being unaware (perform
      # at chance level) or aware with equal probabilities
      delta[i] ~ dbern(.5)
      # For H1 model, set the participants P(correct) (= p) to either chance or
      # the conscious participant p, according to the 'delta' parameter.
      H1_participant_p[i] = ifelse(delta[i] == 0, chance_p, mu[i])
      # Set the participants P(correct) according to the 'M' parameter, deciding
      # between the models
      participant_p[i] = ifelse(M == 0, chance_p, H1_participant_p[i])
      # model the data as distributed binomally according to the participant P(correct),
      # and the number of trials (n_t) ) of the ith participant
      a[i] ~ dbin(participant_p[i], n_t[i])
    }
  }"

  # result_test <- GB_UNINF_MODEL
  result_test <- sapply(1:fixed_params@n_iterations, function(ind)
    generate_GB_UNINF_BF(obs_data$a_mat[,ind],obs_data$trials_mat[,ind])$BF)
  return(result_test)
}
gbf_uninformative_test_imp <- new(awareness_test_class, test_name="GB_Bayes_Uninformative",
run_test = gbf_uninformative_test_f, get_percent_significant=calc_sig_bayes)


## LMM test
# Defines an LMM test (test_name = 'LMM'), returns a p-values vector
LMM_test_f <- function(obs_data, fixed_params) {
  # define a function that returns the p-value for each simulation iteration
  get_iter_result <- function(iter_ind, a_mat, trials_mat) {
    # generate trial-by-trial (long) data frame for the given iteration:
    # is_correct - correctness in each trial as a vector. According to AS, we
    # concatenate the number of correct trials with inaccurate trials
    # subject_id - running number according to the number of trials
    is_correct <- unlist(sapply(1:length(a_mat), function(subj_ind)
      c(rep(1,a_mat[subj_ind]), rep(0,trials_mat[subj_ind]-a_mat[subj_ind]))))
    subject_id <- rep(1:length(a_mat), trials_mat)
    iter_data <- data.frame(subject_id = factor(subject_id), 
                            is_correct = factor(is_correct))
    # fit a logistic mixed model predicting correctness at the group-level
    LMM<-glmer(is_correct ~1+(1|subject_id),data=iter_data,family=binomial)
    coeff_Z <- summary(LMM)$coefficient[1, 'z value']
    #calculate one sided p-value
    pval <- pnorm(coeff_Z, lower.tail=F)
    return(pval)
  }
  # run the LMM function for each iteration to get p-values for one sided tests
  result_test <- sapply(1:ncol(obs_data$as_mat), function(iter)
    get_iter_result(trials_mat = obs_data$trials_mat[,iter],
                    a_mat = obs_data$a_mat[,iter]))
  
  return(result_test)
}
LMM_test_imp <- new(awareness_test_class, test_name="LMM", run_test=LMM_test_f)

## Global Null test
# tests if the prevalence if significant binomial tests on individual's
# accuracy is higher than expected under the hypothesis that there is
# no effect in any participant (test_name = 'GlobalNull')
golbalnull_test_f <- function(obs_data, fixed_params) {
  # get observed data variables
  trials_mat <- obs_data$trials_mat
  a_mat <- obs_data$a_mat
  # determine the expected false-positive rate for the binomial test
  # In the simulation the number of trials is the same for each participant and iteration
  n_trials <- trials_mat[1,1]
  # get the density and cumulative density for a binomial distribution at chance
  cdBinom <- pbinom(0:n_trials, n_trials, .5)

  # we use a two-sided test, so we need to add the cumsum on both ends
  sig_low <- head(which(cdBinom > fixed_params@alpha/2),1) - 1
  sig_high <- tail(which(cdBinom[sig_low] + (1-cdBinom) > fixed_params@alpha),1) + 1
  # index is zero based, so to get to S (number of corrects) we decrease 1
  sig_S_low <- sig_low - 1
  sig_S_high <- sig_high - 1
  exp_fpr <- cdBinom[sig_low] + (1 - cdBinom[sig_high])

  # the function returns the p-value for the global null test
  gn_test_iter <- function(S, N) {
    # get p.value according to a binomial test for each participant,
    # here using a two-sided test
    per_ind_p <- sapply(1:length(S), function(ind_p)
      # decide on significance according to the rejection zone
      (S[ind_p] <= sig_S_low) | (S[ind_p] > sig_S_high)
      )

    # extract number of significant participants (sig_ps) and number of participants (N_ps)
    sig_ps <- sum(per_ind_p)
    N_ps <- length(per_ind_p)
    # here we test if the number of significant participants is higher than
    # the expected FPR calculated above, with a one-sided binomial test
    pval = binom.test(sig_ps, N_ps, exp_fpr, alternative = "greater")$p.value
    return(pval)
  }
  # run the global null (gn) test for each iteration
  result_test <- sapply(1:fixed_params@n_iterations, function(ind)
    gn_test_iter(a_mat[,ind], trials_mat[,ind]))

  return(result_test)
}
golbalnull_test_imp <- new(awareness_test_class, test_name="GlobalNull", run_test=golbalnull_test_f)


#########################    Helper Functions    ############################# 
# gets the instance of the specified awareness test
awareness_tests <- get_instances(awareness_test_class)
get_awaereness_test <- function(test_name) {
  awareness_tests[sapply(awareness_tests, function(at) at@test_name == test_name)][[1]]
}

# gets the names of all available awareness tests
all_awareness_test_names <- sapply(get_instances(awareness_test_class), 
                                   function(at) at@test_name)
