####################    Main Simulation Function    ######################## 
# The function generates data according to the simulation parameters, runs the
# specified awareness test for all iterations and returns results as a data frame 
# including the significant results proportion and the simulation parameters, 
# Input:
# @param sum_params a row from the conditions table including the current
# combination of simulation parameters and awareness test to run
# @param fixed_params the fixed parameters of the simulation
run_simulation <- function(sim_params, fixed_params) {
  # Get the configuration of the analysis to conduct
  analysis_conf <- get_analysis_conf(sim_params$analysis_type)
  # get the observed data:
  obs_data <- generate_data(sim_params, analysis_conf@sample_conf, fixed_params)
  # get the relevant awareness test, and test for awareness
  awareness_test <- get_awaereness_test(sim_params$test)
  sim_result_test <- awareness_test@run_test(obs_data, fixed_params)
  # get the significance results according to the statistical test for all iterations 
  # for the frequentist tests, we get NAs for the H0_res
  significance_results <- awareness_test@get_percent_significant(sim_result_test, fixed_params)
  H1_res <- significance_results$H1 
  H0_res <- significance_results$H0 
  sim_params$sig_perc <- sum(H1_res)/length(H1_res)
  sim_params$null_perc <- sum(H0_res)/length(H0_res)
  
  # add all results of individual iterations as a list (used for the AUC analysis)
  sim_params$all_results <- list(sim_result_test)
  return(sim_params)
}

#########################    Helper Functions    ############################# 
# the function returns a vector of the true awareness scores of all 
# participants ('n_participants') in all iterations ('n_iter'),
# according to the sample configuration ('sample_conf')
get_true_ASs <- function(n_participants, samp_conf, n_iter) {
  # number of subjects in the given group
  n_grp = round(n_participants * samp_conf$n_prop) * n_iter
  # true awareness scores of conscious subjects drawn from a truncated normal distribution,
  # with limits defined according to the sample_configuration [a,1]
  if(samp_conf$SD_AS == 0) {
    true_as <- rep(samp_conf$AS, n_grp)
  } else {
    true_as <- rtnorm(n_grp, mean= samp_conf$AS ,sd=samp_conf$SD_AS, a=samp_conf$a ,b=1)
  }
  return(true_as)
}

# the function gets the simulation configuration arguments and returns all
# observed data features as a list:
# a_mat = number of correct responses (a vector)
# as_mat = success rate (a matrix: participants X iterations)
# trials_mat - number of trials per participant (a matrix: participants X iterations)
generate_data <- function(sim_params, sample_conf, fixed_params) {
  n_participants <- sim_params$n_participants
  iterations <- fixed_params@n_iterations
  # Samples the number of trials for each subject and iteration (a vector of all results)
  n_trials <- get_ntrials(sim_params, fixed_params)
  n_trials_mat=matrix(n_trials,nrow=n_participants ,ncol=iterations)
  # get the true ASs (awareness scores) for all participants, 
  # depending on the sample configuration.
  # for conscious participants we sample from a truncated normal distribution.
  all_as <- do.call(rbind, lapply(1:nrow(sample_conf), function(grp_ind)
    matrix(get_true_ASs(n_participants, sample_conf[grp_ind,], iterations), 
           nrow = round(n_participants * sample_conf[grp_ind,'n_prop']), ncol = iterations)))
  # awareness score correctness drawn from a binomial distribution, according to 
  # the number of trials and true success rate
  a <- rbinom(n= n_participants * iterations, size= n_trials, prob= as.vector(all_as))
  a_mat <- matrix(a,nrow=n_participants ,ncol=iterations)
  # Observed success rate for each subject in each iteration
  as <- a/n_trials
  as_mat <- matrix(as, nrow=n_participants ,ncol=iterations)
  return(list(a_mat = a_mat, as_mat = as_mat, trials_mat = n_trials_mat))
}

# the function returns a vector of the number of trials per participant for 
# all iterations , given the simulation parameters ('sim_params'), and the fixed
# parameters of the simulation ('fixed_params')
get_ntrials <- function(sim_params, fixed_params) {
  iterations <- fixed_params@n_iterations

  # draw number of trials randomly from a truncated normal distribution
  n_all_participants <- sim_params$n_participants * iterations
  if(fixed_params@prop_sd_trials == 0) {
    n_trials <- rep(sim_params$mean_trials,n_all_participants)
  } else {
    n_trials <- round(rtnorm(n_all_participants, mean=sim_params$mean_trials, 
                             sd=sim_params$sd_trials,
                             a=fixed_params@trials_lim[1],b=fixed_params@trials_lim[2]))
  }
  return(n_trials)
}
