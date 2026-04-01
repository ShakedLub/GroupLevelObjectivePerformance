# GBF test
# Defines a GBF test (test_name = 'GB_Bayes'), returns a BF vector
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

# 
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

gbf_f <- function(data, trials, chance = 0.5) {
  res <- generate_GB_BF(data,trials)$BF
  return(res)
}
gbf_test_f <- function(obs_data, fixed_params) {
  # result_test <- GB_MODEL
  result_test <- sapply(1:fixed_params@n_iterations, function(ind)
    gbf_f(obs_data$a_mat[,ind],obs_data$trials_mat[,ind]))
  return(result_test)
}

## prior sensitivity analysis for GB-Bayes (used: theta_mu = .55, theta_sig = .1 and sigma_mu = .025, sigma_sig = .05)
# 1) robustness to theta priors:
# 1.1) wide sigma (.2) + same mu (.55)
# 1.2) wide sigma (.2) + high mu (.65)
# 1.3) same sigma (.1) + high mu (.65)
# 2) robustness to sigma priors:
# 2.1) wide sigma (.15) + same mu (.025)
# 2.2) wide sigma (.15) + high mu (.05)
# 2.3) same sigma (.05) + high mu (.05)

# helper functions to create different prior sensitivity tests
prior_sensitivity_gbf_f <- function(data, trials, chance = 0.5,
                                    theta_mu_prior = .55, theta_sig_prior = .1,
                                    sigma_mu_prior = .025, sigma_sig_prior = .05,
                                    burining_period = 3000, iterations_per_chain = 10000
                                    ) {
  res <- generate_GB_BF(data,trials,
                        theta_mu_prior = theta_mu_prior, 
                        theta_sig_prior = theta_sig_prior,
                        sigma_mu_prior = sigma_mu_prior, 
                        sigma_sig_prior = sigma_sig_prior,
                        burining_period = burining_period,
                        iterations_per_chain = iterations_per_chain 
  )$BF
  return(res)
}
prior_sensitivity_gbf_test_f <- function(prior_args) {
  prior_sens_test_f <- function(obs_data, fixed_params) {
    result_test <- sapply(seq_len(fixed_params@n_iterations), function(ind) {
      args <- c(
        list(
          obs_data$a_mat[, ind],
          obs_data$trials_mat[, ind]
        ),
        prior_args
      )
      do.call(prior_sensitivity_gbf_f, args)
    })
  }
}

## 1) robustness to theta priors:
# 1.1) wide sigma (.15) + same mu (.55)
prior_sensitivity_test_f_1_1 <- prior_sensitivity_gbf_test_f(list(theta_mu_prior = .55, 
                                                                  theta_sig_prior = .15))
# 1.2) wide sigma (.15) + high mu (.6)
prior_sensitivity_test_f_1_2 <- prior_sensitivity_gbf_test_f(list(theta_mu_prior = .6, 
                                                                  theta_sig_prior = .15))
# 1.3) same sigma (.1) + high mu (.6)
prior_sensitivity_test_f_1_3 <- prior_sensitivity_gbf_test_f(list(theta_mu_prior = .6, 
                                                                  theta_sig_prior = .1))
# 2) robustness to sigma priors:
# 2.1) wide sigma (.1) + same mu (.025)
prior_sensitivity_test_f_2_1 <- prior_sensitivity_gbf_test_f(list(sigma_mu_prior = .1, 
                                                                  sigma_sig_prior = .025))
# 2.2) wide sigma (.1) + high mu (.05)
prior_sensitivity_test_f_2_2 <- prior_sensitivity_gbf_test_f(list(sigma_mu_prior = .1, 
                                                                  sigma_sig_prior = .05))
# 2.3) same sigma (.05) + high mu (.05)
prior_sensitivity_test_f_2_3 <- prior_sensitivity_gbf_test_f(list(sigma_mu_prior = .05, 
                                                                  sigma_sig_prior = .05))
# tests implementation
prior_sensitivity_test_f_1_1_imp <- new(awareness_test_class, test_name="ps_1_1_GB_Bayes",
                                        run_test = prior_sensitivity_test_f_1_1, get_percent_significant=calc_sig_bayes)
prior_sensitivity_test_f_1_2_imp <- new(awareness_test_class, test_name="ps_1_2_GB_Bayes",
                                        run_test = prior_sensitivity_test_f_1_2, get_percent_significant=calc_sig_bayes)
prior_sensitivity_test_f_1_3_imp <- new(awareness_test_class, test_name="ps_1_3_GB_Bayes",
                                        run_test = prior_sensitivity_test_f_1_3, get_percent_significant=calc_sig_bayes)
prior_sensitivity_test_f_2_1_imp <- new(awareness_test_class, test_name="ps_2_1_GB_Bayes",
                                        run_test = prior_sensitivity_test_f_2_1, get_percent_significant=calc_sig_bayes)
prior_sensitivity_test_f_2_2_imp <- new(awareness_test_class, test_name="ps_2_2_GB_Bayes",
                                        run_test = prior_sensitivity_test_f_2_2, get_percent_significant=calc_sig_bayes)
prior_sensitivity_test_f_2_3_imp <- new(awareness_test_class, test_name="ps_2_3_GB_Bayes",
                                        run_test = prior_sensitivity_test_f_2_3, get_percent_significant=calc_sig_bayes)
