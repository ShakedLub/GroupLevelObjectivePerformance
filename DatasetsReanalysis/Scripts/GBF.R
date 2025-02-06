generate_GA_BF <- function(accuracy, n_trials, c_mu_mu_prior = .55, chance_level = .5, 
                           c_mu_sd_prior = .1, c_sd_mu_prior = .025, 
                           c_sd_sd_prior = .05, n_chains = 2, 
                           burining_period = 1500, iterations_per_chain = 5000) {
  # group together all parameters feeding the model
  model_data <- list(a = accuracy, n_t = n_trials, chance_p = chance_level,
                     c_mu_mu_prior = c_mu_mu_prior, 
                     c_mu_sd_prior = c_mu_sd_prior, 
                     c_sd_mu_prior = c_sd_mu_prior, 
                     c_sd_sd_prior = c_sd_sd_prior)
  # the parameters to monitor
  monitored_params <-c("M", "z_0", "group_mu_c")
  # create the model
  model <- jags.model( textConnection(GA_MODEL), model_data, n.chains = n_chains)
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
  GA_MODEL <- "model {
    # M parameter, with an uniformative prior,
    # this helps us estiamte the BF comparign the two models:
    # if M == 0, choose the H0 model (p = chance for all participants)
    # if M == 1, choose the H1 model (heirarchical model, with a parameter
    # 'z_0' for each participant indicating if p is sampled from
    # a 'conscious' awareness scores distribution (p != .5), otherwise
    # p is set to chance
    M ~ dbern(.5)
    ## set the prior on the conscious group's mu:
    # calculate percision for the given sd prior:
    # JAGS uses precision to parametrize a normal distribution (percision = sd^-2)
    group_c_mu_percision = pow(c_mu_sd_prior,-2)
    # set the prior: group_mu_c ~ TN(mu, mu_percision, chance-level prformance, 1) -
    # we use the T(,) JAGS function to truncate below chance and above 1 values 
    group_mu_c ~ dnorm(c_mu_mu_prior, group_c_mu_percision) T (chance_p,1)
    
    ## set the prior on the conscious group's sd (individual differences 
    ## between conscious participants):
    # calculate percision for the given sd prior:
    group_c_sd_sd_percision = pow(c_sd_sd_prior,-2)
    # set the prior: group_c_sd ~ TN(sd_mu, sd_percision, 0, 1) -
    # we use the T(,) JAGS function to truncate below 0 and above 1 values 
    group_c_sd ~ dnorm(c_sd_mu_prior, group_c_sd_sd_percision) T(0, 1)
    # get the percision for the sampled sd
    group_c_sd_percision = pow(group_c_sd,-2)
    
    # go over all participants in a (awareness measure # correct responses)
    for(i in 1:length(a) ) {
      # conscious participant's p distributes normally around the conscious group mu,
      # with percision according to the level of individual differences
      c_participant_p[i] ~ dnorm(group_mu_c, group_c_sd_percision)
      # set the prior of each participant's probability for being unaware (perform
      # at chance level) or aware with equal probabilities
      z_0[i] ~ dbern(.5)
      # For H1 model, set the participants P(correct) (= p) to either chance or
      # the conscious participant p, according to the 'z_0' parameter.
      H1_participant_p[i] = ifelse(z_0[i] == 0, chance_p, c_participant_p[i])
      # Set the participants P(correct) according to the 'M' parameter, deciding
      # between the models
      participant_p[i] = ifelse(M == 0, chance_p, H1_participant_p[i])
      # model the data as distributed binomally according to the participant P(correct),
      # and the number of trials (n_t)
      a[i] ~ dbin(participant_p[i], n_t[i])
    }
  }"
