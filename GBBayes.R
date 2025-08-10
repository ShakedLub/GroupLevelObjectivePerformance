## GBBayes test implementation

# Note that you must source the AwarenessTests.R script for the function to run
source(file.path('Common', 'AwarenessTests.R'))
if (!require(rjags)) {
  stop("Package 'rjags' is required but not installed.")
}
if (!require(dplyr)) {
  stop("Package 'rjags' is required but not installed.")
}

# Implemented by: Shaked Lublinsky and Itay Yaron
# Preprint: https://osf.io/preprints/psyarxiv/b967v_v1
GBBayes <- function(R,N,chance = 0.5,BF_threshold = 3,
                    AS_low_bound = .5,
                    theta_mu_prior = .55, theta_sig_prior = .1,
                    sigma_mu_prior = .025, sigma_sig_prior = .05,
                    n_chains = 2, burining_period = 1500, iterations_per_chain = 5000) {
  # Input
  # R: number of correct responses for each participant
  # N: number of trials for each participant
  # chance: chance performance of task
  # BF_threshold: BF_threshold for hypothesis testing
  # AS_low_bound: the lower bound on true awareness scores (for theta parameter)
  # theta_mu_prior: prior on the theta parameter expected value
  # theta_sig_prior: prior on the theta parameter dispersion
  # sigma_mu_prior: prior on the sigma parameter expected value
  # sigma_sig_prior: prior on the sigma parameter dispersion
  # n_chains: number of chains used in BF estimation
  # burning_period: burning period for BF estimation
  # iterations_per_chain: number of chains to use for estimating BF
  
  # Output
  # list including:
  # - h: 1 = above threshold BF, 0 = inconclusive BF, -1 = below 1/threshold BF
  # - BF: the BF10 of the test 
  
  #  validate on inputs
    if (!is.null(dim(R))) { stop('R should be a vector') }
  if (!is.null(dim(N))) { stop('N should be a vector') }
  if (length(N) != length(R)) { stop('N and R should be the same length') }
  # validate values
  validate_input_vec <- function(vec, type) {
    if (!is.numeric(vec)) { stop(paste(type, 'Input vector must be a numeric vector')) }
    if (any(is.na(vec))) { stop(paste(type, 'vector contains NA values')) }
    if (any(vec != round(vec))) { stop(paste(type, 'vector must contain only integers.')) }
    
    return(TRUE)
  }
  validate_input_vec(R, 'R')
  validate_input_vec(N, 'N')
  
  # run test
  BF <- generate_GB_BF(accuracy = R, n_trials = N, chance_level = chance,
                       low_bound = AS_low_bound,
                       theta_mu_prior = theta_mu_prior, 
                       theta_sig_prior = theta_sig_prior,
                       sigma_mu_prior = sigma_mu_prior, 
                       sigma_sig_prior = sigma_sig_prior,
                       n_chains = n_chains, 
                       burining_period = burining_period, 
                       iterations_per_chain = iterations_per_chain)
  BF <- BF$BF
  # hypothesis testing
  h <- ifelse(BF > BF_threshold, 1, ifelse(BF < 1/BF_threshold, -1, 0))
  res <- list(BF = BF, h = h)
  
  return(res)
}
