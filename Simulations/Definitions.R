#########################     Definitions     ########################## 
# common definitions to be used across the project scripts

#########################    Functions    ############################# 

# the function gets all instances implementing a certain class (by 'class_name')
get_instances <- function(class_name) {
  instances_filter <- Filter( function(x) class_name %in% class( get(x) ), 
                              ls(envir = .GlobalEnv))
  return(lapply( instances_filter, function(x) get(x)))
}

# creates a table with all parameters for the simulation according to the
# analysis types and fixed parameters
# analysis_types - a list of strings, one for each analysis type (see 'AnalysisTypes.R')
# fixed_params - all parameters that remain fixed across simulation conditions
create_sim_conditions_table <- function(analysis_types, fixed_params) {
  params_table <- tidyr::crossing(analysis_types,fixed_params@mean_trials,
                           fixed_params@prop_sd_trials, fixed_params@test, 
                           fixed_params@n_participants)
  # note that we allow for variance in the number of trials, yet don't use it
  # for testing our solutions (see below 'sd_trials')
  colnames(params_table) <- c('analysis_type', 'mean_trials', 'prop_sd_trials', 
                       'test', 'n_participants')
  # calculate # trials SD according to the mean and proportion SD of trials 
  params_table$sd_trials <- params_table$prop_sd_trials * params_table$mean_trials
  
  return(params_table)
}

# source awareness tests definitions script
source('AwarenessTests.R')

################## FIXED PARAMETERS DEFINITION #######################
# Define the fixed parameters of the simulation study:
# test = see AwarenessTests.R.
# mean_trials - average number of trials (practically here the average number of
# trials equals the true number of trials of each participant, 
# see 'prop_sd_trials' and 'trials_lim' for an option to introduce variability across
# participants in thre number of trials)
# chance_as - chance awareness score for unconscious participants (0.5 in a 2AFC task)
# prop_sd_trials- proportion of trials std, calculated as proportion from the average number of trials
# n_participants - number of participants
# n_iterations - number of iterations 
# alpha - alpha for the NHST tests
# trials_lim - vector with two values: min number of trials and max number of trials
# BF_threshold - threshold of the Bayesian t-test analysis
# note that prop_sd_trials and trials_lim are extensions that allow variability
# between participants in the number of trials yet were not examined in the paper.
fixed_params_class <- 'fixed_params'
setClass(fixed_params_class, slots=list(test="vector",
                                        chance_as = "numeric",
                                        prop_sd_trials = "numeric",
                                        n_iterations = "numeric",
                                        mean_trials = "vector",
                                        alpha = "numeric",
                                        trials_lim = "vector",
                                        BF_threshold = "numeric",
                                        n_participants ='vector'))
fixed_params <- new(fixed_params_class, 
                    test = all_awareness_test_names,
                    chance_as = 0.5,
                    prop_sd_trials= 0,
                    n_participants = c(10,20,30,50),
                    mean_trials = c(100,200,300),
                    n_iterations = 10000,
                    alpha = 0.05,
                    trials_lim = c(10,10000),
                    BF_threshold = 3)
