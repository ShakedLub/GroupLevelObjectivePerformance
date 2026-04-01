
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
# implementation of an awareness test in other script files
