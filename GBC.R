## GBC test implementation

# Note that JAGS must be installed as a prerequisite.
# Also, note that you must source the AwarenessTests.R script for the function to run
source(file.path('Common', 'AwarenessTests.R'))
if (!require(BSDA)) {
  stop("Package 'rjags' is required but not installed.")
}
if (!require(dplyr)) {
  stop("Package 'rjags' is required but not installed.")
}

# Implemented by: Shaked Lublinsky and Itay Yaron
# Preprint: https://osf.io/preprints/psyarxiv/b967v_v1
GBC <- function(R,N,chance = 0.5,alpha = 0.05, tail = 'right') {
  # Input
  # R: number of correct responses for each participant
  # N: number of trials for each participant
  # chance: chance perfomance of task
  # alpha: alpha of statistical test 
  # tail: type of alternative hypothesis to evaluate 'both', 'right',
  # 'left'
  
  # Output
  # list including:
  # - h: TRUE = significant test result, FALSE = n.s. test result
  # - pval: the p-value of the test 

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
  
  # validate tail options
  if (!(tail %in% c('both','right','left'))) { stop('tail must be one of: both, right or left') }
  
  # transform input to gbc_f argument names
  tail <- ifelse(tail == 'right', 'greater', ifelse(tail == 'left', 'less', 'two.sided'))
  AS = R / N
  
  # run test
  pval <- gbc_f(data_as = AS, trials = N, chance = chance, tail = tail)
  # hypothesis testing
  h <- pval <= alpha
  res <- list(pval = pval, h = h)
  
  return(res)
}
