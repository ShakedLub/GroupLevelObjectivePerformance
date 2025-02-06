#########################    Analysis Types    ############################# 
# Define analysis configuration class:  defines the data generating process 
# in each simulated condition. To add a new analysis, add an instance of the 
# 'analysis_type_class' object with the desired configuration (see below).

analysis_type_class <- 'analysis_type'
# Class fields
# analysis_type - a label for the analysis (e.g, 'Mixed')
# sample_conf - a data frame with (#populations X #sample parameters),
# with the following columns (sample parameters):
# grp - a population group index
# n_prop - the proportion of participants in each group (e.g., 1 for a single population)
# In the paper, parameter D indicates the n_prop for the aware group
# AS - awareness score for each population (the mu parameter for the relevant truncated normal distribution)
# SD_AS - standard deviation of the awareness scores for each population 
# (use 0 for a constant ASs; the sigma parameter for the relevant truncated normal distribution)
# a - the lower limit of AS (note, we do not provide control over max_AS) (
setClass(analysis_type_class, slots=list(analysis_type = 'character',
                                         sample_conf = "data.frame"))

## For example, to simulate a sample with equally sized groups, 
# one with a fixed AS of 0.55, and the another group
# where AS ~tnorm(mu=0.8, sigma=0.1, a=0, b=1) use:
# example_conf <- data.frame(grp = c(1,2), n_prop = c(.5,.5), AS = c(0.55, 0.8), 
#                            SD_AS = c(0, 0.1), a = c(.55, 0))

######   Define simulation analyses   ######
# Define the 'Mixed' analysis (two populations, one at chance, the other above chance)
D_Mixed <- .25
mixed_sample_conf <- data.frame(grp = c(1,2), n_prop = c(D_Mixed, 1-D_Mixed), 
                          AS = c(0.55, fixed_params@chance_as), SD_AS = c(0.05, 0),
                          a = c(fixed_params@chance_as, fixed_params@chance_as))
mixed_analysis_conf <- new(analysis_type_class, analysis_type = 'Mixed',
                           sample_conf = mixed_sample_conf)

# Define the 'Large spread' analysis (large SD around chance): 
D_Large_spread <- 1
large_spread_conf <- mixed_analysis_conf
large_spread_conf@analysis_type <- 'Large_spread'
large_spread_conf@sample_conf <- data.frame(grp = c(1), n_prop = c(D_Large_spread), 
                          AS = c(fixed_params@chance_as), SD_AS = c(.2),
                          a = c(0))

# Define the 'Small spread' analysis (small SD near chance): 
D_Small_spread <- 1
small_spread_conf <- mixed_analysis_conf
small_spread_conf@analysis_type <- 'Small_spread'
small_spread_conf@sample_conf <- data.frame(grp = c(1), n_prop = c(D_Small_spread), 
                                            AS = c(.525), SD_AS = c(.025),
                                            a = c(fixed_params@chance_as))


# Define the 'Unaware' analysis (all participants with chance performance)
D_Unaware <- 0
unaware_conf <- mixed_analysis_conf
unaware_conf@analysis_type <- 'Unaware'
unaware_conf@sample_conf <- data.frame(grp = c(1), n_prop = c(1 - D_Unaware), 
                                           AS = c(fixed_params@chance_as), SD_AS = c(0),
                                           a = fixed_params@chance_as)

#########################    Helper Functions    ############################# 
# gets the instance of the specified analysis, using the 'get_instances' function
all_analysis_types <- get_instances(analysis_type_class)
get_analysis_conf <- function(analysis_type) {
  all_analysis_types[sapply(all_analysis_types, function(at) at@analysis_type == analysis_type)][[1]]
}
