# This simulation tests prior sensitivity for GBBayes to examine group level awareness using the prior settings
# as detailed in the SM Chapter 4

# Load required R packages and sources
rm(list=ls())
library(groundhog)
pkgs <- c("extraDistr","matrixTests", "BSDA", "lme4", "tidyverse", "RColorBrewer",
          "patchwork", "scales", "pROC", "BayesFactor", "gridExtra", "doSNOW", 'parallel',
          "rjags","dplyr")
groundhog.library(pkgs, "2025-03-01", tolerate.R.version = '4.5.1')
source("./Common/TestingInfrastructure.R")
source("./Simulations/Scripts/PriorSensitivityAwarenessTests.R") 
source("./Common/Definitions.R")
source("./Simulations/Scripts/AnalysisTypes.R")
source("./Simulations/Scripts/Simulation.R")

# Configuration: define the simulation configuration (see AnalysisTypes.R)
analysis_types <- c('Mixed', 'Small_spread', 'Large_spread', 'Unaware')

# For prior the sensitivity analysis we use less iterations
fixed_params@n_iterations <- SM_prior_sensitivity_iterations

# Initialize a data frame in which each combination of parameters comprise a 'condition'
# that will be simulated and tested for group-level awareness:
sim_conditions_table <- create_sim_conditions_table(analysis_types, fixed_params)

############################    Simulation    ################################ 
# set up a cluster for running simulation conditions in parallel 
sim_cluster <- makeCluster(detectCores() -1, outfile="") 
parallel::clusterExport(sim_cluster, 
                        c("generate_GB_BF", "GB_MODEL",
                          "prior_sensitivity_gbf_f"))
registerDoSNOW(sim_cluster)
# define a progress bar to track progress of the simulation
progress_bar <- txtProgressBar(max = nrow(sim_conditions_table), style = 3)
progress_f <- function(step) setTxtProgressBar(progress_bar, step)
do_snow_opts <- list(progress = progress_f)

# run the simulation - to debug, change 'dopar' to 'do' (avoids parallelization)
# iterates over the simulated conditions table, gets the current 
# simulated condition and obtain the results of the simulation
all_results <- foreach(ind = 1:nrow(sim_conditions_table), .combine = rbind,
                       .packages = pkgs, .options.snow = do_snow_opts) %dopar%
  {
    #set random number generator for each iteration to facilitate replicating
    set.seed(ind)
    current_params <- sim_conditions_table[ind,]
    # the results are combined into a large results table
    return(run_simulation(current_params, fixed_params))
  }
close(progress_bar)
stopCluster(sim_cluster)

############################    Save results    ################################ 
save_fn <- paste0('Simulations\\Output\\',paste(paste(analysis_types,collapse = '_'), "prior_sensitivity_sim_data.RData", sep = '_'))
save(all_results, sim_conditions_table, fixed_params, file=save_fn)

