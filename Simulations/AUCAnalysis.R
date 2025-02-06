#########################    AUC Analysis    ############################# 
# Performed AUC analysis according to the results of the different tests
# considering the unaware analysis type as negative cases,
# and all other analysis types as positive cases, together providing a
# sensitivity measure that is not dependent on a fixed significance level

# Load required R packages and sources
rm(list=ls())
library(groundhog)
pkgs <- c("extraDistr","matrixTests", "BSDA", "lme4", "tidyverse", "RColorBrewer",
          "patchwork", "scales", "pROC", "BayesFactor", "gridExtra", "doSNOW", 'parallel',
          "rjags","dplyr","pROC")
groundhog.library(pkgs, "2023-08-08", tolerate.R.version = '4.3.2')

## load all results to one dataframe
fileName=Sys.glob('./Output/*.RData')

# load results
load(fileName)

# the function gets all of the simulation results and returns
# a data frame of all "power" conditions, with an additional 'AUC' column
# in which ROC analysis was performed on each power condition compared
# with the respective FPR condition (each power condition was paired with
# the 'Unaware' condition with the same number of trials and participants, for
# the same test, and the resulting p-values / BFs were fed into a ROC
# analysis)
get_AUC_per_cond <- function(all_results, max_res = 10^32) {
  AUCs <- c()
  # separate power and FPR dataframes
  power_results <- all_results %>% 
    filter(analysis_type != 'Unaware')
  fpr_results <- all_results %>% 
    filter(analysis_type == 'Unaware')
  # go over each power condition, find its paired FPR condition, and feed
  # pROC::roc function with the p-values / BFs of the test as well as the
  # true labels for each result (either 0 for an FPR result or 1 for a power result)
  for (cond_ind in 1:nrow(power_results)) {
    # get power condition results (positive cases)
    cond_power <- power_results[cond_ind,]
    # get FPR condition results (negative cases)
    cond_fpr <- fpr_results %>% 
      filter(test == cond_power$test, 
             mean_trials == cond_power$mean_trials, 
             n_participants == cond_power$n_participants) 
    is_bayes <- grepl('Bayes', cond_power$test)
    if (is_bayes) {
      # for Bayesian tests we flip the BF so that lower values would lend
      # evidence for an effect (as is the case for p-values)
      cond_power$all_results[[1]] <- 1/cond_power$all_results[[1]]
      cond_fpr$all_results[[1]] <- 1/cond_fpr$all_results[[1]]
    }
    # generate a results vector (p-values / BFs) and true labels vector
    results <- c(cond_power$all_results[[1]], cond_fpr$all_results[[1]])
    # we want to avoid infinity values, setting the maxmimal result to the
    # 'max_res'
    results[results > max_res] <- max_res
    # generate the true labels for AUC analysis (positive / negative case)
    true <- c(rep(1,length(results)/2), rep(0,length(results)/2)) 
    # get the area under the curve (AUC) for each ROC analysis
    cond_AUC <- pROC::roc(true, results, direction = ">")$auc
    AUCs <- c(AUCs, cond_AUC)
  }
  # add the results to the power results dataframe
  auc_results_df <- power_results
  auc_results_df$AUC <- AUCs
  return(auc_results_df)
}

# generating a plot for the AUC results
# the code here is a merge between code from the RttM paper and from the ScatterPlots.R script
generate_AUC_plot <- function(auc_results_df, plot_tests = c('all'), color_tests) {
  if(all(plot_tests != 'all')) {
    filtered_tests_df <- auc_results_df %>%
      filter(test %in% plot_tests)
  }
  else {
    filtered_tests_df <- auc_results_df
  }
  
  # arrange data for plots
  # change order of analysis type presentation 
  filtered_tests_df$analysis_type=factor(filtered_tests_df$analysis_type,levels=c('Unaware','Mixed','Small_spread','Large_spread'))
  
  # new facet label names for simulation names
  analysisName.labs <- c('Mixed','Aware small var','Aware large var')
  names(analysisName.labs) <- c('Mixed','Small_spread','Large_spread')

  # plot
  plt <- filtered_tests_df %>%
    ggplot(aes(x = n_participants, y = AUC, group = test, color = test)) +
    geom_line(alpha=0.4) +
    geom_point(size=2,alpha=0.4) + 
    theme_bw() +
    xlab('Participants') +
    ylab('AUC') +
    facet_grid(analysis_type ~ mean_trials, switch = 'y', scales = "free", labeller = labeller(analysis_type = analysisName.labs)) +
    scale_color_manual(values=color_tests,name="")+
    theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.y= element_text(size=12),
        axis.text.x = element_text(size=12),
        strip.text.x = element_text(size=12),
        legend.position = "bottom",
        strip.text.y  = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
    
  return(plt)
}

# Update test names
all_results$test=recode_factor(all_results$test, GBorChi="GBC")
all_results$test=recode_factor(all_results$test, LMM="MMLR")
all_results$test=recode_factor(all_results$test, GB_Bayes="GBBayes")

all_tests <- c("GBC","T","MMLR","GBBayes","GB_Bayes_Uninformative","TBayes","GB","Chi","GlobalNull")

main_tests <- c("GBC","T","MMLR","GBBayes","TBayes")
color_main_tests <- c("red","darkorange2","cyan4","purple","blue")

global_null_tests <- c("GBC", "GlobalNull")
color_global_null_tests <- c("red","darkturquoise")

bayesian_tests <- c("GBBayes","GB_Bayes_Uninformative","TBayes")
color_bayesian_tests <- c("purple","forestgreen","blue")

#calculate AUC
all_tests_AUCs <- get_AUC_per_cond(all_results)

# change order of statistical tests 
all_tests_AUCs$test=factor(all_tests_AUCs$test,levels=c("GBC","T","MMLR","GBBayes","GB_Bayes_Uninformative","TBayes","GB","Chi","GlobalNull"))

#create plots
plt_main_AUCs <- generate_AUC_plot(all_tests_AUCs, main_tests, color_main_tests)
plt_GN_AUCs <- generate_AUC_plot(all_tests_AUCs, global_null_tests, color_global_null_tests)
plt_bayes_AUCs <- generate_AUC_plot(all_tests_AUCs, bayesian_tests, color_bayesian_tests)

#save plots
ggsave('./Output/mainAUC.jpg',plt_main_AUCs)
ggsave('./Output/GNAUC.jpg',plt_GN_AUCs)
ggsave('./Output/bayesAUC.jpg',plt_bayes_AUCs)
#ggsave('Output//mainAUC.svg', plt_main_AUCs, width = 12, height = 12)
#ggsave('Output//GNAUC.svg', plt_GN_AUCs, width = 12, height = 12)
#ggsave('Output\\bayesAUC.svg', plt_bayes_AUCs, width = 12, height = 12)
