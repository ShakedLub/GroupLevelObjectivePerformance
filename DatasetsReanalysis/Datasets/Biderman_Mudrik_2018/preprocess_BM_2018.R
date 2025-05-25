# Load libraries
library(R.matlab)
library(dplyr)
library(tidyverse)

get_BM_data <- function(exp = 0, is_exc = FALSE) {
  main_exp_tib <- getTibble(exp,F)
  post_test_tib <- getTibble(exp,T)
  # fix encoding problem
  post_test_tib$subNum[post_test_tib$subNum == 16] <- 15 
  
  #clean errorneously encoded trials
  main_exp_tib <- subset(main_exp_tib, (resTarCon <= 4) &(primeConRes <= 4) &(visRate <=4))
  #clean 99 trials of post test
  post_test_tib <- subset(post_test_tib, (primeConRes != 99))
  
  # exclude subjects based on irrelevant conditions
  # clean subjects that didn't perform the post test (as in the original study)
  if(exp == '1') {
    # subj 16 did the PT but the encoding of responses was incorrect
    exc <- c(16,18,31,34) # they also had too low number of trials
  }
  else if(exp == '2') {
    exc <- c(953,931)# 954 was added, we can't compute an effect (excluded in the original study based on not enough correct trials)
  }
  else {
    # no need to filter out subjects in other experiments  
    exc <- c()
  }
  main_exp_tib <- subset(main_exp_tib, !(subNum %in% exc))
  
  # exclude participants with no low visibility rate
  exc <- c(exc, unique(post_test_tib$subNum[!post_test_tib$subNum %in% subset(post_test_tib, visRate==1)$subNum]))
  
  #if exp 2 keep track of < 70% accuracy subjects for exclusion
  under_performers <- c()
  if(exp == 2) {
    under_performers <- getBadTarAcc(main_exp_tib)
  }
  
  # Main Exp clean RT <300 and >4000
  main_exp_tib <- subset(main_exp_tib, (RTTarRes >= 0.3) &(RTTarRes <=4))
  
  # Main exp exclusion - deviant trials inside a subject
  # create group z score and ungrouped one as well
  main_exp_tib <- main_exp_tib %>%
    group_by(subNum, primeCon, tarPrimeConRel, blockNum) %>%
    mutate(z_score_exp_cell = scale(RTTarRes)) %>%
    ungroup %>%
    mutate(z_score_all_trials = scale(RTTarRes))
  # filter out the deviant rt (grouped z score inside the experiment cell (primeCon*primeTargetRel) > 3) 
  main_exp_tib <- subset(main_exp_tib, abs(z_score_exp_cell) < 3)
  
  # exclude above 1 visibility trials and inaccurate trials
  main_exp_tib_all <- subset(main_exp_tib, visRate==1)
  main_exp_tib <- subset(main_exp_tib, visRate==1 & tarAcc ==1)
  #get < than 25 trials per subject
  low_trial_num_subjects <- getLowerThanNTrials(main_exp_tib)
  # subject 10 had marginal amount of trials and was included
  low_trial_num_subjects <- low_trial_num_subjects[low_trial_num_subjects != 10]
  exc <- c(exc,unique(low_trial_num_subjects))
  
  # filter out non vis 1 and incorrect responses for the post test data
  post_test_tib<- subset(post_test_tib, visRate==1)
  
  # PT exclusion criterion of original study
  if(is_exc == TRUE) {
    over65_post_test_subjects <- getOver65PTAcc(post_test_tib)
  } else {
    over65_post_test_subjects <- c()
  }
  
  # get excluded subjects
  exc <- c(exc, unique(over65_post_test_subjects))
  
  trial_by_trial_pt <-  post_test_tib %>%
    filter(!subNum %in% exc) %>%
    mutate(exp = paste("Biderman & Mudrik_2018", 'Orientation', exp, sep = '_')) %>%
    rename(subj = subNum, correct = primeConAcc) %>%
    dplyr::select(subj, correct, exp)
  trial_by_trial_main <-  main_exp_tib_all %>%
    filter(!subNum %in% exc) %>%
    mutate(exp = paste("Biderman & Mudrik_2018", 'Congruency', exp, sep = '_')) %>%
    rename(subj = subNum, correct = primeConAcc) %>%
    dplyr::select(subj, correct, exp)
  
  summary_visibility_orientation <- trial_by_trial_pt %>%
    group_by(subj, exp) %>%
    summarise(SR = mean(correct), ntrials = n()) %>%
    dplyr::select(subj, ntrials, SR, exp)
  summary_visibility_congruency <- trial_by_trial_main %>%
    group_by(subj, exp) %>%
    summarise(SR = mean(correct), ntrials = n()) %>%
    dplyr::select(subj, ntrials, SR, exp)
  trial_by_trial_data <- rbind(trial_by_trial_pt, trial_by_trial_main)
  
  summary_visibility <- rbind(summary_visibility_orientation, summary_visibility_congruency)
  return (list(trial_by_trial = trial_by_trial_data, summary = summary_visibility)) 
}

getLowerThanNTrials <- function(dataTib, N = 25) {
  #exclusion criterion of original study
  # - < 25 trialsin each experiment
  trials_freq_tib <-  dataTib %>% group_by(subNum, primeCon,tarPrimeConRel) %>% summarise(freq = length(subNum))
  exc_data <- subset(trials_freq_tib, freq < N)
  low_trials_subjects <- unique(exc_data$subNum)
  return (low_trials_subjects)
}

getOver65PTAcc <- function(dataTib, threshold = 0.65) {
  # - > 65% performance
  vis_acc_data_tib <- dataTib %>% group_by(subNum) %>% summarise(visibility = mean(primeConAcc))
  high_perf <- subset(vis_acc_data_tib, visibility > threshold)
  high_perf_subjects <- unique(high_perf$subNum)
  return (high_perf_subjects)
}

getBadTarAcc <- function(data, lowerLim = 0.7) {
  #summerize to get accuracy
  summ_data <- data %>% group_by(subNum) %>% summarise(tarAccPercent = mean(tarAcc))
  low_perf <- subset(summ_data, tarAccPercent < lowerLim)
  under_performers <- unique(low_perf$subNum)
  return (under_performers)
}
getTibble <- function(exp, isPostTest = F) {
  # get the directory containing the files
  prefix <- 'DatasetsReanalysis\\Datasets\\Biderman_Mudrik_2018\\data\\'
  # assign different path and column names according to PT/Main exp data type we are reading
  if(isPostTest) {
    file_prefix = 'PostTest_'
    columnNames <-   c('subNum','trialNum', 'primeCon', 'tarPrimeConRel', 'primeImg', 'tarCon', 'tarImg', 'primeConRes', 'primeConAcc', 'primeConRT' , 'visRate', 'visRT')
  }
  else {
    file_prefix <- 'Data_Main_'
    columnNames <-   c('subNum','trialNum', 'primeCon', 'tarPrimeConRel', 'primeImg', 'tarCon', 'tarImg', 'resTarCon', 'tarAcc', 'RTTarRes', 'visRate', 'visRT', 'primeConRes', 'primeConRT', 'primeConAcc', 'primeContrast', 'maskContrast', 'blockNum')
  }
  postfix <- '.mat'
  path <- paste(prefix,file_prefix,"Exp",exp,postfix, sep="")
  
  # read the data and convert it to a tibble for easier data manipulations
  data <- readMat(path)
  df <- data.frame(data)
  tib <- as_tibble(df)
  names(tib) <- columnNames
  return (tib)
}

all_exps_data <- list(get_BM_data(1), get_BM_data(2), get_BM_data(3))
summary_tables <- do.call(rbind, lapply(all_exps_data, function(dat) dat$summary))
trial_by_trial_tables <- do.call(rbind, lapply(all_exps_data, function(dat) dat$trial_by_trial))
processed_data <- list(trial_by_trial = trial_by_trial_tables, summary_tables = summary_tables)
save(processed_data, file = 'DatasetsReanalysis\\Datasets\\Biderman_Mudrik_2018\\all_Biderman_Mudrik_2018.RData')

## validation:
Table1_exp1_avg_SR <- round(mean(summary_tables %>% 
                                   filter(exp == "Biderman & Mudrik_2018_Congruency_1") %>% 
                                   pull(SR)), 3)
if(Table1_exp1_avg_SR != 0.496) {print('not valid')}