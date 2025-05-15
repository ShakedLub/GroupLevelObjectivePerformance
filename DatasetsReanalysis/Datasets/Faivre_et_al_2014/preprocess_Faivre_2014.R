# Load libraries
library(R.matlab)
library(dplyr)
library(tidyverse)
library(data.table)

get_Faivre_data <- function(exp = 0) {
  expFiles <- dir(paste0('datasets\\Faivre_et_al_2014\\data\\Exp',exp), full.names = T)
  # read first file, then the rest in a loop
  data <- data.table(do.call(rbind, lapply(expFiles, read.table, header = F)))
  # assign column names
  names(data) <- c('initials','subNum','trialNum','primeCon','primeTargetRelCon',
                   'auditoryPrimeUnMasked','visualPrimeUnMasked','auditoryPrimeStim',
                   'auditoryTarStim','visualPrimeStim','visualTarStim','tarResponse','tarRT',
                   'primeResponse','primeRT' )
  # process data:
  # create unique ID and exp label
  # create a 'tarCon' column for target congruency, similar to rel/unrel column
  # create an 'objt' column for accuracy in target congruency response
  # create an 'objp' column for accuracy in prime congruency response
  data <- data %>%
    mutate(exp = paste('Faivre et al.', exp, sep = ' '),
           subj = paste(subNum,exp, initials),
           tarCon = ifelse(visualTarStim == auditoryTarStim, 'cong','incong'),
           objp = ifelse((((primeCon  == 'cong') & (primeResponse == 1)) | 
                            ((primeCon == 'incong') & (primeResponse == 0))),1,0),
           objt = ifelse((((tarCon  == 'cong') & (tarResponse == 1)) | 
                            ((tarCon == 'incong') & (tarResponse == 0))),1,0))
  #get unconscious trails + remove 5 first trials (were considered training)
  #in experiment 2 the auditory prime was masked, 
  #in the other experiments the conscious/unconscious split was according to the visual prime
  if(exp %in% c(2,5, 7)) {
    data <- subset(data,auditoryPrimeUnMasked ==0 & trialNum >5)
  } else {
    data <- subset(data,visualPrimeUnMasked ==0 & trialNum >5)
  }
    
  # remove extreme RT trials
  if(exp < 5) {
    data <- subset(data, !((tarRT>4000 | tarRT<300)))
  }
  # summarize the accuracy for every subject for both prime and target congruecy responses
  # find out which of the subjects performed above 65%
  if(exp == 6) {
    print(11)
  }
  summary_visibility <- data %>% 
    group_by(subj, exp) %>% 
    summarise(SR = mean(objp), ntrials = n())
  over_performers_subs <- getOver65PTAccuracy(summary_visibility)
  summary_visibility <- summary_visibility %>%
    filter(!subj %in% over_performers_subs)
  trial_by_trial_data <- data %>% 
    filter(!subj %in% over_performers_subs) %>%
    rename(correct = objp) %>%
    dplyr::select(subj,correct, exp)
  return (list(trial_by_trial = trial_by_trial_data, summary = summary_visibility)) 
}

getOver65PTAccuracy <- function(data, threshold = 0.65) {
  # - > 65% performance
  high_perf <- subset(data, SR > threshold)
  high_perf_subjects <- unique(high_perf$subj)
  return (high_perf_subjects)
}

all_exps_data <- list(get_Faivre_data(1), get_Faivre_data(2),
                  get_Faivre_data(3), get_Faivre_data(4), 
                  get_Faivre_data(6), get_Faivre_data(7), get_Faivre_data(8))
summary_tables <- do.call(rbind, lapply(all_exps_data, function(dat) dat$summary))
trial_by_trial_tables <- do.call(rbind, lapply(all_exps_data, function(dat) dat$trial_by_trial))
processed_data <- list(trial_by_trial = trial_by_trial_tables, summary_tables = summary_tables)
save(processed_data, file = 'datasets\\Faivre_et_al_2014\\Faivre et al._2014.RData')

## validation:
Table2_all_avg_SR <- summary_tables %>% 
  group_by(exp) %>% 
  summarise(SR = trunc(mean(SR) * 10^3) / 10^3) %>%
  pull(SR) %>%
  first()
Table2_exp1_avg_SR <- summary_tables %>% 
  group_by(exp) %>% 
  summarise(SR = trunc(mean(SR) * 10^3) / 10^3) %>%
  pull(SR) %>%
  first()
if(Table2_exp1_avg_SR != 0.533) {print('not valid')}