library(tidyverse)
library(stats)
library(ggplot2)
library(psych)
library(splithalf)


# task = 0 for category task and task = 1 for shape task
get_Hesselman_2016_data <- function(exp = -1, tsk = 0) {
  task_str <- ifelse(tsk == 0, "Category","Shape")
  
  # get the data
  expNum = as.numeric(exp)
  data_awareness <- read.csv("datasets\\Hesselman_etal_2016\\data\\Hesselmann_2017_-_check1.csv", header = TRUE, sep=";")
  data_main <- read.csv("datasets\\Hesselman_etal_2016\\data\\Hesselmann_2017_-_main.csv", header = TRUE, sep=";")
  
  # rename column name
  colnames(data_awareness)[1] <- 'exp'
  
  # remove data of subjects that were excluded based on conditions not relevant to awareness
  if(exp == 1) {
    irrelevantSubjs <- c(1,2,3) # pilot subjects
    fewPAS1Subs <- c(24,28) # additional exclusions
    alphaAwareness <- 0.05
  }
  else if (exp ==2) {
    irrelevantSubjs <- c(8,17) # two subjects excluded because they didn't complete the task instructions
    fewPAS1Subs <- c()
    alphaAwareness <- 0.025
  }
  
  # add subjective awareness of 2 for participants with more such responses than
  # subjective awareness of 1, if when using both 1,2 ratings keeps performance
  # below significance level
  data_12 <- data_main %>% 
    mutate(resp2 = factor(resp2)) %>% 
    filter(exp == expNum, !subj %in% c(irrelevantSubjs, fewPAS1Subs), 
           as.integer(resp2) <= 2) %>% 
    group_by(subj, resp2, .drop = FALSE) %>% 
    count()
  more_subj2_participants <- unique(data_12$subj)[sapply(unique(data_12$subj), function(s) 
    data_12[data_12$subj == s & data_12$resp2 == 1,]$n < 
      data_12[data_12$subj == s & data_12$resp2 == 2,]$n)]
  
  # take only relevant experiment data
  data_awareness_all <-  data_awareness %>%
    filter(exp == expNum, 
           !subj %in% c(irrelevantSubjs, fewPAS1Subs), # exclude participants
           resp2 <= ifelse(subj %in% more_subj2_participants,2,1)#get only PAS = 1/2 awareness trials
           ) %>%
    mutate(exp = paste('Hesselmann_2016', exp, task_str, sep = '_'))
  data_awareness_task <- data_awareness_all %>% filter(task == tsk)
  summary_visibility <- data_awareness_task %>%
    group_by(subj, exp) %>%
    summarise(SR = mean(correct), ntrials = n())
  # 2 AFC chance level performance
  # subjects were excluded according to objective performance according to task specific control
  chance <- 0.5
  if(exp == 1) {
    awareness_ps <- pbinom(summary_visibility$SR * summary_visibility$ntrials, 
                           summary_visibility$ntrials, chance)
    high_p_subjects <- summary_visibility[awareness_ps > 1 - alphaAwareness,]$subj
  } else if (exp == 2) {
    # regardless of task, participants were excluded from exp 2
    shape_summary_visibility <- data_awareness_all %>% 
      filter(task == 1) %>%
      group_by(subj, exp) %>%
      summarise(SR = mean(correct), ntrials = n())
    shape_awareness_ps <- pbinom(shape_summary_visibility$SR * shape_summary_visibility$ntrials, 
                                 shape_summary_visibility$ntrials, chance)
    shape_high_p_subjects <- shape_summary_visibility[shape_awareness_ps > 1 - alphaAwareness,]$subj
    category_summary_visibility <- data_awareness_all %>% 
      filter(task == 0) %>%
      group_by(subj, exp) %>%
      summarise(SR = mean(correct), ntrials = n())
    category_awareness_ps <- pbinom(category_summary_visibility$SR * category_summary_visibility$ntrials, 
                                 category_summary_visibility$ntrials, chance)
    category_high_p_subjects <- category_summary_visibility[category_awareness_ps > 1 - alphaAwareness,]$subj
    high_p_subjects <- c(shape_high_p_subjects, category_high_p_subjects)
  }
  
  trial_by_trial_data <- data_awareness_task %>%
    filter(! subj %in% high_p_subjects) %>%
    dplyr::select(subj, correct, exp)
  
  summary_visibility <- summary_visibility %>%
    filter(! subj %in% high_p_subjects) %>%
    dplyr::select(subj, SR, ntrials, exp)
  
  return (list(trial_by_trial = trial_by_trial_data, summary = summary_visibility))
}


all_exps_data <- list(get_Hesselman_2016_data(1,0), get_Hesselman_2016_data(2,0),
                      get_Hesselman_2016_data(2,1))
summary_tables <- do.call(rbind, lapply(all_exps_data, function(dat) dat$summary))
trial_by_trial_tables <- do.call(rbind, lapply(all_exps_data, function(dat) dat$trial_by_trial))
processed_data <- list(trial_by_trial = trial_by_trial_tables, summary_tables = summary_tables)
save(processed_data, file = 'datasets\\Hesselman_etal_2016\\Hesselman_etal_2016.RData')

## validation:
Table1_exps_avg_SR <- summary_tables %>% 
  group_by(exp) %>% 
  summarise(SR = round(mean(SR),4)) %>%
  pull(SR)
if(Table1_exps_avg_SR[1] != 0.4887) {print('exp 1 not valid')}
if(Table1_exps_avg_SR[2] != 0.5214) {print('exp2a not valid')}
if(Table1_exps_avg_SR[3] != 0.5096) {print('exp2b not valid')}