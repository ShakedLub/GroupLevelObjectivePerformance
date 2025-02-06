#Create data for group level analysis and save it

library(dplyr)
library(effects)
library(afex)
library(sjmisc)
library(Rmisc)
library(tidyverse) 
library(ggplot2)
############################################################################

#clean workspace
rm(list=ls())

#create function to exclude observations
  #d takes a data frame 
  #subj are the subject to exclude - integer values
exclude_obs <- function(d, subj){
              return(d[!(d$subj %in% subj),])
                                }

############################################################################

#conditions (cond)
#visible certain (1), visible uncertain (2), invisible certain (3), invisible uncertain (4)
# certain = centrally presented primes
# uncertain = (random) peripherally presented primes

#read the data set 
data <- read.csv("./PapersAndDownloadedData/Benthien_Hesselmann_2021/CFSlocation_data.csv", header=F, sep=",", dec=".")

#column names
colnames(data) <- c("exp","subj","trial","prime","cond","pos","probe","alpha","RT","resp1","pre","RT2","resp2")

#split
data_check <- filter(data, exp ==2) #data check trials
data_check <- filter(data_check, resp2 ==1) #PAS 1 trials

#correct responses in check experiment (across 4 conditions)
data_check$correct <- ifelse((data_check$prime<5 & data_check$resp1==1) |
                              (data_check$prime>5 & data_check$resp1==2), 1, 0)

##### subj. 18: positive slope in CFS condition
##### subj. 19 + 29: negative slope in noCFS condition
##### subj. 30: only 50% correct responses in all conditions (resp2 = 2 in all trials)
##### subj. 31: only 50% correct responses in visible certain condition (resp2 = 1 until trial 149)

#exclude subj 18, 19, 29, 30, 31 (due to slopes and discrimination performance) from further analyses
data_check <- exclude_obs(d = data_check, subj = c(18, 19, 29, 30, 31)) # valid measures n = 26 participants

#split further to conditions
data_check_cfs_certain <- filter(data_check, cond==3)
data_check_cfs_uncertain <- filter(data_check, cond==4)

#certain condition
data_check_cfs_certain_correct <- dplyr::summarise(group_by(data_check_cfs_certain,
                                                subj),
                                       SR = sum(correct)/length(correct), 
                                       ntrials = length(trial))
mystats_cfs_certain = summarySE(data_check_cfs_certain_correct, measurevar = 'SR')
CI(data_check_cfs_certain_correct$SR, ci = 0.95)

data_check_cfs_certain_correct$dataset="Benthien_&_Hesselmann_2021_E1-a"
data_check_cfs_certain$dataset="Benthien_&_Hesselmann_2021_E1-a"
data_check_cfs_certain_correct$paper="BH_2021"
data_check_cfs_certain$paper="BH_2021"

#uncertain condition
data_check_cfs_uncertain_correct <- dplyr::summarise(group_by(data_check_cfs_uncertain,
                                                            subj),
                                                   SR = sum(correct)/length(correct), 
                                                   ntrials = length(trial))

mystats_cfs_uncertain = summarySE(data_check_cfs_uncertain_correct, measurevar = 'SR')
CI(data_check_cfs_uncertain_correct$SR, ci = 0.95)

data_check_cfs_uncertain_correct$dataset="Benthien_&_Hesselmann_2021_E1-b"
data_check_cfs_uncertain$dataset="Benthien_&_Hesselmann_2021_E1-b"
data_check_cfs_uncertain_correct$paper="BH_2021"
data_check_cfs_uncertain$paper="BH_2021"

#concatenate data
data_check_cfs=rbind(data_check_cfs_certain,data_check_cfs_uncertain)
data_check_cfs_correct=rbind(data_check_cfs_certain_correct,data_check_cfs_uncertain_correct)

#add excluded subjects variable
data_check_cfs$excObjTest=0
data_check_cfs_correct$excObjTest=0

#save data
processed_data <- list(trial_by_trial = data_check_cfs, summary_tables = data_check_cfs_correct)
save(processed_data, file = "./Data/BH_2021.RData")

