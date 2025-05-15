#clear workspace
rm(list=ls())
# Load required R packages and sources
library(groundhog)
pkgs <- c("BSDA", "lme4", "dplyr", "readxl", "rlang",
          "tidyr", "stringr", "ggplot2", "BayesFactor", "wesanderson",
          "gridExtra", 'matrixTests', 'svglite',
          "rjags","scales")
groundhog.library(pkgs, "2025-03-01", tolerate.R.version = '4.5.0')

#Getting the functions
source("./Common/AwarenessTests.R") 
source("./Common/Definitions.R") 
source("./DatasetsReanalysis/Scripts/CheckAwareness.R")
source("./DatasetsReanalysis/Scripts/CompareDataToPaper.R")
source("./DatasetsReanalysis/Scripts/LoadDataSets.R")
source("./DatasetsReanalysis/Scripts/PlottingFunctions.R")
source("./DatasetsReanalysis/Scripts/ValidateDataset.R")
source("./DatasetsReanalysis/Scripts/PreprocessResultsForPlots.R")

#set random number generator
set.seed(1)

#################### Parameters
incSubjObjTest=0 #1: main analysis- include objective aware subjects
                 #0: exclude objective aware subjects (with RC and AVRC tests)
param<-list("ChanceSuccessRate" = 0.5,
            "NiterationsResmapling" = 10000,
            "alpha" = 0.05,
            "bfThreshold" = 3,
            "incSubjObjTest" = incSubjObjTest,
            "minSampleSubjRCT" = 10)

#################### Initialize results_awareness dataframe
if (param$incSubjObjTest==0) {
  mat = matrix(ncol = 20, nrow = 0)
  results=data.frame(mat)
  colnames(results)<-c("dataset","paper",
                       "h_t_test","h_MMLR_test","h_RC_test","h_AVRC_test","h_RCorAVRC_test",
                       "Pval_t_test","Pval_MMLR_test","Pval_RC_test","Pval_AVRC_test","Pval_RCorAVRC_test",
                       "meanNumTrials","stdNumTrials","numSubj","meanAS","stdAS","steAS","minAS","maxAS")
} else {
  mat = matrix(ncol = 24, nrow = 0)
  results=data.frame(mat)
  colnames(results)<-c("dataset","paper",
                       "h_t_test","h_tBayes_test","h_MMLR_test","h_GB_test","h_Chi_test","h_GBC_test","h_GBBayes_test",
                       "Pval_t_test","bf_tBayes_test","Pval_MMLR_test","Pval_GB_test","Pval_Chi_test","Pval_GBC_test","bf_GBBayes_test",
                       "meanNumTrials","stdNumTrials","numSubj","meanAS","stdAS","steAS","minAS","maxAS")
}

#################### load all datasets to one dataframe
#load comparison to paper table
ComparisonTable=read.csv("./DatasetsReanalysis/Data/ComparisonToPaperTable.csv")

processed_data=LoadDataSets()
data_all=processed_data$trial_by_trial
data_per_subj_all=processed_data$summary_tables
rm(processed_data)

#################### Check all tests for each dataset
# initialize an index counting included datasets
ind_included=0
countDatasetsWithSubjExc=0;
datasetNames=levels(as.factor(data_all$dataset))
for (ds_ind in 1:length(datasetNames)) { #datasets
  cur_ds <- datasetNames[ds_ind]
  ############# filter data according to dataset #############
  data_full <- filter(data_all, dataset == cur_ds)
  data_per_subj_full <- filter(data_per_subj_all, dataset == cur_ds)
  
  ############# count number of datasets with subjects excluded
  if (sum(data_per_subj_full$excObjTest)>0) {
    countDatasetsWithSubjExc=countDatasetsWithSubjExc+1
  }
  
  ############# filter out objective aware subjects if needed #############
  if (param$incSubjObjTest==0) {
    data <- filter(data_full, excObjTest == 0)
    data_per_subj <- filter(data_per_subj_full, excObjTest == 0)
  } else {
    data = data_full
    data_per_subj = data_per_subj_full
  }
  data_per_subj$ntrials <- matrix(data_per_subj$ntrials)
  
  ########### recode number of success (A)  ###########
  data_per_subj_A <- data %>% 
    group_by(subj) %>% 
    summarise(A =  sum(correct)) %>%
    select(subj, A)
  data_per_subj <- merge(data_per_subj, data_per_subj_A, by = "subj", all.x = TRUE)

  ############# compare data to results from paper #############
  ValidateDataset(ComparisonTable, param, data_full ,data_per_subj_full)
  
  ######### extract dataset params for later analysis ##########
  ThresholdObjTest=ComparisonTable[ComparisonTable$Dataset == cur_ds,"ThresholdObjTest"]
  # if objective aware subjects are excluded, include in the analysis only
  # datasets that have a criterion for subject exclusion
  if (param$incSubjObjTest==0 & is.na(ThresholdObjTest)) {
    next
  }
  alpha=ComparisonTable[ComparisonTable$Dataset == cur_ds,"alpha"]
  numExcObjTest=ComparisonTable[ComparisonTable$Dataset == cur_ds,"numExcObjTest"]

  # check awareness in each dataset
  ############# check awareness #############
  ind_included <- ind_included + 1
  results <- CheckAwareness(data_per_subj,data,data_per_subj_full,numExcObjTest,ThresholdObjTest,alpha,param,cur_ds,ind_included,results)
  # add calculated parameters to results dataframe
  results <- AddCalcResults(results, data_per_subj, ind_included)  
  
  # remove large data files before next dataset
  rm(data_full,data_per_subj_full,data,data_per_subj)
}

#################### Code match between tests ####################
# we code the different datasets according to the agreement on whether awareness
# was detected or not:
#match= all tests show the same results
#AllSig= all tests significant
#AllNoSig= all tests none significant
if (param$incSubjObjTest==0) {
  #test checked: t,MMLR,RCorAVRC (for mismatch plots)
  results$match=ifelse(results$h_t_test+results$h_MMLR_test+results$h_RCorAVRC_test == 0 | results$h_t_test+results$h_MMLR_test+results$h_RCorAVRC_test == 3,TRUE,FALSE)

} else {
  #test checked: t,Tbayes,MMLR,GBC,GBBayes (for mismatch plots)
  results$match=ifelse(results$h_t_test+results$h_tBayes_test+results$h_MMLR_test+results$h_GBC_test+results$h_GBBayes_test == 0 | results$h_t_test+results$h_tBayes_test+results$h_MMLR_test+results$h_GBC_test+results$h_GBBayes_test == 5,TRUE,FALSE)
  
  #test checked: t,Tbayes,MMLR,GB,Chi,GBC,GBBayes (for AS histograms)
  results$AllSig=ifelse(results$h_t_test==1 & results$h_tBayes_test==1 & results$h_MMLR_test==1 & results$h_GB_test==1 & results$h_Chi_test==1 & results$h_GBC_test==1 & results$h_GBBayes_test==1,TRUE,FALSE)
  results$AllNoSig=ifelse(results$h_t_test==0 & results$h_tBayes_test==0 & results$h_MMLR_test==0 & results$h_GB_test==0 & results$h_Chi_test==0 & results$h_GBC_test==0 & results$h_GBBayes_test==0,TRUE,FALSE)
}

#################### Summarize statistics per publication ####################
# for the supplementary table detailing the different papers and their stats
if (param$incSubjObjTest==1) {
  #initialize summarizePublication data frame
  mat = matrix(ncol = 4, nrow = 15)
  summarizePublication=data.frame(mat)
  colnames(summarizePublication)<-c("paper","numDataSets","averageNumSubejcts","averageNumTrials")
  
  publicationNames=levels(as.factor(data_all$paper))
  for (pub_ind in 1:length(publicationNames)) { #papers
    summarizePublication$paper[pub_ind] = publicationNames[pub_ind]
    
    ############# filter data according to paper #############
    results_paper <- filter(results, paper == publicationNames[pub_ind])
    
    summarizePublication$numDataSets[pub_ind] = dim(results_paper)[1]
    summarizePublication$averageNumSubejcts[pub_ind]=mean(results_paper$numSubj)
    summarizePublication$averageNumTrials[pub_ind]=mean(results_paper$meanNumTrials)
  }
}

#################### Preprocess data for plots ##########################
res_prep <- PreprocessResultsForPlots(results)

################### Plot results ########################################
GenerateAllPlots(param, res_prep$res, res_prep$res_long)

