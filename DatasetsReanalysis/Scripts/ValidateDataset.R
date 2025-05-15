ValidateDataset <- function(ComparisonTable, param, data_full ,data_per_subj_full) {
  # the function analyzes the dataset and compared its statistics with the 
  # ones reported in the relevant paper
  data_per_subj$ntrials <- matrix(data_per_subj$ntrials)
  data_per_subj$A <- matrix(as.integer(data_per_subj$AS * data_per_subj$ntrials))
  
  ############# compare data to results from paper #############
  # make sure this dataset appears in the comparison table
  if (is_empty(which(ComparisonTable$Dataset == cur_ds))){
    stop(paste0("The dataset doesn't appear in comparison table: ", cur_ds))
  }
  
  # extract the results from paper (reported values)
  AS=ComparisonTable[ComparisonTable$Dataset == cur_ds,"SR"]
  decimal=ComparisonTable[ComparisonTable$Dataset == cur_ds,"decimal"]
  numSubj=ComparisonTable[ComparisonTable$Dataset == cur_ds,"numSubj"]
  numExcObjTest=ComparisonTable[ComparisonTable$Dataset == cur_ds,"numExcObjTest"]
  ThresholdObjTest=ComparisonTable[ComparisonTable$Dataset == cur_ds,"ThresholdObjTest"]
  alpha=ComparisonTable[ComparisonTable$Dataset == cur_ds,"alpha"]
  
  if (length(AS)<1) {
    stop(paste("Problem with dataset name the comparison data is not found"))
  }
  
  # validate average AS without objective aware subjects
  if (!str_detect(cur_ds,"Benthien")) { 
    if (!is.na(AS) & numExcObjTest==0) {
      compareDataToPaper(mean(data_per_subj$AS),AS,decimal,cur_ds)
    }  else if (!is.na(AS) & numExcObjTest>0) {
      data_per_subj_check=filter(data_per_subj, excObjTest == 0)
      compareDataToPaper(mean(data_per_subj_check$AS),AS,decimal,cur_ds)
    }
  } else { #in BH_2021 the AS is given for all datasets together
    data_per_subj_check <- filter(data_per_subj_all, paper == "BH_2021")
    if (!is.na(AS)) {
      compareDataToPaper(mean(data_per_subj_check$AS),AS,decimal,cur_ds)
    }
  }
  
  # validate number of subjects
  if (!is.na(numSubj)) {
    if (param$incSubjObjTest==0) {
      compareDataToPaper(dim(data_per_subj)[1],numSubj,0,cur_ds)
    } else {
      compareDataToPaper(dim(data_per_subj)[1],numSubj+numExcObjTest,0,cur_ds)
    }
  }
  
  # validate number of objective aware subjects
  if (param$incSubjObjTest==1) {
    #check number of objective aware included subjects
    compareDataToPaper(sum(data_per_subj$excObjTest),numExcObjTest,0,cur_ds)
  } else {
    #check the number of subjects excluded is the same as expected
    compareDataToPaper(sum(data_per_subj_full$excObjTest),numExcObjTest,0,cur_ds)
  }  
}
