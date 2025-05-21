GetTestResult <- function(results, test_name, test_f, sig_test_f, data, trials, fixed_params, i_row) {
  res <-test_f(data, trials, fixed_params@chance_as)
  results[i_row,paste0("h_",test_name)]= sig_test_f(res, fixed_params)$H1
  res_type_lbl <- ifelse(identical(sig_test_f, calc_sig_bayes), 'bf_', 'Pval_')
  results[i_row,paste0(res_type_lbl,test_name)]= res
  return(results)  
}

CheckAwareness<-function(data_per_subj,data,data_per_subj_full,numExcObjTest,ThresholdObjTest,alpha,param,DatasetName,i_row, results){
  PlotFlag=1

  results[i_row,"dataset"]=DatasetName
  results[i_row,"paper"]=data_per_subj$paper[1]
  ####################### Tests: #######################
  ####################### Ttest
  results <- GetTestResult(results, 't_test', t_f, calc_sig_freq, data_per_subj$AS, data_per_subj$ntrials, fixed_params, i_row)
  ####################### TBayes
  if (param$incSubjObjTest==1) {
    results <- GetTestResult(results, 'tBayes_test', tbayes_f, calc_sig_bayes, data_per_subj$AS, data_per_subj$ntrials, fixed_params, i_row)
    ####################### GB
    results <- GetTestResult(results, 'GB_test', gb_f, calc_sig_freq, data_per_subj$AS, data_per_subj$ntrials, fixed_params, i_row)
    ####################### Chi
    results <- GetTestResult(results, 'Chi_test', chisq_f, calc_sig_freq, matrix(data_per_subj$A), data_per_subj$ntrials, fixed_params, i_row)
    ####################### GBC
    results <- GetTestResult(results, 'GBC_test', gbc_f, calc_sig_freq, data_per_subj$AS, data_per_subj$ntrials, fixed_params, i_row)
    ####################### GBBayes
    roudned_A=data_per_subj$AS*data_per_subj$ntrials
    if (any(roudned_A-round(roudned_A) > 0.0001)){
      print(paste("In ",DatasetName," the number of correct trials is not an integer"))
      print (roudned_A-round(roudned_A))
      
      roudned_A=round(roudned_A)
    }
    results <- GetTestResult(results, 'GBBayes_test', gbf_f, calc_sig_bayes, as.vector(data_per_subj$A), as.vector(data_per_subj$ntrials), fixed_params, i_row)
  }
  if (param$incSubjObjTest==0) {
    ####################### RC test
    pvalue = resampling_criterion_test_f(data_per_subj$AS,data_per_subj_full,ThresholdObjTest,alpha,param,"RC")
    results[i_row,"Pval_RC_test"]=pvalue
    results[i_row,"h_RC_test"]=results[i_row,"Pval_RC_test"]<param$alpha
    
    ####################### AVRC test
    pvalue = resampling_criterion_test_f(data_per_subj$AS,data_per_subj_full,ThresholdObjTest,alpha,param,"AVRC")
    results[i_row,"Pval_AVRC_test"]=pvalue
    results[i_row,"h_AVRC_test"]=results[i_row,"Pval_AVRC_test"]<param$alpha
    
    ####################### RC or AVRC
    res_RCOrAVRC <- RCOrAVRC_f(data_per_subj$AS,data_per_subj_full,ThresholdObjTest,alpha,param)
    results[i_row,"Pval_RCorAVRC_test"]=res_RCOrAVRC
    results[i_row,"h_RCorAVRC_test"]=results[i_row,"Pval_RCorAVRC_test"]<param$alpha
  }
  
  ####################### Logistic mixed model
  results <- GetTestResult(results, 'MMLR_test', MMLR_f, calc_sig_freq, data_per_subj$A, data_per_subj$ntrials, fixed_params, i_row)

  return(results)
}

AddCalcResults <- function(results, data_per_subj, ind) {
  ############ calculations #############
  #average num trials
  results[ind,"meanNumTrials"]=mean(data_per_subj$ntrials)
  
  #std num trials
  results[ind,"stdNumTrials"]=sd(data_per_subj$ntrials)
  
  #num subjects
  results[ind,"numSubj"]=dim(data_per_subj)[1]
  
  #average success rate
  results[ind,"meanAS"]=mean(data_per_subj$AS)
  
  #std success rate
  results[ind,"stdAS"]=sd(data_per_subj$AS)
  
  #ste success rate
  results[ind,"steAS"]=sd(data_per_subj$AS)/sqrt(dim(data_per_subj)[1])
  
  #max success rate
  results[ind,"maxAS"]=max(data_per_subj$AS)
  
  #min success rate
  results[ind,"minAS"]=min(data_per_subj$AS)
  
  return(results)
}  
