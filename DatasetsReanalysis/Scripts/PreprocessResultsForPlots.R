PreprocessResultsForPlots <- function(results) {
  #change column names
  if (param$incSubjObjTest==0) {
    colnames(results)[which(names(results)=="Pval_t_test")]="T"
    colnames(results)[which(names(results)=="Pval_MMLR_test")]="MMLR"
    colnames(results)[which(names(results)=="Pval_RC_test")]="RC"
    colnames(results)[which(names(results)=="Pval_AVRC_test")]="AVRC"
    colnames(results)[which(names(results)=="Pval_RCorAVRC_test")]="RCorAVRC"
  } else {
    colnames(results)[which(names(results)=="Pval_t_test")]="T"
    colnames(results)[which(names(results)=="bf_tBayes_test")]="TBayes"
    colnames(results)[which(names(results)=="Pval_MMLR_test")]="MMLR"
    colnames(results)[which(names(results)=="Pval_GB_test")]="GB"
    colnames(results)[which(names(results)=="Pval_Chi_test")]="Chi"
    colnames(results)[which(names(results)=="Pval_GBC_test")]="GBC"
    colnames(results)[which(names(results)=="bf_GBBayes_test")]="GBBayes"
  }

  #change BF from BF10 to BF01
  if (param$incSubjObjTest==1) {
    results$TBayes=1/results$TBayes
    results$GBBayes=1/results$GBBayes
  }

  #change the data from wide format to long format
  if (param$incSubjObjTest==0) {
    results_preprocessed_long <- gather(results, test, pvalue,c("T","MMLR","RC","AVRC","RCorAVRC"), factor_key=TRUE)
  } else {
    results_preprocessed_long <- gather(results, test, pvalue,c("T","TBayes","MMLR","GB","Chi","GBC","GBBayes"), factor_key=TRUE)
  }
  
  #deal with extreme results
  results_preprocessed_long$pvalue[results_preprocessed_long$pvalue<10^-6]=10^-6
  
  #calculate log(pvalue)
  results_preprocessed_long$log.pvalue=log10(results_preprocessed_long$pvalue)
  
  #add factor test_type (Bayesian or Frequentist)
  results_preprocessed_long$test_type=ifelse(results_preprocessed_long$test == "TBayes" | results_preprocessed_long$test == "GBBayes","Bayesian","Frequentist")
  
  #change the levels order of test factor
  if (param$incSubjObjTest==0) {
    results_preprocessed_long$test=factor(results_preprocessed_long$test,levels=c("RCorAVRC","RC","AVRC","T","MMLR"))
  } else {
    results_preprocessed_long$test=factor(results_preprocessed_long$test,levels=c("GBC","GB","Chi","T","MMLR","GBBayes","TBayes"))
  }
  
  return (list(res = results, res_long = results_preprocessed_long))
}