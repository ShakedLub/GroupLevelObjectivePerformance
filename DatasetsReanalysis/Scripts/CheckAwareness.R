CheckAwareness<-function(data_per_subj,data,data_per_subj_full,numExcObjTest,ThresholdObjTest,alpha,param,DatasetName,i_row,results){
  PlotFlag=1
  
  results[i_row,"dataset"]=DatasetName
  results[i_row,"paper"]=data_per_subj$paper[1]
  
  ####################### Tests: #######################
  ####################### Ttest
  t_test_data<-t.test(x=data_per_subj$AS,alternative = "greater",mu=param$ChanceSuccessRate)
  results[i_row,"h_t_test"]=t_test_data$p.value<param$alpha
  results[i_row,"Pval_t_test"]=t_test_data$p.value
  
  ####################### TBayes
  if (param$incSubjObjTest==1) {
    bf=ttestBF(x=data_per_subj$AS,mu=param$ChanceSuccessRate)
    results[i_row,"h_tBayes_test"]=exp(bf@bayesFactor$bf)>param$bfThreshold
    results[i_row,"bf_tBayes_test"]=exp(bf@bayesFactor$bf)
  }
  
  ####################### GB
  if (param$incSubjObjTest==1) {
    V=param$ChanceSuccessRate*(1-param$ChanceSuccessRate)/data_per_subj$ntrials
    V=mean(V)
    
    zresult=z.test(x=data_per_subj$AS,alternative = "greater",mu=param$ChanceSuccessRate,sigma.x=sqrt(V))
    results[i_row,"h_GB_test"]=zresult$p.value<param$alpha
    results[i_row,"Pval_GB_test"]=zresult$p.value
  }
  
  ####################### Chi
  if (param$incSubjObjTest==1) {
    # Number of successes per participant
    r=data_per_subj$AS*data_per_subj$ntrials
    
    # Number of "failures" per participant
    fa = data_per_subj$ntrials - r
    
    # Expected number of success = expected number of failures = n / 2
    ex = data_per_subj$ntrials / 2
    
    # Compute observed chi-squared X2 = sum((S - E)^2 / E) + sum((F - E)^2/E)
    X2 = sum((r - ex)^2 / ex) + sum((fa - ex)^2 / ex)
    
    # Under H0, X2 ~ chisq(k)
    # Compute p-value
    pval = 1 - pchisq(X2, df = dim(data_per_subj)[1])
    results[i_row,"h_Chi_test"]=pval<param$alpha
    results[i_row,"Pval_Chi_test"]=pval
  }
  
  ####################### GBC
  if (param$incSubjObjTest==1) {
    results[i_row,"Pval_GBC_test"]=min(results[i_row,"Pval_Chi_test"],results[i_row,"Pval_GB_test"])
    results[i_row,"h_GBC_test"]=ifelse(results[i_row,"Pval_Chi_test"]<(param$alpha/2) | results[i_row,"Pval_GB_test"]<(param$alpha/2),TRUE,FALSE)
  }
  
  ####################### GBBayes
  if (param$incSubjObjTest==1) {
    r=data_per_subj$AS*data_per_subj$ntrials
    if (any(r-round(r) > 0.0001)){
      print(paste("In ",DatasetName," the number of correct trials is not an integer"))
      print (r-round(r))
      
      r=round(r)
    }
    listGBF=generate_GA_BF(r, data_per_subj$ntrials)
    results[i_row,"bf_GBBayes_test"]=listGBF$BF #BF_10
    results[i_row,"h_GBBayes_test"]=listGBF$BF>param$bfThreshold
  }
  
  ####################### RC test
  if (param$incSubjObjTest==0) {
    pvalue = ResamplingCriterionTest(data_per_subj$AS,data_per_subj$ntrials,data_per_subj_full,ThresholdObjTest,alpha,param,"RC")
    results[i_row,"Pval_RC_test"]=pvalue
    results[i_row,"h_RC_test"]=results[i_row,"Pval_RC_test"]<param$alpha
  }
  
  ####################### AVRC test
  if (param$incSubjObjTest==0) {
    pvalue = ResamplingCriterionTest(data_per_subj$AS,data_per_subj$ntrials,data_per_subj_full,ThresholdObjTest,alpha,param,"AVRC")
    results[i_row,"Pval_AVRC_test"]=pvalue
    results[i_row,"h_AVRC_test"]=results[i_row,"Pval_AVRC_test"]<param$alpha
  }
  
  ####################### RC or AVRC
  if (param$incSubjObjTest==0) {
    results[i_row,"Pval_RCorAVRC_test"]=min(results[i_row,"Pval_RC_test"],results[i_row,"Pval_AVRC_test"])
    results[i_row,"h_RCorAVRC_test"]=ifelse(results[i_row,"Pval_RC_test"]<(param$alpha/2) | results[i_row,"Pval_AVRC_test"]<(param$alpha/2),TRUE,FALSE)
  }
  
  ####################### Logistic mixed model
  mat = matrix(ncol = 2, nrow = dim(data)[1])
  df_LMM=data.frame(mat)
  colnames(df_LMM)<-c("subj","correct")
  
  df_LMM$subj=data$subj
  df_LMM$correct=as.numeric(data$correct)
  
  if (PlotFlag) {
    for (bb in 1:dim(data_per_subj)[1]) { #subjects
      ind=1:data_per_subj$ntrials[bb]
      if (bb==1) { 
        indplot=ind
      } else { 
        indplot=c(indplot,ind)
      }
    }
    df_LMM$indplot=indplot
    
    #box plot
    ggplot(df_LMM,aes(group=subj,y=correct)) +
      geom_boxplot() + xlab('subject')
    #seperate graphs for each group
    ggplot(df_LMM,aes(x=indplot,y=correct,group=subj,color=subj)) +
      geom_point()+
      geom_smooth(method="glm",se=F,fullrange=F,formula=y~1,method.args=list(family=binomial)) +
      facet_wrap(~subj)
    #one graph with data points classified to groups
    ggplot(df_LMM,aes(x=indplot,y=correct,group=subj,color=subj)) +
      geom_point()+
      geom_smooth(method="glm",se=F,fullrange=F,formula=y~1,method.args=list(family=binomial)) 
  }
  
  df_LMM$subj=as.factor(df_LMM$subj)
  df_LMM$correct=as.factor(df_LMM$correct)
  
  modelLMM<-glmer(correct ~1+(1|subj),data=df_LMM,family=binomial) # the intercept is undefined; the model looks for it, and the output will tell us if it is above chance
  #calculate one sided p-value
  pval=pnorm(summary(modelLMM)$coefficients[1,3],lower.tail = F)
  results[i_row,"h_LMM_test"]=pval<param$alpha
  results[i_row,"Pval_LMM_test"]=pval
  
  return(results)
}