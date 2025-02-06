ResamplingCriterionTest<- function (ObAS_exp,nTrials_exp,data_per_subj_full,ThresholdObjTest,alpha,param,TestType) {
  
  #Add excluded subjects to ObAS nTrials
  nTrials=data_per_subj_full$ntrials
  
  #number of subjects with objective aware subjects
  numSubj=length(nTrials)
  
  #create number of trials vec
  nT=rep(t(nTrials),times=param$NiterationsResmapling)
  nTMat=matrix(nT,nrow=numSubj,ncol=param$NiterationsResmapling)
  
  #Draw from a binomial distribution the success rate for all resampling iterations
  r=rbinom(param$NiterationsResmapling*numSubj,nT,param$ChanceSuccessRate)
  rMat=matrix(r,nrow=numSubj,ncol=param$NiterationsResmapling)
  
  #Calculate AS for all resampling iterations
  ObAS=r/nT;
  ObASMat=matrix(ObAS,nrow=numSubj,ncol=param$NiterationsResmapling)
  
  ## Exclude subjects according to observed AS
  if (!is.na(ThresholdObjTest)) {
    
    if (identical(ThresholdObjTest,"Chance")) {
      pbinomEachSubj=1-pbinom(q=r,size=nT,prob=param$ChanceSuccessRate)
      excSubj=pbinomEachSubj<alpha
    } else if (identical(ThresholdObjTest,"0.65")) {
      excSubj=ObAS>0.65
    } else if (identical(ThresholdObjTest,"0.6")) {
      excSubj=ObAS>0.6
    }
    excSubjMat=matrix(excSubj,nrow=numSubj,ncol=param$NiterationsResmapling)
    
    ObAS[excSubj==TRUE]=NA
    ObASMat=matrix(ObAS,nrow=numSubj,ncol=param$NiterationsResmapling)
    
    nSubjAfterEx=colSums(!excSubjMat)
    
    #delete samples with less than 10 subjects left because statistical tests can't be run on them
    itDel=which(nSubjAfterEx<param$minSampleSubjRCT)
    
    if (length(itDel) != 0) {
      
      ObASMat=ObASMat[,-itDel]
      ObAS=matrix(ObASMat,nrow=1,ncol=dim(ObASMat)[1]*dim(ObASMat)[2])

      param$NiterationsResmapling=dim(ObASMat)[2]
    }
  }
  
  #flip ObAS mat
  if (identical(TestType,"AVRC")) {
    ObASMat=param$ChanceSuccessRate+abs(param$ChanceSuccessRate-ObASMat)
  }
  
  #Calculate mean AS for each resampling iteration
  mObAS=colMeans(ObASMat,na.rm = TRUE)
  
  #flip real result
  if (identical(TestType,"AVRC")) {
    ObAS_exp=param$ChanceSuccessRate+abs(param$ChanceSuccessRate-ObAS_exp)
  }
  
  #calculate mean ObAS_exp for the real result
  mObAS_exp=mean(ObAS_exp)
    
  p=sum(mObAS>mObAS_exp)/param$NiterationsResmapling
    
  return(p)
}
