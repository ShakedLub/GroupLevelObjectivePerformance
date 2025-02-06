#Load required R packages
library(BSDA) 
library(lme4)
library(tidyverse)
library(dplyr)
library(readxl)
library(BayesFactor)
library(ggplot2)
library(tidyr)
library(stringr)
library(wesanderson)
library(gridExtra)
library(matrixTests)
library(rjags)
library(scales)

#clear workspace
rm(list=ls())

#set random number generator
set.seed(1)

#Getting the functions
source("./Scripts/CheckAwareness.R")
source("./Scripts/ResamplingCriterionTest.R")
source("./Scripts/GBF.R")
source("./Scripts/compareDataToPaper.R")
source("./Scripts/LoadDataSets.R")

#################### Parameters
incSubjObjTest=0 #1: main analysis- include objective aware subjects
                 #0: exclude objective aware subjects (with RC and AVRC tests)
param<-list(0.5,10000,0.05,3,incSubjObjTest,10)
names(param)<-c("ChanceSuccessRate","NiterationsResmapling","alpha","bfThreshold","incSubjObjTest","minSampleSubjRCT")

#################### Initialize results_awareness dataframe
if (param$incSubjObjTest==0) {
  mat = matrix(ncol = 20, nrow = 0)
  results=data.frame(mat)
  colnames(results)<-c("dataset","paper",
                       "h_t_test","h_LMM_test","h_RC_test","h_AVRC_test","h_RCorAVRC_test",
                       "Pval_t_test","Pval_LMM_test","Pval_RC_test","Pval_AVRC_test","Pval_RCorAVRC_test",
                       "meanNumTrials","stdNumTrials","numSubj","meanAS","stdAS","steAS","minAS","maxAS")
} else {
  mat = matrix(ncol = 24, nrow = 0)
  results=data.frame(mat)
  colnames(results)<-c("dataset","paper",
                       "h_t_test","h_tBayes_test","h_LMM_test","h_GB_test","h_Chi_test","h_GBC_test","h_GBBayes_test",
                       "Pval_t_test","bf_tBayes_test","Pval_LMM_test","Pval_GB_test","Pval_Chi_test","Pval_GBC_test","bf_GBBayes_test",
                       "meanNumTrials","stdNumTrials","numSubj","meanAS","stdAS","steAS","minAS","maxAS")
}

#################### load all datasets to one dataframe
#load comparison to paper table
ComparisonTable=read.csv("./Data/ComparisonToPaperTable.csv")

processed_data=LoadDataSets()
data_all=processed_data$trial_by_trial
data_per_subj_all=processed_data$summary_tables
rm(processed_data)

#change SR (success rate) to AS (awareness score)
colnames(data_per_subj_all)[which(names(data_per_subj_all)=="SR")]="AS"

#################### Check all tests for each dataset
ind=0
countDatasetsWithSubjExc=0;
datasetNames=levels(as.factor(data_all$dataset))
for (ii in 1:length(datasetNames)) { #datasets
  ############# filter data according to dataset #############
  data_full <- filter(data_all, dataset == datasetNames[ii])
  data_per_subj_full <- filter(data_per_subj_all, dataset == datasetNames[ii])
  
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
  
  ############# compare data to results from paper #############
  #make sure this dataset appears in the comparison table
  if (is_empty(which(ComparisonTable$Dataset == datasetNames[ii]))){
    stop(paste0("One dataset doesn't appear in comparison table ", datasetNames[ii]))
  }
  
  #results from paper
  AS=ComparisonTable[ComparisonTable$Dataset == datasetNames[ii],"SR"]
  decimal=ComparisonTable[ComparisonTable$Dataset == datasetNames[ii],"decimal"]
  numSubj=ComparisonTable[ComparisonTable$Dataset == datasetNames[ii],"numSubj"]
  numExcObjTest=ComparisonTable[ComparisonTable$Dataset == datasetNames[ii],"numExcObjTest"]
  ThresholdObjTest=ComparisonTable[ComparisonTable$Dataset == datasetNames[ii],"ThresholdObjTest"]
  alpha=ComparisonTable[ComparisonTable$Dataset == datasetNames[ii],"alpha"]
  
  if (length(AS)<1) {
    stop(paste("Problem with dataset name the comparison data is not found"))
  }
  
  #check average AS without objective aware subjects
  if (str_detect(datasetNames[ii],"Benthien")) { #in BH_2021 the AS is given for all datasets together
    data_per_subj_check <- filter(data_per_subj_all, paper == "BH_2021")
    if (!is.na(AS)) {
      compareDataToPaper(mean(data_per_subj_check$AS),AS,decimal,datasetNames[ii])
    }
  } else {
    if (!is.na(AS) & numExcObjTest==0) {
      compareDataToPaper(mean(data_per_subj$AS),AS,decimal,datasetNames[ii])
    }  else if (!is.na(AS) & numExcObjTest>0) {
      data_per_subj_check=filter(data_per_subj, excObjTest == 0)
      compareDataToPaper(mean(data_per_subj_check$AS),AS,decimal,datasetNames[ii])
    }
  }
 
  #check number of subjects
  if (!is.na(numSubj)) {
    if (param$incSubjObjTest==0) {
      compareDataToPaper(dim(data_per_subj)[1],numSubj,0,datasetNames[ii])
    } else {
      compareDataToPaper(dim(data_per_subj)[1],numSubj+numExcObjTest,0,datasetNames[ii])
    }
  }
  
  #check number of objective aware subjects
  if (param$incSubjObjTest==1) {
    #check number of objective aware included subjects
    compareDataToPaper(sum(data_per_subj$excObjTest),numExcObjTest,0,datasetNames[ii])
  } else {
    #check the number of subjects excluded is the same as expected
    compareDataToPaper(sum(data_per_subj_full$excObjTest),numExcObjTest,0,datasetNames[ii])
  }

  ############# if objective aware subjects are excluded, include in the analysis only #############
  ############# datasets that have a criterion for subject exclusion                   #############
  if (param$incSubjObjTest==0 & is.na(ThresholdObjTest)) {
    next
  }
  
  ############# check awareness #############
  ind=ind+1
  results<-CheckAwareness(data_per_subj,data,data_per_subj_full,numExcObjTest,ThresholdObjTest,alpha,param,datasetNames[ii],ind,results)
  
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
  
  rm(data_full,data_per_subj_full,data,data_per_subj)
}

#################### Summarize all tests
#match= all tests have the same results
#AllSig= all tests significant
#AllNoSig= all tests none significant
if (param$incSubjObjTest==0) {
  #test checked: t,LMM,RCorAVRC (for mismatch plots)
  results$match=ifelse(results$h_t_test+results$h_LMM_test+results$h_RCorAVRC_test == 0 | results$h_t_test+results$h_LMM_test+results$h_RCorAVRC_test == 3,TRUE,FALSE)

} else {
  #test checked: t,Tbayes,LMM,GBC,GBBayes (for mismatch plots)
  results$match=ifelse(results$h_t_test+results$h_tBayes_test+results$h_LMM_test+results$h_GBC_test+results$h_GBBayes_test == 0 | results$h_t_test+results$h_tBayes_test+results$h_LMM_test+results$h_GBC_test+results$h_GBBayes_test == 5,TRUE,FALSE)
  
  #test checked: t,Tbayes,LMM,GB,Chi,GBC,GBBayes (for AS histograms)
  results$AllSig=ifelse(results$h_t_test==1 & results$h_tBayes_test==1 & results$h_LMM_test==1 & results$h_GB_test==1 & results$h_Chi_test==1 & results$h_GBC_test==1 & results$h_GBBayes_test==1,TRUE,FALSE)
  results$AllNoSig=ifelse(results$h_t_test==0 & results$h_tBayes_test==0 & results$h_LMM_test==0 & results$h_GB_test==0 & results$h_Chi_test==0 & results$h_GBC_test==0 & results$h_GBBayes_test==0,TRUE,FALSE)
}

#################### Summarize statistics per publication
if (param$incSubjObjTest==1) {
  #initialize summarizePublication data frame
  mat = matrix(ncol = 4, nrow = 15)
  summarizePublication=data.frame(mat)
  colnames(summarizePublication)<-c("paper","numDataSets","averageNumSubejcts","averageNumTrials")
  
  publicationNames=levels(as.factor(data_all$paper))
  for (ii in 1:length(publicationNames)) { #papers
    summarizePublication$paper[ii] = publicationNames[ii]
    
    ############# filter data according to paper #############
    results_paper <- filter(results, paper == publicationNames[ii])
    
    summarizePublication$numDataSets[ii] = dim(results_paper)[1]
    summarizePublication$averageNumSubejcts[ii]=mean(results_paper$numSubj)
    summarizePublication$averageNumTrials[ii]=mean(results_paper$meanNumTrials)
  }
}

#################### preprocess data for plots
results_preprocessed=results

#change column names
if (param$incSubjObjTest==0) {
  colnames(results_preprocessed)[which(names(results_preprocessed)=="Pval_t_test")]="T"
  colnames(results_preprocessed)[which(names(results_preprocessed)=="Pval_LMM_test")]="MMLR"
  colnames(results_preprocessed)[which(names(results_preprocessed)=="Pval_RC_test")]="RC"
  colnames(results_preprocessed)[which(names(results_preprocessed)=="Pval_AVRC_test")]="AVRC"
  colnames(results_preprocessed)[which(names(results_preprocessed)=="Pval_RCorAVRC_test")]="RCorAVRC"
} else {
  colnames(results_preprocessed)[which(names(results_preprocessed)=="Pval_t_test")]="T"
  colnames(results_preprocessed)[which(names(results_preprocessed)=="bf_tBayes_test")]="TBayes"
  colnames(results_preprocessed)[which(names(results_preprocessed)=="Pval_LMM_test")]="MMLR"
  colnames(results_preprocessed)[which(names(results_preprocessed)=="Pval_GB_test")]="GB"
  colnames(results_preprocessed)[which(names(results_preprocessed)=="Pval_Chi_test")]="Chi"
  colnames(results_preprocessed)[which(names(results_preprocessed)=="Pval_GBC_test")]="GBC"
  colnames(results_preprocessed)[which(names(results_preprocessed)=="bf_GBBayes_test")]="GBBayes"
}

#change BF from BF10 to BF01
if (param$incSubjObjTest==1) {
  results_preprocessed$TBayes=1/results_preprocessed$TBayes
  results_preprocessed$GBBayes=1/results_preprocessed$GBBayes
}

#double GBC and RCorAVRC pvalue in order for it to have the same threshold as the other tests (0.05)
if (param$incSubjObjTest==1) {
  results_preprocessed$GBC=2*results_preprocessed$GBC
} else {
  results_preprocessed$RCorAVRC=2*results_preprocessed$RCorAVRC
}

#change the data from wide format to long format
if (param$incSubjObjTest==0) {
  results_preprocessed_long <- gather(results_preprocessed, test, pvalue,c("T","MMLR","RC","AVRC","RCorAVRC"), factor_key=TRUE)
} else {
  results_preprocessed_long <- gather(results_preprocessed, test, pvalue,c("T","TBayes","MMLR","GB","Chi","GBC","GBBayes"), factor_key=TRUE)
}

#deal with extreme results
results_preprocessed_long$pvalue[results_preprocessed_long$pvalue<0.0000001]=0.000001

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

######################################## Plot results ########################################
#################### figure 1 mismatch plot
#include only experiments that don't have the same result in the chosen tests
results_preprocessed_long_misMatch=filter(results_preprocessed_long, match == FALSE)

#change test to be a factor
results_preprocessed_long_misMatch$test=as.factor(results_preprocessed_long_misMatch$test)

#change order of test_type levels
results_preprocessed_long_misMatch$test_type=factor(results_preprocessed_long_misMatch$test_type,levels=c("Frequentist","Bayesian"))

#present only the wanted tests
if (param$incSubjObjTest==1) {
  results_preprocessed_long_misMatch=filter(results_preprocessed_long_misMatch, test != "GB" & test != "Chi")
}

# Create data for horizontal lines
data_hline1= data.frame(test_type = unique(results_preprocessed_long_misMatch$test_type),hline = c(log10(param$alpha),NA))
data_hline2= data.frame(test_type = unique(results_preprocessed_long_misMatch$test_type),hline = c(NA,log10(1/param$bfThreshold)))

# Create color data for x axis text
if (param$incSubjObjTest==1) {
  colourdata=rep("black",1,length(unique(results_preprocessed_long_misMatch$dataset)))
  datasetNamesMismatch=unique(results_preprocessed_long_misMatch$dataset)
  for (ii in 1:length(colourdata)){
    ds=datasetNamesMismatch[ii]
    ind=which(results_preprocessed$dataset==ds)
    if (results_preprocessed$h_t_test[ind]==0 & results_preprocessed$h_tBayes_test[ind]==0 & results_preprocessed$h_LMM_test[ind]==0 & results_preprocessed$h_GBC_test[ind]==1 & results_preprocessed$h_GBBayes_test[ind]==1) {
      colourdata[ii]="red"
    }  else if (results_preprocessed$h_t_test[ind]==0 & results_preprocessed$h_tBayes_test[ind]==0 & results_preprocessed$h_LMM_test[ind]==1 & results_preprocessed$h_GBC_test[ind]==1 & results_preprocessed$h_GBBayes_test[ind]==1) {
      colourdata[ii]="salmon2"
    }  else if (results_preprocessed$h_t_test[ind]==1 & results_preprocessed$h_tBayes_test[ind]==0 & results_preprocessed$h_LMM_test[ind]==1 & results_preprocessed$h_GBC_test[ind]==0 & results_preprocessed$h_GBBayes_test[ind]==0) {
      colourdata[ii]="honeydew4"
    }  else if (results_preprocessed$h_t_test[ind]==1 & results_preprocessed$h_tBayes_test[ind]==0 & results_preprocessed$h_LMM_test[ind]==1 & results_preprocessed$h_GBC_test[ind]==1 & results_preprocessed$h_GBBayes_test[ind]==0) {
      colourdata[ii]="azure3"
    }
  }
}

#create a vector with locations of vertical lines between datasets
vecLines=1:(length(unique(results_preprocessed_long_misMatch$dataset))-1)+0.5


#change dataset names to have space instead of underscore
results_preprocessed_long_misMatch$dataset = gsub('_',' ',results_preprocessed_long_misMatch$dataset)

if (param$incSubjObjTest==1) {
  f1=ggplot(results_preprocessed_long_misMatch, aes(x=dataset, y=log.pvalue, fill=test)) +
    geom_bar(alpha=0.5, stat="identity",position = position_dodge())+
    scale_x_discrete(guide = guide_axis(angle = -60)) +
    geom_hline(data = data_hline1,aes(yintercept = hline), linetype="dashed", color = "black") +
    geom_hline(data = data_hline2,aes(yintercept = hline), linetype="dashed", color = "black") +
    geom_vline(xintercept = vecLines, color = "azure2") +
    theme_minimal() +
    xlab('Tested effect') +
    ylab('') +
    facet_wrap(~test_type, nrow = 2, 
               scales = "free_y",
               strip.position = "left", 
               labeller = as_labeller(c(Frequentist = "log10(pvalue)", Bayesian= "log10(BF01)")))+
    scale_fill_manual(values=c("red","darkorange2","cyan4","purple","blue"),name="")+
    theme(panel.background = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12),
          axis.text.x =element_text(size = 10, colour= colourdata),
          axis.line.y = element_line(),
          legend.text = element_text(size = 10),
          strip.text = element_text(size = 10),
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          strip.placement = "outside")
  
  ggsave('./Output/MisMatchMain.jpg',f1)
  ggsave('./Output/MisMatchMain.svg',f1)
} else {
  f1=ggplot(results_preprocessed_long_misMatch, aes(x=dataset, y=log.pvalue, fill=test)) +
    geom_bar(alpha=0.5,stat="identity",position = position_dodge())+
    scale_fill_manual(values=c("palegreen4","bisque3","skyblue4","darkorange2","cyan4"),name="")+
    geom_hline(yintercept = log10(param$alpha), linetype="dashed", color = "black") +
    theme_minimal() +
    xlab('Tested effect') +
    ylab('log10(pvalue)') +
    theme(panel.background = element_blank(),
          axis.title = element_text(size = 12),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave('./Output/MisMatchSupp.jpg',f1) 
}

#################### figure  2 histograms
if (param$incSubjObjTest==1) {

  #add color vec to results_preprocessed
  results_preprocessed$ColorVec=3
  results_preprocessed$ColorVec[results_preprocessed$AllNoSig==1]=2
  results_preprocessed$ColorVec[results_preprocessed$AllSig==1]=1

  # add color vec to data_per_subj_all
  data_per_subj_all$dataset=as.factor(data_per_subj_all$dataset)
  vec=rep(NA,dim(data_per_subj_all)[1])
  for (ii in 1:length(datasetNames)) { #datasets
    ind=which(datasetNames[ii]==results_preprocessed$dataset)
    value=results_preprocessed$ColorVec[ind]
    vec[which(datasetNames[ii]==data_per_subj_all$dataset)]=value
  }
  data_per_subj_all$ColorVec=as.factor(vec)
  
  ###### figure 1: datasets with all non significant tests
  
  #filter datasets with all non significant tests in data_per_subj_all
  all_not_sig=filter(data_per_subj_all, ColorVec == 2)
  
  #change dataset names to have space instead of underscore
  all_not_sig$dataset = gsub('_',' ',all_not_sig$dataset)
  
  f2=ggplot(all_not_sig, aes(x=AS)) +
    geom_histogram(aes(y = stat(density)*0.02) ,binwidth = 0.02, fill="darkturquoise") +
    theme_bw() + 
    geom_vline(xintercept = param$ChanceSuccessRate, linetype="dashed", color = "gray") +
    scale_y_continuous(labels = percent)+
    ylab('Proportion %') +
    xlab('Awareness score') +
    scale_x_continuous(breaks=seq(from=0.4,to=0.6,by=0.1))+
    facet_wrap(~dataset, ncol=6 ,labeller = label_wrap_gen(multi_line = TRUE))+
    theme(strip.text.x = element_text(size=6),
          axis.title = element_text(size=14),
          axis.text = element_text(size=10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) 
  ggsave('./Output/ASHistNoSig.jpg',f2)
  
  ###### figure 2: datasets with all significant tests
  #filter datasets with all  significant tests in data_per_subj_all
  all_sig=filter(data_per_subj_all, ColorVec == 1 )
  
  #change dataset names to have space instead of underscore
  all_sig$dataset = gsub('_',' ',all_sig$dataset)
  
  f3=ggplot(all_sig, aes(x=AS)) +
    geom_histogram(aes(y = stat(density)*0.02) ,binwidth = 0.02, fill="deeppink3") +
    theme_bw() + 
    geom_vline(xintercept = param$ChanceSuccessRate, linetype="dashed", color = "gray") +
    scale_y_continuous(labels = percent)+
    ylab('Proportion %') +
    xlab('Awareness score') +
    scale_x_continuous(breaks=seq(from=0.4,to=0.8,by=0.2))+
    facet_wrap(~dataset, ncol=6, labeller = label_wrap_gen(multi_line = TRUE))+
    theme(strip.text.x = element_text(size=6),
          axis.title = element_text(size=14),
          axis.text = element_text(size=10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) 
  ggsave('./Output/ASHistSig.jpg',f3)
  
  ###### figure 3: datasets with tests that don't agree
  
  # add color vec to results_preprocessed_long
  results_preprocessed_long$ColorVec=3
  results_preprocessed_long$ColorVec[results_preprocessed_long$AllNoSig==1]=2
  results_preprocessed_long$ColorVec[results_preprocessed_long$AllSig==1]=1
  
  #filter datasets with tests that don't agree in data_per_subj_all and results_preprocessed_long
  not_same_ASPlot=filter(data_per_subj_all, ColorVec == 3)
  not_same_pvaluePlot=filter(results_preprocessed_long, ColorVec == 3)
  not_same_pvaluePlot_F=filter(not_same_pvaluePlot, test_type=="Frequentist")
  not_same_pvaluePlot_B=filter(not_same_pvaluePlot, test_type=="Bayesian")
  
  #change dataset names to have space instead of underscore
  not_same_pvaluePlot_F$dataset = gsub('_',' ',not_same_pvaluePlot_F$dataset)
  not_same_pvaluePlot_B$dataset = gsub('_',' ',not_same_pvaluePlot_B$dataset)
  not_same_ASPlot$dataset = gsub('_',' ',not_same_ASPlot$dataset)
  
  f4= ggplot(not_same_ASPlot, aes(x=AS)) +
    geom_histogram(aes(y = stat(density)*0.02),binwidth = 0.02, fill="steelblue3") +
    theme_bw() +
    geom_vline(xintercept = param$ChanceSuccessRate, linetype="dashed", color = "gray") +
    scale_y_continuous(labels = percent)+
    ylab('Proportion %') +
    xlab('Awareness score') +
    scale_x_continuous(breaks=seq(from=0.3,to=0.7,by=0.2))+
    facet_wrap(~dataset, ncol=5,labeller = label_wrap_gen(multi_line = TRUE))+
    theme(strip.text.x = element_text(size=6),
          axis.title = element_text(size=14),
          axis.text = element_text(size=10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  ggsave('./Output/ASHistMisMatch1.jpg',f4)
  
  f5=ggplot(not_same_pvaluePlot_F, aes(x=test,y=log.pvalue,fill=test)) +
    geom_bar(alpha=0.5,stat="identity")+
    geom_hline(yintercept = log10(param$alpha), linetype="dashed", color = "black") +
    theme_bw() +
    ylab('log10(pvalue)') +
    xlab('') +
    scale_fill_manual(values=c("red","pink3","dodgerblue1","darkorange2","cyan4"),name="")+
    facet_wrap(~dataset,labeller = label_wrap_gen(multi_line = TRUE))+
    theme(strip.text.x = element_text(size=6),
          axis.title = element_text(size = 14),
          axis.text.y = element_text(size=10),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave('./Output/pvalueFreqMisMatch2.jpg',f5)
  
  f6=ggplot(not_same_pvaluePlot_B, aes(x=test,y=log.pvalue,fill=test)) +
    geom_bar(alpha=0.5,stat="identity")+
    geom_hline(yintercept = log10(1/param$bfThreshold), linetype="dashed", color = "black") +
    geom_hline(yintercept = log10(param$bfThreshold), linetype="dashed", color = "black") +
    theme_bw() +
    ylab('log10(BF01)') +
    xlab('') +
    facet_wrap(~dataset,labeller = label_wrap_gen(multi_line = TRUE))+
    scale_fill_manual(values=c("purple","blue"),name="")+
    theme(strip.text.x = element_text(size=6),
          axis.title = element_text(size = 14),
          axis.text.y = element_text(size=10),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.text = element_text(size = 10),
          legend.title = element_blank(),
          legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave('./Output/pvalueBayesMisMatch3.jpg',f6)
}  


