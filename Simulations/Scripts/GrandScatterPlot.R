#this script create one plot from all simulation results and saves the plot

# Load required R packages and sources
rm(list=ls())
library(groundhog)
pkgs <- c("tidyverse","wesanderson", "stringr", "dplyr", "gridExtra", "svglite",
          "RColorBrewer", "scales", "gridExtra", "ggh4x")
groundhog.library(pkgs, "2025-03-01", tolerate.R.version = '4.5.0')

## parameters
optionImage = 4 
#1 = test types: GBC, T, MMLR, GBBayes, TBayes
#create also a Bayesian image:  GBBayes, TBayes, with H0 for supplementary
#2 = test types: GBC, GB, Chi
#3 = GBC, GlobalNull
#4 = GBBayes, TBayes, GB_Bayes_Uninformative

## load all results to one dataframe
fileName=Sys.glob('./Simulations/Output/*.RData')

#load results
load(fileName)

#delete all_results field (data for AUC)
all_results=select(all_results,-all_results)

## Organize data
#change number of subjects to be a factor
all_results$n_participants=as.factor(all_results$n_participants)

#change sig_perc to percent
all_results$sig_perc=all_results$sig_perc*100
all_results$null_perc=all_results$null_perc*100

# Update test names
all_results$test=recode_factor(all_results$test, GB_Bayes="GBBayes")

#include only the statistical tests wanted
if (optionImage==1) {
  all_results=filter(all_results, test == "GBC" | test == "T" | test == "MMLR" | test == "GBBayes" | test == "TBayes")
}  else if (optionImage==2) {
  all_results=filter(all_results, test == "GBC" | test == "GB" | test == "Chi")
}  else if (optionImage==3) {
  all_results=filter(all_results, test == "GBC" | test == "GlobalNull")
}  else if (optionImage==4) {
  all_results=filter(all_results, test == "GBBayes" | test == "GB_Bayes_Uninformative" | test == "TBayes")
}

#calculate CI for alpha=0.05 based on the number of iterations
CI=qbinom(c(0.025,0.975),size=fixed_params@n_iterations,prob=(fixed_params@alpha))
CI=(CI/fixed_params@n_iterations)*100

#change order of analysis type presentation 
all_results$analysis_type=factor(all_results$analysis_type,levels=c('Unaware','Mixed','Small_spread','Large_spread'))

# Create data for lines (here the different analysis type are not in the same order as defined above)
data_hline1= data.frame(analysis_type = unique(all_results$analysis_type),hline = c(NA,NA,NA,CI[1]))
data_hline2= data.frame(analysis_type = unique(all_results$analysis_type),hline = c(NA,NA,NA,CI[2]))
data_hline3= data.frame(analysis_type = unique(all_results$analysis_type),hline = c(NA,NA,NA,fixed_params@alpha*100))

# New facet label names for simulation names
analysisName.labs <- c('Unaware','Mixed','Aware small var','Aware large var')
names(analysisName.labs) <- c('Unaware','Mixed','Small_spread','Large_spread')

# Divide data to frequentist and Bayesian
if (optionImage==1) {
  all_results_B=filter(all_results, test == "GBBayes" | test == "TBayes")
  all_results_F=filter(all_results, test == "GBC" | test == "T" | test == "MMLR")
} else if (optionImage==2 | optionImage==3)  {
  all_results_F=all_results
} else if (optionImage==4)  {
  all_results_B=all_results
}

if (optionImage==1 | optionImage==4) {
  #Create in Bayesian data H0 result and H1 result
  #H1
  all_results_B0=all_results_B
  all_results_B0$sig_perc=all_results_B0$null_perc
  all_results_B0$test=recode_factor(all_results_B0$test, GBBayes="GBBayes H0", TBayes="TBayes H0",GB_Bayes_Uninformative='GBBayes_Uninformative H0')
    
  all_results_B1=all_results_B
  all_results_B1$test=recode_factor(all_results_B1$test, GBBayes="GBBayes H1", TBayes="TBayes H1",GB_Bayes_Uninformative='GBBayes_Uninformative H1')
  
  # all_results_B for supplementary plot
  all_results_B_Supp=rbind(all_results_B0,all_results_B1)
}
  
if (optionImage==1) {
  #Bind Bayesian and Frequentist data to one large dataframe
  all_results_B1$test_type="Bayesian"
  all_results_F$test_type="Frequentist"
  all_results=rbind(all_results_F,all_results_B1)
  
  #rename Bayesian tests (becease H0 is not included)
  all_results$test=recode_factor(all_results$test, "GBBayes H1"="GBBayes", "TBayes H1"="TBayes")
  
  #change order of test type presentation 
  all_results$test_type=factor(all_results$test_type,levels=c('Frequentist','Bayesian'))
}

#change order of statistical tests presentation 
if (optionImage==1) {
  all_results$test=factor(all_results$test,levels=c("GBC","T","MMLR","GBBayes","TBayes"))
  all_results_B_Supp$test=factor(all_results_B_Supp$test,levels=c('GBBayes H0','GBBayes H1','TBayes H0','TBayes H1'))
}  else if (optionImage==2) {
  all_results_F$test=factor(all_results_F$test,levels=c("GBC","GB","Chi"))
}  else if (optionImage==3) {
  all_results_F$test=factor(all_results_F$test,levels=c("GBC","GlobalNull"))
}  else if (optionImage==4) {
  all_results_B_Supp$test=factor(all_results_B_Supp$test,levels=c('GBBayes H0','GBBayes H1','GBBayes_Uninformative H0','GBBayes_Uninformative H1','TBayes H0','TBayes H1'))
}

if (optionImage==1) {
  #Main plot
  f1=ggplot(all_results, aes(x = n_participants, y = sig_perc, group=test, color=test)) +
    geom_line(alpha=0.4)+
    geom_point(size=2,alpha=0.4)+
    theme_bw() + 
    xlab('Participants') +
    ylab('% Significant') +
    ggh4x::facet_grid2(analysis_type ~ test_type+ mean_trials , switch = 'y', scales = "free_y", labeller = labeller(analysis_type = analysisName.labs)) + 
    ggh4x::facetted_pos_scales( 
      y = list( 
        `facet_row == 1` = scale_y_continuous(limits = c(0, 6), breaks = c(1,2,3,4,5)), 
        `facet_row == 2` = scale_y_continuous(limits = c(0, 100), breaks = c(25,50,75,100)), 
        `facet_row == 3` = scale_y_continuous(limits = c(0, 100), breaks = c(25,50,75,100)), 
        `facet_row == 4` = scale_y_continuous(limits = c(0, 100), breaks = c(25,50,75,100)) 
      ) 
    ) +
    geom_hline(data = data_hline1,aes(yintercept = hline), linetype="dashed", color = "black") +
    geom_hline(data = data_hline2,aes(yintercept = hline), linetype="dashed", color = "black") +
    geom_hline(data = data_hline3,aes(yintercept = hline), linetype="dashed", color = "gray") +
    scale_color_manual(values=c("red","darkorange2","cyan4","purple","blue"),name="") +
    theme(axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12),
          axis.text.y= element_text(size=12),
          axis.text.x = element_text(size=12),
          strip.text.x = element_text(size=12),
          legend.position = "bottom",
          strip.text.y  = element_text(size=12),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave('./Simulations/Output/SimulationsResultsMain.jpg',f1)

  #Bayesian plot for supplementary with H0
  f2=ggplot(all_results_B_Supp, aes(x = n_participants, y = sig_perc, group=test, color=test)) +
    geom_line(alpha=0.4)+
    geom_point(size=2,alpha=0.4)+
    theme_bw() + 
    xlab('Participants') +
    ylab('% Significant') +
    ggh4x::facet_grid2(analysis_type ~ mean_trials, switch = 'y', scales = "free", labeller = labeller(analysis_type = analysisName.labs)) + 
    ggh4x::facetted_pos_scales( 
      y = list( 
        `facet_row == 1` = scale_y_continuous(limits = c(0, 100), breaks = c(25,50,75,100)), 
        `facet_row == 2` = scale_y_continuous(limits = c(0, 100), breaks = c(25,50,75,100)), 
        `facet_row == 3` = scale_y_continuous(limits = c(0, 100), breaks = c(25,50,75,100)), 
        `facet_row == 4` = scale_y_continuous(limits = c(0, 100), breaks = c(25,50,75,100)) 
      ) 
    ) +
    geom_hline(data = data_hline1,aes(yintercept = hline), linetype="dashed", color = "black") +
    geom_hline(data = data_hline2,aes(yintercept = hline), linetype="dashed", color = "black") +
    geom_hline(data = data_hline3,aes(yintercept = hline), linetype="dashed", color = "gray") +
    scale_color_manual(values=c("deeppink","purple","deepskyblue","blue"),name="")+
    theme(axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          strip.text.x = element_text(size=12),
          strip.text.y = element_text(size=12),
          legend.position = "bottom",
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  ggsave('./Simulations/Output/SimulationsBayesResultsSupp.jpg',f2)
  
} else if (optionImage==2) {
  
  f1=ggplot(all_results_F, aes(x = n_participants, y = sig_perc, group=test, color=test)) +
    geom_line(alpha=0.4)+
    geom_point(size=2,alpha=0.4)+
    theme_bw() + 
    xlab('Participants') +
    ylab('% Significant') +
    ggh4x::facet_grid2(analysis_type ~ mean_trials, switch = 'y', scales = "free", labeller = labeller(analysis_type = analysisName.labs)) + 
    ggh4x::facetted_pos_scales( 
      y = list( 
        `facet_row == 1` = scale_y_continuous(limits = c(4, 6), breaks = c(4,4.5,5,5.5,6)), 
        `facet_row == 2` = scale_y_continuous(limits = c(0, 100), breaks = c(25,50,75,100)), 
        `facet_row == 3` = scale_y_continuous(limits = c(0, 100), breaks = c(25,50,75,100)), 
        `facet_row == 4` = scale_y_continuous(limits = c(0, 100), breaks = c(25,50,75,100)) 
      ) 
    ) +
    geom_hline(data = data_hline1,aes(yintercept = hline), linetype="dashed", color = "black") +
    geom_hline(data = data_hline2,aes(yintercept = hline), linetype="dashed", color = "black") +
    geom_hline(data = data_hline3,aes(yintercept = hline), linetype="dashed", color = "gray") +
    scale_color_manual(values=c("red","pink3","dodgerblue1"),name="")+
    theme(axis.title.x = element_text(size=12),
          axis.text.x = element_text(size=12),
          strip.text.x = element_text(size=12),
          strip.text.y = element_text(size=12),
          axis.title.y = element_text(size=12),
          axis.text.y = element_text(size=12),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave('./Simulations/Output/SimulationsFreqResultSupp.jpg',f1)
  
} else if (optionImage==3) {
  
  f1=ggplot(all_results_F, aes(x = n_participants, y = sig_perc, group=test, color=test)) +
    geom_line(alpha=0.4)+
    geom_point(size=2,alpha=0.4)+
    theme_bw() + 
    xlab('Participants') +
    ylab('% Significant') +
    ggh4x::facet_grid2(analysis_type ~ mean_trials, switch = 'y', scales = "free", labeller = labeller(analysis_type = analysisName.labs)) + 
    ggh4x::facetted_pos_scales( 
      y = list( 
        `facet_row == 1` = scale_y_continuous(limits = c(0, 6), breaks = c(1,2,3,4,5)), 
        `facet_row == 2` = scale_y_continuous(limits = c(0, 100), breaks = c(25,50,75,100)), 
        `facet_row == 3` = scale_y_continuous(limits = c(0, 100), breaks = c(25,50,75,100)), 
        `facet_row == 4` = scale_y_continuous(limits = c(0, 100), breaks = c(25,50,75,100)) 
      ) 
    ) +
    geom_hline(data = data_hline1,aes(yintercept = hline), linetype="dashed", color = "black") +
    geom_hline(data = data_hline2,aes(yintercept = hline), linetype="dashed", color = "black") +
    geom_hline(data = data_hline3,aes(yintercept = hline), linetype="dashed", color = "gray") +
    scale_color_manual(values=c("red","darkturquoise"),name="")+
    theme(axis.title.x = element_text(size=12),
          axis.text.x = element_text(size=12),
          strip.text.x = element_text(size=12),
          strip.text.y = element_text(size=12),
          axis.title.y = element_text(size=12),
          axis.text.y = element_text(size=12),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave('./Simulations/Output/SimulationsGlobalNullSupp.jpg',f1)
  
} else if (optionImage==4) {
  
  #Bayesian plot for supplementary with H0
  f1=ggplot(all_results_B_Supp, aes(x = n_participants, y = sig_perc, group=test, color=test)) +
    geom_line(alpha=0.4)+
    geom_point(size=2,alpha=0.4)+
    theme_bw() + 
    xlab('Participants') +
    ylab('% Significant') +
    ggh4x::facet_grid2(analysis_type ~ mean_trials, switch = 'y', scales = "free", labeller = labeller(analysis_type = analysisName.labs)) + 
    ggh4x::facetted_pos_scales( 
      y = list( 
        `facet_row == 1` = scale_y_continuous(limits = c(0, 100), breaks = c(25,50,75,100)), 
        `facet_row == 2` = scale_y_continuous(limits = c(0, 100), breaks = c(25,50,75,100)), 
        `facet_row == 3` = scale_y_continuous(limits = c(0, 100), breaks = c(25,50,75,100)), 
        `facet_row == 4` = scale_y_continuous(limits = c(0, 100), breaks = c(25,50,75,100)) 
      ) 
    ) +
    geom_hline(data = data_hline1,aes(yintercept = hline), linetype="dashed", color = "black") +
    geom_hline(data = data_hline2,aes(yintercept = hline), linetype="dashed", color = "black") +
    geom_hline(data = data_hline3,aes(yintercept = hline), linetype="dashed", color = "gray") +
    scale_color_manual(values=c("deeppink","purple","olivedrab3","forestgreen","deepskyblue","blue"),name="")+
    theme(axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          strip.text.x = element_text(size=12),
          strip.text.y = element_text(size=12),
          legend.position = "bottom",
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave('./Simulations/Output/SimulationsBayesUninformativeSupp.jpg',f1)
}
