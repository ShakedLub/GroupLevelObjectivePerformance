#Create data for group level analysis and save it
library(readxl)
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

compareDataToPaper<-function(realdata,paper,num,datasetName){
  if (round(realdata,num)!=paper) {
    stop(paste("In ",datasetName," the data is not the same as in the paper. real:",as.character(realdata)," paper:",as.character(paper)))
  }
}

############################################################################
#Experiment 1b
#read the data set 
<<<<<<< Updated upstream
data <- read_excel("./Datasets/Biderman_et_al_2020/Post1bAllSubj.xlsx",col_names = FALSE)
=======
data <- read_excel("./DatasetsReanalysis/PapersAndDownloadedData/Biderman_et_al_2020/Post1bAllSubj.xlsx",col_names = FALSE)
>>>>>>> Stashed changes
colnames(data)=c("Subject","Distance","Context","Resp13", "RT", "Vis")
#exclude subjects that are excluded not due to the objective test 812,828,831,832
data <- subset(data, Subject!=812 & Subject!=828 & Subject!=831 & Subject!=832)

#correct responses
data$correct <- ifelse(data$Context==data$Resp13, 1, 0)

#Include only visibility 1 trials
data <- filter(data, Vis==1)

colnames(data)[which(names(data)=="Subject")]="subj"

data_per_subj <- dplyr::summarise(group_by(data,subj),
                                  SR = sum(correct)/length(correct), 
                                  ntrials = length(correct))

#check accuracy calculation for subjects who are not excluded to objective awareness
<<<<<<< Updated upstream
data_check_acc <- read.csv("./Datasets/Biderman_et_al_2020/Experimental and analysis codes/AnalysesCodes/Data/Summary_Matrices/accuracy_dprime/E1b_acc_dprime.csv")
=======
data_check_acc <- read.csv("./DatasetsReanalysis/PapersAndDownloadedData/Biderman_et_al_2020/Experimental and analysis codes/AnalysesCodes/AnalysesCodes/Data/Summary_Matrices/accuracy_dprime/E1b_acc_dprime.csv")
>>>>>>> Stashed changes
data_check=subset(data_per_subj, subj!=807 & subj!=808 & subj!=810 & subj!=815 & subj!=817) #objective aware subj
if (!identical(round(data_check$SR,5) ,data_check_acc$acc)) {
  stop(paste("In e1b the data is not the same as in the paper"))
}

#add column excObjTest
data_per_subj$excObjTest=ifelse(data_per_subj$SR>0.65,1,0)
numSubj=data_per_subj$subj
for (ii in 1:length(numSubj)) {
  exc=data_per_subj$excObjTest[ii]
  indSubj=data$subj==numSubj[ii]
  data[indSubj,"excObjTest"]=exc
}

#check number of subjects
if (dim(data_per_subj)[1] != 29) {
  stop(paste("In e1b wrong number of subjects"))
}
d_check <- filter(data_per_subj, excObjTest == 0)
if (dim(d_check)[1]!= 24) {
  stop(paste('In e1b wrong number of subjects'))
}

#check mean d prime is as in the paper
compareDataToPaper(mean(data_check_acc$d_prime),0.06,2,"B2020_1b")

data$paper="B_2020"
data_per_subj$paper="B_2020"
data$dataset="Biderman_et_al_2020_E1B"
data_per_subj$dataset="Biderman_et_al_2020_E1B"

data_1=data
data_per_subj_1=data_per_subj

############################################################################
#Experiment 2
#clean workspace
rm(list=setdiff(ls(),c("data_1","data_per_subj_1","compareDataToPaper")))

#read the data set 
<<<<<<< Updated upstream
data <- read_excel("./Datasets/Biderman_et_al_2020/Post2AllSubj.xlsx",col_names = FALSE)
=======
data <- read_excel("./DatasetsReanalysis/PapersAndDownloadedData/Biderman_et_al_2020/Post2AllSubj.xlsx",col_names = FALSE)
>>>>>>> Stashed changes
colnames(data)=c("Subject","Distance","Context","Resp13", "RT", "Vis")
#exclude subjects that are excluded not due to the objective test 
data <- subset(data, Subject!=603 & Subject!=623 & Subject!=626)

#correct responses
data$correct <- ifelse(data$Context==data$Resp13, 1, 0)

#Include only visibility 1 trials
data <- filter(data, Vis==1)

colnames(data)[which(names(data)=="Subject")]="subj"

data_per_subj <- dplyr::summarise(group_by(data,subj),
                                  SR = sum(correct)/length(correct), 
                                  ntrials = length(correct))

#check accuracy calculation for subjects who are not excluded due to objective test
<<<<<<< Updated upstream
data_check_acc <- read.csv("./Datasets/Biderman_et_al_2020/Experimental and analysis codes/AnalysesCodes/Data/Summary_Matrices/accuracy_dprime/E2_acc_dprime.csv")
=======
data_check_acc <- read.csv("./DatasetsReanalysis/PapersAndDownloadedData/Biderman_et_al_2020/Experimental and analysis codes/AnalysesCodes/AnalysesCodes/Data/Summary_Matrices/accuracy_dprime/E2_acc_dprime.csv")
>>>>>>> Stashed changes
data_check=subset(data_per_subj, subj!=607 & subj!=608 & subj!=619) #objective aware subjects
if (!identical(round(data_check$SR,5) ,data_check_acc$acc)) {
  stop(paste("In e2 the data is not the same as in the paper"))
}

#add column excObjTest
data_per_subj$excObjTest=ifelse(data_per_subj$SR>0.65,1,0)
numSubj=data_per_subj$subj
for (ii in 1:length(numSubj)) {
  exc=data_per_subj$excObjTest[ii]
  indSubj=data$subj==numSubj[ii]
  data[indSubj,"excObjTest"]=exc
}

#check number of subjects
if (dim(data_per_subj)[1] != 27) {
  stop(paste("In e2 wrong number of subjects"))
}
d_check <- filter(data_per_subj, excObjTest == 0)
if (dim(d_check)[1]!= 24) {
  stop(paste('In e2 wrong number of subjects'))
}
  
#check mean d prime is as in the paper
compareDataToPaper(mean(data_check_acc$d_prime),0.10,2,"B2020_2")

data$paper="B_2020"
data_per_subj$paper="B_2020"
data$dataset="Biderman_et_al_2020_E2"
data_per_subj$dataset="Biderman_et_al_2020_E2"

data_2=data
data_per_subj_2=data_per_subj

############################################################################
#Experiment 3b
#clean workspace
rm(list=setdiff(ls(),c("data_1","data_per_subj_1","data_2","data_per_subj_2","compareDataToPaper")))

#read the data set
<<<<<<< Updated upstream
data <- read.csv("./Datasets/Biderman_et_al_2020/Experimental and analysis codes/AnalysesCodes/Data/Summary_Matrices/E3b_Post.csv")
=======
data <- read.csv("./DatasetsReanalysis/PapersAndDownloadedData/Biderman_et_al_2020/Experimental and analysis codes/AnalysesCodes/AnalysesCodes/Data/Summary_Matrices/E3b_Post.csv")
>>>>>>> Stashed changes

#correct responses
data$correct <- data$ClassRef

#Include only visibility 1 trials
data <- filter(data, Vis==1)

colnames(data)[which(names(data)=="Subject")]="subj"

data_per_subj <- dplyr::summarise(group_by(data,subj),
                                  SR = sum(correct)/length(correct), 
                                  ntrials = length(correct))

#check accuracy calculation
<<<<<<< Updated upstream
data_check_acc <- read.csv("./Datasets/Biderman_et_al_2020/Experimental and analysis codes/AnalysesCodes/Data/Summary_Matrices/accuracy_dprime/E3b_acc_dprime.csv")
=======
data_check_acc <- read.csv("./DatasetsReanalysis/PapersAndDownloadedData/Biderman_et_al_2020/Experimental and analysis codes/AnalysesCodes/AnalysesCodes/Data/Summary_Matrices/accuracy_dprime/E3b_acc_dprime.csv")
>>>>>>> Stashed changes

if (!identical(round(data_per_subj$SR,5) ,data_check_acc$acc)) {
  stop(paste("In e3b the data is not the same as in the paper"))
}

#check number of subjects
if (dim(data_per_subj)[1] != 34) {
  stop(paste("In e3b wrong number of subjects"))
}

#check mean d prime is as in the paper
compareDataToPaper(mean(data_check_acc$d_prime),0.03,2,"B2020_3b")

data$paper="B_2020"
data_per_subj$paper="B_2020"
data$dataset="Biderman_et_al_2020_E3B"
data_per_subj$dataset="Biderman_et_al_2020_E3B"

#add column excObjTest
data_per_subj$excObjTest=ifelse(data_per_subj$SR>0.65,1,0)
numSubj=data_per_subj$subj
for (ii in 1:length(numSubj)) {
  exc=data_per_subj$excObjTest[ii]
  indSubj=data$subj==numSubj[ii]
  data[indSubj,"excObjTest"]=exc
} 

data_3=data
data_per_subj_3=data_per_subj

############################################################################
#Experiment 4b ###################
rm(list=setdiff(ls(),c("data_1","data_per_subj_1","data_2","data_per_subj_2","data_3","data_per_subj_3","compareDataToPaper")))

#read the data set 
<<<<<<< Updated upstream
data <- read.csv("./Datasets/Biderman_et_al_2020/Experimental and analysis codes/AnalysesCodes/Data/Summary_Matrices/E7_Post.csv")
=======
data <- read.csv("./DatasetsReanalysis/PapersAndDownloadedData/Biderman_et_al_2020/Experimental and analysis codes/AnalysesCodes/AnalysesCodes/Data/Summary_Matrices/E7_Post.csv")
>>>>>>> Stashed changes
# Condition: 0 = categorical cond; 1 = lexical cond 

#correct responses
data$correct <- data$ClassRef

#Include only visibility 1 trials
data <- filter(data, Vis==1)

#divide the data to lexical condition and categorical condition
data_categorical <- filter(data, Condition==0)
data_lexical <- filter(data, Condition==1)

colnames(data_categorical)[which(names(data_categorical)=="Subject")]="subj"
colnames(data_lexical)[which(names(data_lexical)=="Subject")]="subj"

data_per_subj_categorical <- dplyr::summarise(group_by(data_categorical,subj),
                                              SR = sum(correct)/length(correct), 
                                              ntrials = length(correct))

data_per_subj_lexical <- dplyr::summarise(group_by(data_lexical,subj),
                                              SR = sum(correct)/length(correct), 
                                              ntrials = length(correct))

#check accuracy calculation
<<<<<<< Updated upstream
data_check_acc <- read.csv("./Datasets/Biderman_et_al_2020/Experimental and analysis codes/AnalysesCodes/Data/Summary_Matrices/accuracy_dprime/E7_acc_dprime.csv")
=======
data_check_acc <- read.csv("./DatasetsReanalysis/PapersAndDownloadedData/Biderman_et_al_2020/Experimental and analysis codes/AnalysesCodes/AnalysesCodes/Data/Summary_Matrices/accuracy_dprime/E7_acc_dprime.csv")
>>>>>>> Stashed changes

if (!identical(round(data_per_subj_categorical$SR,5) ,data_check_acc$acc_cat)) {
  stop(paste("In e4b the categorical data is not the same as in the paper"))
}

if (!identical(round(data_per_subj_lexical$SR,5) ,data_check_acc$acc_lex)) {
  stop(paste("In e4b the lexical data is not the same as in the paper"))
}

#add column excObjTest to lexical and categorical dataframes
data_per_subj_lexical$excObjTest=ifelse(data_per_subj_lexical$SR>0.65 | data_per_subj_categorical$SR>0.65,1,0)
data_per_subj_categorical$excObjTest=data_per_subj_lexical$excObjTest

numSubj=data_per_subj_lexical$subj
for (ii in 1:length(numSubj)) {
  exc=data_per_subj_lexical$excObjTest[ii]

  indSubj_lexical=data_lexical$subj==numSubj[ii]
  indSubj_categorical=data_categorical$subj==numSubj[ii]
  
  data_lexical[indSubj_lexical,"excObjTest"]=exc
  data_categorical[indSubj_categorical,"excObjTest"]=exc
}

#check number of subjects
if (dim(data_per_subj_categorical)[1] != 34) {
  stop(paste("In e4b categorical data wrong number of subjects"))
}

if (dim(data_per_subj_lexical)[1] != 34) {
  stop(paste("In e4b lexical data wrong number of subjects"))
}
  
#check mean d prime is as in the paper
compareDataToPaper(mean(data_check_acc$d_prime_cat),0.01,2,"B2020_4b_lexical")
compareDataToPaper(mean(data_check_acc$d_prime_lex),-0.07,2,"B2020_4b_categorical")

data_lexical$paper="B_2020"
data_lexical$dataset="Biderman_et_al_2020_E4B-a"
data_categorical$paper="B_2020"
data_categorical$dataset="Biderman_et_al_2020_E4B-b"

data_per_subj_lexical$paper="B_2020"
data_per_subj_lexical$dataset="Biderman_et_al_2020_E4B-a"
data_per_subj_categorical$paper="B_2020"
data_per_subj_categorical$dataset="Biderman_et_al_2020_E4B-b"

########### concatenate data
D1=subset(data_1,select=c("subj","correct","paper","dataset","excObjTest"))
D2=subset(data_2,select=c("subj","correct","paper","dataset","excObjTest"))
D3=subset(data_3,select=c("subj","correct","paper","dataset","excObjTest"))
D4lex=subset(data_lexical,select=c("subj","correct","paper","dataset","excObjTest"))
D4cat=subset(data_categorical,select=c("subj","correct","paper","dataset","excObjTest"))
  
data_all=rbind(D1,D2,D3,D4lex,D4cat)
data_per_subj_all=rbind(data_per_subj_1,data_per_subj_2,data_per_subj_3,data_per_subj_lexical,data_per_subj_categorical)
  
########### save data
processed_data <- list(trial_by_trial = data_all, summary_tables = data_per_subj_all)
save(processed_data, file = "./DatasetsReanalysis/Data/B_2020.RData")

