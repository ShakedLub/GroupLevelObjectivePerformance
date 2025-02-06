library(readxl)
library(tidyverse)
library(dplyr)

## Startup:
rm(list=ls()) 

############################### Exp 6
#parameters
numTrials=64
chance=0.5
alpha=0.05

#read the data set
raw_data <- read_excel("./PapersAndDownloadedData/Sklar/Exp6.xlsx")

colnames(raw_data)[which(names(raw_data)=="Unaware?")]="Unaware"

#calculate SR ##########################
raw_data$SR <- raw_data$objective_centered_on_0.5+0.5

### exclude subjects
#exclude subjects that are subjective aware 
#(all subjects excluded are due to objective or subjective awareness)
raw_data$R=round(raw_data$SR*numTrials) #round because the SR is rounded to 2 numbers after the digit
raw_data$pbinomEachSubj=1-pbinom(q=raw_data$R,size=numTrials,prob=chance)
raw_data$excObjTest=ifelse(raw_data$pbinomEachSubj<alpha,1,0)
#unaware = 1 subjective or objective aware
#excObjTest = 1 objective aware
#           |
#           V
#excObjTest == 0 & Unaware == 1 unaware
#excObjTest == 1 & Unaware == 0 objective aware
datafilt <- filter(raw_data, excObjTest == 0 & Unaware == 1 | excObjTest == 1 & Unaware == 0) 

#check that there is no subjective aware subjects in the objective aware ones
#by checking that all subjective aware subjects are found by excObjTest == 0 & Unaware == 0 
r_check <- filter(raw_data, excObjTest == 0 & Unaware == 0)
if (dim(r_check)[1] != 4) {
  stop(paste('Wrong number of subjects found for subjective aware condition'))
}

#check number of subjects (unaware+objective aware) is correct
if (dim(datafilt)[1] != 38) {
  stop(paste('Wrong number of subjects found'))
}

#check number of unaware subejcts is correct
d_check <- filter(datafilt, excObjTest == 0)
if (dim(d_check)[1] != 17) {
  stop(paste('Wrong number of subjects found'))
}

# create data_per_subj
data_per_subj<- subset(datafilt, select= c(subject,SR,excObjTest))
colnames(data_per_subj)[which(names(data_per_subj)=="subject")]="subj"

#add number of trials
data_per_subj$ntrials=numTrials

data_per_subj$paper="S_2012"
data_per_subj$dataset="Sklar_et_al_2012_E6"

#create data dataframe
for (ii in 1:dim(data_per_subj)[1]) { #subjects
  subjNum=data_per_subj$subj[ii]
  SR=data_per_subj$SR[ii]
  N=data_per_subj$ntrials[ii]
  exc=data_per_subj$excObjTest[ii]
  
  subj=rep(subjNum,N)
  excObjTest=rep(exc,N)
  R=round(SR*N) #round because the SR is rounded to 2 numbers after the digit
  
  correct=c(rep(1,R),rep(0,N-R))
  datasubj=data.frame(subj,correct,excObjTest)

  #create data dataframe
  if (ii==1) {
    data=datasubj
  } else {
    data=rbind(data,datasubj)
  }
}

data$paper="S_2012"
data$dataset="Sklar_et_al_2012_E6"

data_6=data
data_per_subj_6=data_per_subj

############################### Exp 7
#clean workspace
rm(list=setdiff(ls(),c("data_6","data_per_subj_6","chance","alpha")))

#parameters
numTrials=64

#read the data set
raw_data <- read_excel("./PapersAndDownloadedData/Sklar/Exp7.xlsx")

colnames(raw_data)[which(names(raw_data)=="Unaware?")]="Unaware"

#calculate SR ##########################
raw_data$SR <- raw_data$objective_centered_on_0.5+0.5

### exclude subjects
#exclude subjects that are subjectively aware
#(all subjects excluded are due to objective or subjective awareness)
raw_data$R=round(raw_data$SR*numTrials) #round because the SR is rounded to 2 numbers after the digit
raw_data$pbinomEachSubj=1-pbinom(q=raw_data$R,size=numTrials,prob=chance)
raw_data$excObjTest=ifelse(raw_data$pbinomEachSubj<alpha,1,0)
#unaware = 1 subjective or objective aware
#excObjTest = 1 objective aware
#           |
#           V
#excObjTest == 0 & Unaware == 1 not aware
#excObjTest == 1 & Unaware == 0 objective aware
datafilt <- filter(raw_data, excObjTest == 0 & Unaware == 1 | excObjTest == 1 & Unaware == 0) 

#check that there is no subjective aware subjects in the objective aware ones
#by checking that all subjective aware subjects are found by excObjTest == 0 & Unaware == 0 
r_check <- filter(raw_data, excObjTest == 0 & Unaware == 0)
if (dim(r_check)[1]!= 5) {
  paste('Wrong number of subjects found for subjective aware condition')
}
  
#check number of subjects (unaware and objective aware) is correct
if (dim(datafilt)[1]!= 60) {
  (paste('Wrong number of subjects found'))
}

#check number of unaware subjects is correct
d_check <- filter(datafilt, excObjTest == 0)
if (dim(d_check)[1]!= 30) {
  stop(paste('Wrong number of subjects found'))
}

# create data_per_subj
data_per_subj<- subset(datafilt, select= c(subject,SR,excObjTest))
colnames(data_per_subj)[which(names(data_per_subj)=="subject")]="subj"

#add number of trials
data_per_subj$ntrials=numTrials

data_per_subj$paper="S_2012"
data_per_subj$dataset="Sklar_et_al_2012_E7"

#create data dataframe
for (ii in 1:dim(data_per_subj)[1]) { #subjects
  subjNum=data_per_subj$subj[ii]
  SR=data_per_subj$SR[ii]
  N=data_per_subj$ntrials[ii]
  exc=data_per_subj$excObjTest[ii]
  
  subj=rep(subjNum,N)
  excObjTest=rep(exc,N)
  R=round(SR*N)
  
  correct=c(rep(1,R),rep(0,N-R))
  datasubj=data.frame(subj,correct,excObjTest)
  
  #create data dataframe
  if (ii==1) {
    data=datasubj
  } else {
    data=rbind(data,datasubj)
  }
}

data$paper="S_2012"
data$dataset="Sklar_et_al_2012_E7"

data_7=data
data_per_subj_7=data_per_subj

############################### Exp 9
#clean workspace
rm(list=setdiff(ls(),c("data_6","data_per_subj_6","data_7","data_per_subj_7","chance","alpha")))

#parameters
numTrials=48

#read the data set
raw_data <- read.csv("./PapersAndDownloadedData/Sklar/Exp9.csv")

#SR
colnames(raw_data)[which(names(raw_data)=="objectiv")]="SR"

### exclude subjects
#exclude subjectively aware subjects and subjects excluded due to different reasons:
#12 participants who did not make any mistakes in the incongruent condition 
#1 participant who did not follow the experimental instructions.

#exclude subejctive aware subjects
raw_data$R=round(raw_data$SR*numTrials) #some R have very small remainder and some don't have probably because of rounding when saving to csv file
raw_data$pbinomEachSubj=1-pbinom(q=raw_data$R,size=numTrials,prob=chance)
raw_data$excObjTest=ifelse(raw_data$pbinomEachSubj<alpha,1,0)

#objecctive_subjective_filter = 0  subjective or objective aware
#excObjTest = 1 objective aware
#           |
#           V
#excObjTest == 0 & objecctive_subjective_filter == 1 not aware
#excObjTest == 1 & objecctive_subjective_filter == 0 objective aware
data_filt <- filter(raw_data, excObjTest == 0 & objecctive_subjective_filter == 1 | excObjTest == 1 & objecctive_subjective_filter == 0) 

#check that there is no subjective aware subjects in the objective aware ones
#by checking that all subjective aware subjects are found by excObjTest == 0 & objecctive_subjective_filter == 0 
r_check <- filter(raw_data, excObjTest == 0 & objecctive_subjective_filter == 0)
if (dim(r_check)[1]!= 3) {
  paste('Wrong number of subjects found for subjective aware condition')
}

#more reasons subjects are excluded:
#filters chosen according to the number of subjects that were excluded and common sense
data_filt <- filter(data_filt, cieling_Filter_on_error2  == 1) # 12 participants who did not make any mistakes in the incongruent condition (i.e., they were at ceiling) and there fore, could not have improved in the congruent condition 
#It is error 2 because in the table the incongruent condition is condition 2
data_filt <- filter(data_filt, Error_rate_filter  == 1) # 1 participant who did not follow the experimental instructions.
#The error rate will not be good if the subejct didn't follow instructions

#Check the selection of filters
data_check <- filter(raw_data, objecctive_subjective_filter  == 1) #10 subjective and objective aware subjects
data_check <- filter(data_check, cieling_Filter_on_error2  == 1) # 12 participants who did not make any mistakes in the incongruent condition
data_check <- filter(data_check, Error_rate_filter  == 1) # 1 participant who did not follow the experimental instructions.

#check the general filter is like the filters I chose
raw_data_check <- filter(raw_data, filter_.  == 1)

if (any(raw_data_check$subj != data_check$subj)) {
  stop(paste('Wrong filtering procedure'))
}
 
#check number of subjects (unaware and objective aware) is correct
if (dim(data_filt)[1]!= 40) {
  (paste('Wrong number of subjects found'))
}

#check number of unaware subjects is correct
d_check <- filter(data_filt, excObjTest == 0)
if (dim(d_check)[1]!= 33) {
  stop(paste('Wrong number of subjects found'))
}

# create data_per_subj
data_per_subj<- subset(data_filt, select= c(subj,SR,excObjTest))

#add number of trials
data_per_subj$ntrials=numTrials

data_per_subj$paper="S_2012"
data_per_subj$dataset="Sklar_et_al_2012_E9"

#create data dataframe
for (ii in 1:dim(data_per_subj)[1]) { #subjects
  subjNum=data_per_subj$subj[ii]
  SR=data_per_subj$SR[ii]
  N=data_per_subj$ntrials[ii]
  exc=data_per_subj$excObjTest[ii]
  
  subj=rep(subjNum,N)
  excObjTest=rep(exc,N)
  R=round(SR*N) #some R have very small remainder and some don't have probably because of rounding when saving to csv file
  
  correct=c(rep(1,R),rep(0,N-R))
  datasubj=data.frame(subj,correct,excObjTest)
  
  #create data dataframe
  if (ii==1) {
    data=datasubj
  } else {
    data=rbind(data,datasubj)
  }
}

data$paper="S_2012"
data$dataset="Sklar_et_al_2012_E9"

data_9=data
data_per_subj_9=data_per_subj

data_all=rbind(data_6,data_7,data_9)
data_per_subj_all=rbind(data_per_subj_6,data_per_subj_7,data_per_subj_9)

#save the data
processed_data <- list(trial_by_trial = data_all, summary_tables = data_per_subj_all)
save(processed_data, file = "./Data/S_2012.RData")
