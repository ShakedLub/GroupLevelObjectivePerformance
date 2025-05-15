library(dplyr)
library(readxl)

#clean workspace
rm(list=ls())

compareDataToPaper<-function(realdata,paper,num,datasetName){
  if (round(realdata,num)!=round(paper,num)) {
    stop(paste("In ",datasetName," the data is not the same as in the paper. real:",as.character(realdata)," paper:",as.character(paper)))
  }
}

############################################################################
#Experiment 1 small

#read the data set 
raw_data <- read_excel("./Datasets/VanOpstal/VanOpstal2022_Exp1.xlsx")

#exclude subjects from the unmasked condition
raw_data <- filter(raw_data, Condition == "Masked") 

#create data_per_subj from the raw_data
data_per_subj=data.frame(matrix(nrow = 30 , ncol = 0))
data_per_subj$subj=raw_data$SubjNr
data_per_subj$SR=raw_data$PercentCorrectSmall/100
data_per_subj$ntrials=raw_data$SmallTotal

#Check
if ( all(raw_data$SmallTotal != (raw_data$SmallErr + raw_data$SmallCorr))) {
  stop(paste("Problem with number of trials in Exp 1 Small"))
}

data_per_subj$paper="vO_2022"
data_per_subj$dataset="van_Opstal_&_Rooyakkers_2022_E1-a"

#recreate data from data_per_subj
data=data.frame()
for (ii in 1:dim(data_per_subj)[1]) {
  subjNum=data_per_subj$subj[ii]
  SR=data_per_subj$SR[ii]
  n=data_per_subj$ntrials[ii]
  R=raw_data$SmallCorr[ii]
  
  #check for each subject SR given is the same as calculated according to R
  compareDataToPaper(R/n,SR,5,"SubjectData")
  
  subj=rep(subjNum,n)
  correct=c(rep(1,R),rep(0,n-R))
  
  datasubj=data.frame(subj,correct)
  data=rbind(data,datasubj)
}
data$paper="vO_2022"
data$dataset="van_Opstal_&_Rooyakkers_2022_E1-a"

data_1_Small=data
data_per_subj_1_Small=data_per_subj

############################################################################
#Experiment 1 Medium

#create data_per_subj from the raw_data
data_per_subj=data.frame(matrix(nrow = 30 , ncol = 0))
data_per_subj$subj=raw_data$SubjNr
data_per_subj$SR=raw_data$PercentCorrectMedium/100
data_per_subj$ntrials=raw_data$MediumTotal

#Check
if ( all(raw_data$MediumTotal != (raw_data$MediumErr + raw_data$MediumCorr))) {
  stop(paste("Problem with number of trials in Exp 1 Medium"))
}

data_per_subj$paper="vO_2022"
data_per_subj$dataset="van_Opstal_&_Rooyakkers_2022_E1-b"

#recreate data from data_per_subj
data=data.frame()
for (ii in 1:dim(data_per_subj)[1]) {
  subjNum=data_per_subj$subj[ii]
  SR=data_per_subj$SR[ii]
  n=data_per_subj$ntrials[ii]
  R=raw_data$MediumCorr[ii]
  
  #check for each subject SR given is the same as calculated according to R
  compareDataToPaper(R/n,SR,5,"SubjectData")
  
  subj=rep(subjNum,n)
  correct=c(rep(1,R),rep(0,n-R))
  
  datasubj=data.frame(subj,correct)
  data=rbind(data,datasubj)
}
data$paper="vO_2022"
data$dataset="van_Opstal_&_Rooyakkers_2022_E1-b"

data_1_Medium=data
data_per_subj_1_Medium=data_per_subj

############################################################################
#Experiment 1 Large

#create data_per_subj from the raw_data
data_per_subj=data.frame(matrix(nrow = 30 , ncol = 0))
data_per_subj$subj=raw_data$SubjNr
data_per_subj$SR=raw_data$PercentCorrectLarge/100
data_per_subj$ntrials=raw_data$LargeTotal

#Check
if ( all(raw_data$LargeTotal != (raw_data$LargeErr + raw_data$LargeCorr))) {
  stop(paste("Problem with number of trials in Exp 1 Large"))
}

data_per_subj$paper="vO_2022"
data_per_subj$dataset="van_Opstal_&_Rooyakkers_2022_E1-c"

#recreate data from data_per_subj
data=data.frame()
for (ii in 1:dim(data_per_subj)[1]) {
  subjNum=data_per_subj$subj[ii]
  SR=data_per_subj$SR[ii]
  n=data_per_subj$ntrials[ii]
  R=raw_data$LargeCorr[ii]
  
  #check for each subject SR given is the same as calculated according to R
  compareDataToPaper(R/n,SR,5,"SubjectData")
  
  subj=rep(subjNum,n)
  correct=c(rep(1,R),rep(0,n-R))
  
  datasubj=data.frame(subj,correct)
  data=rbind(data,datasubj)
}
data$paper="vO_2022"
data$dataset="van_Opstal_&_Rooyakkers_2022_E1-c"

data_1_Large=data
data_per_subj_1_Large=data_per_subj

#Bind all 3 conditions of exp 1
data_all_1=rbind(data_1_Small,data_1_Medium,data_1_Large)
data_per_subj_all_1=rbind(data_per_subj_1_Small,data_per_subj_1_Medium,data_per_subj_1_Large)

#clean workspace
rm(list=setdiff(ls(),c("data_all_1","data_per_subj_all_1","compareDataToPaper")))

############################################################################
#Experiment  2 Small
#read the data set 
raw_data <- read_excel("./Datasets/VanOpstal/VanOpstal2022_Exp2.xlsx")

#exclude subjects from the unmasked condition
raw_data <- filter(raw_data, Condition == "U") 

#create data_per_subj from the raw_data
data_per_subj=data.frame(matrix(nrow = 20 , ncol = 0))
data_per_subj$subj=raw_data$SubjNr
data_per_subj$SR=raw_data$PercentCorrectSmall/100
data_per_subj$ntrials=raw_data$SmallTotal

#Check
if ( all(raw_data$SmallTotal != (raw_data$SmallErr + raw_data$SmallCorr))) {
  stop(paste("Problem with number of trials in Exp 2 Small"))
}

data_per_subj$paper="vO_2022"
data_per_subj$dataset="van_Opstal_&_Rooyakkers_2022_E2-a"

#recreate data from data_per_subj
data=data.frame()
for (ii in 1:dim(data_per_subj)[1]) {
  subjNum=data_per_subj$subj[ii]
  SR=data_per_subj$SR[ii]
  n=data_per_subj$ntrials[ii]
  R=raw_data$SmallCorr[ii]
  
  #check for each subject SR given is the same as calculated according to R
  compareDataToPaper(R/n,SR,5,"SubjectData")
  
  subj=rep(subjNum,n)
  correct=c(rep(1,R),rep(0,n-R))
  
  datasubj=data.frame(subj,correct)
  data=rbind(data,datasubj)
}
data$paper="vO_2022"
data$dataset="van_Opstal_&_Rooyakkers_2022_E2-a"

data_2_Small=data
data_per_subj_2_Small=data_per_subj

############################################################################
#Experiment 2 Medium

#create data_per_subj from the raw_data
data_per_subj=data.frame(matrix(nrow = 20 , ncol = 0))
data_per_subj$subj=raw_data$SubjNr
data_per_subj$SR=raw_data$PercentCorrectMedium/100
data_per_subj$ntrials=raw_data$MediumTotal

#Check
if ( all(raw_data$MediumTotal != (raw_data$MediumErr + raw_data$MediumCorr))) {
  stop(paste("Problem with number of trials in Exp 2 Medium"))
}

data_per_subj$paper="vO_2022"
data_per_subj$dataset="van_Opstal_&_Rooyakkers_2022_E2-b"

#recreate data from data_per_subj
data=data.frame()
for (ii in 1:dim(data_per_subj)[1]) {
  subjNum=data_per_subj$subj[ii]
  SR=data_per_subj$SR[ii]
  n=data_per_subj$ntrials[ii]
  R=raw_data$MediumCorr[ii]
  
  #check for each subject SR given is the same as calculated according to R
  compareDataToPaper(R/n,SR,5,"SubjectData")
  
  subj=rep(subjNum,n)
  correct=c(rep(1,R),rep(0,n-R))
  
  datasubj=data.frame(subj,correct)
  data=rbind(data,datasubj)
}
data$paper="vO_2022"
data$dataset="van_Opstal_&_Rooyakkers_2022_E2-b"

data_2_Medium=data
data_per_subj_2_Medium=data_per_subj

############################################################################
#Experiment 2 Large

#create data_per_subj from the raw_data
data_per_subj=data.frame(matrix(nrow = 20 , ncol = 0))
data_per_subj$subj=raw_data$SubjNr
data_per_subj$SR=raw_data$PercentCorrectLarge/100
data_per_subj$ntrials=raw_data$LargeTotal

#Check
if ( all(raw_data$LargeTotal != (raw_data$LargeErr + raw_data$LargeCorr))) {
  stop(paste("Problem with number of trials in Exp 2 Large"))
}

data_per_subj$paper="vO_2022"
data_per_subj$dataset="van_Opstal_&_Rooyakkers_2022_E2-c"

#recreate data from data_per_subj
data=data.frame()
for (ii in 1:dim(data_per_subj)[1]) {
  subjNum=data_per_subj$subj[ii]
  SR=data_per_subj$SR[ii]
  n=data_per_subj$ntrials[ii]
  R=raw_data$LargeCorr[ii]
  
  #check for each subject SR given is the same as calculated according to R
  compareDataToPaper(R/n,SR,5,"SubjectData")
  
  subj=rep(subjNum,n)
  correct=c(rep(1,R),rep(0,n-R))
  
  datasubj=data.frame(subj,correct)
  data=rbind(data,datasubj)
}
data$paper="vO_2022"
data$dataset="van_Opstal_&_Rooyakkers_2022_E2-c"

data_2_Large=data
data_per_subj_2_Large=data_per_subj

#Bind all 3 conditions of exp 2
data_all_2=rbind(data_2_Small,data_2_Medium,data_2_Large)
data_per_subj_all_2=rbind(data_per_subj_2_Small,data_per_subj_2_Medium,data_per_subj_2_Large)

data_all=rbind(data_all_1,data_all_2)
data_per_subj_all=rbind(data_per_subj_all_1,data_per_subj_all_2)

#add excluded subjects variable
data_all$excObjTest=0
data_per_subj_all$excObjTest=0

#save data
processed_data <- list(trial_by_trial = data_all, summary_tables = data_per_subj_all)
save(processed_data, file = "./Data/vO_2022.RData")
