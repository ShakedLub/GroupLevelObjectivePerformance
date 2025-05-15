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
#Experiment 1

#read the data set 
raw_data <- read_excel("./Datasets/VanOpstal/VanOpstal2010_Exp1.xlsx")

#create data_per_subj from the raw_data
data_per_subj=data.frame(matrix(nrow = 22 , ncol = 0))
data_per_subj$subj=raw_data$Subject
data_per_subj$SR=raw_data$correct/100
# 0 or 1 is if subject is wrong or correct, Same or Diff is real prime condition
data_per_subj$ntrials=raw_data$Same0+raw_data$Same1+raw_data$Diff0+raw_data$Diff1
data_per_subj$paper="vO_2010"
data_per_subj$dataset="van_Opstal_et_al_2010_E1"

#recreate data from data_per_subj
data=data.frame()
for (ii in 1:dim(data_per_subj)[1]) {
  subjNum=data_per_subj$subj[ii]
  SR=data_per_subj$SR[ii]
  n=data_per_subj$ntrials[ii]
  R=raw_data$Same1[ii]+raw_data$Diff1[ii]
  
  #check for each subject SR given is the same as calculated according to R
  compareDataToPaper(R/n,SR,5,"SubjectData")
  
  subj=rep(subjNum,n)
  correct=c(rep(1,R),rep(0,n-R))
  
  datasubj=data.frame(subj,correct)
  data=rbind(data,datasubj)
}
data$paper="vO_2010"
data$dataset="van_Opstal_et_al_2010_E1"

data_1=data
data_per_subj_1=data_per_subj

############################################################################
#Experiment 2

#clean workspace
rm(list=setdiff(ls(),c("data_1","data_per_subj_1","compareDataToPaper")))

#read the data set 
raw_data <- read_excel("./Datasets/VanOpstal/VanOpstal2010_Exp2.xlsx")

#create data_per_subj from the raw_data
data_per_subj=data.frame(matrix(nrow = 21 , ncol = 0))
data_per_subj$subj=raw_data$Subject
data_per_subj$SR=raw_data$correct/100
# 0 or 1 is if subject is wrong or correct, Same or Diff is real prime condition
data_per_subj$ntrials=raw_data$Same0+raw_data$Same1+raw_data$Diff0+raw_data$Diff1
data_per_subj$paper="vO_2010"
data_per_subj$dataset="van_Opstal_et_al_2010_E2"

#recreate data from data_per_subj
data=data.frame()
for (ii in 1:dim(data_per_subj)[1]) {
  subjNum=data_per_subj$subj[ii]
  SR=data_per_subj$SR[ii]
  n=data_per_subj$ntrials[ii]
  R=raw_data$Same1[ii]+raw_data$Diff1[ii]
  
  #check for each subject SR given is the same as calculated according to R
  compareDataToPaper(R/n,SR,5,"SubjectData")  
  
  subj=rep(subjNum,n)
  correct=c(rep(1,R),rep(0,n-R))
  
  datasubj=data.frame(subj,correct)
  data=rbind(data,datasubj)
}
data$paper="vO_2010"
data$dataset="van_Opstal_et_al_2010_E2"

data_2=data
data_per_subj_2=data_per_subj

############################################################################
#Experiment 3

#clean workspace
rm(list=setdiff(ls(),c("data_1","data_per_subj_1","data_2","data_per_subj_2","compareDataToPaper")))

#read the data set 
raw_data <- read_excel("./Datasets/VanOpstal/VanOpstal2010_Exp3.xlsx")

#create data_per_subj from the raw_data
data_per_subj=data.frame(matrix(nrow = 16 , ncol = 0))
data_per_subj$subj=1:dim(raw_data)[1]
data_per_subj$SR=raw_data$correct/100
# 0 or 1 is if subject is wrong or correct, Same or Diff is real prime condition
data_per_subj$ntrials=raw_data$Same0+raw_data$Same1+raw_data$Diff0+raw_data$Diff1
data_per_subj$paper="vO_2010"
data_per_subj$dataset="van_Opstal_et_al_2010_E3"

#recreate data from data_per_subj
data=data.frame()
for (ii in 1:dim(data_per_subj)[1]) {
  subjNum=data_per_subj$subj[ii]
  SR=data_per_subj$SR[ii]
  n=data_per_subj$ntrials[ii]
  R=raw_data$Same1[ii]+raw_data$Diff1[ii]
  
  #check for each subject SR given is the same as calculated according to R
  compareDataToPaper(R/n,SR,5,"SubjectData")  
  
  subj=rep(subjNum,n)
  correct=c(rep(1,R),rep(0,n-R))
  
  datasubj=data.frame(subj,correct)
  data=rbind(data,datasubj)
}
data$paper="vO_2010"
data$dataset="van_Opstal_et_al_2010_E3"

data_3=data
data_per_subj_3=data_per_subj

data_all=rbind(data_1,data_2,data_3)
data_per_subj_all=rbind(data_per_subj_1,data_per_subj_2,data_per_subj_3)

#add excluded subjects variable
data_all$excObjTest=0
data_per_subj_all$excObjTest=0

#save data
processed_data <- list(trial_by_trial = data_all, summary_tables = data_per_subj_all)
save(processed_data, file = "./Data/vO_2010.RData")
