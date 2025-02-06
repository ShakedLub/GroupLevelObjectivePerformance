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
raw_data <- read_excel("./PapersAndDownloadedData/VanOpstal/VanOpstal2011_Exp1_rapid.xlsx")

#create data_per_subj from the raw_data
data_per_subj=data.frame(matrix(nrow = 22 , ncol = 0))
data_per_subj$subj=raw_data$Subject
data_per_subj$SR=raw_data$SR/100
# 0 or 1 is if subject is wrong or correct, large or small is real prime result
data_per_subj$ntrials=raw_data$large0+raw_data$large1+raw_data$small0+raw_data$small1
data_per_subj$paper="vO_2011_p1"
data_per_subj$dataset="van_Opstal_et_al_2011a_E1"

#recreate data from data_per_subj
data=data.frame()
for (ii in 1:dim(data_per_subj)[1]) {
  subjNum=data_per_subj$subj[ii]
  SR=data_per_subj$SR[ii]
  n=data_per_subj$ntrials[ii]
  R=raw_data$large1[ii]+raw_data$small1[ii]
  
  #check for each subject SR given is the same as calculated according to R
  compareDataToPaper(R/n,SR,5,"SubjectData")
  
  subj=rep(subjNum,n)
  correct=c(rep(1,R),rep(0,n-R))
  
  datasubj=data.frame(subj,correct)
  data=rbind(data,datasubj)
}
data$paper="vO_2011_p1"
data$dataset="van_Opstal_et_al_2011a_E1"

data_1=data
data_per_subj_1=data_per_subj

############################################################################
#Experiment 2

#clean workspace
rm(list=setdiff(ls(),c("data_1","data_per_subj_1","compareDataToPaper")))

#read the data set 
raw_data <- read_excel("./PapersAndDownloadedData/VanOpstal/VanOpstal2011_Exp2_rapid.xlsx")

#create data_per_subj from the raw_data
data_per_subj=data.frame(matrix(nrow = 24 , ncol = 0))
data_per_subj$subj=raw_data$Subject
data_per_subj$SR=raw_data$SR/100
# 0 or 1 is if subject is wrong or correct, large or small is real prime result
data_per_subj$ntrials=raw_data$large0+raw_data$large1+raw_data$small0+raw_data$small1
data_per_subj$paper="vO_2011_p1"
data_per_subj$dataset="van_Opstal_et_al_2011a_E2"

#recreate data from data_per_subj
data=data.frame()
for (ii in 1:dim(data_per_subj)[1]) {
  subjNum=data_per_subj$subj[ii]
  SR=data_per_subj$SR[ii]
  n=data_per_subj$ntrials[ii]
  R=raw_data$large1[ii]+raw_data$small1[ii]
  
  #check for each subject SR given is the same as calculated according to R
  compareDataToPaper(R/n,SR,5,"SubjectData")  
  
  subj=rep(subjNum,n)
  correct=c(rep(1,R),rep(0,n-R))
  
  datasubj=data.frame(subj,correct)
  data=rbind(data,datasubj)
}
data$paper="vO_2011_p1"
data$dataset="van_Opstal_et_al_2011a_E2"

data_2=data
data_per_subj_2=data_per_subj

data_all=rbind(data_1,data_2)
data_per_subj_all=rbind(data_per_subj_1,data_per_subj_2)

#add excluded subjects variable
data_all$excObjTest=0
data_per_subj_all$excObjTest=0

#save data
processed_data <- list(trial_by_trial = data_all, summary_tables = data_per_subj_all)
save(processed_data, file = "./Data/vO_2011_p1.RData")
