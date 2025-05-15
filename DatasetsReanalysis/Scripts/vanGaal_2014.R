library(dplyr)
library(readxl)

#clean workspace
rm(list=ls())

#experiment 4
#read the data set 
raw_data <- read_excel("./DatasetsReanalysis/Datasets/VanGaal/pc_exp4.xlsx",col_names = FALSE)

names(raw_data)<-c("SRunmasked","SR")
raw_data=as.data.frame(raw_data)

#create data_per_subj from the raw_data
subj=1:dim(raw_data)[1]
SR=raw_data$SR/100
ntrials=rep(40,dim(raw_data)[1])
data_per_subj=data.frame(subj,SR,ntrials)
data_per_subj$paper="vG_2014"
data_per_subj$dataset="van_Gaal_et_al_2014_E4"

#recreate data from data_per_subj
data=data.frame()
for (ii in 1:dim(data_per_subj)[1]) {
  subjNum=data_per_subj[ii,"subj"]
  SR=data_per_subj[ii,"SR"]
  n=data_per_subj[ii,"ntrials"]
  
  R=SR*n
  #check there are no missing trials for the subject
  if (abs(R-round(R))!= 0) {
    stop(paste("This subject has wrong number of trials ",subjNum))
  }
  
  subj=rep(subjNum,n)
  correct=c(rep(1,R),rep(0,n-R))
  
  datasubj=data.frame(subj,correct)
  data=rbind(data,datasubj)
}
data$paper="vG_2014"
data$dataset="van_Gaal_et_al_2014_E4"

#add excluded subjects variable
data$excObjTest=0
data_per_subj$excObjTest=0

#save data
processed_data <- list(trial_by_trial = data, summary_tables = data_per_subj)
save(processed_data, file = "./DatasetsReanalysis/Data/vG_2014.RData")
