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
<<<<<<< Updated upstream
raw_data <- read_excel("./Datasets/VanOpstal/VanOpstal2011_Exp1_setting.xlsx")
=======
raw_data <- read_excel("./DatasetsReanalysis/PapersAndDownloadedData/VanOpstal/VanOpstal2011_Exp1_setting.xlsx")
>>>>>>> Stashed changes

#create data_per_subj from the raw_data
data_per_subj=data.frame(matrix(nrow = 19 , ncol = 0))
data_per_subj$subj=raw_data$Subject
data_per_subj$SR=raw_data$SR/100
#one diff or same is real prime result, other diff or same is subject response
#I do not know the order between them but for these calculations it doesn't matter
data_per_subj$ntrials=raw_data$diffdiff+raw_data$diffsame+raw_data$samediff+raw_data$samesame
data_per_subj$paper="vO_2011_p2"
data_per_subj$dataset="van_Opstal_et_al_2011b"

#recreate data from data_per_subj
data=data.frame()
for (ii in 1:dim(data_per_subj)[1]) {
  subjNum=data_per_subj$subj[ii]
  SR=data_per_subj$SR[ii]
  n=data_per_subj$ntrials[ii]
  R=raw_data$diffdiff[ii]+raw_data$samesame[ii]
  
  #check for each subject SR given is the same as calculated according to R
  compareDataToPaper(R/n,SR,5,"SubjectData")
  
  subj=rep(subjNum,n)
  correct=c(rep(1,R),rep(0,n-R))
  
  datasubj=data.frame(subj,correct)
  data=rbind(data,datasubj)
}
data$paper="vO_2011_p2"
data$dataset="van_Opstal_et_al_2011b"

#add excluded subjects variable
data$excObjTest=0
data_per_subj$excObjTest=0

#save data
processed_data <- list(trial_by_trial = data, summary_tables = data_per_subj)
save(processed_data, file = "./DatasetsReanalysis/Data/vO_2011_p2.RData")
