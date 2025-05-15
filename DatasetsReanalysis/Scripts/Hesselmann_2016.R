
#Load required R packages
library(dplyr)

rm(list=ls()) 

#parameters
chance=0.5
alpha=0.05

#load processed data
load('./DatasetsReanalysis/Datasets/Hesselman_etal_2016/all_Hesselman_etal_2016.RData')

data=processed_data$trial_by_trial
data_per_subj=processed_data$summary_tables

#create dataset variable from exp variable according to nomenclature
data[data$exp=="Hesselmann_2016_1_Category","dataset"]="Hesselmann_et_al_2016_E1"
data[data$exp=="Hesselmann_2016_2_Category","dataset"]="Hesselmann_et_al_2016_E2-a"
data[data$exp=="Hesselmann_2016_2_Shape","dataset"]="Hesselmann_et_al_2016_E2-b"
data_per_subj[data_per_subj$exp=="Hesselmann_2016_1_Category","dataset"]="Hesselmann_et_al_2016_E1"
data_per_subj[data_per_subj$exp=="Hesselmann_2016_2_Category","dataset"]="Hesselmann_et_al_2016_E2-a"
data_per_subj[data_per_subj$exp=="Hesselmann_2016_2_Shape","dataset"]="Hesselmann_et_al_2016_E2-b"

data_per_subj=subset(data_per_subj,select=-exp)
data=subset(data,select=-exp)

#create paper variable
data$paper="H_2016"
data_per_subj$paper="H_2016"

#add column excObjTest to data_per_subj
#exp 1 
data_per_subj_1 <- filter(data_per_subj, dataset == "Hesselmann_et_al_2016_E1")
data_1 <- filter(data, dataset == "Hesselmann_et_al_2016_E1")
R=data_per_subj_1$SR*data_per_subj_1$ntrials
pbinomEachSubj=1-pbinom(q=R,size=data_per_subj_1$ntrials,prob=chance)
data_per_subj_1$excObjTest=ifelse(pbinomEachSubj<alpha,1,0)

numSubj=data_per_subj_1$subj
for (ii in 1:length(numSubj)) {
  exc=data_per_subj_1$excObjTest[ii]
  indSubj=data_1$subj==numSubj[ii]
  data_1[indSubj,"excObjTest"]=exc
}

#exp 2
data_per_subj_2 <- filter(data_per_subj, dataset == "Hesselmann_et_al_2016_E2-a" | dataset == "Hesselmann_et_al_2016_E2-b")
data_2 <- filter(data, dataset == "Hesselmann_et_al_2016_E2-a" | dataset == "Hesselmann_et_al_2016_E2-b")

numSubj=unique(data_per_subj_2$subj)
for (ii in 1:length(numSubj)) {
  indSubj=data_per_subj_2$subj==numSubj[ii]
  
  SRSubj=data_per_subj_2$SR[indSubj]
  nSubj=data_per_subj_2$ntrials[indSubj]
  
  R=SRSubj*nSubj
  pbinomOneSubj=1-pbinom(q=R,size=nSubj,prob=chance)
  exc=ifelse(any(pbinomOneSubj<(alpha/2)),1,0)
  
  data_per_subj_2[indSubj,"excObjTest"]=exc
  
  indSubj=data_2$subj==numSubj[ii]
  data_2[indSubj,"excObjTest"]=exc
}

#combine experiments together
data_per_subj=rbind(data_per_subj_1,data_per_subj_2)
data=rbind(data_1,data_2)

#save data 
rm(processed_data)
processed_data <- list(trial_by_trial = data, summary_tables = data_per_subj)
save(processed_data, file = "./DatasetsReanalysis/Data/H_2016.RData")


