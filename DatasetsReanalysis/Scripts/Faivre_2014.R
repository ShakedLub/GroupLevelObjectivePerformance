#Load required R packages
library(dplyr)

rm(list=ls()) 

#load processed data
<<<<<<< Updated upstream
load('./Datasets/Faivre_et_al_2014/all_Faivre et al._2014.RData')
=======
load('./DatasetsReanalysis/PapersAndDownloadedData/Faivre_et_al_2014/all_Faivre et al._2014.RData')
>>>>>>> Stashed changes

data=processed_data$trial_by_trial
data_per_subj=processed_data$summary_tables

#create dataset variable from exp variable according to nomenclature
data[data$exp=="Faivre et al. 1","dataset"]="Faivre_et_al_2014_E1"
data[data$exp=="Faivre et al. 2","dataset"]="Faivre_et_al_2014_E2"
data[data$exp=="Faivre et al. 3","dataset"]="Faivre_et_al_2014_E3"
data[data$exp=="Faivre et al. 4","dataset"]="Faivre_et_al_2014_E4"
data[data$exp=="Faivre et al. 6","dataset"]="Faivre_et_al_2014_E6"
data[data$exp=="Faivre et al. 7","dataset"]="Faivre_et_al_2014_E7"
data[data$exp=="Faivre et al. 8","dataset"]="Faivre_et_al_2014_E8"

data_per_subj[data_per_subj$exp=="Faivre et al. 1","dataset"]="Faivre_et_al_2014_E1"
data_per_subj[data_per_subj$exp=="Faivre et al. 2","dataset"]="Faivre_et_al_2014_E2"
data_per_subj[data_per_subj$exp=="Faivre et al. 3","dataset"]="Faivre_et_al_2014_E3"
data_per_subj[data_per_subj$exp=="Faivre et al. 4","dataset"]="Faivre_et_al_2014_E4"
data_per_subj[data_per_subj$exp=="Faivre et al. 6","dataset"]="Faivre_et_al_2014_E6"
data_per_subj[data_per_subj$exp=="Faivre et al. 7","dataset"]="Faivre_et_al_2014_E7"
data_per_subj[data_per_subj$exp=="Faivre et al. 8","dataset"]="Faivre_et_al_2014_E8"

data_per_subj=subset(data_per_subj,select=-exp)
data=subset(data,select=-exp)

#create paper variable
data$paper="F_2014"
data_per_subj$paper="F_2014"

#exclude exp 7 as the objective aware subejcts' data is not given
data_per_subj=filter(data_per_subj,dataset != "Faivre_et_al_2014_E7")
data=filter(data,dataset != "Faivre_et_al_2014_E7")

#add column excObjTest
data_per_subj$excObjTest=ifelse(data_per_subj$SR>0.65,1,0)
numSubj=data_per_subj$subj
#check all subjects have unique names
if (length(numSubj) != length(unique(numSubj))) {
  stop(paste("Not all subejcts have unique names"))
}
for (ii in 1:length(numSubj)) {
  exc=data_per_subj$excObjTest[ii]
  indSubj=data$subj==numSubj[ii]
  data[indSubj,"excObjTest"]=exc
}

#save data
rm(processed_data)
processed_data <- list(trial_by_trial = data, summary_tables = data_per_subj)
save(processed_data, file = "./DatasetsReanalysis/Data/F_2014.RData")
