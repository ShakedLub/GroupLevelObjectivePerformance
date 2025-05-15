#Load required R packages
library(dplyr)

rm(list=ls()) 

#load processed data
<<<<<<< Updated upstream
load('./Datasets/Biderman_Mudrik_2018/all_Biderman_Mudrik_2018.RData')
=======
load('./DatasetsReanalysis/PapersAndDownloadedData/Biderman_Mudrik_2018/all_Biderman_Mudrik_2018.RData')
>>>>>>> Stashed changes

data=processed_data$trial_by_trial
data_per_subj=processed_data$summary_tables

#create dataset variable from exp variable according to nomenclature
data[data$exp=="Biderman & Mudrik_2018_Orientation_1","dataset"]="Biderman_&_Mudrik_2018_E1ii"
data[data$exp=="Biderman & Mudrik_2018_Congruency_1","dataset"]="Biderman_&_Mudrik_2018_E1i"
data[data$exp=="Biderman & Mudrik_2018_Orientation_2","dataset"]="Biderman_&_Mudrik_2018_E2ii"
data[data$exp=="Biderman & Mudrik_2018_Congruency_2","dataset"]="Biderman_&_Mudrik_2018_E2i"
data[data$exp=="Biderman & Mudrik_2018_Orientation_3","dataset"]="Biderman_&_Mudrik_2018_E3ii"
data[data$exp=="Biderman & Mudrik_2018_Congruency_3","dataset"]="Biderman_&_Mudrik_2018_E3i"

data_per_subj[data_per_subj$exp=="Biderman & Mudrik_2018_Orientation_1","dataset"]="Biderman_&_Mudrik_2018_E1ii"
data_per_subj[data_per_subj$exp=="Biderman & Mudrik_2018_Congruency_1","dataset"]="Biderman_&_Mudrik_2018_E1i"
data_per_subj[data_per_subj$exp=="Biderman & Mudrik_2018_Orientation_2","dataset"]="Biderman_&_Mudrik_2018_E2ii"
data_per_subj[data_per_subj$exp=="Biderman & Mudrik_2018_Congruency_2","dataset"]="Biderman_&_Mudrik_2018_E2i"
data_per_subj[data_per_subj$exp=="Biderman & Mudrik_2018_Orientation_3","dataset"]="Biderman_&_Mudrik_2018_E3ii"
data_per_subj[data_per_subj$exp=="Biderman & Mudrik_2018_Congruency_3","dataset"]="Biderman_&_Mudrik_2018_E3i"

data_per_subj=subset(data_per_subj,select=-exp)
data=subset(data,select=-exp)

#create paper variable
data$paper="B_2018"
data_per_subj$paper="B_2018"

#add column excObjTest
numSubj=unique(data_per_subj$subj)
for (ii in 1:length(numSubj)) {
  indSubj=data_per_subj$subj==numSubj[ii]
  SRSubj=data_per_subj$SR[indSubj]
  if (length(SRSubj) != 2) {
    stop(paste('One subject appears more or less than 2 times'))
  } 
  exc=ifelse(any(SRSubj>0.65),1,0)
  
  data_per_subj[indSubj,"excObjTest"]=exc
  
  indSubj=data$subj==numSubj[ii]
  data[indSubj,"excObjTest"]=exc
}

#save data
rm(processed_data)
processed_data <- list(trial_by_trial = data, summary_tables = data_per_subj)
save(processed_data, file = "./DatasetsReanalysis/Data/B_2018.RData")