#Create data for group level analysis and save it
library(dplyr)
library(readxl)

############################################################################

#clean workspace
rm(list=ls())

#################################################exp 1
#read the data set 
data <- read_excel("./Datasets/Skora/Skora_et_al_2020_exp_1/Full_long_incl.RT_02.07_100ms_CONTROL ONLY.xlsx")

#exclude conscious trials 
#exclusion based on paper criteria: trials with correct symmetry judgment and with confidence
#data_filtered <- filter(data, awareattrial == 0) #exclude trials with correct symmetry judgment and with confidence
#exclusion based on report
data_filtered <- filter(data, confidence == 0) #exclude trials with confidence

#check percent of excluded trials
data_check <- filter(data, awareattrial == 0) #aware trials = trials with correct symmetry judgment and with confidence
if (round(1-(dim(data_check)[1]/dim(data)[1]),4) != 0.1175 ){
  stop("In Skora the data is not the same as in the paper")  
}

#exclude subj trials
#data_filtered <- filter(data_filtered, subject_exclusions_Control == 0 & subject_exclusions == 0) #subjects excluded taken out
#check number of excluded participants
#data_check <- filter(data, subject_exclusions == 1 | subject_exclusions_Control == 1) #only subjects excluded
#if (length(unique(data_check$subID)) != 10 ){
#  stop("In Skora the data is not the same as in the paper")  
#}

# the number of subject taken out is not as in the paper so I will find the subjects taken out according to the conditions in the paper
data_exclude_subj <- dplyr::summarise(group_by(data,subID),
                                              percentAware = sum(awareattrial)/length(awareattrial), 
                                              numGoResponse = sum(button_press))

subj_exclude<- data_exclude_subj[data_exclude_subj$percentAware > 0.25 | data_exclude_subj$numGoResponse == 0,"subID"]
subj_exclude<-subj_exclude$subID

for (ii in 1:length(subj_exclude)) {
  data_filtered = subset(data_filtered, subID!= subj_exclude[ii]) 
}

#check number of excluded participants
if (length(subj_exclude) != 9 ){
  stop("In Skora the data is not the same as in the paper")  
}

# exclude one more subject who is excluded based on RT analysis
#this is done after trials are excluded based on the paper's criterion for aware trials
data_check <- filter(data, awareattrial == 0) #aware trials = trials with correct symmetry judgment and with confidence
data_exclude_subj <- dplyr::summarise(group_by(data_check,subID),
                                      mRT=mean(RT,na.rm=TRUE),
                                      sdRT=sd(RT,na.rm=TRUE),
                                      numRemoveRT=sum(RT<100 | RT>(mRT+2*sdRT),na.rm=TRUE),
                                      perRemoveRT=sum(RT<100 | RT>(mRT+2*sdRT),na.rm=TRUE)/length(awareattrial))

subj_exclude<- data_exclude_subj[data_exclude_subj$perRemoveRT > 0.25,"subID"]
subj_exclude<-subj_exclude$subID

for (ii in 1:length(subj_exclude)) {
  data_filtered = subset(data_filtered, subID!= subj_exclude[ii]) 
}

#check number of excluded participants
if (length(subj_exclude) != 1 ){
  stop("In Skora the data is not the same as in the paper")  
}

#change column names
colnames(data_filtered)[which(names(data_filtered)=="subID")]="subj"
colnames(data_filtered)[which(names(data_filtered)=="acc_sym")]="correct"

data_filtered$paper="S_2020"
data_filtered$dataset="Skora_et_al_2020_E1"

#calculate data per subj
data_per_subj <- dplyr::summarise(group_by(data_filtered,subj),
                                                   SR = sum(correct)/length(correct), 
                                                   ntrials = length(trial_no))
data_per_subj$paper="S_2020"
data_per_subj$dataset="Skora_et_al_2020_E1"

#################################################exp 2

data_filtered_1=data_filtered
data_per_subj_1=data_per_subj

#clean workspace
rm(list=setdiff(ls(),c("data_filtered_1","data_per_subj_1")))

############################################################################

#read the data set 
data <- read_excel("./Datasets/Skora/Skora_et_al_2020_exp_2/unconscious delay conditioning_Feb2020_full data.xlsx")

#exclude conscious trials
#exclusion based on paper criteria: trials with correct symmetry judgment and with confidence
#data_filtered <- filter(data, trial_aware == 0) #exclude trials with correct symmetry judgment and with confidence
#exclusion based on report
data_filtered <- filter(data, conf == 0) 

#check percent of excluded trials
data_check <- filter(data, trial_aware == 0) #aware trials = trials with correct symmetry judgment and with confidence
if (round(1-(dim(data_check)[1]/dim(data)[1]),3) != 0.318 ){
  stop("In Skora the data is not the same as in the paper")  
}

#exclude subj trials
data_filtered <- filter(data_filtered, exclusions == 0) #subjects excluded taken out
#check number of excluded participants
data_check <- filter(data, exclusions == 1) #only subjects excluded
if (length(unique(data_check$pnum)) != 20 ){
  stop("In Skora the data is not the same as in the paper")  
}

#check percent of excluded trials in included subjects
data_check <- filter(data, exclusions == 0) #subjects excluded taken out
data_check_included_trials <- filter(data_check, trial_aware == 0) #aware trials = trials with correct symmetry judgment and with confidence
if (round(1-(dim(data_check_included_trials)[1]/dim(data_check)[1]),3) != 0.096 ){
  stop("In Skora the data is not the same as in the paper")  
}

#change column names
colnames(data_filtered)[which(names(data_filtered)=="pnum")]="subj"
colnames(data_filtered)[which(names(data_filtered)=="sym")]="correct"

data_filtered$paper="S_2020"
data_filtered$dataset="Skora_et_al_2020_E2"

#calculate data per subj
data_per_subj <- dplyr::summarise(group_by(data_filtered,subj),
                                  SR = sum(correct)/length(correct), 
                                  ntrials = length(trial))

data_per_subj$paper="S_2020"
data_per_subj$dataset="Skora_et_al_2020_E2"

#concatenate data
D2=subset(data_filtered,select=c("subj","block","correct","paper","dataset"))
D1=subset(data_filtered_1,select=c("subj","block","correct","paper","dataset"))
data_filtered=rbind(D1,D2)
data_per_subj=rbind(data_per_subj_1,data_per_subj)

#add excluded subjects variable
data_filtered$excObjTest=0
data_per_subj$excObjTest=0

#save the data
processed_data <- list(trial_by_trial = data_filtered, summary_tables = data_per_subj)
save(processed_data, file = "./Data/S_2020.RData")