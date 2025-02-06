rm(list=ls()) 

############ Experiment 3
#load datasets
SVP3_data<-read_excel("./PapersAndDownloadedData/Stein_Van_Peelen_2020/Stein_Van_Peelen_2020_exp_3_rawdata/SVP3_data.xlsx",col_names = FALSE) 
#data:
#1- subj number
#2- presentation time (1-5)
#3- target present(1), target absent(2)
#4- upright(1), inverted(2)
#5- localization correct
#6- discrimination correct 
#7- PAS 
names(SVP3_data)<-c("subj","presentationTime","TargetAppear","Orientation","localizationCorrect","correct","PAS")
SVP3_data=as.data.frame(SVP3_data)

SVP3_data_per_subj<-read_excel("./PapersAndDownloadedData/Stein_Van_Peelen_2020/Stein_Van_Peelen_2020_exp_3_rawdata/SVP3_data_per_subj.xlsx",col_names = FALSE)
#data_per_subj:
#1- subj number
#2- presentation time
#4- SR
#5- ntrials
names(SVP3_data_per_subj)<-c("subj","presentationTime","SR","ntrials")
SVP3_data_per_subj=as.data.frame(SVP3_data_per_subj)

SVP3_data$paper="SVP_2020"
SVP3_data_per_subj$paper="SVP_2020"
vecLetters=c("a","b","c","d","e")
vecPresentationTimes=sort(unique(SVP3_data_per_subj$presentationTime))
for (ii in 1:length(vecPresentationTimes)){
  SVP3_data$dataset[SVP3_data$presentationTime == vecPresentationTimes[ii]]=paste("Stein_&_van_Peelen_2020_E3-",vecLetters[ii],sep="")
  SVP3_data_per_subj$dataset[SVP3_data_per_subj$presentationTime == vecPresentationTimes[ii]]=paste("Stein_&_van_Peelen_2020_E3-",vecLetters[ii],sep="") 
}

############ Experiment 4a
#load datasets
SVP4a_data<-read_excel("./PapersAndDownloadedData/Stein_Van_Peelen_2020/Stein_Van_Peelen_2020_exp_4a_rawdata/SVP4a_data.xlsx",col_names = FALSE) 
#data:
#1- subj number
#2- presentation time (1-4)
#3- valid(1), invalid(2)
#4- localization correct
#5- discrimination correct 
names(SVP4a_data)<-c("subj","presentationTime","cueValidity","localizationCorrect","correct")
SVP4a_data=as.data.frame(SVP4a_data)

SVP4a_data_per_subj<-read_excel("./PapersAndDownloadedData/Stein_Van_Peelen_2020/Stein_Van_Peelen_2020_exp_4a_rawdata/SVP4a_data_per_subj.xlsx",col_names = FALSE)
#data_per_subj:
#1- subj number
#2- presentation time
#4- SR
#5- ntrials
names(SVP4a_data_per_subj)<-c("subj","presentationTime","SR","ntrials")
SVP4a_data_per_subj=as.data.frame(SVP4a_data_per_subj)

SVP4a_data$paper="SVP_2020"
SVP4a_data_per_subj$paper="SVP_2020"

vecPresentationTimes=sort(unique(SVP4a_data_per_subj$presentationTime))
for (ii in 1:length(vecPresentationTimes)) {
  SVP4a_data$dataset[SVP4a_data$presentationTime == vecPresentationTimes[ii]]=paste("Stein_&_van_Peelen_2020_E4A-",vecLetters[ii],sep="")
  SVP4a_data_per_subj$dataset[SVP4a_data_per_subj$presentationTime == vecPresentationTimes[ii]]=paste("Stein_&_van_Peelen_2020_E4A-",vecLetters[ii],sep="") 
}

############ Experiment 4b
SVP4b_data<-read_excel("./PapersAndDownloadedData/Stein_Van_Peelen_2020/Stein_Van_Peelen_2020_exp_4b_rawdata/SVP4b_data.xlsx",col_names = FALSE) 
#load datasets
#data:
#1- subj number
#2- presentation time (1-4)
#3- valid(1), invalid(2)
#4- localization correct
#5- discrimination correct 
names(SVP4b_data)<-c("subj","presentationTime","cueValidity","localizationCorrect","correct")
SVP4b_data=as.data.frame(SVP4b_data)

SVP4b_data_per_subj<-read_excel("./PapersAndDownloadedData/Stein_Van_Peelen_2020/Stein_Van_Peelen_2020_exp_4b_rawdata/SVP4b_data_per_subj.xlsx",col_names = FALSE)
#data_per_subj:
#1- subj number
#2- presentation time
#4- SR
#5- ntrials
names(SVP4b_data_per_subj)<-c("subj","presentationTime","SR","ntrials")
SVP4b_data_per_subj=as.data.frame(SVP4b_data_per_subj)

SVP4b_data$paper="SVP_2020"
SVP4b_data_per_subj$paper="SVP_2020"

vecPresentationTimes=sort(unique(SVP4b_data_per_subj$presentationTime))
for (ii in 1:length(vecPresentationTimes)){
  SVP4b_data$dataset[SVP4b_data$presentationTime == vecPresentationTimes[ii]]=paste("Stein_&_van_Peelen_2020_E4B-",vecLetters[ii],sep="")
  SVP4b_data_per_subj$dataset[SVP4b_data_per_subj$presentationTime == vecPresentationTimes[ii]]=paste("Stein_&_van_Peelen_2020_E4B-",vecLetters[ii],sep="")
}

#concatenate data
SVP3_data=subset(SVP3_data,select=c("subj","correct","paper","dataset"))
SVP4a_data=subset(SVP4a_data,select=c("subj","correct","paper","dataset"))
SVP4b_data=subset(SVP4b_data,select=c("subj","correct","paper","dataset"))

data=rbind(SVP3_data,SVP4a_data,SVP4b_data)
data_per_subj=rbind(SVP3_data_per_subj,SVP4a_data_per_subj,SVP4b_data_per_subj)

#add excluded subjects variable
data$excObjTest=0
data_per_subj$excObjTest=0

#save data
processed_data <- list(trial_by_trial = data, summary_tables = data_per_subj)
save(processed_data, file = "./Data/SVP_2020.RData")
