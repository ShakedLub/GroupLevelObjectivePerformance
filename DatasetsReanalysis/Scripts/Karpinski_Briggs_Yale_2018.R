library(dplyr)
library(stringi)
library(stringr)
library(genTS)
library(readxl)

## Startup:
rm(list=ls()) 

#read the data set 
data_per_subj <- read.csv("./DatasetsReanalysis/Datasets/Karpinski_Briggs_Yale_2018/Karp.csv")

#exclude subjects that are aware

# The "awareness" variable indicates general exclusion categories:
# 0 = excluded (1120 and 1027 did not follow instructions based on experimenter notes, 1173 and 1176 had low performance)
# 1 = completely unaware (this is the sample we analyzed)
# 2 = objective aware only (indicated awareness on objective check, but not through debriefing)
# 3 = subjective aware only (did not indicate awareness on objective check, but did through debriefing)
# 4 = completely aware (objective and subjective)
data_per_subj <- filter(data_per_subj, awareness == 1 | awareness == 2) #data check trials

#check number of subjects is correct (unaware and objective aware)
if (dim(data_per_subj)[1]!= 147) {
  paste('Wrong number of subjects found')
}
#check number of subjects is correct (unaware)
d_check <- filter(data_per_subj, awareness == 1)
if (dim(d_check)[1]!= 94) {
  stop(paste('Wrong number of subjects found'))
}

colnames(data_per_subj)[which(names(data_per_subj)=="id")]="subj"
colnames(data_per_subj)[which(names(data_per_subj)=="aware")]="SR"

data_per_subj$excObjTest=ifelse(data_per_subj$awareness==2,1,0)

#create data dataframe and count number of trials for each subject
#The awareness codes are recorded in column P, rows 210-273.

#1= accurately identified the suppressed stimulus (e.g. said "letters" 
#when the primed equation was made of letters: a+b+c)  
#0=incorrectly identified the stimulus

#parameters
int_rows=210:273-1 #in the excel and csv tables the first row is a title, and in R first row is already data
int_col_name="...16"
numT=64

#get name of files that are according to subject name and which kind of file they are
cond_12_filenames=list.files(path="./DatasetsReanalysis/Datasets/Karpinski_Briggs_Yale_2018/Karpinski_Briggs_Yale_2018Data/UnconsciousArithmeticData/Conditions12")
cond_34_filenames=list.files(path="./DatasetsReanalysis/Datasets/Karpinski_Briggs_Yale_2018/Karpinski_Briggs_Yale_2018Data/UnconsciousArithmeticData/Conditions34")

for (ii in 1:dim(data_per_subj)[1]) { #subjects
  subjNum=data_per_subj[ii,"subj"]
  SR=data_per_subj[ii,"SR"]
  cond=data_per_subj[ii,"cond"]
  exc=data_per_subj[ii,"excObjTest"]
  
  if (cond == 1 | cond == 2) { #cond 1 2
    fileInd=which(str_detect(cond_12_filenames,as.character(subjNum)))
    fileName=cond_12_filenames[fileInd]
    path="./DatasetsReanalysis/Datasets/Karpinski_Briggs_Yale_2018/Karpinski_Briggs_Yale_2018Data/UnconsciousArithmeticData/Conditions12/"
  } else if (cond == 3 | cond == 4) { #cond 3 4
    fileInd=which(str_detect(cond_34_filenames,as.character(subjNum)))
    fileName=cond_34_filenames[fileInd]
    path="./DatasetsReanalysis/Datasets/Karpinski_Briggs_Yale_2018/Karpinski_Briggs_Yale_2018Data/UnconsciousArithmeticData/Conditions34/"
  }
  
  if(!is_empty(fileInd)) {
    if (str_detect(fileName,'xls')) {
      datasubj=read_excel(paste(path,fileName,sep=""))
      colnames(datasubj)[which(names(datasubj)==int_col_name)]="correct"
    } else if (str_detect(fileName,'csv')) {
      datasubj=read.csv(paste(path,fileName,sep=""))
      colnames(datasubj)[which(names(datasubj)=="X")]="correct"
    }
  } else { #recreate the data
    Subj=rep(subjNum,numT)
    R=SR*numT
    #check there are no missing trials for the subject
    if (abs(R-round(R))!= 0) {
      stop(paste("Subject with missing data has missing trials ",subjNum))
    }
    correct=c(rep(1,R),rep(0,numT-R))
    datasubj=data.frame(Subj,correct)
  }
    
  colnames(datasubj)[which(names(datasubj)=="Subj")]="subj"
  if(!is_empty(fileInd)) {
    int_data=datasubj[int_rows,c("subj","correct")]
  } else { #recreated data
    int_data=datasubj
  }
    
  #exclude missing values
  int_data=na.omit(int_data)
  
  #subject 1137 has a wrong number in his data 10 in trial 5. I replace it with 1
  if (subjNum==1137) {
    #correct mistake in data
    ind=which(int_data$correct!=1 & int_data$correct!=0)
    int_data[ind,"correct"]=1
    
    #update SR according to the correction
    data_per_subj[ii,"SR"]=mean(int_data$correct)
    SR=data_per_subj[ii,"SR"]
    
    #update if this subj is objecvtive aware according to the correction
    data_per_subj[ii,"excObjTest"]=SR>0.6
    exc=data_per_subj[ii,"excObjTest"]
  }
  
  #check this is the correct dataset
  if (round(mean(int_data$correct),6) != round(SR,6)) {
    stop(paste("Wrong data subj ",subjNum))
  }
  
  #add if this subject is excluded due to objective awareness 
  int_data$excObjTest=exc
  
  #create data dataframe
  if (ii==1) {
    data=int_data
  } else {
    data=rbind(data,int_data)
  }
  
  #count number of trials included in awareness check
  data_per_subj[ii,"ntrials"]=dim(int_data)[1]
}

data$paper="KBY_2018"
data$dataset="Karpinski_et_al_2018"

data_per_subj$paper="KBY_2018"
data_per_subj$dataset="Karpinski_et_al_2018"

#save the data
processed_data <- list(trial_by_trial = data, summary_tables = data_per_subj)
save(processed_data, file = "./DatasetsReanalysis/Data/KBY_2018.RData")
