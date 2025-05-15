######################################################################
## Zerweck et al., 2020
## Analysis Experiment 1
##  
##   for the direct task
##  - read data and exclude too fast/slow RT trials
##  - average across trials for each participant
##  - calculate PCorr and dprime
##
######################################################################

## Startup:
rm(list=ls()) 

##################################################################################
##################################################################################
#### Analysis direct task (perctask) ####
##################################################################################
##################################################################################

# direct task
perc <- read.table("./Datasets/Zerweck_et_al_2021/Experiment1/Experiment1_directTask.dat",header=T)

##Remove too fast/slow responses:
perc <- perc[perc$time_error=="Normal",]

## Perform analyses for each participant:
dirResults <- data.frame()
for (part in unique(perc$sub)){
  ##Select this participant's data:
  partData <- perc[perc$sub==part,]
  
  ##Calculate percent correct for this participant:
  partNtrials <- length(partData$key_error)
  partPCorr   <- mean(partData$key_error=="correct")
  
  ## calculate dprime for this participant:
  HR <- mean((partData$key_error=="correct")
            [grepl("[6,9]", partData$prime)])
  FA <- 1 - mean((partData$key_error=="correct")
                [grepl("[1,4]", partData$prime)])
  
  partdprime <- qnorm(HR) - qnorm(FA)
  
  ##Collect results:
  dirResults <- rbind(dirResults,
                      data.frame(subj        = part,
                                 ntrials     = partNtrials,
                                 SR          = partPCorr,
                                 dprime      = partdprime))
}

colnames(perc)[which(names(perc)=="sub")]="subj"
perc$correct<-ifelse(perc$key_error=="correct", 1, 0)

perc$paper="Z_2021"
perc$dataset="Zerweck_et_al_2021_E1"
dirResults$paper="Z_2021"
dirResults$dataset="Zerweck_et_al_2021_E1"

perc_1=perc
dirResults_1=dirResults

######################################################################
## Zerweck et al., 2020
## Analysis Experiment 2
##  
##   for the direct task
##  - read data and exclude too fast/slow RT trials
##  - average across trials for each participant and prime contrast
##  - calculate PCorr and dprime
##
######################################################################

## Startup:
#clean workspace
rm(list=setdiff(ls(),c("perc_1","dirResults_1"))) 

##################################################################################
##################################################################################
#### Analysis direct task (perctask) ####
##################################################################################
##################################################################################

# direct task
perc <- read.table("./Datasets/Zerweck_et_al_2021/Experiment2/Experiment2_directTask.dat",header=T)

##Remove trials with display timing errors:
perc <- perc[perc$TimingError== 0,]

##Exclude practice trials:
perc <- perc[perc$Task== "PercExp",]

##Remove too fast/slow responses:
perc <- perc[perc$ResponseTime=="Normal",]

##Add column with prime contrast values in cd/m:
WD=getwd()
setwd("./Datasets/Zerweck_et_al_2021/Experiment2")
source("local-lib_Exp2.R")
perc <- addColumnWithContrastAsCandela(perc)
setwd(WD)

## perform analysis for each prime contrast and each participant
dirResults = data.frame()
ii=0
LettersVec=c("a","b","c","d","e","f","g","h")
for (primeContrast in sort(unique(perc$contrastInCandela)))
{
  ## Select contrast condition
  ii=ii+1
  partperc = perc[perc$contrastInCandela == primeContrast,]
  perc$dataset[perc$contrastInCandela == primeContrast]=paste("Zerweck_et_al_2021_E2-",LettersVec[ii],sep="")
  
  for(part in unique(partperc$VP))
  {
    ##Select this participant's data:
    partData <- partperc[partperc$VP==part,]
    
    ##Calculate percent correct for this participant:
    partNtrials <- length(partData$ResponseError)
    partPCorr   <- mean(partData$ResponseError=="keypress_correct")
    
    ##calculate dprime for this participant:
    HR <- mean((partData$ResponseError=="keypress_correct")
               [grepl("[6,9]", partData$Prime)])
    FA <- 1 - mean((partData$ResponseError=="keypress_correct")
                   [grepl("[1,4]", partData$Prime)])
    
    ##If HR or FA are 0 or 1, use convention to compute finite d'
    nHR = sum(grepl("[6,9]", partData$Prime)) ## Number of trials for HR calculation
    if (HR == 0) HR = 1/nHR
    if (HR == 1) HR = (nHR-1)/nHR
    nFA = sum(grepl("[1,4]", partData$Prime)) ## Number of trials for FA calculation
    if (FA == 0) FA = 1/nFA
    if (FA == 1) FA = (nFA-1)/nFA
    
    #calculate dprime
    partdprime <- qnorm(HR) - qnorm(FA)
    
    ##Collect results:
    dirResults <- rbind(dirResults,
                        data.frame(subj          = part,
                                   ntrials       = partNtrials,
                                   SR            = partPCorr,
                                   dprime        = partdprime,
                                   dataset       = paste("Zerweck_et_al_2021_E2-",LettersVec[ii],sep="")))
  }
}

colnames(perc)[which(names(perc)=="VP")]="subj"
perc$correct<-ifelse(perc$ResponseError=="keypress_correct", 1, 0)

perc$paper="Z_2021"
dirResults$paper="Z_2021"

perc_2=perc
dirResults_2=dirResults

######################################################################
## Zerweck et al., 2020
## Analysis Experiment 3 : discrimination task
##
##   for the direct task (discrimination direct task)
##  - read data and exclude too fast/slow RT trials
##  - average across trials for each participant 
##  - calculate PCorr and dprime
##
######################################################################

## Startup:
#clean workspace
rm(list=setdiff(ls(),c("perc_1","dirResults_1","perc_2","dirResults_2","LettersVec"))) 

##################################################################################
##################################################################################
#### Analysis direct task (perctask) ####
##################################################################################
##################################################################################

PrimeDurations = c(10,20,30,40,50,60,70,80) 
perc_3 = data.frame()
dirResults_3 = data.frame()
ii=0
for (PrimeDuration in PrimeDurations){
  ## Read data:
  perc <- read.table(paste("./Datasets/Zerweck_et_al_2021/Experiment3/data_Exp3/om1203_percI_",PrimeDuration,".dat",sep=""),header=T)
  
  ##Exclude practice trials:
  perc <- perc[grep(".*Exp.*",perc$block_type),]
  
  ##Remove too fast/slow responses:
  perc <- perc[perc$time_error=="Normal",]
  
  ## perform analysis for each participant
  dirResults = data.frame()
  
  for(part in unique(perc$sub)) {
    ##Select this participant's data:
    partData <- perc[perc$sub==part,]
    
    ##Calculate percent correct for this participant:
    partNtrials <- length(partData$key_error)
    partPCorr   <- mean(partData$key_error=="correct")
    
    ## calculate dprime for this participant:
    HR <- mean((partData$key_error=="correct")
               [grepl("[6,9]", partData$prime)])
    FA <- 1 - mean((partData$key_error=="correct")
                   [grepl("[1,4]", partData$prime)])
    
    partdprime <- qnorm(HR) - qnorm(FA)
    
    ##Collect results:
    dirResults <- rbind(dirResults,
                        data.frame(subj        = part,
                                   ntrials     = partNtrials,
                                   SR          = partPCorr,
                                   dprime      = partdprime))
  }
  
  colnames(perc)[which(names(perc)=="sub")]="subj"
  perc$correct<-ifelse(perc$key_error=="correct", 1, 0)
  
  ii=ii+1
  perc$dataset=paste("Zerweck_et_al_2021_E3i-",LettersVec[ii],sep="")
  dirResults$dataset=paste("Zerweck_et_al_2021_E3i-",LettersVec[ii],sep="")
  perc$paper="Z_2021"
  dirResults$paper="Z_2021"
  
  perc_3=rbind(perc_3,perc)
  dirResults_3=rbind(dirResults_3,dirResults)
}


######################################################################
## Zerweck et al., 2020
## Analysis Experiment 3 : congruency task
##
##   for the direct task (congruency direct task)
##  - read data and exclude too fast/slow RT trials
##  - average across trials for each participant 
##  - calculate PCorr and dprime
##
######################################################################

## Startup:
#clean workspace
rm(list=setdiff(ls(),c("perc_1","dirResults_1","perc_2","dirResults_2","perc_3","dirResults_3","LettersVec"))) 

##################################################################################
##################################################################################
#### Analysis direct task (perctask) ####
##################################################################################
##################################################################################

PrimeDurations = c(10,20,30,40,50,60,70,80) 
perc_4 = data.frame()
dirResults_4 = data.frame()
ii=0
for (PrimeDuration in PrimeDurations){
  ## Read data:
  perc <- read.table(paste("./Datasets/Zerweck_et_al_2021/Experiment3/data_Exp3/om1203_percC_",PrimeDuration,".dat",sep=""),header=T)
  
  ##Exclude practice trials:
  perc <- perc[grep(".*Exp.*",perc$block_type),]
  
  ##Remove too fast/slow responses:
  perc <- perc[perc$time_error=="Normal",]
  
  ## perform analysis for each participant
  dirResults = data.frame()
  
  for(part in unique(perc$sub)) {
    ##Select this participant's data:
    partData <- perc[perc$sub==part,]
    
    ##Calculate percent correct for this participant:
    partNtrials <- length(partData$key_error)
    partPCorr   <- mean(partData$key_error=="correct")
    
    ## calculate dprime for this participant:
    HR <- mean((partData$key_error=="correct")
               [grepl("[6,9]", partData$prime)])
    FA <- 1 - mean((partData$key_error=="correct")
                   [grepl("[1,4]", partData$prime)])
    
    partdprime <- qnorm(HR) - qnorm(FA)
    
    ##Collect results:
    dirResults <- rbind(dirResults,
                        data.frame(subj        = part,
                                   ntrials     = partNtrials,
                                   SR          = partPCorr,
                                   dprime      = partdprime))
  }
  
  colnames(perc)[which(names(perc)=="sub")]="subj"
  perc$correct<-ifelse(perc$key_error=="correct", 1, 0)
  
  ii=ii+1
  perc$dataset=paste("Zerweck_et_al_2021_E3ii-",LettersVec[ii],sep="")
  dirResults$dataset=paste("Zerweck_et_al_2021_E3ii-",LettersVec[ii],sep="")
  perc$paper="Z_2021"
  dirResults$paper="Z_2021"
  
  perc_4=rbind(perc_4,perc)
  dirResults_4=rbind(dirResults_4,dirResults)
}

#concatenate data
D1=subset(perc_1,select=c("subj","correct","paper","dataset"))
D2=subset(perc_2,select=c("subj","correct","paper","dataset"))
D3=subset(perc_3,select=c("subj","correct","paper","dataset"))
D4=subset(perc_4,select=c("subj","correct","paper","dataset"))
dirResults_final=rbind(dirResults_1,dirResults_2,dirResults_3,dirResults_4)
perc_final=rbind(D1,D2,D3,D4)

#add excluded subjects variable
dirResults_final$excObjTest=0
perc_final$excObjTest=0

#save the data
processed_data <- list(trial_by_trial = perc_final, summary_tables = dirResults_final)
save(processed_data, file = "./Data/Z_2021.RData")