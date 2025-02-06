LoadDataSets<- function () {
  # load all datasets to one dataframe
  
  #initialize dataframes
  data_all=data.frame()
  data_per_subj_all=data.frame()

  #get file names
  fileNames=Sys.glob("./Data/*.RData")

  for (ii in 1:length(fileNames)) { #papers
    #load datasets from one paper
    load(fileNames[ii])
    data=processed_data$trial_by_trial
    data_per_subj=processed_data$summary_tables
    rm(processed_data)
    
    #Organize data
    data=subset(data,select=c("subj","correct","paper","dataset","excObjTest"))
    data_per_subj=subset(data_per_subj,select=c("subj","SR","ntrials","paper","dataset","excObjTest"))
    data$subj=as.character(data$subj)
    data_per_subj$subj=as.character(data_per_subj$subj)
    
    #combine all datasets to one dataframe
    data_all=rbind(data_all,data)
    data_per_subj_all=rbind(data_per_subj_all,data_per_subj) 
    
    rm(data,data_per_subj)
  }
  
  processed_data_all <- list(trial_by_trial = data_all, summary_tables = data_per_subj_all)
  return(processed_data_all)
}
                                    
