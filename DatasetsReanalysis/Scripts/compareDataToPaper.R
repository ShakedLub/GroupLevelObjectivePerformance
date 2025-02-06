compareDataToPaper<-function(realdata,paper,decimal,datasetName) {
  if (round(realdata,decimal)!=paper) {
    print(paste("In ",datasetName," the data is not the same as in the paper. real:",as.character(realdata)," paper:",as.character(paper)))
  }
}