plotAwareness<-function(result_awareness) {
  #include only experiments that don't have the same result in the 4 tests
  result_awareness_misMatch=filter(result_awareness, match == FALSE)
  
  #subset the data to include only relevant columns
  result_awareness_new <- subset(result_awareness_misMatch, select = c(DatasetName,Pval_t_test,bf_tBayes_test,Pval_Chi_test,Pval_LMM_test))
  
  #change column names
  colnames(result_awareness_new)[which(names(result_awareness_new)=="Pval_t_test")]="T"
  colnames(result_awareness_new)[which(names(result_awareness_new)=="bf_tBayes_test")]="TBayes"
  colnames(result_awareness_new)[which(names(result_awareness_new)=="Pval_Chi_test")]="Chi"
  colnames(result_awareness_new)[which(names(result_awareness_new)=="Pval_LMM_test")]="LMM"
  
  #change the data from wide format to long format
  result_awareness_long <- gather(result_awareness_new, testType, pvalue, "T":"LMM", factor_key=TRUE)
  
  #take out extreme results
  result_awareness_long$pvalue[result_awareness_long$pvalue<0.0001]=0
  result_awareness_long$pvalue[result_awareness_long$pvalue>200]=200
  
  result_awareness_long$testType=factor(result_awareness_long$testType,levels=c("Chi","LMM","TBayes","T"))
  
  # Create data for lines
  data_hline= data.frame(testType = c("Chi","LMM","TBayes","T"),hline = c(0.05,0.05,3,0.05))

  #plot data
  #figure 1
  ggplot(result_awareness_long, aes(DatasetName, pvalue, fill=testType)) +
    geom_col(position = "dodge") +
    facet_wrap(~testType, ncol = 1, scales = "free_y") +
    theme(legend.position = "none") +
    scale_x_discrete(guide = guide_axis(angle = -60)) +
    #scale_x_discrete(guide = guide_axis(n.dodge = 10)) +
    geom_hline(data = data_hline,aes(yintercept = hline), linetype="dashed", color = "black") +
    ylab("")+
    theme_minimal()
  
  #figure 2
  browser()
  ggplot(result_awareness_long, aes(x=pvalue, y=testType,color=testType, shape=DatasetName)) +
    geom_point(size=3,alpha=0.3,stroke=1)+
    facet_wrap(~testType, ncol = 1, scales = "free_x")
}
