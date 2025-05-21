GenerateAllPlots <- function(param, results_preprocessed, results_preprocessed_long) {
  #################### figure 1 mismatch plot
  #include only experiments that don't have the same result in the chosen tests
  results_preprocessed_long_misMatch=filter(results_preprocessed_long, match == FALSE)
  
  #change test to be a factor
  results_preprocessed_long_misMatch$test=as.factor(results_preprocessed_long_misMatch$test)
  
  #change order of test_type levels
  results_preprocessed_long_misMatch$test_type=factor(results_preprocessed_long_misMatch$test_type,levels=c("Frequentist","Bayesian"))
  
  #present only the wanted tests
  if (param$incSubjObjTest==1) {
    results_preprocessed_long_misMatch=filter(results_preprocessed_long_misMatch, test != "GB" & test != "Chi")
  }
  
  # Create data for horizontal lines
  data_hline1= data.frame(test_type = unique(results_preprocessed_long_misMatch$test_type),hline = c(log10(param$alpha),NA))
  data_hline2= data.frame(test_type = unique(results_preprocessed_long_misMatch$test_type),hline = c(NA,log10(1/param$bfThreshold)))
  
  # Create color data for x axis text
  if (param$incSubjObjTest==1) {
    colourdata=rep("black",1,length(unique(results_preprocessed_long_misMatch$dataset)))
    datasetNamesMismatch=unique(results_preprocessed_long_misMatch$dataset)
    for (col_ind in 1:length(colourdata)){
      ds=datasetNamesMismatch[col_ind]
      ind=which(results_preprocessed$dataset==ds)
      if (results_preprocessed$h_t_test[ind]==0 & results_preprocessed$h_tBayes_test[ind]==0 & results_preprocessed$h_MMLR_test[ind]==0 & results_preprocessed$h_GBC_test[ind]==1 & results_preprocessed$h_GBBayes_test[ind]==1) {
        colourdata[col_ind]="red"
      }  else if (results_preprocessed$h_t_test[ind]==0 & results_preprocessed$h_tBayes_test[ind]==0 & results_preprocessed$h_MMLR_test[ind]==1 & results_preprocessed$h_GBC_test[ind]==1 & results_preprocessed$h_GBBayes_test[ind]==1) {
        colourdata[col_ind]="salmon2"
      }  else if (results_preprocessed$h_t_test[ind]==1 & results_preprocessed$h_tBayes_test[ind]==0 & results_preprocessed$h_MMLR_test[ind]==1 & results_preprocessed$h_GBC_test[ind]==0 & results_preprocessed$h_GBBayes_test[ind]==0) {
        colourdata[col_ind]="honeydew4"
      }  else if (results_preprocessed$h_t_test[ind]==1 & results_preprocessed$h_tBayes_test[ind]==0 & results_preprocessed$h_MMLR_test[ind]==1 & results_preprocessed$h_GBC_test[ind]==1 & results_preprocessed$h_GBBayes_test[ind]==0) {
        colourdata[col_ind]="azure3"
      }
    }
  }
  
  #create a vector with locations of vertical lines between datasets
  vecLines=1:(length(unique(results_preprocessed_long_misMatch$dataset))-1)+0.5
  
  #change dataset names to have space instead of underscore
  results_preprocessed_long_misMatch$dataset = gsub('_',' ',results_preprocessed_long_misMatch$dataset)
  
  if (param$incSubjObjTest==1) {
    f1=ggplot(results_preprocessed_long_misMatch, aes(x=dataset, y=log.pvalue, fill=test)) +
      geom_bar(alpha=0.5, stat="identity",position = position_dodge())+
      scale_x_discrete(guide = guide_axis(angle = -60)) +
      geom_hline(data = data_hline1,aes(yintercept = hline), linetype="dashed", color = "black") +
      geom_hline(data = data_hline2,aes(yintercept = hline), linetype="dashed", color = "black") +
      geom_vline(xintercept = vecLines, color = "azure2") +
      theme_minimal() +
      xlab('Tested effect') +
      ylab('') +
      facet_wrap(~test_type, nrow = 2, 
                 scales = "free_y",
                 strip.position = "left", 
                 labeller = as_labeller(c(Frequentist = "log10(pvalue)", Bayesian= "log10(BF01)")))+
      scale_fill_manual(values=c("red","darkorange2","cyan4","purple","blue"),name="")+
      theme(panel.background = element_blank(),
            axis.title.y = element_text(size = 12),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 12),
            axis.text.x =element_text(size = 10, colour= colourdata),
            axis.line.y = element_line(),
            legend.text = element_text(size = 10),
            strip.text = element_text(size = 10),
            legend.title = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            strip.placement = "outside")
    
    ggsave('./Output/MisMatchMain.jpg',f1)
    
  } else {
    
    f1=ggplot(results_preprocessed_long_misMatch, aes(x=dataset, y=log.pvalue, fill=test)) +
      geom_bar(alpha=0.5,stat="identity",position = position_dodge())+
      scale_fill_manual(values=c("palegreen4","bisque3","skyblue4","darkorange2","cyan4"),name="")+
      geom_hline(yintercept = log10(param$alpha), linetype="dashed", color = "black") +
      theme_minimal() +
      xlab('Tested effect') +
      ylab('log10(pvalue)') +
      theme(panel.background = element_blank(),
            axis.title = element_text(size = 12),
            axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 10),
            legend.text = element_text(size = 10),
            legend.title = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    ggsave('./Output/MisMatchSupp.jpg',f1) 
    
  }
  
  #################### figure  2 histograms
  if (param$incSubjObjTest==1) {
    
    #add color vec to results_preprocessed
    results_preprocessed$ColorVec=3
    results_preprocessed$ColorVec[results_preprocessed$AllNoSig==1]=2
    results_preprocessed$ColorVec[results_preprocessed$AllSig==1]=1
    
    # add color vec to data_per_subj_all
    data_per_subj_all$dataset=as.factor(data_per_subj_all$dataset)
    vec=rep(NA,dim(data_per_subj_all)[1])
    for (ds_name in 1:length(datasetNames)) { #datasets
      ind=which(datasetNames[ds_name]==results_preprocessed$dataset)
      value=results_preprocessed$ColorVec[ind]
      vec[which(datasetNames[ds_name]==data_per_subj_all$dataset)]=value
    }
    data_per_subj_all$ColorVec=as.factor(vec)
    
    ###### figure 1: datasets with all non significant tests
    
    #filter datasets with all non significant tests in data_per_subj_all
    all_not_sig=filter(data_per_subj_all, ColorVec == 2)
    
    #change dataset names to have space instead of underscore
    all_not_sig$dataset = gsub('_',' ',all_not_sig$dataset)
    
    f2=ggplot(all_not_sig, aes(x=AS)) +
      geom_histogram(aes(y = stat(density)*0.02) ,binwidth = 0.02, fill="darkturquoise") +
      theme_bw() + 
      geom_vline(xintercept = param$ChanceSuccessRate, linetype="dashed", color = "gray") +
      scale_y_continuous(labels = percent)+
      ylab('Proportion %') +
      xlab('Awareness score') +
      scale_x_continuous(breaks=seq(from=0.4,to=0.6,by=0.1))+
      facet_wrap(~dataset, ncol=6 ,labeller = label_wrap_gen(multi_line = TRUE))+
      theme(strip.text.x = element_text(size=6),
            axis.title = element_text(size=14),
            axis.text = element_text(size=10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) 
    ggsave('./Output/ASHistNoSig.jpg',f2)
    
    ###### figure 2: datasets with all significant tests
    #filter datasets with all  significant tests in data_per_subj_all
    all_sig=filter(data_per_subj_all, ColorVec == 1 )
    
    #change dataset names to have space instead of underscore
    all_sig$dataset = gsub('_',' ',all_sig$dataset)
    
    f3=ggplot(all_sig, aes(x=AS)) +
      geom_histogram(aes(y = stat(density)*0.02) ,binwidth = 0.02, fill="deeppink3") +
      theme_bw() + 
      geom_vline(xintercept = param$ChanceSuccessRate, linetype="dashed", color = "gray") +
      scale_y_continuous(labels = percent)+
      ylab('Proportion %') +
      xlab('Awareness score') +
      scale_x_continuous(breaks=seq(from=0.4,to=0.8,by=0.2))+
      facet_wrap(~dataset, ncol=6, labeller = label_wrap_gen(multi_line = TRUE))+
      theme(strip.text.x = element_text(size=6),
            axis.title = element_text(size=14),
            axis.text = element_text(size=10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) 
    ggsave('./Output/ASHistSig.jpg',f3)
    
    ###### figure 3: datasets with tests that don't agree
    
    # add color vec to results_preprocessed_long
    results_preprocessed_long$ColorVec=3
    results_preprocessed_long$ColorVec[results_preprocessed_long$AllNoSig==1]=2
    results_preprocessed_long$ColorVec[results_preprocessed_long$AllSig==1]=1
    
    #filter datasets with tests that don't agree in data_per_subj_all and results_preprocessed_long
    not_same_ASPlot=filter(data_per_subj_all, ColorVec == 3)
    not_same_pvaluePlot=filter(results_preprocessed_long, ColorVec == 3)
    not_same_pvaluePlot_F=filter(not_same_pvaluePlot, test_type=="Frequentist")
    not_same_pvaluePlot_B=filter(not_same_pvaluePlot, test_type=="Bayesian")
    
    #change dataset names to have space instead of underscore
    not_same_pvaluePlot_F$dataset = gsub('_',' ',not_same_pvaluePlot_F$dataset)
    not_same_pvaluePlot_B$dataset = gsub('_',' ',not_same_pvaluePlot_B$dataset)
    not_same_ASPlot$dataset = gsub('_',' ',not_same_ASPlot$dataset)
    
    f4= ggplot(not_same_ASPlot, aes(x=AS)) +
      geom_histogram(aes(y = stat(density)*0.02),binwidth = 0.02, fill="steelblue3") +
      theme_bw() +
      geom_vline(xintercept = param$ChanceSuccessRate, linetype="dashed", color = "gray") +
      scale_y_continuous(labels = percent)+
      ylab('Proportion %') +
      xlab('Awareness score') +
      scale_x_continuous(breaks=seq(from=0.3,to=0.7,by=0.2))+
      facet_wrap(~dataset, ncol=5,labeller = label_wrap_gen(multi_line = TRUE))+
      theme(strip.text.x = element_text(size=6),
            axis.title = element_text(size=14),
            axis.text = element_text(size=10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    ggsave('./Output/ASHistMisMatch1.jpg',f4)
    
    f5=ggplot(not_same_pvaluePlot_F, aes(x=test,y=log.pvalue,fill=test)) +
      geom_bar(alpha=0.5,stat="identity")+
      geom_hline(yintercept = log10(param$alpha), linetype="dashed", color = "black") +
      theme_bw() +
      ylab('log10(pvalue)') +
      xlab('') +
      scale_fill_manual(values=c("red","pink3","dodgerblue1","darkorange2","cyan4"),name="")+
      facet_wrap(~dataset,labeller = label_wrap_gen(multi_line = TRUE))+
      theme(strip.text.x = element_text(size=6),
            axis.title = element_text(size = 14),
            axis.text.y = element_text(size=10),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            legend.position = "bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    ggsave('./Output/pvalueFreqMisMatch2.jpg',f5)
    
    f6=ggplot(not_same_pvaluePlot_B, aes(x=test,y=log.pvalue,fill=test)) +
      geom_bar(alpha=0.5,stat="identity")+
      geom_hline(yintercept = log10(1/param$bfThreshold), linetype="dashed", color = "black") +
      geom_hline(yintercept = log10(param$bfThreshold), linetype="dashed", color = "black") +
      theme_bw() +
      ylab('log10(BF01)') +
      xlab('') +
      facet_wrap(~dataset,labeller = label_wrap_gen(multi_line = TRUE))+
      scale_fill_manual(values=c("purple","blue"),name="")+
      theme(strip.text.x = element_text(size=6),
            axis.title = element_text(size = 14),
            axis.text.y = element_text(size=10),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.text = element_text(size = 10),
            legend.title = element_blank(),
            legend.position = "bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    ggsave('./Output/pvalueBayesMisMatch3.jpg',f6)
  }  
}
