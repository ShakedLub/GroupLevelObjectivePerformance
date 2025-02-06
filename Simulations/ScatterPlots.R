ScatterPlots <- function(all_results,sim_conditions_table,fixed_params, add_H0 = FALSE) {
  ## Organize data
  if(add_H0) {
    H0_df <- all_results %>%
      mutate(test = ifelse(grepl('Bayes', test), 
             paste(test, 'H0' , sep = '_'), test),
             sig_perc = null_perc)
    all_results <- all_results %>%
      mutate(test = ifelse(grepl('Bayes', test), 
             paste(test, 'H1' , sep = '_'), test))
    all_results <- rbind(all_results, H0_df)
  }
  #change number of subjects to be a factor
  all_results$Ns=as.factor(all_results$n_participants)
  all_results$sig_perc <- all_results$sig_perc * 100  
  #AlphaErrorRate analysis results
  ResultA=all_results[all_results$analysis_type=="Unaware",]
  
  #Power analysis results 'Mixed', 'Small_spread', 'Large_spread',
  ResultP=all_results[all_results$analysis_type=='Mixed' | all_results$analysis_type=='Small_spread'| all_results$analysis_type=='Large_spread',]
  
  ## Plot
  if (dim(ResultA)[1] != 0 & dim(ResultP)[1] != 0) { #plot alpha error rate and power
    
    #calculate CI for alpha=0.05 based on the number of iterations
    CI=qbinom(c(0.025,0.975),size=fixed_params@n_iterations,prob=fixed_params@alpha)
    CI=CI/fixed_params@n_iterations
    CI = CI * 100
    
    P1 = ggplot(ResultA, aes(x = Ns, y = sig_perc, group=test, color=test)) +
      geom_line()+
      geom_point(size=2)+
      geom_hline(yintercept = CI[1], linetype="dashed", color = "black") +
      geom_hline(yintercept = CI[2], linetype="dashed", color = "black") +
      theme_classic() + 
      xlab('') +
      ylab('% Significant') + 
      facet_grid(analysis_type ~ mean_trials , switch = 'y')+
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            # strip.text.x = element_text(size=14),
            # strip.text.y = element_blank(),
            axis.title.y = element_text(size=16),
            axis.text.y = element_text(size=14),
            legend.position = "none") 
    
    P2 = ggplot(ResultP, aes(x = Ns, y = sig_perc, group=test, color=test)) +
      geom_line()+
      geom_point(size=2)+
      theme_classic() + 
      xlab('Number of Subjects') +
      ylab('% Significant') + 
      facet_grid(analysis_type ~ mean_trials , switch = 'y')+
      theme(axis.text.x = element_text(size=14),
            axis.title.x = element_text(size=16),
            # strip.text.y = element_blank(),
            # strip.text.x = element_blank(),
            axis.text.y = element_text(size=14),
            axis.title.y = element_text(size=16),
            legend.text = element_text(size=16),
            legend.title = element_text(size=16),
            legend.position = "bottom")+ 
    scale_colour_discrete(name = "Test")
    
    f1=grid.arrange(P1,P2,nrow=2, ncol =1,   heights = c(1, 3))
    
  } else if (dim(ResultP)[1] != 0) { #plot only power
    f1=ggplot(ResultP, aes(x = Ns, y = sig_perc, group=test, color=test)) +
      geom_line()+
      geom_point(size=2)+
      theme_classic() + 
      xlab('Participants') +
      ylab('% Significant') + 
      facet_grid(analysis_type ~ mean_trials , switch = 'y')+
      theme(axis.text.x = element_text(size=14),
            axis.title.x = element_text(size=16),
            # strip.text.y = element_blank(),
            strip.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.y = element_text(size=16),
            legend.text = element_text(size=16),
            legend.title = element_text(size=16),
            legend.position = "bottom")+ 
      scale_colour_discrete(name = "Test")
    
  } else if (dim(ResultA)[1] != 0) { #plot only alpha error rate
    
    #calculate CI for alpha=0.05 based on the number of iterations
    CI=qbinom(c(0.025,0.975),size=fixed_params@n_iterations,prob=fixed_params@alpha)
    CI=CI/fixed_params@n_iterations
    
    f1=ggplot(ResultA, aes(x = Ns, y = sig_perc, group=test, color=test)) +
      geom_line()+
      geom_point(size=2)+
      geom_hline(yintercept = CI[1], linetype="dashed", color = "black") +
      geom_hline(yintercept = CI[2], linetype="dashed", color = "black") +
      theme_classic() + 
      xlab('% Significant') +
      ylab('Alpha error rate') + 
      facet_grid(analysis_type ~ mean_trials , switch = 'y')+
      theme(axis.text.x = element_text(size=14),
            axis.title.x = element_text(size=16),
            # strip.text.y = element_blank(),
            strip.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.y = element_text(size=16),
            legend.text = element_text(size=16),
            legend.title = element_text(size=16),
            legend.position = "bottom")+ 
      scale_colour_discrete(name = "Test")
  }
  ggsave('./Output/GBAlphaplotResult.jpg',f1, width = 12, height = 12)
}
