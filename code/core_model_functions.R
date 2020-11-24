
pathways_model = function(cov_curve, pathways, run_duration, 
                          LoS_distribution = "weibull", LoS_rounding = "ceiling"){
  
  #safety checks disabled here
  #instead implemented in multi function, to avoid repeating each time
  # #safety check for pathways probabilities
  # if (!all.equal(sum(list.mapv(pathways, proba)), 1)) stop("Pathways probabilities do not sum to 1!")
  # 
  # #safety check for LoS distribution
  # if (!(LoS_distribution %in% c("weibull", "gamma"))) stop("Incorrect LoS distribution, only weibull or gamma supported!")
  # 
  # #safety check for LoS rounding
  # if (!(LoS_rounding %in% c("ceiling", "round"))) stop("Incorrect LoS rounding, only ceiling or round supported!")
  # 
  # #automatically assume no more admissions after run_duration > cov_curve
  # if (length(cov_curve) < run_duration) {
  #   cov_curve = c(cov_curve, rep(0,run_duration - length(cov_curve)))
  # }
  
  # Dataframe to store beds needed:
  # Dynamically identify beds needed
  beds_needed = list.map(pathways, LoS)
  beds_needed = unique(list.mapv(beds_needed, Bed))
  beds_needed = grep("_sd", beds_needed, value = T, invert = T) #remove sd names
  beds_needed = grep("_proba", beds_needed, value = T, invert = T) #remove proba names
  
  
  results = data.frame(time = c(1:run_duration))
  
  for (i in beds_needed) {
    results = cbind(results, rep(0, run_duration))
  }
  
  colnames(results)[-1] = beds_needed
  
  #deaths currently not implemented, leaving out
  #results$deaths = 0
  results$admissions = cov_curve[1:run_duration]
  
  
  # Dataframe to store admissions details
  admissions_summary = data.frame(time = c(1:run_duration))
  
  for (i in 1:length(pathways)) {
    admissions_summary = cbind(admissions_summary, rep(0, run_duration))
  }
  
  colnames(admissions_summary)[-1] = names(pathways)
  admissions_summary$admissions = cov_curve[1:run_duration]
  
  
  #run model for run_duration days
  for (t in c(1:run_duration)) {
    
    #how many admissions at time t ?
    new_patients = cov_curve[t]
    
    #randomly sort each patient into pathways
    new_patients = sample(list.mapv(pathways, pathway), new_patients, 
                          replace = T, prob = list.mapv(pathways, proba))
    
    #for each admission at time t, run through the pathway
    for (patient_path in new_patients) {
      
      #record that admission
      path_col = which(colnames(admissions_summary) == patient_path)
      admissions_summary[t, path_col] = admissions_summary[t, path_col] + 1
      
      #extract the summary LoS data for that pathway
      path_info_patient = pathways[[patient_path]]$LoS
      #path_death = pathways[[patient_path]]$death
      
      #set the patient time in the system
      patient_time = 0
      
      if(LoS_distribution == "custom"){
        patient_steps = seq(1, nrow(path_info_patient), 2)
      } else {
        patient_steps = c(1:nrow(path_info_patient))
      }
      
      #for each patient step in the patient pathway
      for (patient_step in patient_steps) {
        
        #calculate LoS for that step
        step_bed = path_info_patient$Bed[patient_step]
        
        if(LoS_distribution == "custom") {
          
          #remove 1st column as that's bed name
          #ugly, but fastest way to remove NAs, which were added by autofilling columns
          step_LoS = unlist(sample(x = path_info_patient[(patient_step),-1][!is.na(path_info_patient[(patient_step),-1])],
                                   size = 1,
                                   prob = path_info_patient[(patient_step+1),-1][!is.na(path_info_patient[(patient_step+1),-1])]))
          
        } else {
          
          #weibull or gamma or lognormal or custom distribution
          if(LoS_distribution == "weibull"){
            
            step_LoS = mixdist::weibullpar(path_info_patient$mean[patient_step], path_info_patient$sd[patient_step], 0)
            step_LoS = rweibull(1, shape = step_LoS$shape, scale = step_LoS$scale)
            
          } else if(LoS_distribution == "lognormal"){
            
            step_LoS = rlnorm(1, log(path_info_patient$mean[patient_step]),
                              log(path_info_patient$sd[patient_step]))
            
          } else {
            
            step_LoS = rgamma(1,
                              shape = (path_info_patient$mean[patient_step]/path_info_patient$sd[patient_step])^2,
                              rate = path_info_patient$mean[patient_step]/path_info_patient$sd[patient_step]^2)
          }
          
          if(LoS_rounding == "ceiling"){
            step_LoS = ceiling(step_LoS)
          } else {
            step_LoS = round(step_LoS)
          }
          
        }
        
        
        #define start and end of patient step
        step_begin = t + patient_time
        step_end = step_begin + step_LoS
        
        #store bed needs
        results[(step_begin:step_end), step_bed] = results[(step_begin:step_end), step_bed] + 1
        
        #update patient time
        patient_time = patient_time + step_LoS
        
        #if this was the last step, and this is a pathway with mortality, record death
        # if (patient_step == nrow(path_info_patient) && isTRUE(path_death)) {
        #   results$deaths[step_end] = results$deaths[step_end] + 1
        # }
        
        #no need to worry about transition probas, since that's taken care of when giving probas to pathways
        
      }
    }
  }
  
  #indexing past the results limit just gives NA values, so can remove them
  results = results %>%
    tidyr::drop_na()
  
  return(list(results = results, admissions_summary = admissions_summary))
  
} 






multi_pathways_model = function(nruns, cov_curve, pathways, run_duration, 
                                LoS_distribution = "weibull",
                                LoS_rounding = "ceiling",
                                record_peak = FALSE,
                                verbose=F) {
  
  # this function is a wrapper around the model, to run it multiple times
  
  #extract unique beds in pathways
  beds_needed = list.map(pathways, LoS)
  beds_needed = unique(list.mapv(beds_needed, Bed))
  beds_needed = grep("_sd", beds_needed, value = T, invert = T) #remove sd names
  beds_needed = grep("_proba", beds_needed, value = T, invert = T) #remove proba names
  #beds_needed = c(beds_needed, "deaths")
  
  #create summary list to store each bed type needs
  summary = list()
  index = 1
  for (i in beds_needed) {
    summary[[index]] = matrix(0, run_duration, nruns)
    index = index + 1
  }
  names(summary) = beds_needed
  
  
  #Safety check implemented here rather than in model to avoid repeating each time
  #safety check for pathways probabilities
  if (!all.equal(sum(list.mapv(pathways, proba)), 1)) stop("Pathways probabilities do not sum to 1!")
  
  #safety check for LoS distribution
  if (!(LoS_distribution %in% c("custom", "weibull", "gamma", "lognormal"))) stop("Incorrect LoS distribution, only custom, weibull, gamma or lognormal supported!")
  
  #safety check for LoS rounding
  if (!(LoS_rounding %in% c("ceiling", "round"))) stop("Incorrect LoS rounding, only ceiling or round supported!")
  
  #automatically assume no more admissions after run_duration > cov_curve
  if (length(cov_curve) < run_duration) {
    cov_curve = c(cov_curve, rep(0,run_duration - length(cov_curve)))
  }
  
  
  
  #run the model for nruns times
  for (run in c(1:nruns)) {
    
    if(verbose==T) message("Run ", run, " out of ", nruns)
    
    #run the model
    results_all = pathways_model(cov_curve, pathways, run_duration,
                                 LoS_distribution, LoS_rounding)
    
    #save the output to the corresponding matrix in the summary list
    for (bed_name in beds_needed) {
      summary[[bed_name]][,run] = results_all$results[,bed_name]
    }
  }
  
  
  #combine all results into a single dataframe with mean and sd
  summary_results = data.frame(time = c(1:run_duration))
  
  for (bed_name in beds_needed) {
    summary_results = cbind(summary_results, 
                            rowMeans(summary[[bed_name]]), 
                            apply(summary[[bed_name]], 1, sd))
  }
  
  #extract column names for summary dataframe
  beds_needed_colnames = c()
  for (bed_name in beds_needed) {
    beds_needed_colnames = c(beds_needed_colnames,
                             bed_name,
                             paste0(bed_name, "_sd"))
  }
  
  colnames(summary_results)[-1] = beds_needed_colnames
  
  if(record_peak == T){
    
    summary_peak = data.frame(run = c(1:nruns))
    
    for (bed_name in beds_needed) {
      summary_peak = cbind(summary_peak, 
                           apply(summary[[bed_name]], 2, which.max), 
                           apply(summary[[bed_name]], 2, max))
    }
    
    #extract column names for summary dataframe
    beds_needed_colnames = c()
    for (bed_name in beds_needed) {
      beds_needed_colnames = c(beds_needed_colnames,
                               paste0(bed_name, "_peak_time"),
                               paste0(bed_name, "_peak"))
    }
    
    colnames(summary_peak)[-1] = beds_needed_colnames
    
    return(list(summary_results = summary_results, summary_peak = summary_peak))
    
  } else {
    
    return(summary_results)
    
  }
  
  
}



plot_multi = function(results, save = F, filename = "plot", title = "", silent = F){
  
  # this function takes as input the results dataframe from multi_pathways_model
  # it then plots the bed needs
  
  results = melt(results, id.vars = "time")
  
  #separate mean and sd
  results_mean = results %>%
    filter(!grepl("sd", variable)) %>%
    filter(!grepl("date", variable)) %>%
    arrange(variable)
  
  results_sd = results %>% 
    filter(grepl("sd", variable)) %>%
    arrange(variable)
  
  results_sd$min = results_mean$value - 1.96*results_sd$value
  results_sd$max = results_mean$value + 1.96*results_sd$value
  
  #create plot
  gg = ggplot() +
    geom_line(data = results_mean, aes(time, value, colour = variable)) +
    geom_ribbon(data = results_sd, aes(time,
                                       ymin = min,
                                       ymax = max,
                                       fill = variable),
                alpha = 0.3) +
    labs(colour = "Bed category:", x = "Time (days)", y = "Beds needed", title = title) +
    guides(fill = FALSE) +
    #scale_y_continuous(breaks = seq(0,200,20)) +
    theme_bw()
  
  #return plot object or just plot it based on "silent" argument
  if (silent == T) {
    return(gg)
  } else {
    plot(gg)
    if (save == T) ggsave(paste0(filename,".png"))
  }
}



