
library(ggplot2)
library(cowplot)

set.seed(3210)

all_dat = list.files(here::here("data", "raw_cocin"))

for(filename in all_dat){
  
  assign(strsplit(filename, split = ".csv")[[1]],
         read.csv(here::here("data", "raw_cocin", filename)))
  
}

generate_sample = function(distribution, distribution2 = NULL, distribution3 = NULL){
  
  res2 = res3 = 0
  
  res = sample(distribution$day, prob = distribution$probability, 
               size = 100000, replace = T)
  
  if(!is.null(distribution2)){
    res2 = sample(distribution2$day, prob = distribution2$probability, 
                  size = 100000, replace = T)
  }
  
  if(!is.null(distribution3)){
    res3 = sample(distribution3$day, prob = distribution3$probability, 
                  size = 100000, replace = T)
  }
  
  res + res2 + res3
  
}

step1_CC = generate_sample(distribution_time_icu_nostepdown_startinicu)

step1_ward = generate_sample(distribution_time_hosp_noicu)

step1_ward_CC = generate_sample(distribution_time_hosp_icu_nostepdown)
step2_ward_CC = generate_sample(distribution_time_icu_nostepdown_startinhosp)

step1_CC_ward = generate_sample(distribution_time_icu_stepdown_startinicu)
step2_CC_ward = generate_sample(distribution_time_stepdown_startinicu)

step1_ward_CC_ward = generate_sample(distribution_time_hosp_icu_stepdown)
step2_ward_CC_ward = generate_sample(distribution_time_icu_stepdown_startinhosp)
step3_ward_CC_ward = generate_sample(distribution_time_stepdown_startinhosp)


#WEIBULL VS MULTISTATE #####

#plot distributions
step_LoS = mixdist::weibullpar(mean(step1_CC), sd(step1_CC), 0)
p1s1 = ggplot() +
  geom_col(data = distribution_time_icu_nostepdown_startinicu,
           aes(x = day, y = probability)) +
  scale_x_continuous(limits = c(0,100)) +
  geom_density(aes(ceiling(rweibull(100000, shape = step_LoS$shape, scale = step_LoS$scale))))
# geom_density(aes(ceiling(rgamma(100000,
#                         shape = (mean(step1_CC)/sd(step1_CC))^2,
#                         rate = mean(step1_CC)/sd(step1_CC)^2))))


step_LoS = mixdist::weibullpar(mean(step1_ward), sd(step1_ward), 0)
p2s1 = ggplot() +
  geom_col(data = distribution_time_hosp_noicu,
           aes(x = day, y = probability)) +
  scale_x_continuous(limits = c(0,100)) +
  geom_density(aes(ceiling(rweibull(100000, shape = step_LoS$shape, scale = step_LoS$scale))))
# geom_density(aes(ceiling(rgamma(100000,
#                         shape = (mean(step1_CC)/sd(step1_CC))^2,
#                         rate = mean(step1_CC)/sd(step1_CC)^2))))


step_LoS = mixdist::weibullpar(mean(step1_ward_CC), sd(step1_ward_CC), 0)
p3s1 = ggplot() +
  geom_col(data = distribution_time_hosp_icu_nostepdown,
           aes(x = day, y = probability)) +
  scale_x_continuous(limits = c(0,100)) +
  geom_density(aes(ceiling(rweibull(100000, shape = step_LoS$shape, scale = step_LoS$scale))))
# geom_density(aes(ceiling(rgamma(100000,
#                         shape = (mean(step1_CC)/sd(step1_CC))^2,
#                         rate = mean(step1_CC)/sd(step1_CC)^2))))
step_LoS = mixdist::weibullpar(mean(step2_ward_CC), sd(step2_ward_CC), 0)
p3s2 = ggplot() +
  geom_col(data = distribution_time_icu_nostepdown_startinhosp,
           aes(x = day, y = probability)) +
  scale_x_continuous(limits = c(0,100)) +
  geom_density(aes(ceiling(rweibull(100000, shape = step_LoS$shape, scale = step_LoS$scale))))
# geom_density(aes(ceiling(rgamma(100000,
#                         shape = (mean(step1_CC)/sd(step1_CC))^2,
#                         rate = mean(step1_CC)/sd(step1_CC)^2))))


step_LoS = mixdist::weibullpar(mean(step1_CC_ward), sd(step1_CC_ward), 0)
p4s1 = ggplot() +
  geom_col(data = distribution_time_icu_stepdown_startinicu,
           aes(x = day, y = probability)) +
  scale_x_continuous(limits = c(0,100)) +
  geom_density(aes(ceiling(rweibull(100000, shape = step_LoS$shape, scale = step_LoS$scale))))
# geom_density(aes(ceiling(rgamma(100000,
#                         shape = (mean(step1_CC)/sd(step1_CC))^2,
#                         rate = mean(step1_CC)/sd(step1_CC)^2))))
step_LoS = mixdist::weibullpar(mean(step2_CC_ward), sd(step2_CC_ward), 0)
p4s2 = ggplot() +
  geom_col(data = distribution_time_stepdown_startinicu,
           aes(x = day, y = probability)) +
  scale_x_continuous(limits = c(0,100)) +
  geom_density(aes(ceiling(rweibull(100000, shape = step_LoS$shape, scale = step_LoS$scale))))
# geom_density(aes(ceiling(rgamma(100000,
#                         shape = (mean(step1_CC)/sd(step1_CC))^2,
#                         rate = mean(step1_CC)/sd(step1_CC)^2))))


step_LoS = mixdist::weibullpar(mean(step1_ward_CC_ward), sd(step1_ward_CC_ward), 0)
p5s1 = ggplot() +
  geom_col(data = distribution_time_hosp_icu_stepdown,
           aes(x = day, y = probability)) +
  scale_x_continuous(limits = c(0,100)) +
  geom_density(aes(ceiling(rweibull(100000, shape = step_LoS$shape, scale = step_LoS$scale))))
# geom_density(aes(ceiling(rgamma(100000,
#                         shape = (mean(step1_CC)/sd(step1_CC))^2,
#                         rate = mean(step1_CC)/sd(step1_CC)^2))))
step_LoS = mixdist::weibullpar(mean(step2_ward_CC_ward), sd(step2_ward_CC_ward), 0)
p5s2 = ggplot() +
  geom_col(data = distribution_time_icu_stepdown_startinhosp,
           aes(x = day, y = probability)) +
  scale_x_continuous(limits = c(0,100)) +
  geom_density(aes(ceiling(rweibull(100000, shape = step_LoS$shape, scale = step_LoS$scale))))
# geom_density(aes(ceiling(rgamma(100000,
#                         shape = (mean(step1_CC)/sd(step1_CC))^2,
#                         rate = mean(step1_CC)/sd(step1_CC)^2))))
step_LoS = mixdist::weibullpar(mean(step3_ward_CC_ward), sd(step3_ward_CC_ward), 0)
p5s3 = ggplot() +
  geom_col(data = distribution_time_stepdown_startinhosp,
           aes(x = day, y = probability)) +
  scale_x_continuous(limits = c(0,100)) +
  geom_density(aes(ceiling(rweibull(100000, shape = step_LoS$shape, scale = step_LoS$scale))))
# geom_density(aes(ceiling(rgamma(100000,
#                         shape = (mean(step1_CC)/sd(step1_CC))^2,
#                         rate = mean(step1_CC)/sd(step1_CC)^2))))


plot_grid(p1s1,NULL,NULL,
          p2s1,NULL,NULL,
          p3s1,p3s2,NULL,
          p4s1,p4s2,NULL,
          p5s1,p5s2,p5s3,
          labels = c("CC", "", "",
                     "Ward", "", "",
                     "Ward, CC", "", "",
                     "CC, Ward",  "", "",
                     "Ward, CC, Ward"),
          nrow = 5)



# MULTISTATE VS EMPIRIC #####

#plot distributions
p1s1 = ggplot() +
  geom_col(data = distribution_time_icu_nostepdown_startinicu,
           aes(x = day, y = probability, fill = "multistate"), alpha = 0.5) +
  geom_col(data = obsdistribution_time_icu_nostepdown_startinicu,
           aes(x = day, y = probability, fill = "empiric"), alpha = 0.5) +
  scale_x_continuous(limits = c(0,100)) 


p2s1 = ggplot() +
  geom_col(data = distribution_time_hosp_noicu,
           aes(x = day, y = probability, fill = "multistate"), alpha = 0.5) +
  geom_col(data = obsdistribution_time_hosp_noicu,
           aes(x = day, y = probability, fill = "empiric"), alpha = 0.5) +
  scale_x_continuous(limits = c(0,100)) 


p3s1 = ggplot() +
  geom_col(data = distribution_time_hosp_icu_nostepdown,
           aes(x = day, y = probability, fill = "multistate"), alpha = 0.5) +
  geom_col(data = obsdistribution_time_hosp_icu_nostepdown,
           aes(x = day, y = probability, fill = "empiric"), alpha = 0.5) +
  scale_x_continuous(limits = c(0,100))
p3s2 = ggplot() +
  geom_col(data = distribution_time_icu_nostepdown_startinhosp,
           aes(x = day, y = probability, fill = "multistate"), alpha = 0.5)+
  geom_col(data = obsdistribution_time_icu_nostepdown_startinhosp,
           aes(x = day, y = probability, fill = "empiric"), alpha = 0.5) +
  scale_x_continuous(limits = c(0,100)) 


p4s1 = ggplot() +
  geom_col(data = distribution_time_icu_stepdown_startinicu,
           aes(x = day, y = probability, fill = "multistate"), alpha = 0.5)+
  geom_col(data = obsdistribution_time_icu_stepdown_startinicu,
           aes(x = day, y = probability, fill = "empiric"), alpha = 0.5) +
  scale_x_continuous(limits = c(0,100)) 
p4s2 = ggplot() +
  geom_col(data = distribution_time_stepdown_startinicu,
           aes(x = day, y = probability, fill = "multistate"), alpha = 0.5)+
  geom_col(data = obsdistribution_time_stepdown_startinicu,
           aes(x = day, y = probability, fill = "empiric"), alpha = 0.5) +
  scale_x_continuous(limits = c(0,100)) 


p5s1 = ggplot() +
  geom_col(data = distribution_time_hosp_icu_stepdown,
           aes(x = day, y = probability, fill = "multistate"), alpha = 0.5)+
  geom_col(data = obsdistribution_time_hosp_icu_stepdown,
           aes(x = day, y = probability, fill = "empiric"), alpha = 0.5) +
  scale_x_continuous(limits = c(0,100)) 
p5s2 = ggplot() +
  geom_col(data = distribution_time_icu_stepdown_startinhosp,
           aes(x = day, y = probability, fill = "multistate"), alpha = 0.5)+
  geom_col(data = obsdistribution_time_icu_stepdown_startinhosp,
           aes(x = day, y = probability, fill = "empiric"), alpha = 0.5) +
  scale_x_continuous(limits = c(0,100))
p5s3 = ggplot() +
  geom_col(data = distribution_time_stepdown_startinhosp,
           aes(x = day, y = probability, fill = "multistate"), alpha = 0.5)+
  geom_col(data = obsdistribution_time_stepdown_startinhosp,
           aes(x = day, y = probability, fill = "empiric"), alpha = 0.5) +
  scale_x_continuous(limits = c(0,100))

legend = get_legend(p5s3 +
                      theme(legend.position = "bottom")+
                      scale_fill_discrete(name = "Distribution:", labels = c("Empiric", "Estimated")))

plot_grid(p1s1+theme(legend.position = "none"),NULL,NULL,
          p2s1+theme(legend.position = "none"),NULL,NULL,
          p3s1+theme(legend.position = "none"),p3s2+theme(legend.position = "none"),NULL,
          p4s1+theme(legend.position = "none"),p4s2+theme(legend.position = "none"),NULL,
          p5s1+theme(legend.position = "none"),p5s2+theme(legend.position = "none"),p5s3+theme(legend.position = "none"),
          NULL, legend, NULL,
          labels = c("CC", "", "",
                     "Ward", "", "",
                     "Ward, CC", "", "",
                     "CC, Ward",  "", "",
                     "Ward, CC, Ward", "", ""),
          label_size = 11, label_x = 0, label_y = 1.2,
          rel_heights = c(1,1,1,1,1,0.2),
          nrow = 6)


#prettier!

#model
distribution_time_icu_nostepdown_startinicu$path_f = "CC"
distribution_time_icu_nostepdown_startinicu$step = "1"
distribution_time_icu_nostepdown_startinicu$distribution = "estimated"

distribution_time_hosp_noicu$path_f = "Ward"
distribution_time_hosp_noicu$step = "1"
distribution_time_hosp_noicu$distribution = "estimated"

distribution_time_hosp_icu_nostepdown$path_f = "Ward, CC"
distribution_time_hosp_icu_nostepdown$step = "1"
distribution_time_hosp_icu_nostepdown$distribution = "estimated"
distribution_time_icu_nostepdown_startinhosp$path_f = "Ward, CC"
distribution_time_icu_nostepdown_startinhosp$step = "2"
distribution_time_icu_nostepdown_startinhosp$distribution = "estimated"

distribution_time_icu_stepdown_startinicu$path_f = "CC, Ward"
distribution_time_icu_stepdown_startinicu$step = "1"
distribution_time_icu_stepdown_startinicu$distribution = "estimated"
distribution_time_stepdown_startinicu$path_f = "CC, Ward"
distribution_time_stepdown_startinicu$step = "2"
distribution_time_stepdown_startinicu$distribution = "estimated"

distribution_time_hosp_icu_stepdown$path_f = "Ward, CC, Ward"
distribution_time_hosp_icu_stepdown$step = "1"
distribution_time_hosp_icu_stepdown$distribution = "estimated"
distribution_time_icu_stepdown_startinhosp$path_f = "Ward, CC, Ward"
distribution_time_icu_stepdown_startinhosp$step = "2"
distribution_time_icu_stepdown_startinhosp$distribution = "estimated"
distribution_time_stepdown_startinhosp$path_f = "Ward, CC, Ward"
distribution_time_stepdown_startinhosp$step = "3"
distribution_time_stepdown_startinhosp$distribution = "estimated"

#obs
obsdistribution_time_icu_nostepdown_startinicu$path_f = "CC"
obsdistribution_time_icu_nostepdown_startinicu$step = "1"
obsdistribution_time_icu_nostepdown_startinicu$distribution = "empiric"

obsdistribution_time_hosp_noicu$path_f = "Ward"
obsdistribution_time_hosp_noicu$step = "1"
obsdistribution_time_hosp_noicu$distribution = "empiric"

obsdistribution_time_hosp_icu_nostepdown$path_f = "Ward, CC"
obsdistribution_time_hosp_icu_nostepdown$step = "1"
obsdistribution_time_hosp_icu_nostepdown$distribution = "empiric"
obsdistribution_time_icu_nostepdown_startinhosp$path_f = "Ward, CC"
obsdistribution_time_icu_nostepdown_startinhosp$step = "2"
obsdistribution_time_icu_nostepdown_startinhosp$distribution = "empiric"

obsdistribution_time_icu_stepdown_startinicu$path_f = "CC, Ward"
obsdistribution_time_icu_stepdown_startinicu$step = "1"
obsdistribution_time_icu_stepdown_startinicu$distribution = "empiric"
obsdistribution_time_stepdown_startinicu$path_f = "CC, Ward"
obsdistribution_time_stepdown_startinicu$step = "2"
obsdistribution_time_stepdown_startinicu$distribution = "empiric"

obsdistribution_time_hosp_icu_stepdown$path_f = "Ward, CC, Ward"
obsdistribution_time_hosp_icu_stepdown$step = "1"
obsdistribution_time_hosp_icu_stepdown$distribution = "empiric"
obsdistribution_time_icu_stepdown_startinhosp$path_f = "Ward, CC, Ward"
obsdistribution_time_icu_stepdown_startinhosp$step = "2"
obsdistribution_time_icu_stepdown_startinhosp$distribution = "empiric"
obsdistribution_time_stepdown_startinhosp$path_f = "Ward, CC, Ward"
obsdistribution_time_stepdown_startinhosp$step = "3"
obsdistribution_time_stepdown_startinhosp$distribution = "empiric"


data = bind_rows(mget(unlist(strsplit(all_dat, split = ".csv"))))


ggplot(data) + 
  # geom_bar(aes(x = day, y = probability, fill = distribution), alpha = 0.5,
  #         stat = "identity", position = "identity")+
  # geom_line(aes(x = day, y = probability, colour = distribution), alpha = 0.5)+
  geom_area(aes(x = day, y = probability, fill = distribution), alpha = 0.5, position = "identity")+
  facet_grid(path_f~step,  switch = "y", scales = "free") + 
  scale_x_continuous("Length of stay", limits = c(0,25)) + 
  scale_y_continuous("Probability")+
  theme_bw(base_size = 11)+
  theme(strip.text.y.left = element_text(angle = 0))+
  theme(axis.ticks.y = element_blank())+
  scale_fill_discrete(name = "Distribution:", labels = c("Empirical", "Estimated"))

ggsave(here::here("outputs", "figures", "cocin_distributions.png"))

summary_data = data.frame(n = rep(0, 5),
                          step1_los = 0,
                          step1_sd = 0,
                          step2_los = 0,
                          step2_sd = 0,
                          step3_los = 0,
                          step3_sd = 0,
                          total_los = 0,
                          total_sd = 0,
                          dsterm = 0,
                          pathway = c("Bed CC", "Bed CC, Bed Ward",
                                      "Bed Ward", "Bed Ward, Bed CC", 
                                      "Bed Ward, Bed CC, Bed Ward"))


summary_data$n = c(529, 2880, 35164, 347, 4060)

summary_data$step1_los = c(mean(step1_CC),
                           mean(step1_CC_ward),
                           mean(step1_ward),
                           mean(step1_ward_CC),
                           mean(step1_ward_CC_ward))
summary_data$step1_sd = c(sd(step1_CC),
                          sd(step1_CC_ward),
                          sd(step1_ward),
                          sd(step1_ward_CC),
                          sd(step1_ward_CC_ward))


summary_data$step2_los = c(NA,
                           mean(step2_CC_ward),
                           NA,
                           mean(step2_ward_CC),
                           mean(step2_ward_CC_ward))
summary_data$step2_sd = c(NA,
                          sd(step2_CC_ward),
                          NA,
                          sd(step2_ward_CC),
                          sd(step2_ward_CC_ward))


summary_data$step3_los = c(NA,
                           NA,
                           NA,
                           NA,
                           mean(step3_ward_CC_ward))
summary_data$step3_sd = c(NA,
                          NA,
                          NA,
                          NA,
                          sd(step3_ward_CC_ward))

tot_CC = generate_sample(distribution_time_icu_nostepdown_startinicu)

tot_ward = generate_sample(distribution_time_hosp_noicu)

tot_ward_CC = generate_sample(distribution_time_hosp_icu_nostepdown,
                              distribution_time_icu_nostepdown_startinhosp)

tot_CC_ward = generate_sample(distribution_time_icu_stepdown_startinicu,
                              distribution_time_stepdown_startinicu)

tot_ward_CC_ward = generate_sample(distribution_time_hosp_icu_stepdown,
                                   distribution_time_icu_stepdown_startinhosp,
                                   distribution_time_stepdown_startinhosp)


summary_data$total_los = c(mean(tot_CC),
                           mean(tot_CC_ward),
                           mean(tot_ward),
                           mean(tot_ward_CC),
                           mean(tot_ward_CC_ward))
summary_data$total_sd = c(sd(tot_CC),
                          sd(tot_CC_ward),
                          sd(tot_ward),
                          sd(tot_ward_CC),
                          sd(tot_ward_CC_ward))

write.csv(summary_data, here::here("data", "average_los_by_pathway.csv"), row.names = F)


#to work out averages, resample with relative sample size based on number of patients in each path/step
average_data = data.frame(n = c(7816, 46511),
                          total_los = c(mean(c(sample(step1_CC, 529*10, replace = T), 
                                               sample(step1_CC_ward, 2880*10, replace = T), 
                                               sample(step2_ward_CC, 347*10, replace = T), 
                                               sample(step2_ward_CC_ward,4060*10, replace = T))),
                                        mean(c(sample(step1_ward,35164*10, replace = T), 
                                               sample(step1_ward_CC,347*10, replace = T), 
                                               sample(step1_ward_CC_ward,4060*10, replace = T), 
                                               sample(step3_ward_CC_ward,4060*10, replace = T)))),
                          total_sd = c(sd(c(sample(step1_CC, 529*10, replace = T), 
                                            sample(step1_CC_ward, 2880*10, replace = T), 
                                            sample(step2_ward_CC, 347*10, replace = T), 
                                            sample(step2_ward_CC_ward, 4060*10, replace = T))),
                                       sd(c(sample(step1_ward, 35164*10, replace = T), 
                                            sample(step1_ward_CC, 347*10, replace = T), 
                                            sample(step1_ward_CC_ward, 4060*10, replace = T), 
                                            sample(step3_ward_CC_ward, 4060*10, replace = T)))),
                          dsterm = 0,
                          bedtype = c("CC", "Ward"))


average_total = data.frame(n = c(42980),
                           total_los = mean(c(sample(tot_ward, 35164*10, replace = T),
                                              sample(tot_CC, 529*10, replace = T), 
                                              sample(tot_CC_ward, 2880*10, replace = T), 
                                              sample(tot_ward_CC, 347*10, replace = T), 
                                              sample(tot_ward_CC_ward, 4060*10, replace = T))),
                           total_sd = sd(c(sample(tot_ward, 35164*10, replace = T), 
                                           sample(tot_CC, 529*10, replace = T),
                                           sample(tot_CC_ward, 2880*10, replace = T),
                                           sample(tot_ward_CC, 347*10, replace = T), 
                                           sample(tot_ward_CC_ward, 4060*10, replace = T))),
                           dsterm = 1,
                           bedtype = "Total")

average_data = rbind(average_data, average_total)

write.csv(average_data, here::here("data", "average_los_by_outcome.csv"), row.names = F)
