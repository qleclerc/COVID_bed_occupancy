
library(plyr)
library(rlist)

all_dat = list.files(here::here("data", "raw_cocin"))

for(filename in all_dat){
  
  assign(strsplit(filename, split = ".csv")[[1]],
         read.csv(here::here("data", "raw_cocin", filename)))
  
}

path_CC = cbind(Bed = c("CC", "CC_proba"),
                rbind.fill(data.frame(t(distribution_time_icu_nostepdown_startinicu$day)),
                           data.frame(t(distribution_time_icu_nostepdown_startinicu$probability))))
path_CC = list(pathway = "CC",
               proba = 529/42980,
               LoS = path_CC)

path_ward = cbind(Bed = c("Ward", "Ward_proba"),
                  rbind.fill(data.frame(t(distribution_time_hosp_noicu$day)),
                             data.frame(t(distribution_time_hosp_noicu$probability))))
path_ward = list(pathway = "Ward",
               proba = 35164/42980,
               LoS = path_ward)

path_ward_CC = cbind(Bed = c("Ward", "Ward_proba", "CC", "CC_proba"),
                     rbind.fill(data.frame(t(distribution_time_hosp_icu_nostepdown$day)),
                                data.frame(t(distribution_time_hosp_icu_nostepdown$probability)),
                                data.frame(t(distribution_time_icu_nostepdown_startinhosp$day)),
                                data.frame(t(distribution_time_icu_nostepdown_startinhosp$probability))))
path_ward_CC = list(pathway = "Ward, CC",
               proba = 347/42980,
               LoS = path_ward_CC)

path_CC_ward = cbind(Bed = c("CC", "CC_proba", "Ward", "Ward_proba"),
                     rbind.fill(data.frame(t(distribution_time_icu_stepdown_startinicu$day)),
                                data.frame(t(distribution_time_icu_stepdown_startinicu$probability)),
                                data.frame(t(distribution_time_stepdown_startinicu$day)),
                                data.frame(t(distribution_time_stepdown_startinicu$probability))))
path_CC_ward = list(pathway = "CC, Ward",
               proba = 2880/42980,
               LoS = path_CC_ward)

path_ward_CC_ward = cbind(Bed = c("Ward", "Ward_proba", "CC", "CC_proba", "Ward", "Ward_proba"),
                          rbind.fill(data.frame(t(distribution_time_hosp_icu_stepdown$day)),
                                     data.frame(t(distribution_time_hosp_icu_stepdown$probability)),
                                     data.frame(t(distribution_time_icu_stepdown_startinhosp$day)),
                                     data.frame(t(distribution_time_icu_stepdown_startinhosp$probability)),
                                     data.frame(t(distribution_time_stepdown_startinhosp$day)),
                                     data.frame(t(distribution_time_stepdown_startinhosp$probability))))
path_ward_CC_ward = list(pathway = "Ward, CC, Ward",
               proba = 4060/42980,
               LoS = path_ward_CC_ward)


pathways_all = list(path_CC, path_CC_ward, path_ward, path_ward_CC, path_ward_CC_ward)
names(pathways_all) = list.mapv(pathways_all, pathway)


#saveRDS(pathways_all, here::here("data", "pathways_all_cocin.rds"))
