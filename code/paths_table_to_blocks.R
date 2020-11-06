library(dplyr)
library(tidyr)
library(rlist)

#load in COCIN

# replaced as of 05/10/2020 - now the model can take as input the empiric distributions, which the new script loads
# ## Pathways ####
# 
# table_pathways = read.csv(here::here("data", "average_los_by_pathway.csv"))
# #index is 0 all, 1 discharged, 4 died
# 
# ## All patients 
# 
# paths_all = table_pathways %>%
#   filter(dsterm == 0)
# 
# paths_all_p = sum(paths_all$n)
# 
# pathways_all = list()
# 
# for (i in 1:nrow(paths_all)) {
#   
#   steps = paths_all[i,] %>%
#     select(contains("step"))
#   empty_steps = steps %>%
#     sapply(function(e) all(is.na(e)))
#   steps = steps[!empty_steps]
#   
#   steps_mean = steps %>%
#     select(!contains("sd")) %>%
#     as.numeric(.)
#   
#   steps_sd = steps %>%
#     select(contains("sd")) %>%
#     as.numeric(.)
#   
#   steps_names = gsub("Bed ", "", paths_all[i, "pathway"])
#   
#   new_path = list(pathway = steps_names,
#                   proba = paths_all[i,"n"]/paths_all_p,
#                   #death = F,
#                   LoS = data.frame(Bed = unlist(strsplit(steps_names, ", ")),
#                                    mean = steps_mean,
#                                    sd = steps_sd,
#                                    stringsAsFactors = F))
#   
#   pathways_all[[i]] = new_path
#   
# }
# 
# names(pathways_all) = list.mapv(pathways_all, pathway)
# 
# 

pathways_all = readRDS(here::here("data", "pathways_all_cocin.rds"))

# ## Averaged to single bed LoS ####

table_LoS = read.csv(here::here("data", "average_LoS_by_outcome_cocin.csv"))

## All patients

LoS_all = table_LoS %>%
  filter(dsterm == 0)

LoS_CC_all = list(pathway = "CC",
                  proba = LoS_all$n[1]/(LoS_all$n[1] + LoS_all$n[2]),
                  #death = F,
                  LoS = data.frame(Bed = "CC",
                                   mean = LoS_all$total_los[1],
                                   sd = LoS_all$total_sd[1],
                                   stringsAsFactors = F))

LoS_ward_all = list(pathway = "Ward",
                    proba = LoS_all$n[2]/(LoS_all$n[1] + LoS_all$n[2]),
                    #death = F,
                    LoS = data.frame(Bed = "Ward",
                                     mean = LoS_all$total_los[2],
                                     sd = LoS_all$total_sd[2],
                                     stringsAsFactors = F))

total_LoS_all = list(LoS_CC_all, LoS_ward_all)
names(total_LoS_all) = list.mapv(total_LoS_all, pathway)



#######################################################################

#load in UCLH

## Pathways ####

table_pathways2 = read.csv(here::here("data", "average_los_by_pathway_uclh.csv"))
#index is 0 all, 1 discharged, 4 died

## All patients 

paths_all = table_pathways2

paths_all_p = sum(paths_all$n)

pathways_all_uclh = list()

for (i in 1:nrow(paths_all)) {
  
  steps = paths_all[i,] %>%
    select(contains("step"))
  empty_steps = steps %>%
    sapply(function(e) all(is.na(e)))
  steps = steps[!empty_steps]
  
  steps_mean = steps %>%
    select(!contains("sd")) %>%
    as.numeric(.)
  
  steps_sd = steps %>%
    select(contains("sd")) %>%
    as.numeric(.)
  
  steps_names = gsub("Bed ", "", paths_all[i, "pathway"])
  
  new_path = list(pathway = steps_names,
                  proba = paths_all[i,"n"]/paths_all_p,
                  #death = F,
                  LoS = data.frame(Bed = unlist(strsplit(steps_names, ", ")),
                                   mean = steps_mean,
                                   sd = steps_sd,
                                   stringsAsFactors = F))
  
  pathways_all_uclh[[i]] = new_path
  
}

names(pathways_all_uclh) = list.mapv(pathways_all_uclh, pathway)


## Averaged to single bed LoS ####

table_LoS2 = read.csv(here::here("data", "average_LoS_by_outcome_uclh.csv"))

## All patients


LoS_CC_all = list(pathway = "CC",
                  proba = table_LoS2$n[1]/(table_LoS2$n[1] + table_LoS2$n[2]),
                  #death = F,
                  LoS = data.frame(Bed = "CC",
                                   mean = table_LoS2$LoS[1],
                                   sd = table_LoS2$SD[1],
                                   stringsAsFactors = F))

LoS_ward_all = list(pathway = "Ward",
                    proba = table_LoS2$n[2]/(table_LoS2$n[1] + table_LoS2$n[2]),
                    #death = F,
                    LoS = data.frame(Bed = "Ward",
                                     mean = table_LoS2$LoS[2],
                                     sd = table_LoS2$SD[2],
                                     stringsAsFactors = F))

total_LoS_all_uclh = list(LoS_CC_all, LoS_ward_all)
names(total_LoS_all_uclh) = list.mapv(total_LoS_all_uclh, pathway)

