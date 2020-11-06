#### Setup ############################

library(openxlsx)
library(tidyverse)
library(linelist)

# function for boxplot stats
give.n <- function(x){
  return(data.frame(y = 0, label = paste("n =", length(x)) ))
}


# read in the pathways data excel
data = read.xlsx("data/Copy of COVID_Movements2.xlsx", sheet = "no blanks")
data$DeathInstant = round(as.numeric(data$DeathInstant), 2)

# clean up data
data_clean = data %>%
  rename("DateDeath" = DeathInstant) %>%
  filter(!(is.na(Patient_ID))) %>%
  mutate_at(vars(contains("Date")),
            as.Date, origin = "1899-12-30") %>%
  mutate_at(vars(contains("Date")),
            as.POSIXct) %>%
  arrange(Patient_ID, StartDate) %>%
  select(-c("ID", "CombinedMins", "MoveSequence", "TransferDescription",
            "logik", "Move.sequence", "1st.layer", "2nd.layer", "3rd.layer",
            "4th.layer", "Descr"))


## Alternative analysis ##################

#What about if we put ICU and HDU in the same "critical care" box?
#this would boost our sample size a bit

#mutate ICU and HDU beds to a single "CC" label
data_clean_alt = data_clean %>%
  mutate(Bed_Ward = gsub("HDU", "CC", Bed_Ward)) %>%
  mutate(Bed_Ward = gsub("ICU", "CC", Bed_Ward)) %>%
  mutate(Bed_Ward = gsub("Bed CC, Bed CC", "Bed CC", Bed_Ward))

#need to correct LoS in CC for patients who do ICU -> HDU
#(since ICU -> HDU is now just CC -> CC)
del_row = c()
for (i in 2:nrow(data_clean_alt)) {
  if (data_clean_alt$Patient_ID[i] == data_clean_alt$Patient_ID[i-1] &&
      data_clean_alt$Bed_Ward[i] == data_clean_alt$Bed_Ward[i-1]){
    data_clean_alt$LOS_days[i] = data_clean_alt$LOS_days[i] + data_clean_alt$LOS_days[i-1]
    del_row = c(del_row, i-1)
  }
}
data_clean_alt = data_clean_alt[-del_row,]


# add a column indicating the final pathway of each patient
data_paths_alt = data_clean_alt %>%
  group_by(Patient_ID) %>%
  summarise(pathway = paste(Bed_Ward, sep= "", collapse = ", ")) %>%
  right_join(y = data_clean_alt, by = "Patient_ID")

#recreate a clean "move sequence"
data_paths_alt$Step = 1
for (i in 2:nrow(data_paths_alt)) {
  if (data_paths_alt$Patient_ID[i] == data_paths_alt$Patient_ID[i-1]){
    data_paths_alt$Step[i] = data_paths_alt$Step[i-1] + 1
  }
}


# summarise total LoS for each patient
data_paths_alt_summary = data_paths_alt %>%
  group_by(Patient_ID, pathway) %>%
  summarise(LoS = sum(LOS_days))

#IF WE WANT TOTAL HOSPITAL LOS:
#mean(data_paths_alt_summary$LoS)
#sd(data_paths_alt_summary$LoS)

# 5 different pathways, 137/168 just do "Ward" then discharged/die
table(data_paths_alt_summary$pathway)

#total hosp LoS by pathway
data_paths_alt_summary %>%
  group_by(pathway) %>%
  summarise(meanLoS = mean(LoS),
            sdLoS = sd(LoS))
mean(data_paths_alt_summary$LoS)
sd(data_paths_alt_summary$LoS)

# visualise LoS by pathway
ggplot(data_paths_alt_summary, aes(LoS, pathway)) +
  geom_boxplot() +
  stat_summary(fun.data = give.n, geom = "text", vjust = -0.5)


data_paths_alt_save = data_paths_alt %>%
  group_by(Bed_Ward) %>%
  summarise(n = n(), LoS = mean(LOS_days), SD = sd(LOS_days))

#write.csv(data_paths_alt_save, "average_los_by_outcome_uclh.csv", row.names = F)


#now let's break this up to see LoS for each step in each pathway
pathways_alt_breakdown = data.frame(pathway = names(table(data_paths_alt_summary$pathway)),
                                    n = as.numeric(table(data_paths_alt_summary$pathway)),
                                    step1_LoS = NA, step1_SD = NA,
                                    step2_LoS = NA, step2_SD = NA,
                                    step3_LoS = NA, step3_SD = NA)

for(i in 1:nrow(pathways_alt_breakdown)) {
  
  test_path = pathways_alt_breakdown$pathway[i]
  
  data_test_path = data_paths_alt %>%
    filter(pathway == test_path)
  
  for (j in unique(data_test_path$Step)) {
    
    data_test_path_step = data_test_path %>%
      filter(Step == j)
    LoS = mean(data_test_path_step$LOS_days) # I use mean here, but could use median?
    SD = sd(data_test_path_step$LOS_days)
    pathways_alt_breakdown[i, (j*2 + 1)] = round(LoS, 2)
    pathways_alt_breakdown[i, (j*2 + 2)] = round(SD, 2)
  }
  
}

pathways_alt_breakdown

#write.csv(pathways_alt_breakdown, "average_los_by_pathway_uclh.csv", row.names = F)


tt = data_paths_alt %>%
  filter(pathway == "Bed Ward")
step_LoS = mixdist::weibullpar(mean(tt$LOS_days), sd(tt$LOS_days), 0)
ggplot() +
  geom_histogram(data = tt, aes(LOS_days, y = ..density..)) + 
  geom_density(aes(rgamma(100000,
                                  shape = (mean(tt$LOS_days)/sd(tt$LOS_days))^2,
                                  rate = mean(tt$LOS_days)/sd(tt$LOS_days)^2))) +
  # geom_density(aes(rweibull(100000, shape = step_LoS$shape, scale = step_LoS$scale)))+
  theme_bw()


## Extract true admissions & bed occupancy curve ####

data_admissions = data_clean_alt %>%
  group_by(Patient_ID) %>%
  summarise(date = min(StartDate)) %>%
  ungroup() %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(value = n()) %>%
  ungroup() %>%
  mutate(value_type = "admission")
  
data_occupancy = data_clean_alt %>%
  mutate(StartDate = as.Date(StartDate)) %>%
  mutate(LOS_days = ceiling(LOS_days)) %>%
  mutate(Bed_Ward = replace(Bed_Ward, Bed_Ward == "Bed Ward", "Ward")) %>%
  mutate(Bed_Ward = replace(Bed_Ward, Bed_Ward == "Bed CC", "CC")) %>%
  select(StartDate, LOS_days, Bed_Ward)

hosp_occupancy = data.frame()

for(i in 1:nrow(data_occupancy)) {
  
  dates = seq(data_occupancy$StartDate[i],
              data_occupancy$StartDate[i]+data_occupancy$LOS_days[i],
              1)
  
  date = data.frame(date = dates, value_type = data_occupancy$Bed_Ward[i])
  
  hosp_occupancy = rbind(hosp_occupancy, date)
  
}

hosp_occupancy = hosp_occupancy %>%
  group_by(date, value_type) %>%
  summarise(value = n()) %>%
  ungroup() %>%
  rbind(data_admissions) %>%
  arrange(date)

#write.csv(hosp_occupancy, "uclh_occupancy.csv", row.names = F)

