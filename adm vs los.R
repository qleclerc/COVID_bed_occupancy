library(openxlsx)
library(tidyverse)
library(linelist)
library(knitr)
library(reshape2)
library(rlist)
library(here)

source(here::here("code", "core_model_functions.R"))
source(here::here("code", "paths_table_to_blocks.R"))


hospital_data = read.csv(here::here("data", "uk_hospital_data.csv")) %>%
  mutate(date = as.Date(date))

regions_hospital_data = hospital_data %>%
  filter(!(geography %in% c("England", "Scotland", "Wales", "Northern Ireland")))

cov_curve = hospital_data %>%
  filter(geography == "England") %>%
  filter(value_type == "hospital_inc")

uclh_data = read.csv(here::here("data", "uclh_occupancy.csv")) %>%
  mutate(date = as.Date(date))

#need to extend cov_curve to fill in gaps and match max data date!
#in a future update, better to implement date column recognition in the model...
cov_curve_uclh = uclh_data %>%
  filter(value_type == "admission") %>%
  complete(date = seq.Date(min(date), max(uclh_data$date), by="day"), value_type) %>%
  mutate(value = replace_na(value, 0))

#default
res_average_LoS_uclh = multi_pathways_model(100, 
                                            cov_curve_uclh$value, 
                                            total_LoS_all_uclh, 
                                            length(cov_curve_uclh$date),
                                            verbose = T)
res_average_LoS_uclh$date = as.Date(cov_curve_uclh$date)

#+10% LoS
total_LoS_all_alt = total_LoS_all_uclh
total_LoS_all_alt$CC$LoS$mean = total_LoS_all_alt$CC$LoS$mean*1.30
total_LoS_all_alt$Ward$LoS$mean = total_LoS_all_alt$Ward$LoS$mean*1.30

res_average_LoS_uclh_los = multi_pathways_model(100, 
                                                cov_curve_uclh$value, 
                                                total_LoS_all_alt, 
                                                length(cov_curve_uclh$date),
                                                verbose = T)
res_average_LoS_uclh_los$date = as.Date(cov_curve_uclh$date)

#+10% admissions
res_average_LoS_uclh_adm = multi_pathways_model(100, 
                                                cov_curve_uclh$value*1.30, 
                                                total_LoS_all_uclh, 
                                                length(cov_curve_uclh$date),
                                                verbose = T)
res_average_LoS_uclh_adm$date = as.Date(cov_curve_uclh$date)


melt_results = function(results){
  
  results = results %>%
    select(-time) %>%
    melt(id.vars = "date")

  #separate mean and sd
  results_mean = results %>%
    filter(!grepl("sd", variable)) %>%
    arrange(variable) %>%
    mutate(date = as.Date(date))
  
  results_sd = results %>% 
    filter(grepl("sd", variable)) %>%
    arrange(variable) %>%
    mutate(date = as.Date(date))
  
  results_sd$min = results_mean$value - 1.96*results_sd$value
  results_sd$max = results_mean$value + 1.96*results_sd$value
  
  list(mean = results_mean, sd = results_sd)
  
}

res_average_LoS_uclh = melt_results(res_average_LoS_uclh)
res_average_LoS_uclh_los = melt_results(res_average_LoS_uclh_los)
res_average_LoS_uclh_adm = melt_results(res_average_LoS_uclh_adm)


p1 = ggplot() +
  geom_line(data = res_average_LoS_uclh$mean,
            aes(date, value, colour = variable, linetype = "Model")) +
  geom_ribbon(data = subset(res_average_LoS_uclh$sd, variable == "CC_sd"), 
              aes(date,
                  ymin = min,
                  ymax = max,
                  fill = "CC"),
              alpha = 0.3) +
  geom_ribbon(data = subset(res_average_LoS_uclh$sd, variable == "Ward_sd"),
              aes(date,
                  ymin = min,
                  ymax = max,
                  fill = "Ward"),
              alpha = 0.3) +
  geom_line(data = uclh_data %>% filter(value_type == "Ward"),
            aes(date, value, linetype = "Data", colour = "Ward")) +
  geom_line(data = uclh_data %>% filter(value_type == "CC"),
            aes(date, value, linetype = "Data", colour = "CC")) +
  labs(colour = "Bed type:", linetype = "Value:",
       x = "Time (days)", y = "Bed occupancy", title = "Default") +
  guides(fill = FALSE) +
  scale_x_date(breaks = "1 week", date_labels = format("%d %b %Y")) +
  theme_bw()


p2 = ggplot() +
  geom_line(data = res_average_LoS_uclh_los$mean,
            aes(date, value, colour = variable, linetype = "Model")) +
  geom_ribbon(data = subset(res_average_LoS_uclh_los$sd, variable == "CC_sd"), 
              aes(date,
                  ymin = min,
                  ymax = max,
                  fill = "CC"),
              alpha = 0.3) +
  geom_ribbon(data = subset(res_average_LoS_uclh_los$sd, variable == "Ward_sd"),
              aes(date,
                  ymin = min,
                  ymax = max,
                  fill = "Ward"),
              alpha = 0.3) +
  geom_line(data = uclh_data %>% filter(value_type == "Ward"),
            aes(date, value, linetype = "Data", colour = "Ward")) +
  geom_line(data = uclh_data %>% filter(value_type == "CC"),
            aes(date, value, linetype = "Data", colour = "CC")) +
  labs(colour = "Bed type:", linetype = "Value:",
       x = "Time (days)", y = "Bed occupancy", title = "Increase LoS") +
  guides(fill = FALSE) +
  scale_x_date(breaks = "1 week", date_labels = format("%d %b %Y")) +
  theme_bw()

p3 = ggplot() +
  geom_line(data = res_average_LoS_uclh_adm$mean,
            aes(date, value, colour = variable, linetype = "Model")) +
  geom_ribbon(data = subset(res_average_LoS_uclh_adm$sd, variable == "CC_sd"), 
              aes(date,
                  ymin = min,
                  ymax = max,
                  fill = "CC"),
              alpha = 0.3) +
  geom_ribbon(data = subset(res_average_LoS_uclh_adm$sd, variable == "Ward_sd"),
              aes(date,
                  ymin = min,
                  ymax = max,
                  fill = "Ward"),
              alpha = 0.3) +
  geom_line(data = uclh_data %>% filter(value_type == "Ward"),
            aes(date, value, linetype = "Data", colour = "Ward")) +
  geom_line(data = uclh_data %>% filter(value_type == "CC"),
            aes(date, value, linetype = "Data", colour = "CC")) +
  labs(colour = "Bed type:", linetype = "Value:",
       x = "Time (days)", y = "Bed occupancy", title = "Increase adm") +
  guides(fill = FALSE) +
  scale_x_date(breaks = "1 week", date_labels = format("%d %b %Y")) +
  theme_bw()


cowplot::plot_grid(p1, p2, p3)
