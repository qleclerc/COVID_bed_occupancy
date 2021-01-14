
library(dplyr)
library(tidyr)
library(rio)

nhstrusts = readLines(here::here("data", "nhstrusts.txt"))
nhstrustshtml = gsub(" ", "%20" , nhstrusts)
nhstrustshtml = gsub("'", "%27" , nhstrustshtml)


#they actually have a nice api system in place, so might be a nicer way to do this!
#tried playing a bit with sprintf(), but doesn't work well with url format

#base query url
base_url <- "https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=nhstrust;areaName="

#available data types:
url_hosp_inc <- "&structure=%7B%22date%22:%22date%22,%22geography%22:%22areaName%22,%22value%22:%22newAdmissions%22%7D&format=csv"
url_hosp_prev <- "&structure=%7B%22date%22:%22date%22,%22geography%22:%22areaName%22,%22value%22:%22hospitalCases%22%7D&format=csv"
url_icu_prev <- "&structure=%7B%22date%22:%22date%22,%22geography%22:%22areaName%22,%22value%22:%22covidOccupiedMVBeds%22%7D&format=csv"

#create empty dataframe for storing
data <- data.frame()


#loop through all regions to extract data
for(i in nhstrustshtml){
  
  cat("Working on", i, "...\n")
  
  #create full urls
  url_reg_hospi_inc <- paste0(base_url, i, url_hosp_inc)
  url_reg_hosp_prev <- paste0(base_url, i, url_hosp_prev)
  url_reg_icu_prev <- paste0(base_url, i, url_icu_prev)
  
  #extract datasets
  data_reg_hospi_inc <- rio::import(url_reg_hospi_inc, format = "csv") %>%
    mutate(value_type = "hospital_inc") %>%
    arrange(date)
  
  data_reg_hosp_prev <- rio::import(url_reg_hosp_prev, format = "csv") %>%
    mutate(value_type = "hospital_prev") %>%
    arrange(date)
  
  data_reg_icu_prev <- rio::import(url_reg_icu_prev, format = "csv") %>%
    mutate(value_type = "icu_prev") %>%
    arrange(date)
  
  #fix hospital numbers
  data_reg_hosp_prev$value = rbind(data_reg_hosp_prev, data_reg_icu_prev) %>%
    select(date, value_type, value) %>%
    spread(value_type, value) %>%
    mutate(icu_prev = replace_na(icu_prev, 0)) %>%
    mutate(diff = hospital_prev - icu_prev) %>%
    select(diff) %>%
    na.omit %>%
    pull
  
  
  #combine & save datasets
  data <- rbind(data, data_reg_hospi_inc,
                data_reg_hosp_prev, data_reg_icu_prev)
  
}

#some final tweaks
#notably need to deal with missing days, assigning a 0 value for admissions
#this makes the model work consistently with this
data <- data %>%
  select(date, geography, value_type, value) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  complete(date = seq.Date(min(date), max(date), by="day"), geography, value_type)

#ugly but no idea how to implement in dplyr...
datat = data %>%
  filter(value_type == "hospital_inc") %>%
  mutate(value = replace_na(value, 0))

datat2 = data  %>% 
  filter(value_type != "hospital_inc")

data = rbind(datat, datat2) %>%
  arrange(date)

# data = data %>% filter(date > as.Date("2020-09-01"))
# data = data %>% filter(date < as.Date("2020-12-25"))


write.csv(data, here::here("data", "uk_hospital_data_nhstrust2.csv"), row.names = F)
