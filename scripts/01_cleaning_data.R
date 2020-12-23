#### Preamble ####
# Purpose: prepare and clean the Trip record data from NYC Taxi and Limousine comission(TLC). The data can be found at https://www1.nyc.gov/site/tlc/about/tlc-trip-record-data.page
# Author: Anees Shaikh
# Date: 21 December 2020
# Contact: anees.shaikh@mail.utoronto.ca 
# License: MIT
# Pre-requisites: 
# - Need to have downloaded a month of yellow taxi trip data and saved it to inputs/raw_data/
# - Need to have downloaded the location lookup dataset and saved it to the same folder
# - Don't forget to gitignore it!





#### Workspace setup ####
library(haven)
library(tidyverse)
library(ggplot2)
library(tidymodels)
library(caTools)

#Reading in location lookup table
location_lookup <- 
  read_csv("inputs/taxi+_zone_lookup.csv") %>% 
  rename(locationid = LocationID, zone = Zone) %>% 
  select(locationid, zone)



#raw file is stored in this location, change it to your path if using another
#adding two columns:
# 1) trip_length- how long the trip lasted. This could be an explanatory variable as shorter trips are unlikely to have high tips
# 2) total_without_tip- the total_amount already includes tips, this might be another explanatory variable as the presence of other surcharges may reduce tips

raw_data <- 
  read_csv("inputs/yellow_tripdata_2019-05.csv") %>% 
  filter(payment_type == 1, 
         trip_distance > 0,
         tpep_pickup_datetime < tpep_dropoff_datetime,
         passenger_count > 0) %>% 
  mutate(trip_length = round(difftime(tpep_dropoff_datetime,tpep_pickup_datetime, units = c("mins")))) %>% 
  mutate(total_without_tip = total_amount - tip_amount) %>% 
  mutate(tip_percentage = round(tip_amount/fare_amount*100)) %>% 
  mutate(tip_percentage_binary = ifelse(tip_percentage>20,"greater than 20","less than or equal to 20")) %>%
  mutate(tip_percentage_ordinal = ifelse(tip_percentage == 0, "no tip", 
                                         ifelse(tip_percentage == 20, "20%", 
                                                ifelse(tip_percentage == 25,"25%", 
                                                       ifelse(tip_percentage == 30, "30%","custom"))))) %>% 
  mutate(trip_distance = round(trip_distance)) %>% 
  mutate(speed = round((trip_distance*1.609)/(as.numeric(trip_length)/60))) %>% 
  filter(trip_distance > 0,
         trip_length > 0,
         speed > 0,
         speed < 150,
         trip_length < 300) %>% 
  drop_na()



#adding the locations of the pickup and dropoff
raw_data <- raw_data %>% 
  left_join(location_lookup, by = c("PULocationID" = "locationid")) %>% 
  rename(pickup_zone = zone) %>%
  left_join(location_lookup, by = c("DOLocationID" = "locationid")) %>% 
  rename(dropoff_zone = zone)



#extracting time from datetime object
raw_data$pickup_time <- as.numeric(format(raw_data$tpep_pickup_datetime, format = "%H"))


#Creating grouping for time
raw_data <- raw_data %>% 
  mutate(time_of_day = case_when(pickup_time >= 5 & pickup_time < 12 ~ "5am-12pm",
                                 pickup_time >= 12 & pickup_time < 16 ~ "12pm-4pm",
                                 pickup_time >= 16 & pickup_time < 20 ~ "4pm-8pm",
                                 pickup_time >= 20 | pickup_time < 5 ~ "8pm-5am"))






#EDA










#sampling our cleaned data for ease of use
df<- raw_data %>% 
  sample_n(100000) %>% 
  select(-tpep_pickup_datetime,
         -tpep_dropoff_datetime,
         -payment_type,
         -total_amount,
         -store_and_fwd_flag,
         -PULocationID,
         -DOLocationID)%>% drop_na()

#write this df, to be used for descriptive statistics

saveRDS(df, file = "output/data/tip_data.rds")



#df we are going to use for modelling
modellingdf <- df %>% 
  select(-tip_percentage_ordinal, 
         -tip_percentage_binary,
         -tip_percentage, 
         -pickup_time, 
         -pickup_zone,
         -dropoff_zone) %>% 
  filter(tip_amount < 20) %>% 
  mutate(rowid = row_number())

#split into test and train
set.seed(1080)
sample = sample.split(Y = modellingdf$rowid, SplitRatio = 0.8)
train = subset(modellingdf, sample == TRUE)
test = subset(modellingdf, sample == FALSE)




#write these dfs, to be used for modelling
saveRDS(train, "output/data/train_data.rds")
saveRDS(test, "output/data/test_data.rds")



 

#saving our output




