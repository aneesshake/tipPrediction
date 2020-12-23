#### Preamble ####
# Purpose: Perform modelling on the cleaned data as well as some diagnostics
# Author: Anees Shaikh
# Date: 21 December 2020
# Contact: anees.shaikh@mail.utoronto.ca 
# License: MIT
# Pre-requisites: 
# - need to have ran 01_cleaning_data.R that will produce the cleaned file.

library(here)
library(tidyverse)
library(car)

#Loading in cleaned dataset

training_data <- readRDS(here::here("output/data","train_data.rds"))





#model with all variables
model <- lm(tip_amount~.,data = lineardf)


#stepwise regression
steppedLinearModel <- stats::step(model)

#VIF check
car::vif(steppedLinearModel)



finalmodel <- lm(tip_amount~VendorID + speed + fare_amount + RatecodeID + time_of_day, data = training_data)

saveRDS(finalmodel, "output/model/finalmodel.rds")

