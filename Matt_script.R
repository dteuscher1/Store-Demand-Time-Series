#####################
### Matt's Script ###
#####################

# Reading Libraries
library(caret)
library(DataExplorer)
library(forecast)
library(lubridate)
library(tidyverse)
library(zoo)

# Reading in Data
test <- read_csv('test.csv') %>% 
  mutate(day = weekdays(date),
         year = year(date),
         yearmon = as.yearmon(date))

train <- read_csv('train.csv') %>% 
  mutate(day = weekdays(date),
         year = year(date),
         yearmon = as.yearmon(date))



