# Loading libraries
library(tidyverse)
library(caret)
library(StanHeaders)
library(prophet)
library(zoo)
library(lubridate)

train <- read_csv('train.csv') %>% mutate(ds = date,
                                          y = sales,
                                          day = weekdays(date),
                                          year = year(date),
                                          yearmon = as.yearmon(date)) %>% 
  select(-date, -sales)

test <- read_csv('test.csv') %>% mutate(ds = date,
                                        day = weekdays(date),
                                        year = year(date),
                                        yearmon = as.yearmon(date)) %>% 
  select(-date)

