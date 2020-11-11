# Loading libraries
library(tidyverse)
library(caret)
library(StanHeaders)
library(prophet)
library(zoo)
library(lubridate)
library(parallel)

train <- read_csv('train.csv') %>% mutate(ds = date,
                                          y = sales) %>% 
  select(-date, -sales) 

test <- read_csv('test.csv') %>% mutate(ds = date) %>% 
  select(-date)

train_splitting <- train %>% group_split(item, store) %>%
  lapply(., function(adf){select(adf, ds, y)})

prophet.predictions <- function(df)
{
  df <- df %>% mutate(y = log1p(y))
  
  m <- prophet(n_changepoints = 0)
  m <- add_country_holidays(m, country_name = 'US')
  m <- fit.prophet(m, df)
  
  future <- make_future_dataframe(m, periods = 90)
  forecast <- predict(m, future)
  forecast_final <- expm1(xts::last(forecast[, "yhat"], 90))
  return(forecast_final)
}

# Implement Parallel Processing
# prediction_final <- as.data.frame(mclapply(train_splitting, prophet.predictions, mc.cores = 8))

# Non-Parallel Processing
prediction_final <- unlist(lapply(train_splitting[1:10], prophet.predictions))

submission <- data.frame(id = test %>% pull(id) %>% head(900),
                         sales = prediction_final)
