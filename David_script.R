# Store Demand Contest
# 30.10.2020
# David Teuscher

# Load packages
library(tidyverse)
library(caret)

# Read in the data

train <- read_csv("train.csv")
test <- read_csv("test.csv")

# Combine data
head(test)
all_data <- train %>% bind_rows(test)

# Check the data structure
glimpse(train)

# Change items and stores to a factor
all_data <- all_data %>%
    mutate(item = factor(item), 
           store = factor(store))

# Visualizations of stores over time
ggplot(all_data %>% filter(item == 1), aes(x= as.Date(date), y = sales)) +
    geom_line() + 
    facet_wrap(~store)

myControl <- trainControl(method = "repeatedcv",
                          number = 2,
                          repeats = 1)
obs <- sample(1:nrow(train), nrow(train) *.01)
small_train <- train[obs,]
rf.model <- train(sales~.,
                  data = small_train,
                  trControl = myControl,
                  method = "lm")
preds <- predict(rf.model, test)
frame <- data.frame(id = test$id, sales = preds)

library(StanHeaders)
library(prophet)
train_2 <- train %>% 
    mutate(ds = date,
           y = sales) %>%
    select(ds, y)
m <- prophet(train_2)
test_2 <- test %>%
    mutate(ds = date) %>%
    select(ds)
forecast <- predict(m, test_2)
future <- make_future_dataframe(m, periods = 90)

pred_frame <- data.frame(id = test$id, sales = forecast$yhat)
plot(m, forecast)

prophet_plot_components(m, forecast)


# I'm going to try things with a single store and item to begin to understand it 
train_store_1 <- train %>% 
    filter(store == 1, item == 1) %>%
    mutate(ds = date,
           y = sales) %>%
    select(ds, y)

test_2 <- test %>% 
    filter(store == 1, item == 1) %>% 
    mutate(ds = date) %>%
    select(ds)
small_model <- prophet(train_store_1)
forecast_small <- make_future_dataframe(small_model, periods = 90)
forecast <- predict(small_model, forecast_small)
plot(small_model, forecast) + add_changepoints_to_plot(m)


forecast <- predict(m, forecast_small)
plot(m, forecast)+ add_changepoints_to_plot(m)
m <- prophet( n_changepoints = 0)
m <- add_country_holidays(m, country_name = 'US')
m <- fit.prophet(m, train_store_1)
forecast <- predict(m, forecast_small)
plot(m, forecast) + add_changepoints_to_plot(m)
prophet_plot_components(m, forecast)
