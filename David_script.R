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
all_preds <- data.frame(sales = 0)
for(i in 1:50){
    for(j in 1:10){
        train_item <- train %>% 
            dplyr::filter(item == i, store == j) %>%
            mutate(ds = date,
                   y = sales) %>%
            select(ds, y)
        
        test_item <- test %>% 
            dplyr::filter(item == i, store == j) %>% 
            mutate(ds = date) %>%
            select(ds)
        
        m <- prophet( n_changepoints = 0)
        m <- add_country_holidays(m, country_name = 'US')
        m <- fit.prophet(m, train_item)
        forecast <- predict(m, test_item)
        preds_frame <- data.frame(sales = forecast$yhat)
        all_preds <- bind_rows(all_preds, preds_frame)
    }
}

all_preds_final <- all_preds[-1,]
all_preds_final <- data.frame(id = test$id, sales = all_preds_final)

write_csv(all_preds_final, "submission.csv")

