# Store Demand Contest
# 30.10.2020
# David Teuscher

# Load packages
library(tidyverse)
library(caret)
library(DataExplorer)
library(lubridate)

# Read in the data

train <- read_csv("train.csv")
test <- read_csv("test.csv")



# Combine data
all_data_2 <- train %>% bind_rows(test)

# Check the data structure
glimpse(train)

overall_mean <- mean(all_data_2$sales, na.rm = TRUE)

# Change items and stores to a factor
all_data <- all_data %>%
    mutate(item = factor(item), 
           store = factor(store),
           month = lubridate::month(date), 
           weekday = weekdays(date)) %>%
    group_by(store, item, month) %>%
    mutate(mean_month_sales = mean(sales, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(store, item, weekday) %>%
    mutate(mean_weekday_sales = mean(sales,na.rm = TRUE)) %>%
    ungroup()
train <- all_data %>%
    filter(!is.na(sales))
test <- all_data %>%
    filter(is.na(sales))


# Check for missing values
plot_missing(train, ggtheme = theme_minimal())
plot_missing(test, ggtheme = theme_minimal())

# Check the distribution of the response variable
ggplot(train, aes(x = sales)) + 
    geom_histogram(fill = "deepskyblue4", bins = 20) +
    theme_minimal() +
    labs(x = "Sales", y = "Count")

# The distribution of the response variable is skewed right. Let's check if 
# occurs when splitting by store and items
ggplot(train, aes(x = sales)) + 
    geom_histogram(fill = "deepskyblue4", bins = 20) +
    theme_minimal() +
    facet_wrap(~store) +
    labs(x = "Sales", y = "Count")

ggplot(train, aes(x = sales)) + 
    geom_histogram(fill = "deepskyblue4", bins = 20) +
    theme_minimal() +
    facet_wrap(~item)
    labs(x = "Sales", y = "Count")

# Visualizations of stores over time
ggplot(all_data %>% filter(item == 1), aes(x= as.Date(date), y = sales)) +
    geom_line() + 
    facet_wrap(~store)


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
                   y = log1p(sales)) %>%
            select(ds, y, mean_month_sales, mean_weekday_sales)
        
        test_item <- test %>% 
            dplyr::filter(item == i, store == j) %>% 
            mutate(ds = date) %>%
            select(ds, mean_month_sales, mean_weekday_sales)
        
        m <- prophet(holidays.prior.scale = 4, changepoint_range=0.9,interval.width = 0.95,changepoint.prior.scale = 0.006,
                     daily.seasonality = TRUE, prior.scale = 0.5, yearly.seasonality = 4)
        m <- add_regressor(m, "mean_month_sales")
        m <- add_regressor(m, "mean_weekday_sales")
        m <- add_country_holidays(m, country_name = 'US')
        m <- add_seasonality(m, name='daily', period=60, fourier.order=5)
        m <- fit.prophet(m, train_item)
        forecast <- predict(m, test_item)
        preds_frame <- data.frame(sales = expm1(forecast$yhat))
        all_preds <- bind_rows(all_preds, preds_frame)
    }
}

all_preds_final <- all_preds[-1,]
all_preds_final <- data.frame(id = test$id, sales = all_preds_final)

write_csv(all_preds_final, "submission.csv")
