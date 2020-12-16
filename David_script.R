# Store Demand Contest
# 30.10.2020
# David Teuscher

# Load packages
library(tidyverse)
library(caret)
library(DataExplorer)
library(lubridate)
library(StanHeaders)
library(prophet)

# Read in the data
train <- read_csv("train.csv")
test <- read_csv("test.csv")

# Combine data
all_data <- train %>% bind_rows(test)

# Check the data structure
glimpse(train)


# Change items and stores to a factor
# Calculate average sales for each month and each weekday
all_data <- all_data %>%
    mutate(item = factor(item), 
           store = factor(store),
           month = lubridate::month(date), 
           weekday = weekdays(date),
           year = year(date)) %>%
    group_by(store, item, month) %>%
    mutate(mean_month_sales = mean(sales, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(store, item, weekday) %>%
    mutate(mean_weekday_sales = mean(sales,na.rm = TRUE)) %>%
    ungroup()

# Split back into the train and test set
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
    facet_wrap(~item) +
    labs(x = "Sales", y = "Count")
    
# Stores are skewed right a lot. Items are less skewed right, but I think
# it would be good for a transformation as well
    
ggplot(train, aes(x = log1p(sales))) +
    geom_histogram(fill = "deepskyblue4", bins = 20) +
    theme_minimal() +
    labs(x = "Sales", y = "Count")

ggplot(train, aes(x = log1p(sales))) + 
    geom_histogram(fill = "deepskyblue4", bins = 20) +
    theme_minimal() +
    facet_wrap(~store) +
    labs(x = "Sales", y = "Count")

ggplot(train, aes(x = log1p(sales))) + 
    geom_histogram(fill = "deepskyblue4", bins = 20) +
    theme_minimal() +
    facet_wrap(~item) + 
    labs(x = "Sales", y = "Count")

# It becomes a little left skewed when doing this, but it is better than what 
# it previously was and outliers won't has as big of an impact. Also, log1p was
# used because there was a value that had 0 sales. 

# Average total sales per day over time
avg_sales <- train %>%
    group_by(date) %>%
    summarize(avg_sale = mean(sales))

ggplot(avg_sales, aes(x = date, y = avg_sale)) +
    geom_line(size =1, color = "deepskyblue4") +
    theme_minimal() +
    labs(x = "Date", y = "Average Sales")
# The average total sales seems to be increasing over time

# Average sales per month
avg_month <- train %>%
    group_by(year, month) %>%
    summarize(avg_sale = mean(sales)) %>%
    mutate(yr_month = fct_reorder2(factor(paste0(year, "-", month)), desc(year), desc(month)))
ggplot(avg_month, aes(x = yr_month , y = avg_sale)) +
    geom_point(size =4, color = "deepskyblue4", alpha = .5) +
    geom_line(aes(group = 1), color = "deepskyblue4") + 
    theme_minimal() +
    labs(x = "Date", y = "Average Sales") +
    theme(axis.text.x = element_text(angle = 90))

# Average sales per year
avg_sales_year <- train %>%
    group_by(year) %>%
    summarize(avg_sale = mean(sales))
ggplot(avg_sales_year, aes(x = year, y = avg_sale)) +
    geom_point(size =4, color = "deepskyblue4", alpha = .5) +
    geom_line(aes(group = 1), size = 1.5, color = "deepskyblue4") +
    theme_minimal() +
    labs(x = "Date", y = "Average Sales")

avg_sales_weekday <- train %>%
    group_by(weekday) %>%
    summarise( 
        n=n(),
        mean=mean(sales),
        sd=sd(sales)) %>%
    mutate(se=sd/sqrt(n))  %>%
    mutate(ic=se * qt((1-0.05)/2 + .5, n-1))
ggplot(avg_sales_weekday, aes(x = weekday, y = mean)) +
    geom_point(size =2, color = "deepskyblue4", alpha = .5) +
    geom_errorbar(aes(x=weekday, ymin=mean-ic, ymax=mean+ic), size = 1, color = "deepskyblue4") +
    theme_minimal() +
    labs(x = "Weekday", y = "Average Sales")

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
        m <- add_seasonality(m, name='twomonth', period=60, fourier.order=5)
        m <- fit.prophet(m, train_item)
        forecast <- predict(m, test_item)
        preds_frame <- data.frame(sales = expm1(forecast$yhat))
        all_preds <- bind_rows(all_preds, preds_frame)
    }
}

all_preds_final <- all_preds[-1,]
all_preds_final <- data.frame(id = test$id, sales = all_preds_final)

write_csv(all_preds_final, "submission.csv")
