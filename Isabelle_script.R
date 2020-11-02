library(tidyverse)
library(ggplot2)
library(caret)
library(lubridate)
library(DataExplorer)


## read in data
train <- read_csv("train.csv")
test <- read_csv("test.csv")
  
## check for missing values - none!
plot_missing(train)
plot_missing(test)

## plot avg daily sales - looks like there's a yearly cycle
avg_daily <- train %>%
  group_by(date) %>% 
  summarise(avg = mean(sales))
ggplot(avg_daily, aes(date, avg)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, span = 0.1, color = "springgreen", size = 0.5)

## plot avg daily sales by store - yearly cycle looks to be consistent for all stores
avg_store <- train %>% 
  group_by(store, date) %>% 
  summarise(avg = mean(sales))
ggplot(avg_store, aes(date, avg)) +
  geom_line(aes(color = as.factor(store)), alpha = 0.25) +
  geom_smooth(aes(color = as.factor(store)), method = "loess", 
              se = FALSE, span = 0.1)

ggplot(avg_store, aes(avg)) +
  geom_density(aes(fill = as.factor(store)), alpha = 0.3)

## plot avg daily sales by item



