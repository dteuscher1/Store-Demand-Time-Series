### McKay's Script ###
######################


### Reading Libraries
library(caret)
library(tidyverse)

### Reading in Data
train <- read_csv("train.csv")
test <- read_csv("test.csv")

### Changing Date Variable
train$date %>% as.Date(format = "%Y-%M-%D")


train <- train %>% mutate(year = as.numeric(substr(date,1,4)), 
                 month = as.numeric(substr(date,6,7)),
                 day = as.numeric(substr(date,9,10)),
                 date = as.Date(date, format = '%Y-%M-%D'),
                 graph_date = year + (month - .5)/12 + day/365)

train %>% filter(store == 1, item == 1) %>% ggplot() + 
  geom_line(mapping = aes(x = date, y = sales))


item_facet_plot <- train %>% ggplot() + 
  geom_line(aes(x = date, y = sales, color = store)) + facet_wrap(~item)

store_facet_plot <- train %>% ggplot() + 
  geom_line(aes(x = date, y = sales, color = item)) + facet_wrap(~store)
