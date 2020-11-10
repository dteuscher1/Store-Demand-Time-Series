install.packages(c("prophet", "svMisc"))
library(tidyverse)
library(ggplot2)
library(caret)
library(lubridate)
library(DataExplorer)
library(prophet)
library(svMisc)


## read in data
train <- read_csv("train.csv") %>% mutate(store = as.factor(store),
                                          item = as.factor(item))
test <- read_csv("test.csv")%>% mutate(store = as.factor(store),
                                       item = as.factor(item))
  
## check for missing values - none!
# plot_missing(train)
# plot_missing(test)
# 
# ## plot avg daily sales - looks like there's a yearly cycle
# avg_daily <- train %>%
#   group_by(date) %>% 
#   summarise(avg = mean(sales))
# ggplot(avg_daily, aes(date, avg)) +
#   geom_line() +
#   geom_smooth(method = "loess", se = FALSE, span = 0.1, color = "springgreen", size = 0.5)
# 
# ## plot avg daily sales by store - yearly cycle looks to be consistent for all stores
# avg_store <- train %>% 
#   group_by(store, date) %>% 
#   summarise(avg = mean(sales))
# ggplot(avg_store, aes(date, avg)) +
#   geom_line(aes(color = as.factor(store)), alpha = 0.25) +
#   geom_smooth(aes(color = as.factor(store)), method = "loess", 
#               se = FALSE, span = 0.1)
# 
# ggplot(avg_store, aes(avg)) +
#   geom_density(aes(fill = as.factor(store)), alpha = 0.3)
# 
# ## plot avg daily sales by item - yearly cycle looks to be consistent for all items too
# avg_item <- train %>%
#   group_by(item, date) %>%
#   summarize(avg = mean(sales))
# p <- ggplot(avg_item, aes(date, avg)) +
#   geom_line() +
#   geom_smooth(method = "loess", se = FALSE, span = 0.1, color = "springgreen", size = 0.5) +
#   facet_wrap(~item, ncol = 4)
# ggsave("ItemSales.pdf", device = pdf, plot = p, height = 15, width = 10, units = "in")



## train a model to run for every store with yearly seasonality
all_preds <- data.frame(id = integer(), sales = numeric())
for (i in 1:10) {
  progress(i * 10)
  Sys.sleep(0.01)
  
  item_train <- train %>%
    filter(store == i) %>%
    mutate(ds = date,
           y = sales) %>%
    select(c(ds, y))
  
  ids <- test %>%
    filter(store == i) %>%
    pull(id)
  
  item_test <- test %>%
    filter(store == i) %>%
    mutate(ds = date) %>%
    select(ds)
  
  model <- prophet()
  model <- fit.prophet(model, item_train)
  preds <- predict(model, item_test)
  preds_df <- data.frame(id = ids, sales = preds$yhat)
  all_preds <- bind_rows(all_preds, preds_df) %>%
    arrange(id)
}

write_csv(all_preds)

