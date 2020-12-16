# Store Item Demand Forcasting Challenge

This repository shows work done to predict the sales of various items in various stores over a 3 month period for the Store Item Demand Forcasting Kaggle competition (https://www.kaggle.com/c/demand-forecasting-kernels-only). 

The .Rmd file with the full analysis and the detailed write-up is available. A notebook was created with the write-up on Kaggle and can be found [here](https://www.kaggle.com/davidkt2/time-series-modeling-with-prophet). 

The data for this analysis includes the quantity of an item sold at a store each day from January 1, 2013 to December 31, 2017. The goal is to predict the quantity of each item sold at each store for the next 90 days. There are a total of 50 different unknown items and 10 different stores. There is no information given about what kind of items these are or any information about the different stores. This data, both the traning and test sets, can be accessed from the Kaggle competition. It is also available in this respository as well. 

There were no missing values in the data, so there was no work done to deal with missing values. The distribution of sales appeared to be skewed to the right, so a `log1p` transformation was applied to the quantity of sales. The `log1p` was used because there was an instance or two where an item had sales of 0 for a day. 

When looking at the trends over time, the average sales increases from year to year. There was also a positive trend for the days of the week, with sales being the lowest on Monday and getting increasingly higher throughout the week and being the highest on the weekend. There were trends from month to month as well as it seems the average sales increased from January until July and then decreased until the end of the year, with some spikes around holidays at the end of the year. 

A time series model was fit using the `prophet` package in R. The model included yearly, weekly, daily, and two-month seasonality. The effect for holidays was also included in the model. Additional regressors of the average sales for the day of the week and the average sales for the month were included as part of the model as well. 

The result of the model was a SMAPE (symetric mean absolute percentage error) of 14.36478 for the public score and a SMAPE of 13.0312 for the private score. Using the private score, that would be in the 57th percentile for the competition, with the lowest SMAPE being 12.58015. 

Although the model doesn't do the best in the competition, it is applicapble to many other time series problems and I feel I could use a similar model approach for a different time series problem. The `prophet` package gives a lot of additional flexibility to fitting a model, which makes it ideal for dealing with different types of time series problems. If more information was available about the types of items used or where the different stores were located, the models could also be customized more for each individual store and item, which possibly could improve the predictions in this scenario. The ability to have flexible models for time series allows this to be useful for other time series projects. 

If there are any questions or comments about the analysis and work done, feel free to email me at david.teuscher.96@gmail.com
