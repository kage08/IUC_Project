# Forecasting and Analysis of Housing Market Response to Hurricane Based on Time Series Hawkes Process and Spatio-Temporal Analysis
---
![image](https://user-images.githubusercontent.com/14800775/233230348-7e5ec137-2d84-4661-bb45-bdc1924dc8db.png)

[Paper](https://drive.google.com/file/d/1Gz9OQfuu2qGvD0H6kER18pgWzkVeTVlX/view?usp=sharing), [Poster](https://github.com/kage08/IUC_Project/blob/main/Poster.pdf)

## Abstract

Hurricane events challenge urban housing and infrastructure. Understanding and forecasting housing
price changes in response to hurricane events are fundamental building block
for understanding land use, transportation modeling, community risks, etc. This work aims to tackle this problem with a modified
Hawkes Process, to model both sequential time series and events
data with deep-learning-based modification to Hawkes Process. Our approach provides more accurate predictor of price changes after hurricane events and better forecasts of housing prices for upto one year.
We also leverage census data and spatio-temporal relations across zip codes to imporve models performance and gain crucial insights combined effect of hurricane events and demographics
for determining the price changes of an area.


## Important Results

### Data sources
![image](https://user-images.githubusercontent.com/14800775/233231623-13f1c8ce-747b-44d8-9313-fb5fa5f7b9b0.png)

We collected price data from zillow, hurricane data from NOAA, location data from openstreetmap and demographic data from US census.

## Model

![IUCModel](https://user-images.githubusercontent.com/14800775/233233455-c58b451b-d650-4bbf-9081-459d9e187092.png)

Model aggregates temporal information from price time-series, sequence of past urricane events and demographics data from census.
For classification of price changes, we train the model assuming a Hawkes Process. 
For forecasting we train the aggregated features as a regressor. We also perform forecasting using spatial locations of zip codes.

## Results

1. Accurate forecasting and price change classification

![image](https://user-images.githubusercontent.com/14800775/233233773-6c97a2fe-6b05-42cd-95c2-d259934c8670.png)

2. Exploratory analysis on importance of census features derived using Integrated Gradient method on the model

![image](https://user-images.githubusercontent.com/14800775/233233953-ba8af37f-79fd-4ae1-a5b0-4f2c83b55211.png)

3. Leveraging spatial structure imporves overall forecast performance.

![image](https://user-images.githubusercontent.com/14800775/233234141-aba3105f-ee88-4927-a18b-2b16c9ffc544.png)

## Authors
![Names](https://user-images.githubusercontent.com/14800775/233234842-9069018c-76b7-4947-9041-03ff4dd92428.png)

