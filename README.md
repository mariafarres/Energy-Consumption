## Energy Consumption for Smart Homes - Project Description 

**Task goal:** build predictive models to forecast future electricity consumption in Smart Homes and demonstrate the client how data can be used to help households make decisions regarding power usage. 

**Data characteristics:** a multivariate time series containing power usage records of a Smart Home. The data was collected between December 2006 and November 2010 and observations of power consumption within the household were collected every minute
-> Source: http://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption


## Technical Approach
**Language used:** R programming

**1. PRE-PROCESSING (DATA QUALITY)**
- SQL queries
- Data types conversion and scaling
- Missing values treatment: calendar heat + pad + NA interpolation

- Feature engineering
- Group_by to visualize different data granularities 
- Filter for descriptive analysis
  * VISUALIZATION TOOL: PLOT_LY

- Outliers treatment


**2. TIME SERIES CREATION & DECOMPOSITION**

**3. REMAINDER ANALYSIS & VISUALIZATION**


**4. FORECASTING**
- Model ETS
- Model Arima
- Model Holt Winters
- Model Hybrid Forecasting

**5. ACCURACY AND CONFIDENCE ANALYSIS**
  * VISUALIZATION TOOL: autoplot

**6. PREDICTION**
