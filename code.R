#### SET THE ENVIRONMENT  ####

## load libraries

pacman::p_load("RMySQL","dplyr", "tidyr","lubridate","esquisse","padr","imputeTS",
               "ggplot2", "chron","plotly", "forecast", "zoo")


## read SQL 
con = dbConnect(MySQL(),
                user='deepAnalytics',
                password='Sqltask1234!',
                dbname='dataanalytics2018',
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com') #connecting SQL

dbListTables(con)    


#### Create a query requesting specific information from the original db 

query <- "
SELECT * FROM yr_2006
UNION ALL
SELECT * FROM yr_2007
UNION ALL
SELECT * FROM yr_2008
UNION ALL
SELECT * FROM yr_2009
UNION ALL
SELECT * FROM yr_2010
"

## df with the information in the query 
rawtable <- dbGetQuery(con, query)

## check data
str(ratable)
head(rawtable)
tail(rawtable)





#### PRE-PROCESS  ####

## scale variables 
rawtable$Global_active_power <- round(rawtable$Global_active_power*1000/60, digits = 4)
rawtable$Global_reactive_power <- round(rawtable$Global_reactive_power*1000/60, digits = 4)

#### convert data types ####
summary(rawtable)
str(rawtable)
rawtable$Date <- lubridate:: ymd(rawtable$Date)
rawtable$DateTime <- lubridate:: ymd_hms(paste(rawtable$Date, rawtable$Time))
 

#### NA's ####
sum(is.na(rawtable))

## calendar heat to see missing records 
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")

calendarheat.NA <- calendarHeat(rawtable$Date, rawtable$Global_active_power)


## pad
pad.table <- pad(rawtable, interval = NULL, by = "DateTime", break_above = 3) #adding blank rows
sum(is.na(pad.table)) # check blank rows presence
pad.table$Date <- lubridate:: ymd(pad.table$Date)


## arrange df
pad.table <- arrange(pad.table, DateTime)


#### imputing values in missing values 
imputed.table <- na.interpolation(pad.table, option = "linear")
imputed.table$Date <- NULL
imputed.table$Time <- NULL
sum(is.na(imputed.table))


#### Feature Engineering ####
imputed.table$Submeterings <- imputed.table$Sub_metering_1+imputed.table$Sub_metering_2+imputed.table$Sub_metering_3
imputed.table$other_areas <- imputed.table$Global_active_power-imputed.table$Submeterings
imputed.table$year <- year(imputed.table$DateTime)
imputed.table$month <- month(imputed.table$DateTime)
imputed.table$week <- week(imputed.table$DateTime)
imputed.table$day <- day(imputed.table$DateTime)
imputed.table$hour <- hour(imputed.table$DateTime)
imputed.table$minute <- minute(imputed.table$DateTime)

imputed.table$Season <- ifelse(imputed.table$month == 12|imputed.table$month == 1|imputed.table$month == 2,"Winter",
                     ifelse(imputed.table$month == 3|imputed.table$month == 4|imputed.table$month == 5, "Spring",
                            ifelse(imputed.table$month == 6|imputed.table$month == 7|imputed.table$month == 8, "Summer", "Autumn"))) #season info


#### Sampling for visualization ####

aggregated_df <- c()
plots.gap.sub <- c()
plots.gap.sumsub <- c()

##### Loop to study granularity ####

granularity <- c("year", "month", "day", "hour", "30 mins")

for(g in granularity){
  aggregated_df[[g]] <- imputed.table %>%
    group_by(DateTime=floor_date(DateTime, g)) %>%
    dplyr::summarize_at(vars(
      Sub_metering_1,
      Sub_metering_2,
      Sub_metering_3,
      Global_active_power, 
      other_areas,
      Submeterings,
      year,
      month,
      week,
      day
    ),
    funs(sum))

  ##### Plots to study granularity ####
  
  plots.gap.sub[[g]] <- plot_ly(aggregated_df[[g]], x = ~aggregated_df[[g]]$DateTime, y = ~aggregated_df[[g]]$Sub_metering_1, 
          name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
    add_trace(y = ~aggregated_df[[g]]$Sub_metering_2,
              name = 'Laundry Room', mode = 'lines') %>%
    add_trace(y = ~aggregated_df[[g]]$Sub_metering_3,
              name = 'Water Heater & AC', mode = 'lines') %>%
    add_trace(y = ~aggregated_df[[g]]$other_areas,
              name = 'Other Areas', mode = 'lines') %>%
    layout(title = paste("Power Consumption per sub-meter by", g),
           xaxis = list(title = "Time"),
           yaxis = list (title = "Power (watt-hours)"))

  #### PLOT -> Global Power vs Submetering Records ####
  plots.gap.sumsub[[g]] <- ggplot(data = aggregated_df[[g]], aes(x = DateTime)) +
    geom_line(aes(y = Submeterings, color = "Submetering")) +
    geom_line(aes(y = Global_active_power, color = "Global Power")) +
    theme_minimal()+
    labs(title = paste("Global power vs submetering records", g),
         x = "Time",
         y = "Power")
  
  }




variables <- c("Global_active_power", "Sub_metering_1", "Sub_metering_2", "Sub_metering_2", "Sub_metering_3","other_areas")
for(v in variables){


#### seasonaly grouped ####
seasonaly <- imputed.table %>% group_by(Season) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power, 
                           other_areas, Submeterings, year, month, week, day), funs(sum))  #grouping by season
colnames(seasonaly)[1] <- "DateTime"  





#### visualization ####
#### PLOT -> Global Power vs Submetering Records ####
plots[[g]] <- ggplot(data = aggregated_df[[g]], aes(x = DateTime)) +
  geom_line(aes(y = Submeterings, color = "Submetering")) +
  geom_line(aes(y = Global_active_power, color = "Global Power")) +
  theme_minimal()+
  labs(title = paste("Global power vs submetering records", g),
       x = "Time",
       y = "Power")



#### PLOT -> Power Consumption per sub-meter by month ####
plot_ly(aggregated_df[[g]], x = ~aggregated_df[[g]]$DateTime, y = ~aggregated_df[[g]]$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~aggregated_df[[g]]$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~aggregated_df[[g]]$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~aggregated_df[[g]]$other_areas,
            name = 'Other Areas', mode = 'lines') %>%
  layout(title = "Power Consumption per sub-meter by month",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#### PLOT -> Power Consumption per submeter by week ####
plot_ly(weekly, x = ~aggregated_df[["week"]]$DateTime, y = ~weekly$Sub_metering_1, #plot week
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~weekly$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~weekly$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~weekly$other_areas,
            name = 'Other Areas', mode = 'lines') %>%
  layout(title = "Power Consumption per submeter by week",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))





#### Filtering visualizations  #### 

#### PLOT -> VACATION PERIODS  ####

#### 2007 ####

vacation2007 <- filter(imputed.table, year== "2007", month == "8", day >= 16 & day <= 20)

plot_ly(vacation2007, x = ~vacation2007$DateTime, y = ~vacation2007$Sub_metering_1, #plot week
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~vacation2007$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~vacation2007$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~vacation2007$other_areas,
            name = 'Other Areas', mode = 'lines') %>%
  layout(title = "Power Consumption per submeter in vacation",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#### 2008 ####
vacation2008 <- filter(imputed.table, year== "2008", month == "8")
plot_ly(vacation2008, x = ~vacation2008$DateTime, y = ~vacation2008$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~vacation2008$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~vacation2008$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~vacation2008$other_areas,
            name = 'Other Areas', mode = 'lines') %>%
  layout(title = "Power Consumption per submeter in vacation",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


#### 2009 ####
vacation2009 <- filter(imputed.table, year== "2009", month == "8", day >= 1 & day <= 14)
plot_ly(vacation2009, x = ~vacation2009$DateTime, y = ~vacation2009$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~vacation2009$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~vacation2009$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~vacation2009$other_areas,
            name = 'Other Areas', mode = 'lines') %>%
  layout(title = "Power Consumption per submeter in vacation",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))




#### PLOT -> SEASONS ####
plot_ly(seasonaly, x = ~seasonaly$DateTime, y = ~seasonaly$Sub_metering_1, #plot Summer 2009
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~seasonaly$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~seasonaly$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~seasonaly$other_areas,
            name = 'Other Areas', mode = 'lines') %>%
  layout(title = "Seasonal Power Consumption per sub-meter",
         xaxis = list(title = "Season"),
         yaxis = list (title = "Power (watt-hours)"))







#### Forecasting ####

#### create time series objects ####
ts_month <- ts(monthly, start = c(2006,12), frequency = 12)
ts_week <- ts(weekly, start = c(2006,12), frequency = 52)
ts_day <- ts(daily, start = c(2006,12), frequency = 365)
ts_hour <- ts(hourly, start = c(2006,12), frequency = 8760)
ts_min <- ts(imputed.table, frequency = 24*60*7)


## plot possibly predictable vars ##

  #### MONTHLY ts / variable ####
monthly_gap <- ts_month [ ,"Global_active_power"] 
monthly_S1 <- ts_month [ ,"Sub_metering_1"]
monthly_S2 <- ts_month [ ,"Sub_metering_2"]
monthly_S3 <- ts_month [ ,"Sub_metering_3"]
monthly_OA <- ts_month [ ,"other_areas"]



#### Monthly decomposition visualizations & remainder analysis ####

monthly_gap_decomposed <- monthly_gap %>% stl(s.window = 12)

monthly_gap_decomposed %>%
  autoplot() + xlab("Year") +
  ggtitle("Monthly decomposition
          of Global Active Power")

m_gap_random <- monthly_gap_decomposed$time.series
remainder <- monthly_gap_decomposed$time.series[ ,3]
random_analysis <- as.data.frame(sum(abs(remainder)))
colnames(random_analysis) <- "gap"






ts_month [,"Global_active_power"] %>% stl(s.window = 12) %>%
  autoplot() + xlab("Year") +
  ggtitle("Monthly decomposition
          of Global Active Power")




ts_month [,"Sub_metering_1"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Monthly decomposition
          of Sub_metering_1")


ts_month [,"Sub_metering_2"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Monthly decomposition
          of Sub_metering_2")


ts_month [,"Sub_metering_3"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Monthly decomposition
          of Sub_metering_3")

ts_month [,"other_areas"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Monthly decomposition
          of other_areas")


  #### WEEKLY ####

#### WEEKLY ts / variable ####
weekly_gap <- ts_week [ ,"Global_active_power"] 
weekly_S1 <- ts_week [ ,"Sub_metering_1"]
weekly_S2 <- ts_week [ ,"Sub_metering_2"]
weekly_S3 <- ts_week [ ,"Sub_metering_3"]
weekly_OA <- ts_week [ ,"other_areas"]

weekly_gap %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Weekly decomposition
          of Global Active Power")


weekly_S1 %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Weekly decomposition
          of Sub_metering_1")


ts_week [,"Sub_metering_2"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Weekly decomposition
          of Sub_metering_2")


ts_week [,"Sub_metering_3"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Weekly decomposition
          of Sub_metering_3")

ts_week [,"other_areas"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Weekly decomposition
          of other_areas")



  #### DAILY ####

ts_day [,"Global_active_power"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Daily decomposition
          of Global Active Power")


ts_day [,"Sub_metering_1"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Daily decomposition
          of Sub_metering_1")


ts_day [,"Sub_metering_2"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Daily decomposition
          of Sub_metering_2")


ts_day [,"Sub_metering_3"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Daily decomposition
          of Sub_metering_3")

ts_day [,"other_areas"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Daily decomposition
          of other_areas")


  #### HOURLY  ####

ts_hour [,"Global_active_power"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Hourly decomposition
          of Global Active Power")


ts_hour [,"Sub_metering_1"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Hourly decomposition
          of Sub_metering_1")


ts_hour [,"Sub_metering_2"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Hourly decomposition
          of Sub_metering_2")


ts_hour [,"Sub_metering_3"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Hourly decomposition
          of Sub_metering_3")

ts_hour [,"other_areas"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Hourly decomposition
          of other_areas")





