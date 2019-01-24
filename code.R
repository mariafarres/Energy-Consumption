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
#### every 30 min ####
min30 <- imputed.table %>% group_by(cut(DateTime, "30 min")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power, 
                           other_areas, Submeterings, year, month, week, day), funs(sum)) 

colnames(min30)[1] <- "DateTime" 


#### hourly grouped ####
hourly <- imputed.table %>% group_by(DateTime=floor_date(DateTime, "hour")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power, 
                           other_areas, Submeterings, year, month, week, day), funs(sum)) 

colnames(hourly)[1] <- "DateTime" 


#### daily grouped ####
daily <- imputed.table %>% group_by(DateTime=floor_date(DateTime, "day")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power, 
                           other_areas, Submeterings, year, month, week, day), funs(sum)) 

colnames(daily)[1] <- "DateTime" 


#### weekly grouped ####
weekly <- imputed.table %>% group_by(DateTime=floor_date(DateTime, "week")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power, 
                           other_areas, Submeterings, year, month, week, day), funs(sum)) 
colnames(weekly)[1] <- "DateTime" 


#### monthly grouped ####
monthly <- imputed.table %>% group_by(DateTime=floor_date(DateTime, "month")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power, 
                            other_areas, Submeterings, year, month, week, day), funs(sum))  #grouping by month
colnames(monthly)[1] <- "DateTime" 


#### seasonaly grouped ####
seasonaly <- imputed.table %>% group_by(Season) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power, 
                           other_areas, Submeterings, year, month, week, day), funs(sum))  #grouping by season
colnames(seasonaly)[1] <- "DateTime"  


#### yearly grouped ####
yearly <- imputed.table %>% group_by(DateTime=floor_date(DateTime, "year")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power, 
                           other_areas, Submeterings, year, month, week, day), funs(sum)) #grouping by year



#### visualization ####
#### PLOT -> Global Power vs Submetering Records ####
ggplot(data = monthly, aes(x = DateTime)) +
  geom_line(aes(y = Submeterings, color = "Submetering")) +
  geom_line(aes(y = Global_active_power, color = "Global Power")) +
  theme_minimal()+
  labs(title = "Global Power vs Submetering Records",
       x = "Time",
       y = "Power")



#### PLOT -> Power Consumption per sub-meter by month ####
plot_ly(monthly, x = ~monthly$DateTime, y = ~monthly$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~monthly$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~monthly$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~monthly$other_areas,
            name = 'Other Areas', mode = 'lines') %>%
  layout(title = "Power Consumption per sub-meter by month",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#### PLOT -> Power Consumption per submeter by week ####
plot_ly(weekly, x = ~weekly$DateTime, y = ~weekly$Sub_metering_1, #plot week
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







#### Subset  #### 

#### VACATION  ####

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
vacation2008 <- filter(imputed.table, year== "2007", month == "8")
plot_ly(vacation2008, x = ~vacation2008$DateTime, y = ~vacation2008$Sub_metering_1, 
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



vacation2009 <- filter(imputed.table, year== "2009", month == "8")


plot_ly(weekly, x = ~weekly$DateTime, y = ~weekly$Sub_metering_1, #plot week
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


#### comparison between years ####

august2008 <- hourly%>% group_by(year = 2008, month == 8) #holidays 2008

years.month <- bind_rows(by2007, by2008, by2009)

plot_ly(by2007, x = ~by2007$DateTime, y = ~by2007$Sub_metering_1, #plot 2007
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~by2007$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~by2007$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 2007",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(by2008, x = ~by2008$DateTime, y = ~by2008$Sub_metering_1, #plot 2008
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~by2008$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~by2008$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(august2008, x = ~august2008$DateTime, y = ~august2008$Sub_metering_1, #Holidays 2008
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~august2008$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~august2008$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(by2009, x = ~by2009$DateTime, y = ~by2009$Sub_metering_1, #plot 2009
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~by2009$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~by2009$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(byseason, x = ~byseason$Season, y = ~byseason$Sub_metering_1, #plot Seasons
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~byseason$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~byseason$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption by Season",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#### Seasons ####
houseWinter <- filter(hourly, Season == "Winter", year == 2009)
houseSummer <- filter(hourly, Season == "Summer", year == 2009)
houseAutumn <- filter(min30, Season == "Autumn")
houseSpring <- filter(min30, Season == "Spring")

plot_ly(houseSummer, x = ~houseSummer$DateTime, y = ~houseSummer$Sub_metering_1, #plot Summer 2009
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseSummer$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseSummer$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption by Season",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#### Forecasting ####

## create df to forecast ##


## NULL season cause character ##
hourly$Season <- NULL 
daily$Season <- NULL 
weekly$Season <- NULL 
monthly$Season <- NULL 

## by hour ##
ts_hour <- ts(hourly, frequency = )

## by day ##
tsday <- ts(daily, frequency = 365.25*7)

## by week ##
ts_week <- ts(weekly,start = c(2007,1), end = c(2010, 47), frequency = 52)

## by month ##
ts_month <- ts(monthly, start = c(2007,1), end = c(2010,11), frequency = 12)

## ecplore ts ##
ts_hour
tsday
ts_week
ts_month

#### month forecasting ####
plot.ts(ts_month[,"Sub_metering_1"])
plot.ts(ts_month[,"Sub_metering_2"])
plot.ts(ts_month[,"Sub_metering_3"])
plot.ts(ts_month[,"Global_active_power"])
plot.ts(ts_month[,"Submeterings"])

#### week forecasting ####
plot.ts(ts_week[,"Sub_metering_1"])
plot.ts(ts_week[,"Sub_metering_2"])
plot.ts(ts_week[,"Sub_metering_3"])
plot.ts(ts_week[,"Global_active_power"])
plot.ts(ts_week[,"Submeterings"])

## week ##
ts_week[,"Submeterings"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Submeterings decomposition
          of weeks")

ts_week[,"Sub_metering_3"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Submeterings decomposition
          of weeks")

## month ##
ts_month[,"Global_active_power"] %>% stl(s.window = 12) %>%
  autoplot() + xlab("Year") +
  ggtitle("Submeterings decomposition
          of months")

ts_month[,"Sub_metering_1"] %>% stl(s.window = 12) %>%
  autoplot() + xlab("Year") +
  ggtitle("Submetering 1 decomposition
          of months")

ts_month[,"Sub_metering_2"] %>% stl(s.window = 12) %>%
  autoplot() + xlab("Year") +
  ggtitle("Submetering 2 decomposition
          of months")

ts_month[,"Sub_metering_3"] %>% stl(s.window = 12) %>%
  autoplot() + xlab("Year") +
  ggtitle("Submetering 3 decomposition
          of months")

ts_month[,"other_areas"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Other Areas decomposition
          of months")
