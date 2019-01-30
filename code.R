#### SET THE ENVIRONMENT  ####

## load libraries

pacman::p_load("RMySQL","dplyr", "tidyr","lubridate","esquisse","padr","imputeTS",
               "ggplot2", "chron","plotly", "forecast", "zoo", "shiny")


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
str(rawtable)
head(rawtable)
tail(rawtable)






#### PRE-PROCESS  ####

## scale variables 
rawtable$Global_active_power <- round(rawtable$Global_active_power*1000/60, digits = 4)
rawtable$Global_reactive_power <- round(rawtable$Global_reactive_power*1000/60, digits = 4)

## convert data types 
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


## ifelse to set the season attribute
imputed.table$Season <- ifelse(imputed.table$month == 12|imputed.table$month == 1|imputed.table$month == 2,"Winter",
                     ifelse(imputed.table$month == 3|imputed.table$month == 4|imputed.table$month == 5, "Spring",
                            ifelse(imputed.table$month == 6|imputed.table$month == 7|imputed.table$month == 8, "Summer", "Autumn"))) #season info






#### GROUP & VISUALIZE GRANULARITY ####
#### seasonaly grouped ####
seasonaly <- imputed.table %>% group_by(Season) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power, 
                           other_areas, Submeterings, year, month, week, day), funs(sum))  #grouping by season
colnames(seasonaly)[1] <- "DateTime"  


# plot seasons
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




#### other grouping ####

aggregated_df <- c()
plots.gap.sub <- c()
plots.gap.sumsub <- c()


granularity <- c("year", "month", "day", "week", "hour", "30 mins")

for(g in granularity){
  aggregated_df[[g]] <- imputed.table %>%
    group_by(DateTime=floor_date(DateTime, g)) %>%
    dplyr::summarize_at(vars(
      Sub_metering_1,
      Sub_metering_2,
      Sub_metering_3,
      Global_active_power, 
      other_areas,
      Submeterings),
    funs(sum))


  

##### VISUALIZATION to study granularity ####
  # PLOT -> Global Power vs Submetering Records in [[g]] 
  plots.gap.sumsub[[g]] <- ggplot(data = aggregated_df[[g]], aes(x = DateTime)) +
    geom_line(aes(y = Submeterings, color = "Submetering")) +
    geom_line(aes(y = Global_active_power, color = "Global Power")) +
    theme_minimal()+
    labs(title = paste("Global power vs submetering records", g),
         x = "Time",
         y = "Power")
  }


  #### PLOT Power Consumption per sub-meter monthly ####

plot_ly(aggregated_df[["month"]], 
        x = ~aggregated_df[["month"]]$DateTime, 
        y = ~aggregated_df[["month"]]$Sub_metering_1, 
         name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~aggregated_df[["month"]]$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~aggregated_df[["month"]]$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~aggregated_df[["month"]]$other_areas,
            name = 'Other Areas', mode = 'lines') %>%
  layout(title = paste("Power Consumption per sub-meter monthly"),
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))





  #### vacation periods filter  ####
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








#### APPLIANCE ANALYSIS -> analyse fridge behaviour ####

laundryRoom <- as.data.frame(imputed.table$Sub_metering_2)
laundryRoom$Datetime <- imputed.table$DateTime
laundryRoom$year <- year(laundryRoom$Datetime)


## create column to identify running fridge power consumption

appliance <- c("stopped fridge", "fridge", "other")

laundryRoom$appliance <- ifelse(
  laundryRoom$`imputed.table$Sub_metering_2` < 43,
    ifelse(
    laundryRoom$`imputed.table$Sub_metering_2` == 0,
    "stopped fridge",
    "fridge"),
  "other")




## Plot Fridge in Submeter 2

allAppl.plot <- ggplot(laundryRoom, aes(x = Datetime, 
                        y = `imputed.table$Sub_metering_2`, 
                        color= appliance)) + geom_line() 

fridge.plot <- ggplot(laundryRoom, aes(x = Datetime, 
                        y = `imputed.table$Sub_metering_2`, 
                        color= appliance == "fridge")) + geom_line() 


## How much does the fridge consumes of the power of the Power consumed in the laundry room?
sum(laundryRoom$`imputed.table$Sub_metering_2`) # sum laundry room power

# create df ONLY with running fridge results
fridge.final <- filter(laundryRoom, appliance == "fridge")
sum(fridge.final$`imputed.table$Sub_metering_2`)   # the fridge consumes an 88.65% of the power of the laundry room 

others <- filter(laundryRoom, appliance == "other")
sum(others$`imputed.table$Sub_metering_2`) # what is not the fridge means 11.35% of the power of the laundry room 




## How much does the fridge consumes of the power of TOTAL POWER?
laundryRoom$GAP <- imputed.table$Global_active_power
sum(laundryRoom$GAP) # sum laundry room power 37710207 watts

fridge.final2 <- filter(laundryRoom, appliance == "fridge")
sum(fridge.final2$GAP) # sum fridge power to compare with TOTAL house consumption 13347204 watts


#the fridge consumes a 35.4% of the TOTAL power in the house






#### OUTLIERS ESCRIBIR PASOS!!!!! ####
outliers.treatment <- aggregated_df[["month"]]$Global_active_power 
outliers.treatment <- as.data.frame(outliers.treatment)
names(outliers.treatment)[1]<-paste("GAP")
outliers.treatment$DateTime <- aggregated_df[["month"]]$DateTime



outliers.treatment <- ifelse(outliers.treatment$GAP < 206000, 
                             mean(c(outliers.treatment[9,1], outliers.treatment[33,1], outliers.treatment[45,1])), 
                             outliers.treatment$GAP)
outliers.treatment <- as.data.frame(outliers.treatment)
names(outliers.treatment)[1]<-paste("Global_active_power")
outliers.treatment$DateTime <- aggregated_df[["month"]]$DateTime






#### create [[g]] time series objects ####


ts_month <- ts(aggregated_df[["month"]], start = c(2006,12), frequency = 12)
ts_month_table <- as.data.frame(ts_month)
tail(ts_month)

ts_month_outliers <- ts(outliers.treatment, start = c(2006,12), frequency = 12)

ts_month_cutted <-  ts(aggregated_df[["month"]], start = c(2006,12), end = c(2010,10), frequency = 12)  


ts_week <- ts(aggregated_df[["week"]], start = c(2006,51), frequency = 52)
ts_week_table <- as.data.frame(ts_week)
tail(ts_week)

ts_day <- ts(aggregated_df[["day"]], start = c(2006,350), frequency = 365)
ts_day_table <- as.data.frame(ts_day)
tail(ts_day)

ts_hour <- ts(aggregated_df[["hour"]], start = c(2006,8417), frequency = 8760)
ts_hour_table <- as.data.frame(ts_hour)
tail(ts_hour)




# LO ANTERIOR EN LOOP
# periods <- c("month", "week", "day", "hour")
# frequencies <- c(month = 12, week = 52, day = 365, hour = 8760)
# vector_of_ts <- c()
# for(p in periods){
#   vector_of_ts[[p]] <- ts(
#     aggregated_df[[p]],
#     start = c(2006,12),
#     frequency = frequencies[[p]]
#   )
# }


#### create [[g]] time series objects for every single [[v]] ####

ts_month_vars <- list()
ts_week_vars <- list()
ts_day_vars <- list()
ts_hour_vars <- list()
variables <- list("Global_active_power", "Sub_metering_1", "Sub_metering_2",
                  "Sub_metering_3", "other_areas", "Submeterings")
decomposing_m_vars <- list()
deco.m.plots <- list()
remainder_m_table <- c()

decomposing_w_vars <- list()
deco.w.plots <- list()
remainder_w_table <- c()

decomposing_d_vars <- list()
deco.d.plots <- list()
remainder_d_table <- c()

decomposing_h_vars <- list()
deco.h.plots <- list()
remainder_h_table <- c()



for(v in variables) {
  ts_month_vars[[v]] <- ts_month [ ,v]
  ts_week_vars[[v]] <- ts_week [ ,v]
  ts_day_vars[[v]] <- ts_day [ ,v]
  ts_hour_vars[[v]] <- ts_hour [ ,v]
  
# decomposing (stl) + plotting each [[v]] in [[g]] + remainder analysis -> possible predictable vars 

  # Decomposition By MONTH
  decomposing_m_vars[[v]] <- ts_month_vars[[v]] %>% stl(s.window = 12) 
  
  deco.m.plots[[v]] <- ts_month_vars[[v]] %>% stl(s.window = 12)%>%
    autoplot() + xlab("Year") +
    ggtitle(paste("Monthly decomposition
            of", v))
  
  remainder_m_table[[v]] <- ts_month_vars[[v]] %>% stl(s.window = 12) %>% remainder()

  
  # Decomposition By WEEK
  decomposing_w_vars[[v]] <- ts_week_vars[[v]] %>% stl(s.window = 52) 
  
  deco.w.plots[[v]] <- ts_week_vars[[v]] %>% stl(s.window = 52) %>%
    autoplot() + xlab("Year") +
    ggtitle(paste("Weekly decomposition
                  of", v)) 
  
  remainder_w_table[[v]] <- ts_week_vars[[v]] %>% stl(s.window = 52) %>% remainder()
  
  
  # Decomposition By DAY
  decomposing_d_vars[[v]] <- ts_day_vars[[v]] %>% stl(s.window = 365.25) 
  
  deco.d.plots[[v]] <- ts_day_vars[[v]] %>% stl(s.window = 365.25) %>%
    autoplot() + xlab("Year") +
    ggtitle(paste("Daily decomposition
                  of", v))
  
  remainder_d_table[[v]] <- ts_day_vars[[v]] %>% stl(s.window = 365.25) %>% remainder()
  
  # Decomposition By HOUR
  decomposing_h_vars[[v]] <- ts_hour_vars[[v]] %>% stl(s.window = 8760) 
  
  deco.h.plots[[v]] <-  ts_hour_vars[[v]] %>% stl(s.window = 8760)%>%
    autoplot() + xlab("Year") +
    ggtitle(paste("Hourly decomposition
                  of", v))
  
  remainder_h_table[[v]] <- ts_hour_vars[[v]] %>% stl(s.window = 8760) %>% remainder()
  
  }


#### remainder analysis for each [[v]] in [[g]] -> ABS ERROR df ####

remainder_m <- as.data.frame(remainder_m_table$Global_active_power)
remainder_m <- mean(abs(remainder_m$x)) / mean(ts_month_table$Global_active_power)

remainder_w <- as.data.frame(remainder_w_table$Global_active_power)
remainder_w <-mean(abs(remainder_w$x)) / mean(ts_week_table$Global_active_power)

remainder_d <- as.data.frame(remainder_d_table$Global_active_power)
remainder_d <- mean(abs(remainder_d$x)) / mean(ts_day_table$Global_active_power)

remainder_h <- as.data.frame(remainder_h_table$Global_active_power)
remainder_h <- mean(abs(remainder_h$x)) / mean(ts_hour_table$Global_active_power)


#### Plot remainders for each granularity ####
granularity_remainders <- c(remainder_m, remainder_w, remainder_d, remainder_h)
granularity_remainders <- as.data.frame(granularity_remainders)
granularity_remainders$granularity <- c("month", "week", "day", "hour")
colnames(granularity_remainders) <- c("remainders", "granularity")


library(ggplot2)
ggplot(data = granularity_remainders) +
  aes(x = granularity, weight = remainders) +
  geom_bar(fill = "#0c4c8a") +
  labs(title = "Randomness for each granularity",
    y = "Randomness (Relative)") +
  theme_minimal()




#### DATA PARTITION TO FORECAST ####

train <- window(ts_month[,"Global_active_power"], start = c(2006,12), end = c(2009,12))
test <- window(ts_month[,"Global_active_power"], start= c(2010,1))



train_outliers <- window(ts_month_outliers[,"lala"], start = c(2006,12), end = c(2009,12))
test_outliers <- window(ts_month_outliers[,"lala"], start= c(2010,1))



train_cutted <- window(ts_month_cutted[,"Global_active_power"], start = c(2006,12), end = c(2009,12))
test_cutted <- window(ts_month_cutted[,"Global_active_power"], start= c(2010,1))


#### FORECASTING ####

#### ETS ####
modelETS = ets(train)
predictionETS_monthly = forecast(modelETS, h=18) # forecast 24 months into the future
predictionETS_monthly
plot(predictionETS_monthly)

#check metrics comparing prediction in test
accuracy(predictionETS_monthly, test) 


#cutted november 2010
modelETS_cutted = ets(train_cutted)
predictionETS_cutted_monthly = forecast(modelETS_cutted, h=18) # forecast 24 months into the future
predictionETS_cutted_monthly
plot(predictionETS_cutted_monthly)

#check metrics comparing prediction in test
accuracy(predictionETS_monthly, test)

autoplot(ts_month_cutted[,"Global_active_power"], series = "real") + 
  autolayer(predictionETS_cutted_monthly, series= "ETS prediction", PI= FALSE)


####ARIMA####
modelArima <- auto.arima(train)
predictionArima_monthly <- forecast(modelArima, h= 18)  #forecast 24 month ahead
predictionArima_monthly
plot(predictionArima_monthly, ylim=range(predictionArima_monthly$lower,predictionArima_monthly$upper))

#check metrics comparing prediction in test
accuracy(predictionArima_monthly, test) 


##### ARIMA no outliers ####
modelArima_outliers <- auto.arima(train_outliers)
predictionArima_outliers_monthly <- forecast(modelArima_outliers, h= 18)  #forecast 24 month ahead
predictionArima_outliers_monthly
plot(predictionArima_outliers_monthly)

#check metrics comparing
accuracy(predictionArima_outliers_monthly, test_outliers) 


#comparing Arima () outlier august 2008 treated)

autoplot(ts_month[,"Global_active_power"], series = "real") + 
  autolayer(predictionArima_monthly, series= "ARIMA prediction", PI= FALSE) + 
  autolayer(predictionArima_outliers_monthly, series= "Arima prediction treated", PI=FALSE)



#### HOLT WINTERS ####
modelHW <- HoltWinters(train)
predictionHW_monthly <- forecast(modelHW, h= 18)
predictionHW_monthly
plot(predictionHW_monthly, ylim=range(predictionHW_monthly$lower,predictionHW_monthly$upper))
#check metrics comparing(postresample)
accuracy(predictionHW_monthly, test)


# HW beta FALSE
modelHW_bfalse <- HoltWinters(train, beta = FALSE)
predictionHW_bfalse_monthly <- forecast(modelHW_bfalse, h= 18)



# comparing HW parameters 
autoplot(ts_month[,"Global_active_power"], series = "real") + 
  autolayer(predictionHW_monthly, series= "HW prediction", PI= FALSE) + 
  autolayer(predictionHW_bfalse_monthly, series= "HW prediction 2", PI= FALSE) 



####FORECASTING HYBRID -> combine models ####


####COMPARING MODELS ####


autoplot(ts_month[,"Global_active_power"], series = "real") + 
  autolayer(predictionArima_monthly, series= "ARIMA prediction", PI= FALSE) + 
  autolayer(predictionHW_monthly, series= "HW prediction", PI=FALSE)+
  autolayer(predictionETS_monthly, series= "ETS prediction", PI=FALSE)+
  autolayer(predictionArima_outliers_monthly, series= "ARIMA 2 prediction", PI=FALSE)




