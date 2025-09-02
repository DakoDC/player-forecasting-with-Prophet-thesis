
library(tidyverse) # tidyverse

# modelling
library(prophet)   # create Prophet model
library(tsibble)   # make tsibble object
library(fable)     # model arima
library(forecast)

# plots
library(ggplot2)   # plots
library(patchwork) # show multiple plots
library(gridExtra)



# Load the data -----------------------------------------------------------

# Load functions
source("functions.R")


# Load Data
data_raw <- read_delim("252950.csv", delim = ",", show_col_types = FALSE)

summary(data_raw)
data_raw %>% nrow()



# Preprocessing -----------------------------------------------------------

# Example of values below the 0.0001 quantile
low_values <- data_raw[which(data_raw$Playercount < 1000 & data_raw$Playercount > 0), ]
low_values

data_raw[data_raw$Time >= as.POSIXct(low_values$Time[1]) - 1800, ]
data_raw[data_raw$Time >= as.POSIXct(low_values$Time[2]) - 1800, ]
data_raw[data_raw$Time >= as.POSIXct(low_values$Time[3]) - 1800, ]
data_raw[data_raw$Time >= as.POSIXct(low_values$Time[4]) - 1800, ]
# they correspond to the period just after the end of stop in teh servers

# Converts zeros and values very close to zeros into NAs
data <- data_raw %>% zero_to_NA()

data_raw$Playercount %>% is.na() %>% sum()
data$Playercount %>% is.na() %>% sum()

summary(data)


# interpolates missing values (for Arima)
filled_data <- data %>% fill_NA()

# Hourly Mean of Active Players
hourly_df <- data %>% get_max_time(time_interval_str = "hour")

# interpolates missing values (for Arima)
filled_hourly_df <- filled_data %>% get_max_time(time_interval_str = "hour")



# Daily Mean of Active Players
daily_df <- data %>% get_max_time(time_interval_str = "day") %>% 
  mutate(Time = as.Date(Time))


# Holidays and Regressors -------------------------------------------------

# dummy forecast used to better check holidays and regressors
test_model = fit_prophet_model(daily_df, regressors_df = regressors_df)
test_forecast <- get_forecast(test_model, regressors_df = regressors_df,n_days_pred = 2)

# interactive plot
dyplot.prophet(test_model, test_forecast)
prophet_plot_components(test_model, test_forecast)

# italian version
grid.arrange(a[[1]] + ylab("Trend") + xlab("Giorno"), 
             a[[2]] + ylab("Festività") + xlab("Giorno"), 
             a[[3]] + ylab("Settimanale") + xlab("Giorno della settimana"), 
             a[[4]] + ylab("Annuale") + xlab("Giorno"), 
             a[[5]] + ylab("Regressori") + xlab("Giorno"), 
             ncol = 1)

# Main Holidays
holidays_df <- tibble(
  holiday = c(
    rep("Halloween", 2),      # Halloween
    rep("Ringraziamento", 2), # Thanksgiving
    rep("Natale", 3),         # Christmas
    rep("Capodanno", 3),      # New Year
    rep("San Valentino", 3)   # Saint Valentine
    
  ),
  ds = as.POSIXct(c(
    "2018-10-31", "2019-10-31",
    "2018-11-22", "2019-11-22",
    "2017-12-25", "2018-12-25", "2019-12-25",
    "2018-01-01", "2019-01-01", "2020-01-01",
    "2018-02-14", "2019-02-14", "2020-02-14"
  )),
  lower_window = c(
    rep(0, 2),
    rep(0, 2),
    rep(0, 3),
    rep(-1, 3),
    rep(0, 3)
  ),
  upper_window = c(
    rep(3, 2),
    rep(3, 2),
    rep(3, 3),
    rep(3, 3),
    rep(0, 3)
  )
)


regressors_df <- tibble(
  name = c(
    "Third_Anniversary",    "Rocket_Pass_1", 
    "Rocket_Pass_2",        "Rocket_Pass_3", 
    "Rocket_Pass_4",        "Halloween_Event_2019",
    "Lucky_Lanterns_Event_2020", "Fifth_Anniversary"
  ),
  start = as.POSIXct(c(
    "2018-07-09", "2018-09-05", 
    "2018-12-10", "2019-04-17", 
    "2019-08-28", "2019-10-14", 
    "2020-01-20", "2020-06-30"
  )),
  end = as.POSIXct(c(
    "2018-07-23", "2018-11-26",
    "2019-01-06", "2019-05-05",
    "2019-12-03", "2019-11-11", 
    "2020-02-10", "2020-07-13"
  ))
)
# rocket pass 2 ends in 2019-03-18 instead of 2019-01-06, but the effect gets reduced after some time
# same for  rocket pass 3, true end: 2019-08-27 
# df placeholder for storing the time each cross validation took


# EDA ---------------------------------------------------------------------

# Data Overview 
data %>% glimpse()
data %>% summary()


# Daily data Overview 
hourly_df %>% head()
hourly_df %>% summary()


# Daily data Overview
daily_df %>% head()
daily_df %>% summary()

boxplot(data$Playercount, hourly_df$Playercount, daily_df$Playercount)

# daily  plot
ggplot(daily_df, aes(y = Playercount, x = Time)) +
  geom_line(col = "navy") +
  theme_minimal()

# Daily plot by weekday
daily_df %>% make_daily_plot()

daily_df$weekday %>% table()

# boxplot by weekday  
ggplot(daily_df, aes(x = weekday, y = Playercount)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 120000, by = 20000)) +
  labs(x = "Giorno della settimana", y = "Giocatori Attivi") +
  theme_minimal()

# mondays have high mean since many still play from the afterhours of sunday
plot(data[data$Time >= as.POSIXct("2018-11-18") & data$Time < as.POSIXct("2018-11-29"), ])
# 19 and 26 are mondays
# means
daily_df %>%
  group_by(weekday) %>%
  summarise(avg = mean(Playercount, na.rm = TRUE))


# hourly trend
hourly_df %>%
  filter(complete.cases(Playercount)) %>% 
  ggplot(aes(x = hour, y = Playercount, group = hour)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  
  labs(x = "Ora del Giorno (UTC)", y = "Giocatori Attivi") +
  theme_minimal()

# peak between 19 and 21 UTC

hourly_means <- hourly_df %>%
  filter(complete.cases(Playercount)) %>% 
  group_by(hour) %>% 
  summarise(Playercount = mean(Playercount))

# normal percentage changes (01:00–23:00 vs previous)
changes <- diff(hourly_means$Playercount) / head(hourly_means$Playercount, -1) * 100

# wrap-around change from 23:00 → 00:00
last_to_first <- (hourly_means$Playercount[1] - tail(hourly_means$Playercount, 1)) / 
  tail(hourly_means$Playercount, 1) * 100

# combine into full vector, aligned with hours 00:00–23:00
hourly_means$change <- c(last_to_first, changes)


ggplot(hourly_means, aes(x = factor(hour), y = PctChange, fill = PctChange > 0)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "firebrick")) +
  geom_text(aes(label = sprintf("%.1f%%", PctChange)), 
            vjust = ifelse(hourly_means$PctChange >= 0, -0.5, 1.5),
            size = 3) +
  labs(x = "Ora",
       y = "Cambio percentuale \n(dall'ora precedente a quella attuale)") +
  theme_minimal() +
  theme(legend.position = "none")

print(hourly_means, n = 24)


par(mfrow = c(1,2))
# ACF and PACF
acf(daily_df$Playercount[1:500], main = "ACF - Daily Player Count",lag.max = 1000 )
pacf(daily_df$Playercount[1:500], main = "PACF - Daily Player Count",lag.max = 1000)
par(mfrow = c(1,1))



###

# placeholder for the cross-validations execution times
prophet_execution_times <- data.frame(
  cv = c("1h", "1h covid", "2w", "2w covid", "2m", "2m covid"),
  Prophet = rep(NA, 6)
)
arima_execution_times <- data.frame(
  cv = c("1h", "1h covid", "2w", "2w covid", "2m", "2m covid"),
  ARIMA = rep(NA, 6)
)


# Forecast - 1 hour -------------------------------------------------------


## Prophet - 1 hour --------------------------------------------------------


# fit prophet model
prophet_1h <- fit_prophet_model(data, # dataset
                                regressors_df = regressors_df,      # regressors
                                start_date = "2019-05-01 22:00:00", # first date used for the training
                                end_date = "2019-07-31 22:00:00"    # last date used for the training (not included)
)

# cross validation
a <- Sys.time()
prophet_1h_cv <- cross_validation(prophet_1h, # model
                                  initial = 24 * 7*8, # minimum time for the training set
                                  period = 24 * 7,    # Time between each fold
                                  horizon = 1,        # time forecasted (1 hour)
                                  units = "hours",    # units used for the options above
                                   ) %>% 
  mutate(
    ds = as.POSIXct(ds, tz = "UTC"),
    cutoff = as.POSIXct(cutoff, tz = "UTC")
  )
b <- Sys.time()
prophet_execution_times$Prophet[1] <- time_diff(a,b)


## Arima - 1 hour ----------------------------------------------------------

# make Arima tsibble object
arima_1h_ts <- make_tsibble(filled_data,
             start_date = "2019-05-01 21:00:00",
             end_date = "2019-07-31 22:00:00"
             )

a <- Sys.time()
# Arima cross validation
arima_1h_cv <- rolling_cv_arima(
  ts_data = arima_1h_ts,
  initial = 7*8 * 24 * 12 + (7 * 24 * 12), # added days to match with Prophet's
  horizon = 12,                            # 1-hour forecast (12 x 5min)
  period = 12 * 24 * 7                     # Step every 7 days
)
b <- Sys.time()
arima_execution_times$ARIMA[1] <- time_diff(a,b)

## Comparison - 1 hour -----------------------------------------------------

# mean performance by horizon
prophet_1h_perf <- performance_metrics(prophet_1h_cv, rolling_window=0)
arima_1h_perf <- performance_metrics_arima(arima_1h_cv)

prophet_1h_perf
arima_1h_perf

# total metrics mean comparison
arima_1h_metrics <- arima_1h_cv %>% get_metrics()
prophet_1h_metrics <- prophet_1h_cv %>% get_metrics()

prophet_1h_metrics
arima_1h_metrics


# plot of the forecasts
plot_cv_comparison(prophet_1h_cv, arima_1h_cv)

# mape and rmse comparison
plot_perf(prophet_1h_perf, arima_1h_perf, metric = "mape") + 
  plot_perf(prophet_1h_perf, arima_1h_perf, metric = "rmse")


# Error per forecast
mean_error_per_forecast(prophet_1h_cv, arima_1h_cv)


# errors comparison
ggplot() +
  geom_line(data = arima_1h_cv, aes(x = 1:nrow(arima_1h_cv), y = (y - yhat), group = cutoff), col = "red3") +
  geom_line(data = prophet_1h_cv, aes(x = 1:nrow(prophet_1h_cv), y = (y - yhat), group = cutoff), col = "blue") +
  geom_hline(yintercept = 0)




## Prophet with Covid - 1 hour ---------------------------------------------

prophet_1h_covid <- fit_prophet_model(data, 
                                   regressors_df = regressors_df, 
                                   start_date = "2020-05-01 22:00:00",
                                   end_date = "2020-07-31 22:00:00"

)


# cross validation every 24 hours
a <- Sys.time()
prophet_1h_covid_cv <- cross_validation(prophet_1h_covid,
                                        initial = 24*60,
                                        period = 24*7, horizon = 1, units = "hours"
                                        ) %>%
  mutate(
    ds = as.POSIXct(ds, tz = "UTC"),
    cutoff = as.POSIXct(cutoff, tz = "UTC")
  )
b <- Sys.time()
prophet_execution_times$Prophet[2] <- time_diff(a,b)

## Arima with Covid - 1 hour -----------------------------------------------

arima_1h_covid_ts <- make_tsibble(filled_data,
                                  "2020-05-01 21:00:00",
                                  "2020-07-31 22:00:00"
                                  )


a <- Sys.time()
arima_1h_covid_cv <- rolling_cv_arima(
  ts_data = arima_1h_covid_ts,
  initial = 63 * 24 * 12,
  horizon = 12,              # 1-hour forecast (12 x 5min)
  period = 12 * 24 * 7       # Step every 7 days
)
b <- Sys.time()
arima_execution_times$ARIMA[2] <- time_diff(a,b)

# Comparison with covid - 1 hour ------------------------------------------

# mean performance by horizon
prophet_1h_covid_perf <- performance_metrics(prophet_1h_covid_cv, rolling_window=0)
arima_1h_covid_perf <- performance_metrics_arima(arima_1h_covid_cv)

prophet_1h_covid_perf
arima_1h_covid_perf

# total metrics mean comparison
prophet_1h_covid_metrics <- prophet_1h_covid_cv %>% get_metrics()
arima_1h_covid_metrics <- arima_1h_covid_cv  %>% get_metrics()

prophet_1h_covid_metrics
arima_1h_covid_metrics


# plot of the forecasts
plot_cv_comparison(prophet_1h_covid_cv, arima_1h_covid_cv)

# mape and rmse comparison
plot_perf(prophet_1h_covid_perf, arima_1h_covid_perf, metric = "mape") + 
  plot_perf(prophet_1h_covid_perf, arima_1h_covid_perf, metric = "rmse")


# Error per forecast
mean_error_per_forecast(prophet_1h_covid_cv, arima_1h_covid_cv)

execution_times

# errors comparison
ggplot() +
  geom_line(data = arima_1h_covid_cv, aes(x = 1:nrow(arima_1h_covid_cv), y = (y - yhat), group = cutoff), col = "red3") +
  geom_line(data = prophet_1h_covid_cv, aes(x = 1:nrow(prophet_1h_covid_cv), y = (y - yhat), group = cutoff), col = "blue") +
  geom_hline(yintercept = 0)














# Forecast - 2 weeks -------------------------------------------------------


## Prophet - 2 weeks -------------------------------------------------------

prophet_2w <- fit_prophet_model(hourly_df, 
                                regressors_df = regressors_df, 
                                end_date = "2019-10-31 22:00:00"
                                # end_date = "2019-11-01 20:00:00"
                                
)


# cross validation
a <- Sys.time()
prophet_2w_cv <- cross_validation(prophet_2w,
                                  initial = 350,
                                  period = 7*12,      # step every 12 weeks
                                  horizon = 7*2,      # forecast 2 weeks
                                  units = "days"      # use days as unit
) %>%
  mutate(
    ds = as.POSIXct(ds, tz = "UTC"),
    cutoff = as.POSIXct(cutoff, tz = "UTC")
  )
b <- Sys.time()
prophet_execution_times$Prophet[3] <- time_diff(a,b)

## Arima - 2 weeks ---------------------------------------------------------

arima_2w_ts <- make_tsibble(filled_hourly_df, 
                            start_date = "2017-12-14 22:00:00",
                            end_date = "2019-10-31 22:00:00"
)

a <- Sys.time()
arima_2w_cv <- rolling_cv_arima(
  ts_data = arima_2w_ts,
  horizon = 24 * 14,         # 2-week forecast horizon
  initial = 24 * 350 + (31 + 31 + 8)*24, # start matches with prophet
  period = 24 * 7 * 12       # step every 12 weeks
)
b <- Sys.time()
arima_execution_times$ARIMA[3] <- time_diff(a,b)

## Comparison - 2 weeks ----------------------------------------------------

# metrics per horizon
prophet_2w_perf <- performance_metrics(prophet_2w_cv, rolling_window=0)
arima_2w_perf <- performance_metrics_arima(arima_2w_cv)

prophet_2w_perf
arima_2w_perf

# total mean metrics comparison
prophet_2w_metrics <- prophet_2w_cv %>% get_metrics()
arima_2w_metrics <- arima_2w_cv %>% get_metrics()

prophet_2w_metrics
arima_2w_metrics

# plot of the forecasts
plot_cv_comparison(prophet_2w_cv, arima_2w_cv)


# mape and rmse comparison
plot_perf(prophet_2w_perf, arima_2w_perf, metric = "mape") + 
  plot_perf(prophet_2w_perf, arima_2w_perf, metric = "rmse")


# Error per forecast
mean_error_per_forecast(prophet_2w_cv, arima_2w_cv)


# errors comparison
ggplot() +
  geom_line(data = arima_2w_cv, aes(x = 1:nrow(arima_2w_cv), y = (y - yhat), group = cutoff), col = "red3") +
  geom_line(data = prophet_2w_cv, aes(x = 1:nrow(prophet_2w_cv), y = (y - yhat), group = cutoff), col = "blue") +
  geom_hline(yintercept = 0)





## Prophet with Covid - 2 weeks --------------------------------------------

prophet_2w_covid <- fit_prophet_model(hourly_df, 
                                      regressors_df = regressors_df, 
                                      end_date = "2020-08-13"
)


# cross validation
a <- Sys.time()
prophet_2w_covid_cv <- cross_validation(prophet_2w_covid,
                                        initial = 750,
                                        period = 7*4*2,   # step every 8 weeks
                                        horizon = 14,     # forecast horizon = 2 weeks
                                        units = "days"
) %>%
  mutate(
    ds = as.POSIXct(ds, tz = "UTC"),
    cutoff = as.POSIXct(cutoff, tz = "UTC")
  )
b <- Sys.time()
prophet_execution_times$Prophet[4] <- time_diff(a,b)







## Arima with Covid - 2 weeks ----------------------------------------------

arima_2w_covid_ts <- make_tsibble(filled_hourly_df,
                                  end_date = "2020-08-13 23:00:00"
)

a <- Sys.time()
# ARIMA cross validation
arima_2w_covid_cv <- rolling_cv_arima(
  ts_data = arima_2w_covid_ts,
  initial = 24 * 750 + 24*29 + 24*12,  # match with prophet
  horizon = 24 * 14,      # 2 weeks
  period = 24 * 7 * 8     # step every 8 weeks
)
b <- Sys.time()
arima_execution_times$ARIMA[4] <- time_diff(a,b)


## Comparison with Covid - 2 weeks -----------------------------------------
prophet_2w_covid_perf <- performance_metrics(prophet_2w_covid_cv, rolling_window=0)
arima_2w_covid_perf <- performance_metrics_arima(arima_2w_covid_cv)

prophet_2w_covid_perf
arima_2w_covid_perf

prophet_2w_covid_metrics <- prophet_2w_covid_cv %>% get_metrics()
arima_2w_covid_metrics <- arima_2w_covid_cv %>% get_metrics()

# metrics comparison
prophet_2w_covid_metrics
arima_2w_covid_metrics

# plot of the forecasts
plot_cv_comparison(prophet_2w_covid_cv, arima_2w_covid_cv)


# mape and rmse comparison
plot_perf(prophet_2w_covid_perf, arima_2w_covid_perf, metric = "mape") + 
  plot_perf(prophet_2w_covid_perf, arima_2w_covid_perf, metric = "rmse")


# Error per forecast
mean_error_per_forecast(prophet_2w_covid_cv, arima_2w_covid_cv)

# errors comparison
ggplot() +
  geom_line(data = arima_2w_covid_cv, aes(x = 1:nrow(arima_2w_covid_cv), y = (y - yhat), group = cutoff), col = "red3") +
  geom_line(data = prophet_2w_covid_cv, aes(x = 1:nrow(prophet_2w_covid_cv), y = (y - yhat), group = cutoff), col = "blue") +
  geom_hline(yintercept = 0)




# Forecast - 2 months -----------------------------------------------------


## Prophet - 2 months ------------------------------------------------------
difftime(as.Date("2019-03-04"),as.Date("2017-12-14"))

# Fit Prophet model
prophet_2m <- fit_prophet_model(daily_df,
                                regressors_df = regressors_df,
                                end_date = "2019-10-31"
                                
)

# Cross validation
a <- Sys.time()
prophet_2m_cv <- cross_validation(prophet_2m,
                                  initial = 400,
                                  period = 60,     # time between each fold (60 days)
                                  horizon = 60,    # forecast horizon (60 days)
                                  units = "days"   # units for above parameters
) %>%
  mutate(
    ds = as.POSIXct(ds, tz = "UTC"),
    cutoff = as.POSIXct(cutoff, tz = "UTC")
  )
b <- Sys.time()
prophet_execution_times$Prophet[5] <- time_diff(a,b)

## Arima - 2 months --------------------------------------------------------

# Create daily tsibble
arima_2m_ts <- make_tsibble(daily_df, end_date = "2019-10-31", daily = TRUE)


# ARIMA cross-validation
a <- Sys.time()
arima_2m_cv <- rolling_cv_arima(
  ts_data = arima_2m_ts,
  initial = 400 + 14 + 28 + 4, # initial training window  (matches with Prophet)
  horizon = 60,                # forecast horizon (60 days)
  period = 60,                 # step forward (60 days)
  daily = TRUE
) 
b <- Sys.time()
arima_execution_times$ARIMA[5] <- time_diff(a,b)





## Comparison - 2 months ---------------------------------------------------

# CV Performance metrics
prophet_2m_perf <- performance_metrics(prophet_2m_cv, rolling_window=0)
arima_2m_perf <- performance_metrics_arima(arima_2m_cv)

prophet_2m_perf
arima_2m_perf

# Compute metrics
prophet_2m_metrics <- prophet_2m_cv %>% get_metrics()
arima_2m_metrics <- arima_2m_cv %>% get_metrics()

# Metrics comparison
prophet_2m_metrics
arima_2m_metrics


# plot of the forecasts
plot_cv_comparison(prophet_2m_cv, arima_2m_cv)


# MAPE and RMSE comparison
plot_perf(prophet_2m_perf, arima_2m_perf, metric = "mape") + 
  plot_perf(prophet_2m_perf, arima_2m_perf, metric = "rmse")


# Error per forecast
mean_error_per_forecast(prophet_2m_cv, arima_2m_cv)

# Errors comparison
ggplot() +
  geom_line(data = arima_2m_cv, aes(x = 1:nrow(arima_2m_cv), y = (y - yhat), group = cutoff), col = "red3") +
  geom_line(data = prophet_2m_cv, aes(x = 1:nrow(prophet_2m_cv), y = (y - yhat), group = cutoff), col = "blue") +
  geom_hline(yintercept = 0)




## Prophet with Covid - 2 months -------------------------------------------

prophet_2m_covid <- fit_prophet_model(daily_df, 
                                      regressors_df = regressors_df, 
                                      end_date = "2020-08-13"
)


# Cross validation every 14 days
a <- Sys.time()
prophet_2m_covid_cv <- cross_validation(prophet_2m_covid,
                                        initial = 725, 
                                        period = 60, 
                                        horizon = 60, 
                                        units = "days"
) %>%
  mutate(
    ds = as.POSIXct(ds, tz = "UTC"),
    cutoff = as.POSIXct(cutoff, tz = "UTC")
  )
b <- Sys.time()
prophet_execution_times$Prophet[6] <- time_diff(a,b)



## Arima with Covid - 2 months ---------------------------------------------
arima_2m_covid_ts <- make_tsibble(daily_df, daily = TRUE)


# Rolling CV
a <- Sys.time()
arima_2m_covid_cv <- rolling_cv_arima(
  ts_data = arima_2m_covid_ts,
  initial = 725 + 8,  # initial training set, matches with Prophet
  horizon = 60,       # forecast horizon (60 days)
  period = 60,        # step forward (60 days)
  daily = TRUE
)
b <- Sys.time()
arima_execution_times$ARIMA[6] <- time_diff(a,b)


## Comparison with Covid - 2 months ----------------------------------------

prophet_2m_covid_perf <- performance_metrics(prophet_2m_covid_cv, rolling_window=0)
arima_2m_covid_perf <- performance_metrics_arima(arima_2m_covid_cv)

prophet_2m_covid_perf
arima_2m_covid_perf

# Metrics comparison
prophet_2m_covid_metrics <- prophet_2m_covid_cv %>% get_metrics()
arima_2m_covid_metrics <- arima_2m_covid_cv %>% get_metrics()

prophet_2m_covid_metrics
arima_2m_covid_metrics


# plot of the forecasts
plot_cv_comparison(prophet_2m_covid_cv, arima_2m_covid_cv)


# MAPE and RMSE comparison
plot_perf(prophet_2m_covid_perf, arima_2m_covid_perf, metric = "mape") + 
  plot_perf(prophet_2m_covid_perf, arima_2m_covid_perf, metric = "rmse")


# Error per forecast
mean_error_per_forecast(prophet_2m_covid_cv, arima_2m_covid_cv)

# Errors comparison
ggplot() +
  geom_line(data = arima_2m_covid_cv, aes(x = 1:nrow(arima_2m_covid_cv), y = (y - yhat), group = cutoff), col = "red3") +
  geom_line(data = prophet_2m_covid_cv, aes(x = 1:nrow(prophet_2m_covid_cv), y = (y - yhat), group = cutoff), col = "blue") +
  geom_hline(yintercept = 0) +
  theme_minimal()



# Execution times ---------------------------------------------------------

prophet_execution_times
arima_execution_times

execution_times <- prophet_execution_times %>% left_join(arima_execution_times, by = "cv")
execution_times

convert_to_seconds <- function(x) {
  sapply(x, function(t) {
    parts <- strsplit(t, ":")[[1]]
    as.numeric(parts[1]) * 60 + as.numeric(parts[2])
  })
}

execution_times_long <- execution_times %>%
  mutate(
    horizon = cv,
    Prophet = convert_to_seconds(Prophet),
    ARIMA   = convert_to_seconds(ARIMA)
  ) %>%
  dplyr::select(horizon, Prophet, ARIMA) %>%
  pivot_longer(cols = c("Prophet", "ARIMA"),
               names_to = "Model", values_to = "Seconds")


# Ensure correct order of bars (x) and within-group order (fill)
execution_times_long <- execution_times_long %>%
  mutate(
    horizon = factor(horizon, levels = c("1h", "1h covid", "2w", "2w covid", "2m", "2m covid")),
    Model   = factor(Model,   levels = c("Prophet", "ARIMA")),   # Prophet left, ARIMA right
    label   = sprintf("%dm %02ds", floor(Seconds/60), as.integer(Seconds %% 60))  # mm:ss label
  )

ggplot(execution_times_long, aes(x = horizon, y = Seconds/60, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "gray66") +
  geom_text(aes(label = label),
            position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c(Prophet = "blue", ARIMA = "red3")) +
  scale_y_continuous(
    name = "Tempo di esecuzione (minuti)",
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(x = "Orizzonte di previsione", fill = "Modello") +
  theme_minimal()


