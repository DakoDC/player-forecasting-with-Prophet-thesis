

# Preprocessing functions -------------------------------------------------

# clean the data from zeros (keeps NA only if there are over 12 in a row)
zero_to_NA <- function(data_raw){
  data_raw %>%
    # Convert zeros to NA
    mutate(Playercount = ifelse(Playercount == 0, NA, Playercount)) %>%
    
    # Convert to NA all zeros or extremely low values below a very small quantile
    mutate(Playercount = ifelse(Playercount <= quantile(Playercount, probs = 0.0001, na.rm = TRUE), NA, Playercount))
}

# Interpolate linearly missing values
fill_NA <- function(data){
  data %>% mutate(Playercount = zoo::na.approx(Playercount, x = Time, na.rm = FALSE))
}


# aggregates the 5 min data into a bigger time interval, keeping the maximum value in that interval
get_max_time <- function(data, time_interval_str = "day") {
  
  data %>%
    group_by(Time = floor_date(Time, unit = time_interval_str)) %>%
    summarise(
      # if there aren't any non NA observations in the aggregated time it returns NA instead of Inf
      Playercount = ifelse(all(is.na(Playercount)), NA, max(Playercount, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      weekday = wday(Time, label = TRUE, week_start = 1),
      hour = hour(Time)
    )
}


# EDA functions -----------------------------------------------------------

# plot of the daily data by weekday
make_daily_plot <- function(df, y_text = max(df$Playercount, na.rm = TRUE), plot_title = ""){
  plot_holidays <- holidays_df[-which(holidays_df$holiday == "Capodanno"),]
  
  ggplot(data = df %>% drop_na, aes(x = Time, y = Playercount, group = weekday, colour = weekday)) +
    geom_point(size = 1) +
    geom_line(linewidth = 0.7) +
    
    scale_color_viridis_d(option = "C", direction = -1, name = "Giorno\ndella settimana") +
    
    # Add vertical lines for the holidays
    geom_vline(xintercept = plot_holidays$ds %>% as.Date(), col = "cyan3", alpha = 0.7, lty = 2, lwd = 0.4) +
    
    # Add holiday labels
    geom_text(data = plot_holidays, 
              aes(x = ds %>% as.Date(), y = y_text, label = holiday), 
              angle = 270, size = 4.0, hjust = 0, vjust = -0.4 , inherit.aes = FALSE) +
    
    # Title and axis labels
    labs(title = plot_title, x = "Giorno", y = "Giocatori Attivi") +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.title.position = "plot"
    ) +
    theme_minimal()
}


# Prophet model functions -------------------------------------------------

# Adds regressors to the dataframe (for events like holidays, etc.)
add_regressors <- function(df, regressors_df = NULL){
  
  if (is.null(regressors_df)) regressors_df <- get("regressors_df", envir = .GlobalEnv)
  
  # Iterate through the regressors dataframe
  for (i in seq_len(nrow(regressors_df))){
    name  <- regressors_df$name[i]
    start <- as.POSIXct(regressors_df$start[i], tz = "UTC")
    end   <- as.POSIXct(regressors_df$end[i], tz = "UTC")
    
    # Add binary columns for each regressor (1 if the event is happening, else 0)
    df[name] <- ifelse(df$ds >= start & df$ds <= end, 1, 0)
  }
  
  df
}

# Fits the Prophet model with specified start and end dates and optional regressors
fit_prophet_model <- function(df, 
                              start_date = "2017-12-14", # uses data after 2017-12-14 (included) 
                              end_date = "2020-08-13",  # uses data before 2020-08-13 (not included)
                              regressors_df = NULL){
  
  if (is.null(regressors_df)) regressors_df <- get("regressors_df", envir = .GlobalEnv)
  
  start <- as.POSIXct(start_date, tz = "UTC")
  end <- as.POSIXct(end_date, tz = "UTC")
  
  # Prepare the data for Prophet
  prophet_df <- df %>%
    filter(Time >= start & Time < end) %>%
    rename(ds = Time, y = Playercount) %>%
    add_regressors(regressors_df)

  # Adds the covid regressor only if covid already happened
  if (end > as.POSIXct("2020-03-12", tz = "UTC")) {
    prophet_df <- prophet_df %>%
      # add covid regressor, up to the point of known data
      mutate(Covid = ifelse(ds >= as.POSIXct("2020-03-12", tz = "UTC") & 
                              ds <= min(as.POSIXct("2020-05-15", tz = "UTC"), end), 1, 0))
  }
  
  # Initialize the Prophet model
  m <- prophet(
    # yearly.seasonality = TRUE, # if you want to force the yearly seasonality
    # changepoint.prior.scale = 0.2 # 0.05 default
    holidays = holidays_df)
  
  
  # Add the regressors to the model
  for (i in seq_len(nrow(regressors_df))){
    m <- add_regressor(m, regressors_df$name[i])
  }
  
  if (end > as.POSIXct("2020-03-12", tz = "UTC")) {
    m <- add_regressor(m, "Covid")
  }

    # Fit the Prophet model to the data
  fit.prophet(m, prophet_df)
  
}


# Get forecast from the model and add weekday and hour columns
get_forecast <- function(m,
                         regressors_df = NULL,
                         n_days_pred = 14 # number of days forecasted
) {
  
  if (is.null(regressors_df)) regressors_df <- get("regressors_df", envir = .GlobalEnv)
  
  # Time interval between observations
  time_interval <- as.numeric(difftime(m$history$ds[2], m$history$ds[1], units = "sec"))
  
  # Create the future dataframe for prediction
  future <- make_future_dataframe(
    m, 
    periods = (86400 / time_interval) * n_days_pred,
    freq = time_interval
    ) %>%
    add_regressors(regressors_df)
  
  # Adds Covid regressor only if historical data goes beyond 2020-03-12
  if (max(m$history$ds) > as.POSIXct("2020-03-12", tz = "UTC")) {
    future <- future %>%
      mutate(Covid = ifelse(ds >= as.POSIXct("2020-03-12", tz = "UTC") & 
                              ds <= min(as.POSIXct("2020-05-15", tz = "UTC"), max(m$history$ds)), 
                            1, 0))
  }
  
  forecast <- predict(m, future)
  
  # Add weekday and hour columns to the forecast
  forecast %>%
    mutate(
      weekday = wday(ds, label = TRUE, abbr = TRUE, week_start = 1), 
      hour = hour(ds)
    )
}




# ARIMA functions ---------------------------------------------------------

# creates a tsibble object from data
make_tsibble <- function(data, start_date = "2017-12-14", end_date = "2020-08-13", daily = FALSE){
  # with daily data it gives problems later if it is used POSIXct instead of Date format
  if(daily){
    tsibble_df <- data %>%
      as_tibble() %>%
      mutate(ds = as.Date(Time),
             y = Playercount) %>%
      filter(ds >= as.Date(start_date), ds <= as.Date(end_date)) %>%
      select(ds, y) %>% 
      as_tsibble(index = ds)
  }
  else{
    tsibble_df <- data %>%
      as_tibble() %>%
      mutate(ds = as.POSIXct(Time, tz = "UTC"),
             y = Playercount) %>%
      filter(ds >= as.POSIXct(start_date, tz = "UTC"), ds <= as.POSIXct(end_date, tz = "UTC")) %>%
      select(ds, y) %>% 
      as_tsibble(index = ds)
  }
  tsibble_df
  
}



# Function for ARIMA cross-validation
rolling_cv_arima <- function(ts_data, initial, horizon, period, daily = FALSE) {
  stretched <- ts_data %>%
    stretch_tsibble(.init = initial, .step = period) %>%
    group_by(.id) %>%
    filter(max(ds) + horizon <= max(ts_data$ds)) %>%
    ungroup()
  
  n_folds <- n_distinct(stretched$.id)
  cutoffs <- stretched %>%
    group_by(.id) %>%
    slice_tail(n = 1) %>%
    pull(ds) # last date in training set for each window
  
  message("Starting CV with ", n_folds, " folds")
  message("Cutoff dates:\n", paste(format(cutoffs, "%Y-%m-%d %H:%M:%S"), collapse = "\n"))
  
  models <- stretched %>%
    group_by(.id) %>%
    group_map(~ {
      message("Fitting model for window ", .y$.id, 
              ", ending at ", max(.x$ds))
      model(.x, arima = ARIMA(y))
    }, .keep = TRUE)
  
  forecasts <- map(models, ~ forecast(.x, h = horizon) %>% select(-.id))
  
  cv_result <- tibble(
    .id = unique(stretched$.id),
    cutoff = cutoffs,
    forecast = forecasts
  ) %>%
    unnest(forecast) %>%
    left_join(ts_data, by = "ds") %>%
    rename(y = y.y,
           yhat = .mean)
  
  if(daily){
    cv_result <- cv_result %>% mutate(
      ds = as.POSIXct(ds)
      )
  }
  
  cv_result
}


# computes total RMSE and MAPE of the Cross Validation 
get_metrics <- function(cv_results){
  cv_results %>%
    summarise(
      RMSE = sqrt(mean((y - yhat)^2, na.rm = TRUE)),
      MAPE = mean(abs((y - yhat)) / abs(y), na.rm = TRUE)
    )
}


# similar to Prophet's performance_metrics()
performance_metrics_arima <- function(cv_data) {
  cv_data %>%
    mutate(
      horizon = difftime(ds, cutoff, units = "auto"),
      error = y - yhat,
      abs_error = abs(error),
      pct_error = abs_error / abs(y),
      mape = abs_error / abs(y),
      smape = abs(y - yhat) / ((abs(y) + abs(yhat)) / 2),
      covered = 1  # ARIMA here has no intervals, so assume always covered
    ) %>%
    group_by(horizon) %>%
    summarise(
      mse = mean(error^2, na.rm = TRUE),
      rmse = sqrt(mse),
      mae = mean(abs_error, na.rm = TRUE),
      mape = mean(mape, na.rm = TRUE),
      mdape = median(abs_error / abs(y), na.rm = TRUE),
      smape = mean(smape, na.rm = TRUE),
      coverage = mean(covered, na.rm = TRUE),
      .groups = "drop"
    )
}


# plot the CVs
plot_cv_comparison <- function(prophet_cv, arima_cv,
                               line_color1 = "blue",
                               line_color2 = "red3") {
  p1 <- ggplot() +
    # Prophet predictions
    geom_line(data = prophet_cv,
              aes(y = yhat, x = ds, group = cutoff, color = "Prophet"),
              linewidth = 0.9) +
    
    # ARIMA predictions
    geom_line(data = arima_cv,
              aes(y = yhat, x = ds, group = cutoff, color = "ARIMA"),
              linewidth = 0.9) +
    
    # True observed values
    geom_line(data = prophet_cv,
              aes(y = y, x = ds, group = 1, color = "Osservato"),
              linewidth = 0.6, alpha = 0.8) +
    
    ylim(min(prophet_cv$y, prophet_cv$yhat, arima_cv$yhat),
         max(prophet_cv$y, prophet_cv$yhat, arima_cv$yhat)) +
    facet_wrap(~ cutoff, scales = "free_x") +
    scale_color_manual(
      name = "Modello",
      values = c("Prophet" = line_color1,
                 "ARIMA" = line_color2,
                 "Osservato" = "black")
    ) +
    labs(x = "Tempo", y = "Giocatori Attivi") +
    theme_minimal()
  
  p1
}



# plot a Prophet vs ARIMA metric
plot_perf <- function(prophet_perf, arima_perf, metric = "mape"){
  # binds the 2 performance df
  all_perf <- bind_rows(
    mutate(prophet_perf, model = "Prophet"),
    mutate(arima_perf, model = "ARIMA")
  ) %>% mutate(horizon = as.numeric(horizon))
  
  # Calculate mean MAPE and RMSE by model
  metric_means <- all_perf %>%
    group_by(model) %>%
    summarise(metric_mean = mean(.data[[metric]], na.rm = TRUE))
  
  if (metric == "mape") {
    metric_means <- metric_means %>% 
      mutate(rounded_metric_mean = format(round(metric_mean, 2), nsmall = 2))
  } else {
    metric_means <- metric_means %>% 
      mutate(rounded_metric_mean = format(round(metric_mean, 0), nsmall = 0))
  }
  
  xmax <- max(all_perf$horizon)
  
  ggplot(all_perf, aes(x = horizon, y = .data[[metric]], color = model)) +
    geom_point(size = 1.8) +
    geom_line(lwd = 1.2) +
    geom_hline(yintercept = 0) +
    scale_color_manual(name = "Model", values = c("red3", "blue")) +
    labs(x = "Orizzonte", y = toupper(metric)) +
    geom_hline(data = metric_means, aes(yintercept = metric_mean, color = model), linetype = "dashed") +
    geom_text(
      data = metric_means,
      aes(x = xmax * 1.05,  # put labels slightly to the right of the last point
          y = metric_mean,
          label = rounded_metric_mean,
          color = model),
      hjust = 0, # left align so it doesn’t overlap with the line
      vjust = -0.5,
      size = 3,
      show.legend = FALSE
    ) +
    expand_limits(x = xmax * 1.1) + # extend x-axis to avoid cutting labels
    guides(color = guide_legend(title = "Modello")) +
    theme_minimal()
}







# Error per forecast
mean_error_per_forecast <- function(prophet_cv, arima_cv){
  prophet_cv %>% 
    group_by(cutoff) %>% 
    summarise(Prophet = mean(abs(y-yhat), na.rm = TRUE)) %>% 
    left_join(
      arima_cv %>% 
        group_by(cutoff) %>% 
        summarise(ARIMA = mean(abs(y-yhat), na.rm = TRUE)),
      by = "cutoff"
    )
}

# calculates the time difference in minutes between 2 times
# (used to calculate the time spent for each CV)
time_diff <- function(start, end) {
  # difference in seconds
  diff <- as.numeric(difftime(end, start, units = "secs"))
  
  mins <- floor(diff / 60)
  secs <- round(diff %% 60)
  
  sprintf("%02d:%02d", mins, secs)
}



