#Required Packages to use these functions :
#dplyr, tidyr, ggplot2, lubridate, zoo, strucchange, tsoutliers, purrr, tibble, forecast, prophet

## Simple CPI plot
plot_cpi_ts <- function(df, series_name) {
  ggplot(df, aes(x = Date, y = .data[[series_name]])) +
    geom_line(size = 1) +
    labs(
      title = paste0(series_name, " CPI over time"),
      x     = "Date",
      y     = "CPI Index"
    ) +
    theme_minimal()
}

## Year-over-Year percentage change plot
plot_cpi_yoy <- function(df, series_name) {
  df %>%
    arrange(Date) %>%
    mutate(YoY_pct = 100 * (.data[[series_name]] / lag(.data[[series_name]], 12) - 1)) %>%
    filter(!is.na(YoY_pct)) %>%
    ggplot(aes(x = Date, y = YoY_pct)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    geom_line(size = 1) +
    labs(
      title = paste0(series_name, " YoY % Change"),
      x     = "Date",
      y     = "YoY % Change"
    ) +
    theme_minimal()
}

## STL decomposition plot with zoomed-in seasonality pattern
plot_stl_components <- function(df, series_name) {
  # 1. Create ts and fit STL
  start_year  <- year(min(df$Date))
  start_month <- month(min(df$Date))
  ts_obj <- ts(df[[series_name]],
               start     = c(start_year, start_month),
               frequency = 12)
  stl_fit <- stl(ts_obj,
                 s.window = "periodic",
                 t.window = 13,
                 robust   = TRUE)
  
  # 2. Main decomposition plot
  p_main <- autoplot(stl_fit) +
    ggtitle(paste(series_name, "STL Decomposition")) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  # 3. Extract and plot 2013 seasonal pattern
  seasonal_vec <- stl_fit$time.series[, "seasonal"]
  dates_full   <- seq(min(df$Date), max(df$Date), by = "month")
  
  df_seasonal2013 <- tibble(
    Date     = dates_full,
    Seasonal = seasonal_vec
  ) %>%
    filter(year(Date) == 2015) %>%
    mutate(Month = month(Date, label = TRUE, abbr = TRUE))
  
  p_zoom <- ggplot(df_seasonal2013, aes(x = Month, y = Seasonal, group = 1)) +
    geom_line(size = 1) +
    geom_point() +
    labs(
      title = "Seasonal Pattern",
      y     = "Seasonal Effect",
      x     = NULL
    ) +
    theme_minimal()
  
  # 4. Stack the two plots vertically
  (p_main / p_zoom) + plot_layout(heights = c(3, 1))
}

## Volatility and Month-over-Month percentage change plot
plot_volatility <- function(df, colname) {
  df_mom <- df %>%
    select(Date, value = all_of(colname)) %>%
    arrange(Date) %>%
    mutate(
      MoM = 100 * (value - lag(value)) / lag(value),
      Volatility = rollapply(MoM, width = 12, FUN = sd, fill = NA, align = "right")
    )
  
  # Plot
  ggplot(df_mom, aes(x = Date)) +
    geom_line(aes(y = MoM), color = "steelblue", size = 1) +
    geom_line(aes(y = Volatility), color = "red", linetype = "dashed", size = 1) +
    labs(
      title = paste("MoM % Change and Rolling Volatility for", colname),
      y = "% Change / Volatility",
      x = "Date"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
}

## Plot showing breakpoints
plot_breakpoints <- function(df, varname, title = NULL) {
  ts_data <- ts(df[[varname]], start = c(2013, 1), frequency = 12)
  
  bp <- breakpoints(ts_data ~ 1)
  conf <- confint(bp)
  
  # Get the break dates
  breakpoints_index <- bp$breakpoints
  break_dates <- df$Date[breakpoints_index]
  
  # Get confidence intervals (lower and upper bounds)
  ci_bounds <- data.frame(
    lower = df$Date[conf$confint[, 1]],
    upper = df$Date[conf$confint[, 2]]
  )
  
  ggplot(df, aes(x = Date, y = .data[[varname]])) +
    geom_line(color = "black") +
    geom_rect(data = ci_bounds,
              aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf),
              fill = "lightblue", alpha = 0.4,
              inherit.aes = FALSE) +
    geom_vline(xintercept = as.numeric(break_dates), color = "red", linetype = "dashed") +
    labs(
      title = title %||% paste("Structural Breaks in", varname),
      x = "Date",
      y = "CPI Index"
    ) +
    theme_minimal()
}

## Plot for outliers
plot_outliers <- function(df, varname, start_year = 2013, start_month = 1) {
  ts_data <- ts(df[[varname]], start = c(start_year, start_month), frequency = 12)
  out_info <- tsoutliers(ts_data)
  
  # Construct dates
  dates <- seq.Date(
    from = as.Date(paste0(start_year, "-", sprintf("%02d", start_month), "-01")),
    by = "month", length.out = length(ts_data)
  )
  df_plot <- data.frame(Date = dates, Value = as.numeric(ts_data))
  
  # Identify outlier points and their replacements
  if (!is.null(out_info$index)) {
    outlier_dates <- dates[out_info$index]
    outlier_vals  <- ts_data[out_info$index]
    repl_vals     <- out_info$replacements
  } else {
    outlier_dates <- NULL
    outlier_vals  <- NULL
    repl_vals     <- NULL
  }
  
  # Build the plot
  p <- ggplot(df_plot, aes(x = Date, y = Value)) +
    geom_line(color = "black") +
    geom_point(data = data.frame(Date = outlier_dates, Value = outlier_vals),
               aes(x = Date, y = Value), color = "red", size = 2) +
    geom_point(data = data.frame(Date = outlier_dates, Value = repl_vals),
               aes(x = Date, y = Value), color = "blue", shape = 18, size = 2) +
    labs(
      title = paste("Outliers in", varname),
      x = "Date",
      y = varname
    ) +
    theme_minimal()
  
  return(p)
}

## Forecasting functions

# 1. Compute rolling forecasts (ARIMA + Prophet)
compute_cpi_forecasts <- function(df, date_col = "Date", value_col = "y",
                                  cutoffs = seq(from = as.Date("2021-02-01"),
                                                to   = as.Date("2023-02-01"),
                                                by   = "year")) {
  df <- df %>% select({{date_col}} := !!sym(date_col), {{value_col}} := !!sym(value_col)) %>% arrange({{date_col}})
  
  map_dfr(cutoffs, function(cutoff) {
    train <- df %>% filter({{date_col}} <= cutoff)
    test  <- df %>% filter({{date_col}} >  cutoff,
                           {{date_col}} <= (cutoff %m+% months(12)))
    
    # ARIMA
    fit_a <- forecast::auto.arima(train[[value_col]])
    fc_a  <- forecast::forecast(fit_a, h = nrow(test))
    arima_tbl <- tibble(
      Date     = test[[date_col]],
      model    = "ARIMA",
      cutoff   = cutoff,
      forecast = as.numeric(fc_a$mean)
    )
    
    # Prophet
    df_prop <- train %>% rename(ds = {{date_col}}, y = {{value_col}})
    m_p      <- prophet::prophet(df_prop,
                                 yearly.seasonality = TRUE,
                                 weekly.seasonality = FALSE,
                                 daily.seasonality  = FALSE)
    future   <- prophet::make_future_dataframe(m_p,
                                               periods = nrow(test),
                                               freq    = "month")
    fc_p     <- prophet::predict(m_p, future)
    prop_tbl <- tibble(
      Date     = test[[date_col]],
      model    = "Prophet",
      cutoff   = cutoff,
      forecast = tail(fc_p$yhat, nrow(test))
    )
    
    bind_rows(arima_tbl, prop_tbl)
  })
}

# 2. Plot forecasts vs actual
plot_cpi_forecasts <- function(df, forecasts, date_col = "Date", value_col = "y",
                               model = c("ARIMA", "Prophet")) {
  df_act <- df %>% select({{date_col}} := !!sym(date_col), {{value_col}} := !!sym(value_col))
  
  ggplot() +
    geom_line(data = df_act, aes(x = {{date_col}}, y = {{value_col}}), color = "black") +
    geom_line(data = forecasts %>% filter(model == !!model),
              aes(x = Date, y = forecast, group = cutoff, color = as.character(cutoff)),
              linetype = "dashed", size = 0.8) +
    labs(
      title = paste0(model, " Forecasts vs Actual (", value_col, ")"),
      x = "",
      y = value_col,
      color = "Train cutoff"
    ) +
    theme_minimal()
}

# 3. Summarize forecast error metrics
summarize_cpi_forecast_errors <- function(actual_df, forecasts, date_col = "Date", value_col = "y") {
  actual_vals <- actual_df[[value_col]]
  
  forecasts %>%
    group_by(model, cutoff) %>%
    summarise(
      RMSE = yardstick::rmse_vec(actual_vals, forecast),
      MAPE = yardstick::mape_vec(actual_vals, forecast),
      .groups = "drop"
    ) %>%
    arrange(model, cutoff)
}