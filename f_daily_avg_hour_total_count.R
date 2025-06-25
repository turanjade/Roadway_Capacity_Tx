#### calculate daily average hourly total count for reference stations
daily_avg_hr_total = function(df) { # ID, DateTime, Total required
  
  library(dplyr)
  library(lubridate)

  df <- df %>%
    mutate(
      date = as.Date(DateTime),
      hour = sprintf("%02d", hour(DateTime)),
      wday = wday(date, label = TRUE)
    ) %>%
    filter(!wday %in% c("Sat", "Sun"))
  
  hourly_totals <- df %>%
    group_by(ID, date, hour) %>%
    summarise(
      hourly_total = if (n() == 4) sum(Total) else NA_real_,
      .groups = "drop"
    )
  
  all_locations <- unique(df$ID)
  all_dates <- unique(df$date)
  all_hours <- sprintf("%02d", 0:23)
  
  full_grid <- expand.grid(
    ID = all_locations,
    date = all_dates,
    hour = all_hours
  )
  
  full_data <- full_grid %>%
    left_join(hourly_totals, by = c("ID", "date", "hour"))
  
  avg_hourly_by_location <- full_data %>%
    group_by(ID, hour) %>%
    summarise(
      avg_total = mean(hourly_total, na.rm = T),
      .groups = "drop"
    )
  return(avg_hourly_by_location)
}

