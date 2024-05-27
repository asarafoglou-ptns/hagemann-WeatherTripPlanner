# functions for weather trip planner

#' @title Get Coordinates
#' @description 
#' get_coordinates is used to get the latitude and longitude of any location 
#' given as input when calling the function.
#' @param location A location in the format 'City/zip code, Country'.
#' @return A list with the coordinates (latitude and longitude) of the given location.
#' The keys of the list are "latitude" and "longitude", and their respective values 
#' are the latitude and longitude coordinates extracted from the API response.
#' @examples
#' get_coordinates("Amsterdam, Netherlands")
#' get_coordinates("Berlin, Germany")
#' get_coordinates("5020, Austria")
#' @export
get_coordinates <- function(location) {
  
  api_key <- "f64ebf508b21463287ef1fc29cbaf119"
  base_url <- "https://api.opencagedata.com/geocode/v1/json"
  query <- utils::URLencode(location)
  request_url <- paste0(base_url, "?q=", query, "&key=", api_key)
  
  response <- httr::GET(request_url)
  
  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve data. Please check the location and try again.")
  }
  
  data <- jsonlite::fromJSON(httr::content(response, as = "text"))
  
  # Check if there are results
  if (length(data$results) == 0) {
    stop("No results found for the given location.")
  }
  
  latitude <- data$results$geometry$lat[1]
  longitude <- data$results$geometry$lng[1]
  
  return(list(latitude = latitude, longitude = longitude))
}

#' @title Get Weather Data
#' @description 
#' get_weather_data is used to retrieve historical weather data from the 
#' Open-Meteo API for a specific location and date range.
#' @param location A location in the format 'City/zip code, Country'.
#' @param start_date The start date for the weather data in the format "YYYY-MM-DD".
#' The default value is 2014-01-01.
#' @param end_date The end date for the weather data in the format "YYYY-MM-DD".
#' The default value is today's date. 
#' @return The parsed weather data as a list.
#' @examples
#' get_weather_data("Amsterdam, Netherlands", "2014-01-01", "2014-01-02")
#' get_weather_data("Amsterdam, Netherlands")
#' get_weather_data("Tokyo, Japan", "2024-03-03", "2024-03-15")
#' get_weather_data("Tokyo, Japan")
#' @export
get_weather_data <- function(location,  
                             start_date = "2014-01-01", 
                             end_date = as.character(lubridate::today())) {
  
  api_url <- "https://archive-api.open-meteo.com/v1/archive"
  
  coordinates <- get_coordinates(location)
  
  params <- list(
    latitude = coordinates$latitude,
    longitude = coordinates$longitude,
    start_date = start_date,
    end_date = end_date,
    daily = paste(c("weather_code", "temperature_2m_max", "temperature_2m_min", "temperature_2m_mean", "daylight_duration", "sunshine_duration", "precipitation_sum", "rain_sum", "snowfall_sum", "precipitation_hours", "wind_speed_10m_max"), collapse = ",")
  )
  
  res <- httr::GET(api_url, query = params)
  
  # Check for request success
  if (httr::status_code(res) != 200) {
    stop("Failed to fetch data. Status code: ", httr::status_code(res))
  }
  
  # Parse the JSON content
  content <- httr::content(res, "text", encoding = "UTF-8")
  weather_data <- jsonlite::fromJSON(content, flatten = TRUE)
  
  return(weather_data)
}


#' @title Process Weather Data
#' @description 
#' process_weather_data processes the data which was retrieved with the 
#' get_weather_data function and creates a new data frame with the daily weather data.
#' @param location A location in the format 'City/zip code, Country'.
#' @param start_date The start date for the weather data in the format "YYYY-MM-DD".
#' The default value is 2014-01-01.
#' @param end_date The end date for the weather data in the format "YYYY-MM-DD".
#' The default value is today's date. 
#' @return A data frame with the weather data for each day in the indicated
#' time period at the given location.
#' @examples
#' process_weather_data("Tokyo, Japan", "2024-03-03", "2024-03-15")
#' process_weather_data("Amsterdam, Netherlands")
#' @export
process_weather_data <- function(location,  
                                 start_date = "2014-01-01", 
                                 end_date = as.character(lubridate::today())) {
  
  weather_data <- get_weather_data(location, start_date, end_date)
  
  daily <- weather_data$daily
  
  daily_data <- data.frame(
    date = seq(
      as.Date(start_date),
      as.Date(end_date),
      by = "day"
    )[1:length(daily$weather_code)], 
    weather_code = daily$weather_code,
    temperature_max = daily$temperature_2m_max,
    temperature_min = daily$temperature_2m_min,
    temperature_mean = daily$temperature_2m_mean,
    daylight_duration = daily$daylight_duration,
    sunshine_duration = daily$sunshine_duration,
    precipitation_sum = daily$precipitation_sum,
    rain_sum = daily$rain_sum,
    snowfall_sum = daily$snowfall_sum,
    precipitation_hours = daily$precipitation_hours,
    wind_speed_10m_max = daily$wind_speed_10m_max
  )
  return(daily_data)
}

#' @title Filter Weather Data 
#' @description filter_by_season filters a data frame by the indicated season. 
#' @param location A location in the format 'City/zip code, Country'.
#' @param start_date The start date for the weather data in the format "YYYY-MM-DD".
#' The default value is 2014-01-01.
#' @param end_date The end date for the weather data in the format "YYYY-MM-DD".
#' The default value is today's date. 
#' @param season A season (winter, spring, summer, fall).
#' @return a filtered data frame containing the original data for all of the months
#' of the indicated season. 
#' @examples
#' filter_by_season("Munich, Germany", "2023-01-10", "2024-05-15", "spring")
#' filter_by_season("Leiden, Netherlands", season = "summer")
#' filter_by_season("08001, Spain", season = "winter")
#' @export
filter_by_season <- function(location,  
                             start_date = "2014-01-01", 
                             end_date = as.character(lubridate::today()), 
                             season) {
  
  df <- process_weather_data(location, start_date, end_date)
  
  # Ensure the first column is in Date format
  if (!inherits(df[[1]], "Date")) {
    df[[1]] <- as.Date(df[[1]], format = "%Y-%m-%d")
  }
  
  # Extract the month from the date
  df$Month <- format(df[[1]], "%m")
  
  # Define the months for each season
  seasons <- list(
    Winter = c("12", "01", "02"),
    Spring = c("03", "04", "05"),
    Summer = c("06", "07", "08"),
    Fall = c("09", "10", "11")
  )
  
  # Check if the input season is valid
  if (!season %in% names(seasons)) {
    stop("Invalid season. Choose from 'winter', 'spring', 'summer', or 'fall'.")
  }
  
  # Filter the dataframe for the given season
  filtered_df <- df[df$Month %in% seasons[[season]], ]
  
  # Drop the Month column before returning the filtered dataframe
  filtered_df$Month <- NULL
  
  return(filtered_df)
}


#' @title Consecutive Day Intervals with Average Weather Parameters
#' @description generate_intervals creates all possible intervals of 
#' consecutive days based on the specified number of days. The intervals contain 
#' the averages of the weather parameters for all days of the months in the indicated season. 
#' @param num_days The number of consecutive days in each interval.
#' @param location A location in the format 'City/zip code, Country'.
#' @param start_date The start date for the weather data in the format "YYYY-MM-DD".
#' The default value is 2014-01-01.
#' @param end_date The end date for the weather data in the format "YYYY-MM-DD".
#' The default value is today's date. 
#' @param season A season (winter, spring, summer, fall).
#' @return A data frame containing all possible intervals of consecutive days 
#' with the average weather parameters.
#' @examples
#' generate_intervals(5, "Munich, Germany", "2023-01-10", "2024-05-15", "spring")
#' generate_intervals(7, "Leiden, Netherlands", season = "summer")
#' generate_intervals(3, "08001, Spain", season = "winter")
#' @export
generate_intervals <- function(num_days,
                               location, 
                               start_date = "2014-01-01", 
                               end_date = as.character(lubridate::today()), 
                               season) {
  
  # Filter the weather data for the specified season
  filtered_df <- filter_by_season(location, start_date, end_date, season)
  
  # Remove the year information from the date column
  filtered_df$date <- format(filtered_df$date, "%m-%d")
  
  # Create all possible intervals of consecutive days
  all_intervals <- lapply(1:(nrow(filtered_df) - num_days + 1), function(i) filtered_df[i:(i + num_days - 1), ])
  
  return(all_intervals)
}


#' @title Rank Intervals by Temperature
#' @description rank_intervals_temp ranks the intervals based on temperature preference.
#' @param intervals A list of data frames containing intervals of consecutive days with weather data.
#' @param temp_pref The ranking criteria: "warm" to rank by highest temperature or "cold" to rank by lowest temperature.
#' @return A list of ranked intervals based on temperature criteria.
#' @examples
#' rank_intervals_temp(5, "Munich, Germany", "2023-01-10", "2024-05-15", "spring", "warm")
#' rank_intervals_temp(7, "Amsterdam, Netherlands", "2024-01-01", "2024-01-10", "winter", "cold")
#' rank_intervals_temp(7, "Milan, Italy", "2024-01-01", "2024-03-01", "winter", "cold")
rank_intervals_temp <- function(num_days, 
                                location, 
                                start_date = "2014-01-01", 
                                end_date = as.character(lubridate::today()), 
                                season,
                                temp_pref) {
  
  intervals <- generate_intervals(num_days, location, start_date, end_date, season) 
  
  # Calculate mean temperature for each interval
  mean_temperatures <- sapply(intervals, function(interval) mean(interval$temperature_mean))
  
  # Rank intervals based on mean temperature
  if (temp_pref == "warm") {
    ranked_intervals <- intervals[order(mean_temperatures, decreasing = TRUE)]
  } else if (temp_pref == "cold") {
    ranked_intervals <- intervals[order(mean_temperatures)]
  } else {
    stop("Invalid rank criteria. Choose from 'warm' or 'cold'.")
  }
  
  return(ranked_intervals)
}


#' @title Rank Intervals by Precipitation
#' @description rank_intervals_temp first calculates the total precipitation, 
#' snowfall, and rain for each interval. It then ranks the intervals based on 
#' the specified precipitation preference ("none", "snow", or "rain")
#' @param intervals A list of data frames containing intervals of consecutive days with weather data.
#' @param precip_pref The ranking criteria: "none", "snow", or "rain".
#' @return A list of ranked intervals based on the precipitation preference.
#' @examples
#' rank_intervals_precip(5, "Munich, Germany", "2023-01-10", "2024-05-15", "spring", "none")
#' rank_intervals_precip(7, "Amsterdam, Netherlands", "2024-01-01", "2024-01-10", "winter", "snow")
#' rank_intervals_precip(7, "Milan, Italy", "2024-01-01", "2024-03-01", "winter", "rain")
rank_intervals_precip <- function(num_days, 
                                  location, 
                                  start_date = "2014-01-01", 
                                  end_date = as.character(lubridate::today()), 
                                  season,
                                  precip_pref) {
  
  intervals <- generate_intervals(num_days, location, start_date, end_date, season) 
  
  # Calculate total precipitation, snow, and rain for each interval
  total_precip <- sapply(intervals, function(interval) sum(interval$precipitation_sum))
  total_snow <- sapply(intervals, function(interval) sum(interval$snowfall_sum))
  total_rain <- sapply(intervals, function(interval) sum(interval$rain_sum))
  
  # Rank intervals based on precipitation preference
  if (precip_pref == "none") {
    ranked_intervals <- intervals[order(total_precip)]
  } else if (precip_pref == "snow") {
    ranked_intervals <- intervals[order(total_snow, decreasing = TRUE)]
  } else if (precip_pref == "rain") {
    ranked_intervals <- intervals[order(total_rain, decreasing = TRUE)]
  } else {
    stop("Invalid precipitation preference. Choose from 'none', 'snow', or 'rain'.")
  }
  
  return(ranked_intervals)
}