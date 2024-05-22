# functions for weather trip planner

# rewrite: arguments instead of input function! 

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
  query <- URLencode(location)
  request_url <- paste0(base_url, "?q=", query, "&key=", api_key)
  
  response <- httr::GET(request_url)
  
  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve data. Please check the location and try again.")
  }
  
  data <- jsonlite::fromJSON(content(response, as = "text"))
  
  # Check if there are results
  if (length(data$results) == 0) {
    stop("No results found for the given location.")
  }
  
  latitude <- data$results$geometry$lat
  longitude <- data$results$geometry$lng
  
  # Check: print extracted coordinates
  # cat("Latitude:", latitude, "\n")
  # cat("Longitude:", longitude, "\n")
  
  return(list(latitude = latitude, longitude = longitude))
}

#' @title Get Weather Data
#' @description 
#' get_weather_data is used to retrieve historical weather data from the 
#' Open-Meteo API for a specific location and date range.
#' @param location A location in the format 'City/zip code, Country'.
#' @param start_date The start date for the weather data in the format "YYYY-MM-DD".
#' The default value is 2014-01-01
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
#' The default value is 2014-01-01
#' @param end_date The end date for the weather data in the format "YYYY-MM-DD".
#' The default value is today's date. 
#' @return A data frame with the weather data for each day in the indicated
#' time period at the gven location.
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
      as.Date("2014-01-01"),
      lubridate::today(),
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
