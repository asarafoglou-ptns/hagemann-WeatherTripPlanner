# enter path to where functions file is stored
source('/Users/JasminHagemann/Desktop/GitHub/hagemann-WeatherTripPlanner/WeatherTripPlanner/R/functions_TripPlanner.R')

# to do:
# 1) in overview tab: change so that Start.Date and End.Date is displayed as "Start Date" and "End Date"
# 2) in comparison graph: make weather parameter on y-axis bigger and horizontal and better labeling
# instead of temperature_max it should be Maximum Temperature

# helper function to define UI for each period details tab
period_details_ui <- function(period_index) {
  tagList(
    fluidRow(
      column(4, shiny::icon("calendar-alt"), shiny::textOutput(paste0("date_range_", period_index))),
      column(4, shiny::icon("sun"), shiny::textOutput(paste0("sunlight_", period_index))),
      column(4, shiny::icon("cloud-rain"), shiny::textOutput(paste0("precipitation_", period_index))),
      column(4, shiny::icon("snowflake"), shiny::textOutput(paste0("snow_", period_index))),
      column(4, shiny::icon("thermometer-half"), shiny::textOutput(paste0("temperature_", period_index))),
      column(4, shiny::icon("thermometer-full"), shiny::textOutput(paste0("max_temperature_", period_index))),
      column(4, shiny::icon("thermometer-empty"), shiny::textOutput(paste0("min_temperature_", period_index))),
      column(4, shiny::icon("wind"), shiny::textOutput(paste0("wind_", period_index)))
    )
  )
}

ui <- fluidPage(
  titlePanel("Weather Trip Planner"),
  sidebarLayout(
    sidebarPanel(
      textInput("location", "Location (City/zip code, Country)"),
      selectInput("season", "Season", choices = c("Winter", "Spring", "Summer", "Fall")),
      radioButtons("preference", "Preference", choices = list("Temperature" = "temp", "Precipitation" = "precip")),
      conditionalPanel(
        condition = "input.preference == 'temp'",
        selectInput("temp_pref", "Temperature Preference", choices = list("Warm" = "warm", "Cold" = "cold"))
      ),
      conditionalPanel(
        condition = "input.preference == 'precip'",
        selectInput("precip_pref", "Precipitation Preference", choices = list("None" = "none", "Rain" = "rain", "Snow" = "snow"))
      ),
      numericInput("duration", "Trip Duration (days)", value = 7),
      actionButton("submit", "Show Best Periods")
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Overview", tableOutput("overview")),
                  tabPanel("Period 1", period_details_ui(1)),
                  tabPanel("Period 2", period_details_ui(2)),
                  tabPanel("Period 3", period_details_ui(3)),
                  tabPanel("Comparison",
                           checkboxGroupInput("compare_params", "Select Parameters to Compare", 
                                              choices = c("Max Temperature" = "temperature_max", "Min Temperature" = "temperature_min", 
                                                          "Mean Temperature" = "temperature_mean", "Daylight Duration" = "daylight_duration", 
                                                          "Precipitation" = "precipitation_sum", "Rain" = "rain_sum", 
                                                          "Snowfall" = "snowfall_sum", "Precipitation Hours" = "precipitation_hours", 
                                                          "Wind Speed" = "wind_speed_10m_max"),
                                              selected = c("temperature_max", "temperature_min", "temperature_mean")),
                           plotOutput("comparison"))
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$submit, {
    location <- input$location
    season <- input$season
    duration <- input$duration
    
    if (input$preference == "temp") {
      temp_pref <- input$temp_pref
      ranked_intervals <- rank_intervals_temp(duration, location, season = season, temp_pref = temp_pref) # WeatherTripPlanner::
    } else if (input$preference == "precip") {
      precip_pref <- input$precip_pref
      ranked_intervals <- rank_intervals_precip(duration, location, season = season, precip_pref = precip_pref) # WeatherTripPlanner::
    }
    
    top_intervals <- ranked_intervals[1:3]
    
    output$overview <- renderTable({
      data.frame(
        Period = paste0("Period ", 1:3),
        "Start Date" = sapply(top_intervals, function(x) min(x$date)),
        "End Date" = sapply(top_intervals, function(x) max(x$date))
      )
    })
    
    for (i in 1:3) {
      period_index <- i
      selected_period <- top_intervals[[period_index]]
      
      local({
        index <- period_index
        period <- selected_period
        
        output[[paste0("date_range_", index)]] <- renderText({
          paste("Date Range:", min(period$date), "to", max(period$date))
        })
        
        output[[paste0("sunlight_", index)]] <- renderText({
          paste("Sunlight Duration:", round(mean(period$daylight_duration, na.rm = TRUE), digits = 1), "hours")
        })
        
        output[[paste0("temperature_", index)]] <- renderText({
          paste("Average Temperature:", round(mean(period$temperature_mean, na.rm = TRUE), digits = 1), "°C")
        })
        
        output[[paste0("max_temperature_", index)]] <- renderText({
          paste("Maximum Temperature:", round(mean(period$temperature_max, na.rm = TRUE), digits = 1), "°C")
        })
        
        output[[paste0("min_temperature_", index)]] <- renderText({
          paste("Minimum Temperature:", round(mean(period$temperature_min, na.rm = TRUE), digits = 1), "°C")
        })
        
        output[[paste0("precipitation_", index)]] <- renderText({
          paste("Precipitation:", round(mean(period$precipitation_sum, na.rm = TRUE), digits = 1), "mm")
        })
        
        output[[paste0("snow_", index)]] <- renderText({
          paste("Snowfall:", round(mean(period$snowfall_sum, na.rm = TRUE), digits = 1), "mm")
        })
        
        output[[paste0("wind_", index)]] <- renderText({
          paste("Wind Speed:", round(mean(period$wind_speed_10m_max, na.rm = TRUE), digits = 1), "m/s")
        })
      })
    }
    
    # the comparison plots are still under construction
    # check x axis labeling!!!
    
    output$comparison <- renderPlot({
      selected_params <- input$compare_params
      
      label_mapping <- c(
        "temperature_max" = "Maximum Temperature",
        "temperature_min" = "Minimum Temperature",
        "temperature_mean" = "Mean Temperature",
        "daylight_duration" = "Daylight Duration",
        "precipitation_sum" = "Precipitation",
        "rain_sum" = "Rain",
        "snowfall_sum" = "Snowfall",
        "precipitation_hours" = "Precipitation Hours",
        "wind_speed_10m_max" = "Wind Speed"
      )
      
      readable_params <- label_mapping[selected_params]
      
      comparison_data <- do.call(rbind, lapply(top_intervals, function(interval) {
        sapply(selected_params, function(param) mean(interval[[param]], na.rm = TRUE))
      }))
      
      comparison_data <- data.frame(comparison_data)
      colnames(comparison_data) <- readable_params
      comparison_data$Period <- paste0("Period ", 1:3)
      
      comparison_data_long <- reshape2::melt(comparison_data, id.vars = "Period")
      
      ggplot2::ggplot(comparison_data_long, aes(x = variable, y = value, fill = Period)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::labs(y = "Value", fill = "Period") +  
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 14, angle = 45, hjust = 1),
          axis.text.y = ggplot2::element_text(size = 14) 
        ) +
        ggplot2::scale_x_discrete(labels = function(x) label_mapping[x], breaks = names(label_mapping))
    })
  })
}

shinyApp(ui = ui, server = server)