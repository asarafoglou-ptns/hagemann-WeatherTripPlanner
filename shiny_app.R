# Script for the shiny app Weather Trip Planner by Jasmin Hagemann

## Please start the application by clicking on "Run App" in the top right corner.

devtools::install_github("asarafoglou-ptns/hagemann-WeatherTripPlanner/WeatherTripPlanner")
library("WeatherTripPlanner")

ui <- fluidPage(
  titlePanel("Weather Trip Planner"),
  div(id = "intro_text", 
      p("Welcome to the Weather Trip Planner!"),
      p("This application is designed to help you decide the best time for your next trip. 
        Start by entering the location you want to visit, the season you plan to travel in 
        - Spring (March until May), Summer (June until August), 
        Fall (September until November), Winter (December until Feburary) -  
        and your weather preferences. You can specify your preference for temperature—whether 
        you prefer it warm or cold—and the app will rank all possible time periods accordingly. 
        Alternatively, you can select your preference for precipitation 
        (any kind of frozen or liquid water falling from the sky, such as rain, snow, sleet, or hail). 
        You can choose whether you prefer rainy, snowy, or dry weather. 
        If you prefer dry conditions, select 'none.' Continue by entering the 
        number of days you plan to stay at your destination and click 'Show Best Periods' 
        to get your results."),
      p("Bon Voyage!")
  ),
  sidebarLayout(
    sidebarPanel(
      textInput("location", "Location (City/zip code, Country)"),
      selectInput("season", "Season", choices = c("Spring", "Summer", "Fall", "Winter")),
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
                  tabPanel("Overview", 
                           tableOutput("overview"),
                           uiOutput("overview_text")
                  ),
                  tabPanel("Period 1", period_details_ui(1)),
                  tabPanel("Period 2", period_details_ui(2)),
                  tabPanel("Period 3", period_details_ui(3)),
                  tabPanel("Comparison",
                           checkboxGroupInput("compare_params", "Select Parameters to Compare", 
                                              choices = c("Max Temperature" = "temperature_max", 
                                                          "Min Temperature" = "temperature_min", 
                                                          "Mean Temperature" = "temperature_mean", 
                                                          "Precipitation" = "precipitation_sum", 
                                                          "Rain" = "rain_sum", 
                                                          "Snowfall" = "snowfall_sum", 
                                                          "Precipitation Hours" = "precipitation_hours", 
                                                          "Wind Speed" = "wind_speed_10m_max"),
                                              selected = c("temperature_max", "temperature_min", "temperature_mean")),
                           plotOutput("comparison"))
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$submit, {
    removeUI(selector = "#intro_text")
    
    location <- input$location
    season <- input$season
    duration <- input$duration
    
    if (input$preference == "temp") {
      temp_pref <- input$temp_pref
      ranked_intervals <- rank_intervals_temp(duration, location, season = season, temp_pref = temp_pref) 
    } else if (input$preference == "precip") {
      precip_pref <- input$precip_pref
      ranked_intervals <- rank_intervals_precip(duration, location, season = season, precip_pref = precip_pref)
    }
    
    top_intervals <- ranked_intervals[1:3]
    
    output$overview <- renderTable({
      data.frame(
        Period = paste0("Period ", 1:3),
        "Start Date" = sapply(top_intervals, function(x) min(x$date)),
        "End Date" = sapply(top_intervals, function(x) max(x$date)),
        check.names = FALSE
      )
    })
    
    output$overview_text <- renderUI({
      tagList(
        p("According to the weather data of the last 10 years, these periods best match your weather preferences at your chosen location."),
        p("To the right, there is a tab for each period where you can see the average values of different weather parameters in more detail."),
        p("Please note that the start and end dates are in the format mm-dd.")
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
    
    output$comparison <- renderPlot({
      selected_params <- input$compare_params
      
      validate(
        need(length(selected_params) > 0, "To visually compare the periods, please select at least one weather parameter.")
      )
      
      label_mapping <- c(
        "temperature_max" = "Maximum Temperature",
        "temperature_min" = "Minimum Temperature",
        "temperature_mean" = "Mean Temperature",
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
      colnames(comparison_data) <- selected_params
      comparison_data$Period <- paste0("Period ", 1:nrow(comparison_data))
      comparison_data_long <- tidyr::pivot_longer(comparison_data, cols = -Period, names_to = "variable", values_to = "value")
      comparison_data_long$variable <- factor(comparison_data_long$variable, levels = selected_params, labels = readable_params)
      
      ggplot2::ggplot(comparison_data_long, ggplot2::aes(x = variable, y = value, fill = Period)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::labs(x = "Weather Parameter", y = "Value", fill = "Period") +  
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 14, angle = 45, hjust = 1),
          axis.text.y = ggplot2::element_text(size = 14) 
        )
    })
  })
}

shinyApp(ui = ui, server = server)
