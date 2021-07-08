# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(shiny)
library(googlesheets4)
library(plotly)
library(thematic)
library(shinyWidgets)
library(styler)

# For problems:
library(reactlog)
reactlog_enable()
# reactlogShow()
# reactlogReset()

# For Raspberry Pi, uncomment this:
options(bitmapType='cairo')


# Functions ---------------------------------------------------------------

# This function will run to import data whenever the user switches sensors
# or datasets from the two dropdown menus.
import_dataset <- function(sheet_id){
  # Avoiding permissions (make sure sheet is public)
  gs4_deauth()
  df <- read_sheet(sheet_id)
  
  # Making a single column for date and time
  df$date_time <- ymd_hms(paste(df$date, df$time))
  # Changing the data column from a string to date type
  df$date <- ymd(df$date)
  
  return(df)
}

# # This function gets the current temp, humidity, and time for the header.
# get_current_stats <- function(input_sheet){
#   # Getting the current temp
#   current_temp <- input_sheet$temp_c[nrow(input_sheet)]
#   
#   # Getting the current humidity
#   current_humidity <- input_sheet$humidity[nrow(input_sheet)]
#   
#   # Getting the current time string
#   current_time <- paste(toString(input_sheet$date[nrow(input_sheet)]),
#                         "at",
#                         toString(input_sheet$time[nrow(input_sheet)]),
#                         sep = " ")
#   
#   output_list <- list(current_temp, current_humidity, current_time)
#   names(output_list) <- c("temp", "humidity", "time")
#   return(output_list)
# }


# Making an app -----------------------------------------------------------

ui <- fillPage(
  padding = c(10, 40, 10, 40),
  theme = bslib::bs_theme(bootswatch = "darkly"),
  chooseSliderSkin("Shiny", color = "#404040"),
  
  # CSS for formatting the current temp and humidity text at the top of the page
  tags$head(
    tags$style(HTML("
      #current_temp {
        color: #ff6bd3;
        font-size: 24px;
        font-weight: bold;
      }
      #current_humidity {
        color: #42ff55;
        font-size: 24px;
        font-weight: bold;
      }
      #current_time {
        color: #ffffff;
        font-size: 24px;
        font-weight: bold;
      }"))
  ),
  
  # CSS for making the plots fit the page vertically
  # tags$style(HTML("#temp_plot {height: calc(50vh - 80px) !important;}
  #                 #humidity_plot {height: calc(50vh - 80px) !important;}")),
  tags$style(HTML("#temp_plot {height: calc(50vh - 40px) !important;}
                  #humidity_plot {height: calc(50vh - 40px) !important;}")),
  
  fluidRow(
    column(
      3, offset = 2, align = "center",
      uiOutput("current_temp")
    ),
    column(
      3, align = "center",
      textOutput("current_humidity")
    ),
    column(
      4, align = "center",
      textOutput("current_time")
    )
  ),
  fluidRow(
    column(
      2,
      # Lets the user choose the sensor to display
      selectInput("sensor", "Sensor", 
                  c("Forbes East", "Forbes West", "Marley Kelsey", "Marley Cedar")
      ),
      # Lets the user pick the date that the plot shows
      uiOutput("date_slider"),
      # sliderInput("chosen_date", "Date",
      #             min = min(mesquite$date),
      #             max = max(mesquite$date),
      #             value = c(
      #               min(mesquite$date),
      #               max(mesquite$date)
      #             )
      # ),
      # Lets the user pick a data sampling interval
      sliderInput("chosen_interval", "Sample interval (min)",
                  min = 2, # Fix this, it's actually doing every 4 minutes since the natural interval is now 2
                  max = 20,
                  value = 4,
                  step = 2
      ),
      actionButton(
        inputId = "refresh_data",
        label = "Refresh data"
      )
    ),
    column(
      10,
      fluidRow(
        # Shows temperature plot
        plotlyOutput("temp_plot", height = "100%", width = "100%")
      ),
      fluidRow(
        # Shows humidity plot
        plotlyOutput("humidity_plot", height = "100%", width = "100%")
      )
    )
  )
)

server <- function(input, output, session) {
  # Importing the correct dataset
  selected <- eventReactive(c(input$refresh_data, input$sensor), {
    if (input$sensor == "Forbes East"){
      dataset <- import_dataset("1Vn5eo55o_ABrSc9ekSKeelteE-NmFAAeg8tsUIR_9JA")
    }
    else if (input$sensor == "Forbes West"){
      dataset <- import_dataset("1y4H7i5Ay6bmXjTwesw4RoBO3nNCprKDssm3WPZ-BuuQ")
    }
    else if (input$sensor == "Marley Kelsey"){
      dataset <- import_dataset("1cF2DWhlWZ6CYsZU7B6akx4X9EFrqh33oHst0ZKH6AYA")
    }
    else if (input$sensor == "Marley Cedar"){
      dataset <- import_dataset("1T4WOJAhyQWPGwmT65P76vGczjl4_2LVEhvAzhqwzcrw")
    }
    return(dataset)
  }, ignoreNULL=FALSE, label = "Selected sensor") # Is this really necessary?
  
  # Making the date slider output
  output$date_slider <- renderUI({
    sliderInput("chosen_date", "Date", 
                min = min(selected()$date),
                max = max(selected()$date),
                value = c(max(selected()$date) - 2, max(selected()$date))
                
    )
  })
  
  # Filtering by interval and date
  filtered <- reactive({
    shiny::validate(need(input$chosen_date, message=FALSE)) # Necessary so that the initial 'null' of the date slider doesn't fuck it up
    (selected() %>%
       # Change this to "input$chosen_interval == 0" if 
       # spreadsheet time interval is 1 instead of 2
       filter(row_number() %% (input$chosen_interval / 2) == 0) %>%
       filter(date >= input$chosen_date[1] &
              date <= input$chosen_date[2]))
  }, label = "Filtered by date / interval")
  
  # Getting the current stats for the selected datasheet
  current_temp <- reactive(selected()$temp_c[nrow(selected())])
  current_humidity <- reactive(selected()$humidity[nrow(selected())])
  current_time <- reactive(paste(toString(selected()$date[nrow(selected())]),
                        "at",
                        toString(selected()$time[nrow(selected())]),
                        sep = " "))
  
  output$temp_plot <- renderPlotly({
    ggplotly(filtered() %>%
               ggplot(aes(date_time, temp_c)) +
               # geom_smooth(method = "loess", se = FALSE, span = 0.01, color = "white", size = 0.5) +
               geom_point(size = 0.5, shape = 16, alpha = 0.8, color = "#ff6bd3") +
               labs(x = "", y = "ºC") +
               theme_bw() +
               theme(
                 panel.border = element_blank(),
                 axis.line = element_line(color = "white", size = 1),
                 panel.grid = element_blank(),
                 axis.ticks = element_line(color = "white", size = 1),
                 axis.text = element_text(size = 10, face = "bold", color = "white"),
                 axis.title = element_text(size = 14, face = "bold", color = "white"),
                 panel.background = element_rect(fill = "#222222"),
                 plot.background = element_rect(fill = "#222222")
               )) %>%
      config(displayModeBar = FALSE)
  })
  
  output$humidity_plot <- renderPlotly({
    ggplotly(filtered() %>%
               ggplot(aes(date_time, humidity)) +
               # geom_smooth(method = "loess", se = FALSE, span = 0.01, color = "white", size = 0.5) +
               geom_point(size = 0.5, shape = 16, alpha = 0.8, color = "#42ff55") +
               labs(x = "Date and time", y = "% Humidity") +
               theme_bw() +
               theme(
                 panel.border = element_blank(),
                 axis.line = element_line(color = "white", size = 1),
                 panel.grid = element_blank(),
                 axis.ticks = element_line(color = "white", size = 1),
                 axis.text = element_text(size = 10, face = "bold", color = "white"),
                 axis.title = element_text(size = 14, face = "bold", color = "white"),
                 panel.background = element_rect(fill = "#222222"),
                 plot.background = element_rect(fill = "#222222")
               )) %>%
      config(displayModeBar = FALSE)
  })

  # Making the "Current temp, humidity, time" banner
  output$current_temp <- renderUI({
    paste0("Current temperature: ", current_temp(), " ºC")
  })
  output$current_humidity <- renderText({
    paste0("Current humidity: ", current_humidity(), "%")
  })
  output$current_time <- renderText({
    paste0("Updated: ", current_time())
  })
}


shinyApp(ui, server)
