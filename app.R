# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(shiny)
library(googlesheets4)
library(plotly)
library(thematic)
library(shinyWidgets)
library(styler)




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
      textOutput("current_temp")
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
                  c("Forbes East", "Forbes West", "Marley 1", "Marley 2")
      )#,
      # # Lets the user pick the date that the plot shows
      # sliderInput("chosen_date", "Date",
      #             min = min(mesquite$date),
      #             max = max(mesquite$date),
      #             value = c(
      #               min(mesquite$date),
      #               max(mesquite$date)
      #             )
      # ),
      # sliderInput("chosen_interval", "Sample interval (min)",
      #             min = 1,
      #             max = 20,
      #             value = 5
      # )
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
  selected <- reactive({
    if (input$sensor == "Forbes East"){
      dataset <- import_dataset("1Vn5eo55o_ABrSc9ekSKeelteE-NmFAAeg8tsUIR_9JA")
    }
    else if (input$sensor == "Forbes West"){
      dataset <- import_dataset("1y4H7i5Ay6bmXjTwesw4RoBO3nNCprKDssm3WPZ-BuuQ")
    }
    else if (input$sensor == "Marley 1"){
      dataset <- import_dataset("1cF2DWhlWZ6CYsZU7B6akx4X9EFrqh33oHst0ZKH6AYA")
    }
    else if (input$sensor == "Marley 2"){
      dataset <- import_dataset("1T4WOJAhyQWPGwmT65P76vGczjl4_2LVEhvAzhqwzcrw")
    }
    return(dataset)
  })
  
  # Getting the current stats for the selected datasheet
  current_temp <- reactive(selected()$temp_c[nrow(selected())])
  current_humidity <- reactive(selected()$humidity[nrow(selected())])
  current_time <- reactive(paste(toString(selected()$date[nrow(selected())]),
                        "at",
                        toString(selected()$time[nrow(selected())]),
                        sep = " "))
  
  # selected <- reactive(input_dataset %>%
  #                        filter(row_number() %% input$chosen_interval == 0) %>%
  #                        filter(date >= input$chosen_date[1] &
  #                                 date <= input$chosen_date[2]))
  
  output$temp_plot <- renderPlotly({
    ggplotly(selected() %>%
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
    ggplotly(selected() %>%
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
  
  output$current_temp <- renderText({
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
