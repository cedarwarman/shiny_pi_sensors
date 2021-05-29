# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(shiny)
library(googlesheets4)
library(plotly)
library(thematic)
library(shinyWidgets)
library(styler)


# Importing and looking at the data ---------------------------------------

# Importing the data from a Google sheet
gs4_deauth()
# home
# sheet_address <- "https://docs.google.com/spreadsheets/d/1v0W5jSeF_wNWV9JCeZlG_zNOOsADDlPmwQSymcgLEJQ/edit?usp=sharing"
# lab
sheet_address <- "https://docs.google.com/d/1Byxkjq1Jl_O6cyOrBp6yO_vHAI4FSbJkKZetLsOhwZc/edit?usp=sharing"

mesquite <- read_sheet(sheet_address)

# Making a single column for date and time
mesquite$date_time <- ymd_hms(paste(mesquite$date, mesquite$time))

# Changing the data column from a string to date type
mesquite$date <- ymd(mesquite$date)

# Getting the current temp
current_temp <- mesquite$temp_c[nrow(mesquite)]

# Getting the current husmidity
current_humidity <- mesquite$humidity[nrow(mesquite)]

# Getting the current time string
current_time <- paste(toString(mesquite$date[nrow(mesquite)]),
                      "at",
                      toString(mesquite$time[nrow(mesquite)]),
                      sep = " ")

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
      # Lets the user pick the date that the plot shows
      sliderInput("chosen_date", "Date",
                  min = min(mesquite$date),
                  max = max(mesquite$date),
                  value = c(
                    min(mesquite$date),
                    max(mesquite$date)
                  )
      ),
      sliderInput("chosen_interval", "Sample interval (min)",
                  min = 1,
                  max = 20,
                  value = 5
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
  selected <- reactive(mesquite %>%
                         filter(row_number() %% input$chosen_interval == 0) %>%
                         filter(date >= input$chosen_date[1] &
                                  date <= input$chosen_date[2]))
  
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
    paste0("Current temperature: ", current_temp, " ºC")
  })
  
  output$current_humidity <- renderText({
    paste0("Current humidity: ", current_humidity, "%")
  })
  
  output$current_time <- renderText({
    paste0("Updated ", current_time)
  })
}


shinyApp(ui, server)
