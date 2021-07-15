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
# library(reactlog)
# reactlog_enable()
# reactlogShow()
# reactlogReset()

# For Raspberry Pi, uncomment this:
options(bitmapType = "cairo")


# Functions ---------------------------------------------------------------

# This function will run to import data whenever the user switches sensors
# or datasets from the two dropdown menus.
import_dataset <- function(sheet_id) {
  # Avoiding permissions (make sure sheet is public)
  gs4_deauth()
  df <- read_sheet(sheet_id)

  # Making a single column for date and time
  df$date_time <- ymd_hms(paste(df$date, df$time))
  # Changing the data column from a string to date type
  df$date <- ymd(df$date)

  return(df)
}

# This function will make a temperature plot
make_temp_plot <- function(input_df) {
  output_plot <- ggplotly(input_df %>%
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

  return(output_plot)
}

# This function will make a temperature plot with downtime highlighted
make_temp_plot_downtime <- function(input_df, original_df) {
  # Calculating intervals between sensor readings on the original selected() df
  # so that they won't be affected by changes in the interval slider. First
  # cropping the original df to match the filtered() df (here input_df)
  cropped_df <- original_df %>%
    filter(date >= min(input_df$date) &
      date <= max(input_df$date))

  # Sorting. Note: because of time inaccuracy, intervals shorter than ~10 minutes
  # probably aren't accurate (blame UA for blocking NTP port 123 and making me
  # use htpdate...)
  cropped_df <- cropped_df[order(cropped_df$date_time), ]

  # Calculating intervals
  cropped_df$intervals <- as.integer(difftime(cropped_df$date_time,
    lag(cropped_df$date_time, ),
    units = "mins"
  ))

  # Making the downtime rectangles
  if (length(na.omit(cropped_df$date_time[lead(cropped_df$intervals > 4)])) > 0) {
    rectangles <- data.frame(
      "xmins" = na.omit(cropped_df$date_time[lead(cropped_df$intervals > 4)]),
      "xmaxes" = na.omit(cropped_df$date_time[cropped_df$intervals > 4]),
      "ymins" = min(cropped_df$temp_c),
      "ymaxes" = max(cropped_df$temp_c)
    )

    # Making the plot with rectangles
    output_plot <- ggplotly(input_df %>%
      ggplot(aes(date_time, temp_c)) +
      # geom_smooth(method = "loess", se = FALSE, span = 0.01, color = "white", size = 0.5) +
      geom_rect(
        data = rectangles,
        inherit.aes = FALSE,
        aes(
          xmin = xmins,
          xmax = xmaxes,
          ymin = ymins,
          ymax = ymaxes
        ),
        fill = "orangered3"
      ) +
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

    return(output_plot)
  } else {
    # If there are no downtimes, it just makes a regular plot
    output_plot <- make_temp_plot(input_df)
    return(output_plot)
  }
}

# This function will make a humidity plot
make_humid_plot <- function(input_df) {
  output_plot <- ggplotly(input_df %>%
    ggplot(aes(date_time, humidity)) +
    # geom_smooth(method = "loess", se = FALSE, span = 0.01, color = "white", size = 0.5) +
    geom_point(size = 0.5, shape = 16, alpha = 0.8, color = "#42ff55") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), labels = seq(0, 100, 20)) +
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

  return(output_plot)
}

# This function will make a humidity plot with downtime highlighted
make_humid_plot_downtime <- function(input_df, original_df) {
  # Calculating intervals between sensor readings on the original selected() df
  # so that they won't be affected by changes in the interval slider. First
  # cropping the original df to match the filtered() df (here input_df)
  cropped_df <- original_df %>%
    filter(date >= min(input_df$date) &
      date <= max(input_df$date))

  # Sorting. Note: because of time inaccuracy, intervals shorter than ~10 minutes
  # probably aren't accurate (blame UA for blocking NTP port 123 and making me
  # use htpdate...)
  cropped_df <- cropped_df[order(cropped_df$date_time), ]

  # Calculating intervals
  cropped_df$intervals <- as.integer(difftime(cropped_df$date_time,
    lag(cropped_df$date_time, ),
    units = "mins"
  ))

  # Making the downtime rectangles
  if (length(na.omit(cropped_df$date_time[lead(cropped_df$intervals > 4)])) > 0) {
    rectangles <- data.frame(
      "xmins" = na.omit(cropped_df$date_time[lead(cropped_df$intervals > 4)]),
      "xmaxes" = na.omit(cropped_df$date_time[cropped_df$intervals > 4]),
      "ymins" = 0,
      "ymaxes" = 100
    )

    # Making the plot with rectangles
    output_plot <- ggplotly(input_df %>%
      ggplot(aes(date_time, humidity)) +
      # geom_smooth(method = "loess", se = FALSE, span = 0.01, color = "white", size = 0.5) +
      geom_rect(
        data = rectangles,
        inherit.aes = FALSE,
        aes(
          xmin = xmins,
          xmax = xmaxes,
          ymin = ymins,
          ymax = ymaxes
        ),
        fill = "orangered3"
      ) +
      geom_point(size = 0.5, shape = 16, alpha = 0.8, color = "#42ff55") +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), labels = seq(0, 100, 20)) +
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

    return(output_plot)
  } else {
    # If there are no downtimes, it just makes a regular plot
    output_plot <- make_humid_plot(input_df)
    return(output_plot)
  }
}


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
  tags$style(HTML("#temp_plot {height: calc(50vh - 40px) !important;}
                  #humidity_plot {height: calc(50vh - 40px) !important;}")),
  fluidRow(
    column(
      3,
      offset = 2, align = "center",
      uiOutput("current_temp")
    ),
    column(
      3,
      align = "center",
      textOutput("current_humidity")
    ),
    column(
      4,
      align = "center",
      textOutput("current_time")
    )
  ),
  fluidRow(
    column(
      2,
      # Lets the user choose the sensor to display (lab)
      selectInput(
        "sensor", "Sensor",
        c("Forbes East", "Forbes West", "Marley Kelsey", "Marley Cedar")
      ),
      # # Lets the user choose the sensor to display (home)
      # selectInput("sensor", "Sensor",
      #             c("Living room", "Outside")
      # ),
      # Lets the user pick the date that the plot shows
      uiOutput("date_slider"),
      # Lets the user pick a data sampling interval
      sliderInput("chosen_interval", "Sample interval (min)",
        min = 2,
        max = 20,
        value = 4,
        step = 2
      ),
      checkboxInput("checkbox", "Highlight downtime", value = FALSE),
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
  # Importing the correct dataset (lab)
  selected <- eventReactive(c(input$refresh_data, input$sensor),
    {
      if (input$sensor == "Forbes East") {
        dataset <- import_dataset("1Vn5eo55o_ABrSc9ekSKeelteE-NmFAAeg8tsUIR_9JA")
      }
      else if (input$sensor == "Forbes West") {
        dataset <- import_dataset("1y4H7i5Ay6bmXjTwesw4RoBO3nNCprKDssm3WPZ-BuuQ")
      }
      else if (input$sensor == "Marley Kelsey") {
        dataset <- import_dataset("1cF2DWhlWZ6CYsZU7B6akx4X9EFrqh33oHst0ZKH6AYA")
      }
      else if (input$sensor == "Marley Cedar") {
        dataset <- import_dataset("1T4WOJAhyQWPGwmT65P76vGczjl4_2LVEhvAzhqwzcrw")
      }
      return(dataset)
    },
    ignoreNULL = FALSE,
    label = "Selected sensor"
  )

  # # Importing the correct dataset (home)
  # selected <- eventReactive(c(input$refresh_data, input$sensor), {
  #   if (input$sensor == "Living room"){
  #     dataset <- import_dataset("1pb0uU-8VST4gp8zkbDiO0YNfAKdsnvoKgLZ63juG27I")
  #   }
  #   else if (input$sensor == "Outside"){
  #     dataset <- import_dataset("1ELcNVhti0JHUYfMLyu9pRZ_hoGN8uQ-Y8WViw9TLz8Q")
  #   }
  #   return(dataset)
  # }, ignoreNULL=FALSE, label = "Selected sensor")

  # Making the date slider output
  output$date_slider <- renderUI({
    sliderInput("chosen_date", "Date",
      min = min(selected()$date),
      max = max(selected()$date),
      value = c(max(selected()$date) - 2, max(selected()$date))
    )
  })

  # Filtering by interval and date
  filtered <- reactive(
    {
      shiny::validate(need(input$chosen_date, message = FALSE)) # Necessary so that the initial 'null' of the date slider doesn't fuck it up
      (selected() %>%
        # Change this to "input$chosen_interval == 0" if
        # spreadsheet time interval is 1 instead of 2
        filter(row_number() %% (input$chosen_interval / 2) == 0) %>%
        filter(date >= input$chosen_date[1] &
          date <= input$chosen_date[2]))
    },
    label = "Filtered by date / interval"
  )

  # Getting the current stats for the selected datasheet
  current_temp <- reactive(selected()$temp_c[nrow(selected())])
  current_humidity <- reactive(selected()$humidity[nrow(selected())])
  current_time <- reactive(paste(toString(selected()$date[nrow(selected())]),
    "at",
    toString(selected()$time[nrow(selected())]),
    sep = " "
  ))

  # Making the plots
  output$temp_plot <- renderPlotly({
    if (input$checkbox) {
      make_temp_plot_downtime(filtered(), selected())
    } else {
      make_temp_plot(filtered())
    }
  })

  output$humidity_plot <- renderPlotly({
    if (input$checkbox) {
      make_humid_plot_downtime(filtered(), selected())
    } else {
      make_humid_plot(filtered())
    }
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
