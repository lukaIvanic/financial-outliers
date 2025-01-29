library(shiny)
library(forecast)
library(ggplot2)
library(tseries)
library(lubridate)

# 1) Define the generatePlot function outside of ui and server
generatePlot <- function(x, y, plot_type = c("line", "bar"), 
                         main_title = "My Plot", 
                         x_label = "X-Axis", 
                         y_label = "Y-Axis") {
  plot_type <- match.arg(plot_type)
  if (plot_type == "line") {
    plot(x, y, type = "l", main = main_title, xlab = x_label, ylab = y_label)
  } else if (plot_type == "bar") {
    barplot(height = y, names.arg = x, main = main_title, xlab = x_label, ylab = y_label)
  }
}

ui <- fluidPage(
  # Make app fullscreen by setting margins to 0 and height to 100vh
  tags$head(
    tags$style(HTML("
      body {
        margin: 0;
        height: 100vh;
      }
      .container-fluid {
        padding: 15px;
        height: 100%;
      }
    "))
  ),
  
  # Create a row for the three dropdowns
  fluidRow(
    column(4,
           selectInput("dropdown1", "Select Cryptocurrency",
                       choices = c("Bitcoin"),
                       width = "100%"
           )
    ),
    column(4,
           dateRangeInput("dateRange", "Select Date Range",
                          start = Sys.Date() - 250,
                          end = Sys.Date()-150,
                          min = "2000-01-01",
                          max = Sys.Date(),
                          format = "yyyy-mm-dd",
                          separator = " to "
           )
    ),
    column(4,
           selectInput("dropdown3", "Select Analysis",
                       choices = c("Average volume in one day per hour"),
                       width = "100%"
           )
    )
  ),
  
  # Add centered button row
  fluidRow(
    column(12,
           div(style = "text-align: center; margin: 20px 0;",
               actionButton("calculate", "Generate Plots", 
                            class = "btn-primary btn-lg"
               )
           )
    )
  ),
  
  # Create space for plots
  fluidRow(
    column(12,
           plotOutput("plot1", height = "600px")
    )
  )
)


generatePlot <- function(x, y, plot_type = c("line", "bar"), 
                         main_title = "My Plot", 
                         x_label = "X-Axis", 
                         y_label = "Y-Axis") {
  plot_type <- match.arg(plot_type)
  
  if (plot_type == "line") {
    plot(x, y, type = "l",
         main = main_title,
         xlab = x_label,
         ylab = y_label)
  } else if (plot_type == "bar") {
    barplot(height = y, 
            names.arg = x, 
            main = main_title,
            xlab = x_label,
            ylab = y_label)
  }
}



source("analyses.R")



server <- function(input, output) {
  # Create reactive values to store plot state
  plot_data <- reactiveVal(NULL)
  
  # Observer for the calculate button
  observeEvent(input$calculate, {
    # Update plot data when button is clicked
    plot_data(TRUE)
  })
  
  # Render the Bitcoin closing price plot
  output$plot1 <- renderPlot({
    # Require button click before showing plot
    req(plot_data())
    
    # 1. Read CSV
    data <- read.csv("BTCUSDT1h.csv")  # columns: openTimeMs, open, high, low, close, volume, datetime_str
    
    # 2. Convert datetime_str to POSIXct
    data$datetime <- as.POSIXct(data$datetime_str)
    
    # 3. (Optional) Sort data by datetime (in case CSV is not in chronological order)
    data <- data[order(data$datetime), ]
    
    # 4. Subset data based on dateRange input
    #    Note: as.POSIXct() ensures compatible comparison
    start_date <- as.POSIXct(input$dateRange[1])
    end_date   <- as.POSIXct(input$dateRange[2])
    
    data_sub <- subset(data, datetime >= start_date & datetime <= end_date)
    
    # If no data falls in the date range, handle gracefully
    validate(
      need(nrow(data_sub) > 0, "No data available for the selected date range.")
    )
    
    # 5. Extract the x and y valus
    result <- getAnalysis(data_sub)
    x_vals <- result$x_values
    y_vals <- result$y_values
    
    plot_df <- data.frame(
      category = x_vals,
      avg_change = y_vals
    )
    
    # Bar plot of average price change by category  


    # 6. Decide the plot type (here we use "line" for a time series)
    my_plot_type <- "bar"
    
    # 7. Call your custom plotting function
    generatePlot(
      x = x_vals,
      y = y_vals,
      plot_type = my_plot_type,
      main_title = paste("Selected:", input$dropdown1,
                         "Dates:", format(input$dateRange[1]), "to", format(input$dateRange[2]),
                         input$dropdown3),
      x_label = "Time",
      y_label = "Closing Price (USD)"
    )
  })
}
shinyApp(ui = ui, server = server)
