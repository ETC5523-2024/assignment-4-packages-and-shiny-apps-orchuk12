library(shiny)
library(tidyverse)

# Define the UI
ui <- fluidPage(
  titlePanel("Electric Vehicle Finder"),

  sidebarLayout(
    sidebarPanel(
      # Slider input for electric range
      sliderInput("mileage",
                  "Desired Electric Range (in miles):",
                  min = 0, max = 337, value = c(0, 100)),

      # Select input for car make
      selectInput("make",
                  "Select Car Make:",
                  choices = unique(clean_vehicle$make),
                  selected = NULL,
                  multiple = TRUE),

      # Select input for vehicle type
      selectInput("vehicle_type",
                  "Select Type of Vehicle:",
                  choices = c("Battery Electric Vehicle (BEV)",
                              "Plug-in Hybrid Electric Vehicle (PHEV)"),
                  selected = NULL,
                  multiple = TRUE),

      # Action button to trigger filtering
      actionButton("filter_btn", "Find Vehicles")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Filtered Vehicles", tableOutput("filtered_table")),
        tabPanel("Summary", verbatimTextOutput("summary_text"))
      )
    )
  )
)

# Define the server
server <- function(input, output, session) {

  # Reactive expression to filter data based on user input
  filtered_data <- eventReactive(input$filter_btn, {
    filter_vehicles(
      data = clean_vehicle,
      mileage_range = input$mileage,
      car_make = input$make,
      vehicle_type = input$vehicle_type
    ) %>%
      select(model_year,
             make,
             model,
             electric_vehicle_type,
             electric_range)  # Select relevant columns
  })

  # Display the filtered vehicle data in a table
  output$filtered_table <- renderTable({
    req(filtered_data())  # Ensure filtered data exists
    head(filtered_data())  # Display only the selected columns
  })

  # Generate and display summary statistics for the filtered data
  output$summary_text <- renderPrint({
    req(filtered_data())  # Ensure filtered data exists
    summary_stats <- summary_filtered_data(filtered_data())

    cat("Average Electric Range:", summary_stats$avg_electric_range, "\n")
    cat("Most Common Make:", summary_stats$most_common_make, "\n")
    cat("Vehicle Type Distribution:\n")
    print(summary_stats$vehicle_type_count)
  })
}


# Run the application
shinyApp(ui = ui, server = server)
