library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(plotly)
library(kableExtra)
library(DT)
library(leaflet)
library(sf)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Electric Vehicle Finder"),

  # Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Your Options", tabName = "options", icon = icon("car"))
    )
  ),

  # Body content
  dashboardBody(
    # Apply a theme from the dashboardthemes package
    shinyDashboardThemes(theme = "poor_mans_flatly"),

    tabItems(
      # Home Tab
      tabItem(tabName = "home",
              h1("Examining Electric Vehicle Data in Washington State, USA.",
                 br(),
                 br(),
                 style = "text-align: center; font-weight: bold"),
              h2("Purpose of This Dashboard",
                 style = "text-align: center; font-weight: bold"),
              fluidRow(
                column(width = 2),
                column(width = 8,
                       h4("This dashboard is part of the Shiny app developed for Assignment 4 for
                       the unit Communicating with Data (ETC5523) taken in Semester 2 of 2024 at
                       Monash University Clayton Campus. This dashboard aims to display statistics
                       and visualizations related to electric vehicles in the State of Washington,
                       USA. Factors such as electric vehicle range, types (BEV or PHEV), and
                       manufacturers are visualized to help users understand trends and
                       characteristics of the electric vehicle landscape. With this visualization,
                       the author aims to provide a general understanding to the public who wish
                       to explore the growing adoption of electric vehicles presented here.",
                          style = "font-size: 28px; font-family: 'Times New Roman';
                          text-align: center;")),
                column(width = 2)
              ),
              h2("How To Use",
                 style = "text-align: center; font-weight: bold"),
              fluidRow(
                column(width = 2),
                column(width = 8,
                       h4("To use this dashboard, simply navigate using the menu on the left side
                          of the screen, where you can switch between different pages related to
                          the topics listed. Each page contains interactive visualizations that
                          allow you to explore the data through options such as hovering,
                          filtering, and short animations. Additionally, each page provides
                          explanations to help you better understand the displayed information.",
                          style = "font-size: 28px; font-family: 'Times New Roman';
                          text-align: center;")),
                column(width = 2)
              ),
              h3("Created By",
                 style = "text-align: center; font-weight: bold"),
              fluidRow(
                column(width = 2),
                column(width = 8,
                       h5("Name: Rayyan Aamir",
                          style = "text-align: center; font-weight: bold"),
                       h5("Student ID: 32065647",
                          style = "text-align: center; font-weight: bold"),
                       h5("Email: raam0001@student.monash.edu",
                          style = "text-align: center; font-weight: bold"))
              )
      ),

      # Analysis Tab
      tabItem(tabName = "analysis",
              h1("So Where Does Washington State Stand?",
                 style = "text-align: center; font-weight: bold"),
              fluidRow(
                column(width = 3),
                column(width = 6,
                       box(
                         width = 12,
                         title = "Map",
                         status = "primary",
                         solidHeader = TRUE,
                         leafletOutput("ev_map")
                ))
              )
      ),

      # Your Options Tab (with filtering options and summary)
      tabItem(tabName = "options",
              # Title and description at the top
              fluidRow(
                column(width = 12,
                       h2("Explore Your Electric Vehicle Options",
                          style = "font-size: 32px; font-family: 'Times New Roman';
                            text-align: center; font-weight: bold; margin-top: 20px;"),
                       p("Use the criteria below to filter and find electric vehicles that match your preferences.
                   Adjust the range, make, type, and model year to narrow down your search.",
                         style = "font-size: 18px; font-family: 'Times New Roman';
                            text-align: center; margin-bottom: 20px;")
                )
              ),

              # Filtering options
              fluidRow(
                # Selection criteria box
                box(
                  title = "Select Criteria for Analysis",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,

                  # Slider input for electric range
                  sliderInput("mileage",
                              "Desired Electric Range (in miles):",
                              min = 0, max = 337, value = c(0, 337)),

                  # Select input for car make
                  selectInput("make",
                              "Select Car Make:",
                              choices = c("All", sort(unique(clean_vehicle$make))),
                              selected = "All",
                              multiple = TRUE),

                  # Select input for vehicle type
                  selectInput("vehicle_type",
                              "Select Type of Vehicle:",
                              choices = c("All",
                                          "Battery Electric Vehicle (BEV)",
                                          "Plug-in Hybrid Electric Vehicle (PHEV)"),
                              selected = "All",
                              multiple = TRUE),

                  # Select input for model year
                  selectInput("model_year",
                              "Select Model Year:",
                              choices = c("All", as.character(2010:2024)),
                              selected = "All",
                              multiple = TRUE),

                  # Action button to trigger filtering
                  actionButton("filter_btn", "Find Vehicles")
                ),

                # Summary and filtered vehicle data table
                box(
                  title = "Summary Statistics",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("summary_text")
                )
                ),
              fluidRow(
                column(width = 2),
                box(
                  title = "Filtered Vehicle Options",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  DTOutput("filtered_table")
                ),
                column(width = 2)
              )
      )
    )
  )
)


# Define the server
server <- function(input, output, session) {

  ## Creating a leaflet map

  # Create the spatial data frame from clean_vehicle data
  clean_vehicle_sf <- st_as_sf(clean_vehicle, coords = c("lon", "lat"), crs = 4326)

  # Create a grid over the area with a specific cell size (e.g., 0.05 degrees)
  grid <- st_make_grid(clean_vehicle_sf, cellsize = 0.05, square = TRUE)

  # Convert the grid to an sf object
  grid_sf <- st_as_sf(grid)

  # Perform a spatial join to count the number of vehicles in each grid cell
  grid_counts <- st_join(clean_vehicle_sf, grid_sf, join = st_intersects) %>%
    group_by(geometry) %>%
    summarize(count = n()) %>%
    st_as_sf()

  # Simplifying the data for faster map generation
  grid_counts_simplified <- st_simplify(grid_counts, dTolerance = 0.01)

  # Locations of most populous cities to overlay on the map
  cities <- data.frame(
    city = c("Seattle", "Olympia", "Tacoma", "Spokane"),
    lat = c(47.6062, 47.0379, 47.2529, 47.4588),
    lon = c(-122.3321, -122.9007, -122.4443, -117.4360)
  )

  # Render the Leaflet map in the server
  output$ev_map <- renderLeaflet({
    # Original map code
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(data = grid_counts_simplified,
                         lng = ~st_coordinates(geometry)[, 1],
                         lat = ~st_coordinates(geometry)[, 2],
                         radius = ~log10(count),
                         color = "red",
                         fillOpacity = 0.7,
                         popup = ~paste("EV Count:", count)) %>%
        addLabelOnlyMarkers(data = cities,
                            lng = ~lon,
                            lat = ~lat,
                            label = ~city,
                            labelOptions = labelOptions(noHide = TRUE,
                                                        direction = "top",
                                                        textOnly = TRUE,
                                                        style = list("color" = "blue",
                                                                     "font-size" = "12px",
                                                                     "font-weight" = "bold"))) %>%
        addLegend("topright",
                  colors = "red",
                  labels = "EV Count",
                  title = "Aggregated EV Observations")
  })

  # Reactive expression to filter data based on user input
  filtered_data <- eventReactive(input$filter_btn, {
    # Handle "All" option for car make
    car_make_selected <- if ("All" %in% input$make) NULL else input$make
    vehicle_type_selected <- if ("All" %in% input$vehicle_type) NULL else input$vehicle_type
    model_year_selected <- if ("All" %in% input$model_year) NULL else as.integer(input$model_year)

    filter_vehicles(
      data = clean_vehicle,
      mileage_range = input$mileage,
      car_make = car_make_selected,
      vehicle_type = vehicle_type_selected,
      year_range = if (is.null(model_year_selected)) NULL else range(model_year_selected)
    ) %>%
      select(model_year,
             make,
             model,
             electric_vehicle_type,
             electric_range)  # Select relevant columns
  })

  # Reactive expression to group and calculate average mileage
  grouped_data <- reactive({
    req(filtered_data())  # Ensure filtered data exists

    filtered_data() %>%
      group_by(model_year, make, model, electric_vehicle_type) %>%
      summarise(average_mileage = round(mean(electric_range, na.rm = TRUE), 0), .groups = 'drop')
  })

  # Display the grouped data in a datatable
  output$filtered_table <- renderDT({
    req(grouped_data())  # Ensure grouped data exists

    # Arrange the data by highest average mileage and display it as a datatable
    grouped_data() %>%
      arrange(desc(average_mileage)) %>%
      datatable(options = list(pageLength = 10, autoWidth = TRUE,
                               order = list(list(5, 'desc'))))
    })

  # Generate and display enhanced summary statistics for the filtered data
  output$summary_text <- renderPrint({
    req(filtered_data())  # Ensure filtered data exists
    summary_stats <- summary_filtered_data(filtered_data())

    cat("Average Electric Range:", summary_stats$avg_electric_range, "miles\n")
    cat("Median Electric Range:", summary_stats$median_electric_range, "miles\n")
    cat("Min Electric Range:", summary_stats$min_electric_range, "miles\n")
    cat("Max Electric Range:", summary_stats$max_electric_range, "miles\n\n")
    cat("Top Common Makes:", paste(summary_stats$top_common_makes, collapse = ", "), "\n\n")
    cat("Vehicle Type Distribution (in %):\n")
    print(summary_stats$vehicle_type_percentage)
  })
}

# Run the application
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
