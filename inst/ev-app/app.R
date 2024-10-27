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
                 style = "text-align: center; font-weight: bold"),
              h2("Purpose of This Dashboard",
                 style = "text-align: center; font-weight: bold"),
              fluidRow(
                column(width = 2),
                column(width = 8,
                       h4("This dashboard is part of the Shiny app developed for Assignment 4
                          for the unit Communicating with Data (ETC5523) taken in Semester 2 of 2024
                          at Monash University Clayton Campus. This dashboard aims to display statistics
                          and visualizations related to electric vehicles in the State of Washington, USA.
                          Factors such as electric vehicle range, types (BEV or PHEV), and manufacturers
                          are visualized to help users understand trends and characteristics of the electric
                          vehicle landscape.",
                          style = "font-size: 28px; font-family: 'Times New Roman'; text-align: center;")),
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
                          allow you to explore the data through options such as hovering, filtering,
                          and short animations.",
                          style = "font-size: 28px; font-family: 'Times New Roman'; text-align: center;")),
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
                column(width = 3,
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       h3("The map is fully interactive, the user may hover over points on
                       the map to view the aggregated EV count in a certain location or zoom in to
                       study further micro clusters and come upon their own conclusions as to whether
                       there are other locations that show strangely high densities of EVs.",
                      style = "font-size: 17px; font-family: 'Times New Roman';
                      text-align: justify;")),
                column(width = 6,
                       box(
                         width = 12,
                         title = "Map",
                         status = "primary",
                         solidHeader = TRUE,
                         leafletOutput("ev_map")
                       )),
                column(width = 3)
              ),

              fluidRow(
                column(width = 3),
                column(width = 6,
                       h3("It can be seen that the distribution of electric vehicles (EVs) across
                          Washington State. The spread is somewhat scattered, but there are clear
                          clusters in areas like Seattle, Spokane, and Portland. Seattle, as a hub
                          of technology and innovation, is home to multinational corporations (MNCs)
                          like Oracle and Amazon, both of which offer perks like rebates or free charging
                          stations for employees who own an EV. It’s no surprise that a large portion of
                          EV owners in this area are either part of higher-income households or are
                          eco-conscious consumers looking to make a difference.",
                          style = "font-size: 23px; font-family: 'Times New Roman';
                          text-align: justify;"),
                       h3("But why are there so many EVs in Washington? Well, in mid-2023,
                          Washington introduced an EV instant rebate program, offering between
                          $2,500 to $9,000 toward the purchase or lease of an EV (Washington State
                          Department of Commerce, 2024). The program aims to issue around 6,500 to
                          8,000 rebates, making it one of the first low-cost EV lease programs in
                          the U.S. With initiatives like these, the state is rapidly increasing its
                          share of EVs on the road.",
                          style = "font-size: 23px; font-family: 'Times New Roman';
                          text-align: justify;"),
                       br()),
                column(width = 3)
              ),

              fluidRow(
                column(width = 3,
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       h3("The bar chart is interactive upon hovering and upon filter selection
                          through the buttons on the right of the chart. The user is free to examine
                          the plot and filter it according to their requirement and hover on the
                          chart for further information if they desire",
                          style = "font-size: 17px; font-family: 'Times New Roman';
                      text-align: justify;")),
                column(width = 6,
                       box(
                         width = 12,
                         title = "Vehicle Type Distribution by Model Year",
                         status = "primary",
                         solidHeader = TRUE,
                         plotlyOutput("stacked_bar_chart")
                       )),
                column(width = 3,
                       box(
                         width = 12,
                         title = "Select Type",
                         status = "primary",
                         solidHeader = TRUE,
                       radioButtons("bar_filter_type",
                             "Select Vehicle Type:",
                             choices = c("All", "Battery Electric Vehicle (BEV)",
                                         "Plug-in Hybrid Electric Vehicle (PHEV)"),
                             selected = "All",
                             inline = TRUE)))
              ),

              fluidRow(
                column(width = 3),
                column(width = 6,
                       h3("This chart shows how the adoption of electric vehicles (EVs)
                          in Washington State has evolved from 2010 to 2024, focusing on two
                          main types: Battery Electric Vehicles (BEVs) and Plug-in Hybrid Electric
                          Vehicles (PHEVs). You can clearly see a gradual rise in registrations
                          over the years, with a noticeable surge starting around 2017. BEVs,
                          in particular, have taken off significantly since 2020, becoming the
                          more popular choice by a wide margin.",
                          style = "font-size: 23px; font-family: 'Times New Roman';
                          text-align: justify;"),
                       h3("In fact, 2023 saw a peak in BEV
                          registrations, suggesting a growing shift in consumer preference
                          towards fully electric cars, possibly driven by new policies or a
                          broader interest in sustainable transportation. PHEVs have also seen
                          a steady increase, but their growth is much smaller compared to BEVs
                          in recent years. Overall, this visualization captures how the EV
                          landscape has changed, with BEVs leading the charge as the go-to
                          option for consumers by 2024.",
                          style = "font-size: 23px; font-family: 'Times New Roman';
                          text-align: justify;"),
                       br()),
                column(width = 3)

              ),

              fluidRow(
                column(width = 3,
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       h3("The box plots here follow the same concept as the bar chart above, the
                          user is more than welcome to select the years they wish to study and hover
                          for further summary statistics: outliers, minimum, maximum, lower quartile,
                          median, upper quartile.",
                          style = "font-size: 17px; font-family: 'Times New Roman';
                      text-align: justify;")),
                column(width = 6,
                       box(
                         width = 12,
                         title = "Electric Range Distribution by Model Year",
                         status = "primary",
                         solidHeader = TRUE,
                         plotlyOutput("range_boxplot")
                       )),
                column(width = 3,
                       box(
                         width = 12,
                         title = "Select Year",
                         status = "primary",
                         solidHeader = TRUE,
                       radioButtons("range_filter_year",
                                    "Select Model Year:",
                                    choices = c("All", sort(unique(clean_vehicle$model_year))),
                                    selected = "All",
                                    inline = TRUE)))
              ),

              fluidRow(
                column(width = 3),
                column(width = 6,
                       h3("This boxplot visualizes the electric range distribution of vehicles
                          across model years from 2010 to 2024, offering a glimpse into the
                          evolution of battery capacity over time. In the early years (2010-2014),
                          electric ranges were relatively low and clustered, reflecting the
                          limited technology of the time. Starting from 2016, there’s a
                          noticeable increase in the median range, with several models
                          offering over 200 miles, showcasing advancements in battery technology.
                          By 2017 and 2018, the spread of electric ranges becomes wider,
                          indicating a broader variety of models with varying capabilities.
                          The peak diversity is seen around 2020, with a mix of high-range
                          vehicles pushing the limits of electric capacity.",
                          style = "font-size: 23px; font-family: 'Times New Roman';
                          text-align: justify;"),
                       h3("Interestingly, after 2021, the boxplots show a narrowing of the
                          electric range distribution. This could be linked to reduced vehicle
                          purchases during the COVID-19 pandemic, as manufacturers might have
                          focused more on consistent and cost-effective models, resulting in less
                          variety in electric ranges. The 2024 boxplot, with a tighter spread
                          and a lower median range, suggests a potential stabilization in the
                          market, as manufacturers align around more standardized ranges.
                          Overall, this plot captures the trends, growth, and potential impact
                          of external events like COVID-19 on electric vehicle offerings over
                          the years.",
                          style = "font-size: 23px; font-family: 'Times New Roman';
                          text-align: justify;"),
                       br()),
                column(width = 3)
              )
      ),

      # Your Options Tab
      tabItem(tabName = "options",
              fluidRow(
                column(width = 12,
                       h2("Explore Your Electric Vehicle Options",
                          style = "font-size: 32px; font-family: 'Times New Roman';
                          text-align: center; font-weight: bold; margin-top: 20px;"),
                       p("Use the criteria below to filter and find electric vehicles
                         that match your preferences.",
                         style = "font-size: 18px; font-family: 'Times New Roman';
                         text-align: center; margin-bottom: 20px;"))
              ),

              fluidRow(
                box(
                  title = "Select Criteria for Analysis",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,

                  sliderInput("mileage",
                              "Desired Electric Range (in miles):",
                              min = 0, max = 337, value = c(0, 337)),

                  selectInput("make",
                              "Select Car Make:",
                              choices = c("All", sort(unique(clean_vehicle$make))),
                              selected = "All",
                              multiple = TRUE),

                  selectInput("vehicle_type",
                              "Select Type of Vehicle:",
                              choices = c("All", "Battery Electric Vehicle (BEV)",
                                          "Plug-in Hybrid Electric Vehicle (PHEV)"),
                              selected = "All",
                              multiple = TRUE),

                  selectInput("model_year",
                              "Select Model Year:",
                              choices = c("All", as.character(2010:2024)),
                              selected = "All",
                              multiple = TRUE),

                  actionButton("filter_btn", "Find Vehicles")
                ),

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

  # Reactive data filtering for electric range boxplot
  filtered_range_data <- reactive({
    plot_data <- clean_vehicle

    # Apply model year filter
    if (!"All" %in% input$range_filter_year) {
      plot_data <- plot_data %>% filter(model_year %in% input$range_filter_year)
    }

    plot_data
  })

  # Reactive data filtering for vehicle type stacked bar chart
  filtered_bar_data <- reactive({
    plot_data <- clean_vehicle

    # Apply vehicle type filter
    if (!"All" %in% input$bar_filter_type) {
      plot_data <- plot_data %>% filter(electric_vehicle_type %in% input$bar_filter_type)
    }

    plot_data
  })

  # Render the boxplot of electric range by model year
  output$range_boxplot <- renderPlotly({
    plot_data <- filtered_range_data() %>%
      filter(!is.na(electric_range), !is.na(model_year))  # Ensure no missing values

    p <- ggplot(plot_data, aes(x = factor(model_year), y = electric_range)) +
      geom_boxplot(aes(fill = factor(model_year))) +
      labs(title = "Electric Range Distribution by Model Year",
           x = "Model Year",
           y = "Electric Range (miles)") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p)
  })

  # Render the stacked bar chart
  output$stacked_bar_chart <- renderPlotly({
    plot_data <- filtered_bar_data() %>%
      count(model_year, electric_vehicle_type)

    p <- ggplot(plot_data, aes(x = factor(model_year), y = n, fill = electric_vehicle_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Vehicle Type Distribution by Model Year",
           x = "Model Year",
           y = "Number of Vehicles",
           fill = "Vehicle Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p)
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


  ## Creating a leaflet map

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
}

# Run the application
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
