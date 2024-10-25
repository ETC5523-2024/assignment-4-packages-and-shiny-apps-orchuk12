library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Electric Vehicles Analysis in Washington State, USA"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("testplot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$testplot <- renderPlot({
    # Extract the electric_range column from the clean_vehicle dataset
    x <- clean_vehicle$electric_range

    # Generate bins based on input$bins from the UI
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # Plot the histogram using ggplot2
    clean_vehicle |>
      ggplot(aes(x = electric_range)) +
      geom_histogram(breaks = bins,
                     fill = 'darkgray',
                     color = 'white') +
      labs(title = 'Histogram of Electric Range',
           x = 'Electric Range',
           y = 'Count') +
      theme_bw()
  })

}

# Run the application
shinyApp(ui = ui, server = server)
