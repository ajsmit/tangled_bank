library(shiny)
library(TidyDensity)
library(tidyverse)
library(DT)

# Define UI
ui <- fluidPage(
    titlePanel("Density App"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "functions",
                        label = "Select Function",
                        choices = c(
                            "Normal" = "tidy_normal",
                            "Bernoulli" = "tidy_bernoulli",
                            "Beta" = "tidy_beta",
                            "Gamma" = "tidy_gamma"
                        )
            ),
            numericInput(inputId = "num_sims",
                         label = "Number of simulations:",
                         value = 1,
                         min = 1,
                         max = 15),
            numericInput(inputId = "n",
                         label = "Sample size:",
                         value = 50,
                         min = 30,
                         max = 200)
        ),
        mainPanel(
            plotOutput("density_plot"),
            DT::dataTableOutput("data_table")
        )
    )
)

# Define server
server <- function(input, output) {

    # Create reactive data
    data <- reactive({
        # Call selected function with user input
        match.fun(input$functions)(.num_sims = input$num_sims, .n = input$n)
    })

    # Create density plot
    output$density_plot <- renderPlot({
        # Call autoplot on reactive data
        p <- data() |>
            tidy_autoplot()

        print(p)
    })

    # Create data table
    output$data_table <- DT::renderDataTable({
        # Return reactive data as a data table
        DT::datatable(data())
    })

}

# Run the app
shinyApp(ui = ui, server = server)
