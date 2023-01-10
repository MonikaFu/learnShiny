library(shiny)

ui <- fluidPage(
  titlePanel("Central limit theorem"),
  fluidRow(
    column(6, plotOutput("hist1")),
    column(6, plotOutput("hist2"))
  ),
  fluidRow(
    column(
      6,
      numericInput("m1", "Number of samples plot1:", 2, min = 1, max = 100, width = "100%")
    ),
    column(
      6,
      numericInput("m2", "Number of samples plot2:", 2, min = 1, max = 100, width = "100%")
    )
  )
)
server <- function(input, output, session) {
  output$hist1 <- renderPlot({
    means <- replicate(1e4, mean(runif(input$m1)))
    hist(means, breaks = 20)
  }, res = 96)
  output$hist2 <- renderPlot({
    means <- replicate(1e4, mean(runif(input$m2)))
    hist(means, breaks = 20)
  }, res = 96)
}

shinyApp(ui, server)