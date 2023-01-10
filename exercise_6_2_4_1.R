library(shiny)

ui <- fluidPage(
  titlePanel("Central limit theorem"),
  sidebarLayout(
    sidebarPanel(
      numericInput("m", "Number of samples:", 2, min = 1, max = 100),
      width = 4
    ),
    mainPanel(
     #plotOutput("hist"),
      width = 8
    )
  ),
  fluidRow(
    column(4, numericInput("m", "Number of samples:", 2, min = 1, max = 100)),
    column(8, plotOutput("hist"))
  )
)
server <- function(input, output, session) {
  output$hist <- renderPlot({
    means <- replicate(1e4, mean(runif(input$m)))
    hist(means, breaks = 20)
  }, res = 96)
}

shinyApp(ui, server)