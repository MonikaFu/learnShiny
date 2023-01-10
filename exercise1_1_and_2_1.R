library(shiny)
ui <- fluidPage(
  textInput("name", label = "What is your name?", placeholder = "Name"),
  textOutput("greeting"),
)
server <- function(input, output, session) {
  output$greeting <- renderText({
    paste0("Hello ", input$name)
  })
}
shinyApp(ui, server)