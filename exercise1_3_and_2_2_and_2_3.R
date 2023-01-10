library(shiny)

longList <- c("Digits" = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), 
              "Tens" = c("10", "11", "12", "13","14", "15", "16", "17"))

ui <- fluidPage(
  sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", label = "and y is", min = 1, max = 50, value = 5),
  "then x times y is",
  textOutput("product"),
  "And this is just a random date slider",
  sliderInput(
    "date", 
    label = "When should we deliver?", 
    min = as.Date("2020-09-16", "%Y-%m-%d"), 
    max = as.Date("2020-09-23", "%Y-%m-%d"), 
    value = as.Date("2020-09-17", "%Y-%m-%d"),
    timeFormat = "%Y-%m-%d"
  ),
  sliderInput(
    "step", 
    "Let's step through 0 to 100", 
    value = 0, 
    min = 0, 
    max = 100, 
    step = 5, 
    animate = TRUE
    ),
  selectInput("long_list", "Please choose:", choices = longList)
)

server <- function(input, output, session) {
  output$product <- renderText({ 
    input$x * input$y
  })
}

shinyApp(ui, server)