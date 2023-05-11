
  
  ui <- fluidPage(
    numericInput("obs", "Observations:", 10, min = 1, max = 100),
    verbatimTextOutput("value")
  )
  server <- function(input, output) {
    output$value <- renderText({ input$obs })
  }
  shinyApp(ui, server)

