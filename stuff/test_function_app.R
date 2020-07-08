make_app <- function(d) {
  # numeric variables
  v <- names(d)
  v <- v[sapply(d, is.numeric)]

  # app definition
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput("x", "X", v, v[1]),
        selectInput("y", "Y", v, v[2]),
        actionButton("finish", "Finish") # when pressed, stops the app
      ),
      mainPanel(
        plotOutput("plot")
      )
    )
  )
  server <- function(input, output) {
    output$plot <- renderPlot({
      plot(d[,input$x], d[,input$y])
    })
    # return a result when button is pressed
    observeEvent(input$finish,
      stopApp(c(input$x, input$y))
    )
  }

  runApp(shinyApp(ui, server))
}


load("radehydro_ctd.RData")
make_app(d)
