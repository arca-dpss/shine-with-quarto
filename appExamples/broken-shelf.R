library(shiny)
ui <- fluidPage(
  sidebarLayout(sidebarPanel(
    selectInput(inputId = "dataset",
                label = "Choose a dataset:",
                choices = list("rock" = 1, 
                               "pressure" = 2,
                               "cars" = 3)), 
    actionButton("load", "Select dataset")
  ),
  mainPanel(
    plotOutput(
      "graph"   
    ), 
    verbatimTextOutput( 
      "summary"   
    ))))
server <- function(input, output){
  values <- reactiveValues()
  dataInput <- reactive({
    if(input$dataset == 1){
      data <- rock
    } else if (input$dataset == 2 ){
      data <- pressure
    } else if (input$dataset == 3) {
      data <- cars
    }
  })
  observeEvent(input$load, { 
    values$data <- data.frame(dataInput())
  })
  
  output$graph <- renderPlot({
    plot(values$data[, c(1:2)])
  })
  
  output$summary <- renderPrint({
    summary(values$data)
  })
}
shinyApp(ui, server)