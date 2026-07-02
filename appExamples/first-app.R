ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "dataset", # name of the input (for the server)
                  label = "Choose a dataset:", # name of the input (for the users)
                  choices = c("rock", "pressure", "cars")) # options (for  both 
      # users & server)
    ),
    mainPanel(
      plotOutput( # define the graphical output (we're telling R that this output 
        "graph"   # container must contain a plot)
      ),
      verbatimTextOutput( # define the verbatim output (we're telling R that
        "summary"   # this output container must contain a Verbatim output)
      )
    ) 
  )
)

server <- function(input, output){
  output$graph <- renderPlot({
    if(input$dataset == "rock"){ # call the input and its options with their label
      data <- rock
    } else if (input$dataset == "pressure" ){
      data <- pressure
    } else if (input$dataset == "cars") {
      data <- cars
    }
    plot(data[, c(1:2)])
  })
  output$summary <- renderPrint({
    if(input$dataset == "rock"){
      data <- rock
    } else if (input$dataset == "pressure" ){
      data <- pressure
    } else if (input$dataset == "cars") {
      data <- cars
    }
    summary(data[, c(1:2)])
  })
}
shinyApp(ui, server)