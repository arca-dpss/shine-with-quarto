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
    data = switch(input$dataset,
           rock = rock,
           pressure = pressure,
           cars = cars)
    plot(data[, c(1:2)])
  })
  output$summary <- renderPrint({
    data = switch(input$dataset,
                  rock = rock,
                  pressure = pressure,
                  cars = cars)
    summary(data[, c(1:2)])
  })
}
shinyApp(ui, server)