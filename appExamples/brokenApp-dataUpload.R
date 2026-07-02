library(shiny)
ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "dataset",
                    label = "Choose a dataset:",
                    choices = list("rock" = 1, 
                                   "pressure" = 2,
                                   "cars" = 3,
                                   "I want to use my data!!" =4)), # we add an option
        # Let me introduce you Mr. Conditional Panel
        conditionalPanel(
          condition = "input.dataset == '4'", # What is the condition for which I should show up?
          # What should be displayed inside me once I show up?
          fileInput("example",  # label for the server
                    "Wow my data set!", 
                    accept = c("csv")) # what format do we accept?
        ),
        actionButton("load", "Upload data")
      ),
      
      mainPanel(
        plotOutput(
          "graph"
        ),
        verbatimTextOutput(
          "summary"
        )
      ) 
    )
  )
server = function(input, output){
    values <- reactiveValues()
    dataInput <- reactive({
      if(input$dataset == 1){ 
        data <- rock
      } else if (input$dataset == 2 ){
        data <- pressure
      } else if (input$dataset == 3) {
        data <- cars
      } else if (input$dataset == 4) {
        data <- read.csv(input$example$datapath) # we call the input for the data 
        #import by its label and we ask for the data path selected by the user
      }
    })
    observeEvent(input$load, # we load the data set (whatever it is only once the 
                 #  user has hit the button)
                 {
                   values$data <- data.frame(dataInput())
                 })
    
    output$graph <- renderPlot({
      validate(
        need(input$load > 0, "Waiting for data")
      )
      if (any(colnames(values$data) == "condition") ){ # I had to change the code 
        #just for the new data set, which is different from all the others
        # check if there are characters
        if (any(sapply(values$data, is.character)) == TRUE) {
          values$data[, sapply(values$data, is.character) == T] = lapply(values$data[, sapply(values$data, is.character) == T], 
                                                                         as.factor)
        } else {
          values$data = values$data
        }
        plot(values$data$tr ~ values$data$condition,
             xlab = "Condition", ylab = "TR")
      } else {
        plot(values$data[, c(1:2)])
      }
    })
    
    output$summary <- renderPrint({
      validate(
        need(input$load > 0, "Waiting for data")
      )
      if (any(colnames(values$data) == "condition") ){
        summary(values$data[, c(2:3)])
      } else {
        summary(values$data[, c(1:2)])
      }
    })
}
shinyApp(ui, server)


