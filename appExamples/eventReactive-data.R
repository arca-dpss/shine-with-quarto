library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "dataset",
        "Choose a dataset:",
        choices = c(
          "rock" = 1,
          "pressure" = 2,
          "cars" = 3,
          "Upload data" = 4
        )
      ),
      
      conditionalPanel(
        condition = "input.dataset == '4'",
        fileInput(
          "file",
          "",
          accept = "csv"
        ),
        radioButtons(
          "sep",
          "Choose column separator",
          choices = c(";" = ";", "," = ",")
        )
      ),
      
      actionButton(
        "load",
        "Load dataset"
      ),
      
      uiOutput("x_ui"),
      uiOutput("y_ui"),
      
      actionButton(
        "analyze",
        "Update analysis"
      )
      
    ),
    
    mainPanel(
      plotOutput("graph"),
      verbatimTextOutput("summary")
    )
  )
)

server = function(input, output, session){
  dataInput = eventReactive(input$load, {
    if (input$dataset == 1) {
      rock
    } else if (input$dataset == 2) {
      pressure
    } else if (input$dataset == 3) {
      cars
    } else {
      read.csv(input$file$datapath, sep = input$sep)
    }
  })
  vars <- reactive({
    names(dataInput())
  })
  
  output$x_ui <- renderUI({
    selectInput(
      "x",
      "X variable:",
      choices = vars(),
      selected = vars()[1]
    )
  })
  
  output$y_ui <- renderUI({
    selectInput(
      "y",
      "Y variable:",
      choices = vars(),
      selected = vars()[min(2, length(vars()))]
    )
  })
  
  newdata <- eventReactive(input$analyze, {
    df <- dataInput()
    df[, c(input$x, input$y), drop = FALSE]
  })
  
  output$graph <- renderPlot({
    df <- newdata()
    x <- df[[1]]
    y <- df[[2]]
    if (is.factor(x) || is.character(x)) {
      boxplot(y ~ x,
              xlab = "Group",
              ylab = "Value")
    } else {
      plot(x, y)
    }
  })
  
  output$summary <- renderPrint({
    df <- newdata()
    summary(df)
  })
  
}

shinyApp(ui, server)