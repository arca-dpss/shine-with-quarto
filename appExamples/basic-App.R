# other app ----- 
library(shiny)
ui = fluidPage(selectInput(inputId = "mySelection", 
                                    label = h3("Select box"),
                                    choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                                    selected = 1),
                        verbatimTextOutput(
                          outputId = "myOutput"
                        )) 
server = function(input, output) {
           output$myOutput = renderPrint({
             paste(paste("This is my choice"), input$mySelection)
           })
}
shinyApp(ui, server)