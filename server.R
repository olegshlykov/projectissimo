server <- function(input, output) {
  
  output$textfile <- renderDataTable({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath)
    
  })
  
}