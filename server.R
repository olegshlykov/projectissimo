server <- function(input, output) {
  
  output$textfile <- renderTable({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath)
    
  })
  
}