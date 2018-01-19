server <- function(input, output) {
  
  output$textfile <- DT::renderDataTable({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath)
    
  })
  
}