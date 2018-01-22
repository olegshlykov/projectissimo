server <- function(input, output) {
#proverochka  
  output$textfile <- DT::renderDataTable({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath)
    
  })
  
  punka <- eventReactive(input$update, {
    subset(mtcars, hp <= input$hrspwr & cyl %in% input$cylinders)
  })
  output$cars <- DT::renderDataTable({
    
    if (input$update!=0) {
      punka()
      
    } else {
      mtcars
    }
    
  })
  
}