server <- function(input, output) {
  
  output$textfile <- DT::renderDataTable({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath)
    
  })
  punka <- reactive({subset(mtcars, hp <= input$hrspwr & cyl %in% input$cylinders)})
 output$cars <- DT::renderDataTable({
   req(!is.null( punka() ))
   
    punka()
 })
  
}