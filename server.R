server <- function(input, output) {
  output$textfile <- DT::renderDataTable({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quo)
    
  })
  punka <- reactiveValues()
  observeEvent(input$update, {
    punka$a <-
      subset(mtcars, hp <= input$hrspwr & cyl %in% input$cylinders)
  })
  output$cars <- DT::renderDataTable({
    if (input$update != 0) {
      punka$a
      
    } else {
      mtcars
    }
  })
  
}