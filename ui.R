library(shiny)
library(DT)

ui <- fluidPage(
  
  titlePanel("Uploading a CSV"),
  
  sidebarLayout(

    sidebarPanel(
      
      fileInput("file1", "Please choose a CSV file:", 
                multiple = FALSE,
                accept = c("text/csv", 
                           "text/comma-separated-values,text/plain", 
                           ".csv")),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", dataTableOutput("textfile")),
        tabPanel("Poostaya")
      )
      
      
    )
  )
)