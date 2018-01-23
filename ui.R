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
        tabPanel("Data Table", 
                 dataTableOutput("textfile")),
        tabPanel("Poostaya", 
                 sliderInput("hrspwr", "Horsepower", min = 51, max = 335, value = 335),
                 checkboxGroupInput("cylinders", "Cylinders", c("4", "6", "8"), selected = c("4", "6", "8")),
                 checkboxInput("quo", "Quotes"),
                 actionButton("update", "Update"),
                 
                 dataTableOutput("cars"))
      )
      
      
    )
  )
)