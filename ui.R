library(shiny)
library(DT)
library(ggplot2)
library(ggdendro)
library(datasets)

ui <- fluidPage(titlePanel("Uploading a CSV"),
                
                tabsetPanel(
                  tabPanel("Data Table",
                           
                           sidebarLayout(
                             sidebarPanel(
                               fileInput(
                                 "file1",
                                 "Please choose a CSV file:",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")
                               ),
                               tags$hr(),
                               checkboxInput("header", "Header", TRUE),
                               radioButtons("sep", "Separators", 
                                            choices = c("Comma" = ",", "Semi" = ";", "Tab" = "\t"),
                                            selected = ","),
                               radioButtons(
                                 "quo",
                                 "Quote",
                                 choices = c(
                                   "None" = "",
                                   "Double Quote" = '"',
                                   "Single Quote" = "'"),
                                 selected = '"'),
                               width = 3
                             ),
                             
                             mainPanel(dataTableOutput("textfile"))
                           )),
                  
                  tabPanel(
                    "Poostaya",
                    fluidRow(
                      column(
                        4,
                        offset = 1,
                        sliderInput(
                          "hrspwr",
                          "Horsepower",
                          min = 51,
                          max = 335,
                          value = 335
                        )
                      ),
                      
                      column(
                        1,
                        checkboxGroupInput(
                          "cylinders",
                          "Cylinders",
                          c("4", "6", "8"),
                          selected = c("4", "6", "8")
                        )
                      ),
                      
                      column(2, wellPanel(
                        actionButton("update", "Update"), align = "center")
                      )),
                    
                    fluidRow(column(10, offset = 1,
                                    dataTableOutput("cars")))
                  ),
                  
                  tabPanel("Iris Clustering",
                           sidebarLayout(
                             sidebarPanel(width = 3,
                                          selectInput("col1", "Please select column 1", 
                                                      choices = colnames(iris), 
                                                      selected = "Petal.Length"),
                                          selectInput("col2", "Please select column 2", 
                                                      choices = colnames(iris), 
                                                      selected = "Petal.Width"),
                                          selectInput("ctype", "Please select the clustering method", 
                                                      choices = c("K-Means", "Hierarchical"), 
                                                      selected = "K-Means"),
                                          tags$hr(),
                                          uiOutput("controls"),
                                          tags$hr(),
                                          actionButton("clupdate", "Run Clustering", icon = icon("rocket"))
                             ),
                             mainPanel(
                               plotOutput("Plotidze")
                             )
                           )
                  )
                ))