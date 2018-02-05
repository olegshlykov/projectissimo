library(shiny)
library(DT)
library(ggplot2)
library(ggdendro)
library(datasets)
ui <- fluidPage(titlePanel("Projectissimo"),
                
                tabsetPanel(
                  tabPanel("CSV upload",
                           
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
                    "Column selection",
                    dataTableOutput("ColSelect")
                  ),
                  
                  tabPanel("Clustering",
                           sidebarLayout(
                             sidebarPanel(width = 3,
                                          
                                          selectInput("ctype", "Please select the clustering method", 
                                                      choices = c("K-Means", "Hierarchical"), 
                                                      selected = "K-Means"),
                                          selectInput("col1", "Please select column 1", 
                                                      choices = NULL),
                                          selectInput("col2", "Please select column 2", 
                                                      choices = NULL),
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