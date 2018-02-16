library(shiny)
library(DT)
library(ggplot2)
library(ggdendro)
library(datasets)
library(rhandsontable)
library(caret)
library(psych)
library(rpart)
library(randomForest)

ui <- fluidPage(titlePanel("Projectissimo"),
                
                tabsetPanel(
                  tabPanel("CSV upload",
                           sidebarLayout(
                             sidebarPanel(width = 3,
                                          fileInput("file1", "Please choose a CSV file:", multiple = FALSE,
                                                    accept = c("text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv")
                                          ),
                                          tags$hr(),
                                          checkboxInput("header", "Header", TRUE),
                                          radioButtons("sep", "Separators", choices = c("Comma" = ",", "Semi" = ";", "Tab" = "\t"),
                                                       selected = ","),
                                          radioButtons("quo", "Quote", choices = c("None" = "", "Double Quote" = '"', "Single Quote" = "'"),
                                                       selected = '"'),
                                          tags$hr(),
                                          rHandsontableOutput("datatypechange"),
                                          conditionalPanel(
                                            condition = "output.datatypechange",
                                            tags$hr(),
                                            actionButton("change.apply", "Change data type", icon = icon("rocket"))
                                          )
                             ),
                             mainPanel(dataTableOutput("textfile"))
                           )),
                  
                  tabPanel(
                    "Column selection",
                    dataTableOutput("ColSelect")
                  ),
                  
                  tabPanel("Clustering",
                           sidebarLayout(
                             sidebarPanel(width = 2,
                                          selectInput("ctype", "Please select the clustering method", 
                                                      choices = c("K-Means", "Hierarchical"), 
                                                      selected = "K-Means"),
                                          tags$hr(),
                                          uiOutput("controls"),
                                          tags$hr(),
                                          actionButton("clupdate", "Run Clustering", icon = icon("rocket"))       
                             ),
                             mainPanel(
                               plotOutput("Plotidze"),
                               conditionalPanel(
                                 condition = "output.Plotidze && input.ctype == 'K-Means'",
                                 selectInput("col1", "Please select column 1", 
                                             choices = NULL),
                                 selectInput("col2", "Please select column 2", 
                                             choices = NULL)
                               )
                             )
                           )
                  ),
                  tabPanel("PCA",
                           sidebarLayout(
                             sidebarPanel( width = 2,
                                           numericInput("pcnum", "Number of Principal Components", value = 2, min = 2, max = 20),
                                           selectInput("pcrotate", "Rotation", 
                                                       choices = c("none", "varimax", "quatimax", "promax"),
                                                       selected = "varimax"),
                                           numericInput("pcafilter", "Ignore values less than:", value = 0, min = 0, max = 1),
                                           actionButton("pcarun", "Run PCA", icon = icon("rocket"))
                             ),
                             mainPanel(
                               dataTableOutput("eigen"),
                               dataTableOutput("loadings") 
                             )
                           )
                  ),
                  tabPanel("Tree-based classification",
                           sidebarLayout(
                             sidebarPanel(width = 2,
                                          numericInput("train.percent.sel", "Please select % in train", value = 75, min = 1, max = 100 ),
                                          selectInput("preval", "Please select predict value:", choices = NULL),
                                          selectInput("treechoose", "Please select preffered method", choices = c("Decision Trees", "Random Forest"),
                                                      selected = "Decision Trees"),
                                          conditionalPanel(
                                            condition = "input.treechoose == 'Random Forest'",
                                            numericInput("ntree.sel", "Ntree", value = 500, min = 10, max = 1000),
                                            numericInput("mtry.sel", "Mtry", value = 2, min =1, max = 15)
                                          ),
                                          actionButton("run.tree", "Apply", icon = icon("rocket"))
                             ),
                             mainPanel(
                               column(width = 4,
                                 tags$h1("Train prediction"),
                               rHandsontableOutput("train.prediction"),
                               textOutput("accutrain")
                               ),
                               column(width = 4,
                               tags$h1("Test prediction"),
                               rHandsontableOutput("test.prediction"),
                               textOutput("accutest")
                               )
                             )
                           )
                  )
                ))