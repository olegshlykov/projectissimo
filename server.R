server <- function(input, output, session) {
  source("functions.R")  
  dfram <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quo)
  }) 
  output$textfile <- DT::renderDataTable({
    head(dfram(), 10)
  })
  
  output$ColSelect <- renderDataTable({
    req(!is.null(dfram()))
    numers <- sapply(dfram(), is.numeric)
    cleaned <- dfram()[, numers]
    cnames <- colnames(cleaned)
    cmean <- colMeans(cleaned)
    cmean <- as.vector(cmean)
    cmedian <- apply(cleaned, 2, median)
    cmedian <- as.vector(cmedian)
    cmode <-  apply(cleaned, 2, mean)
    cmode <- as.vector(cmode)
    crange <- sapply(cleaned, range)
    crange <- paste(crange[1, ], crange[2, ], sep=" - ")
    ColSel <- data.frame(cnames, cmean, cmedian, cmode, crange)
    colnames(ColSel) <- c("Column Name", "Mean", "Median", "Mode", "Range")
    ColSel
  })
  
  output$controls <- renderUI({
    if (input$ctype == "K-Means") {
      tagList(
        numericInput("centers", "Please choose the number of clusters", value = 3, min = 2, max = 20),
        numericInput("iter", "Please choose the max number of iterations", value = 10, min = 1, max = 100),
        numericInput("nstart", "Please choose nstart", value = 1, min = 1, max = 300), 
        selectInput("algo", "Please select preffered algorithm", 
                    choices = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), 
                    selected = "Hartigan-Wong")
      )
    } else {
      tagList(
        selectInput("method", "Please select preffered method", 
                    choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
                    selected = "ward.D2")
      )
    }
    
  })
  
  ris = reactiveValues(data = NULL, cluster = NULL, hcl = NULL)
  
  observeEvent(input$clupdate, {
    if ( input$ctype == "K-Means") {
      req(!is.null(dfram()))
      numers <- sapply(dfram(), is.numeric)
      cleaned <- dfram()[, numers]  
      ris$data <- cleaned
      features <- c(cleaned[, input$ColSelect_rows_selected])
      n_clusters <- input$centers
      iris_clusters <- kmeans(cleaned[, input$ColSelect_rows_selected], n_clusters, iter.max = input$iter, nstart = input$nstart, algorithm = input$algo)
      ris$cluster <- as.factor(iris_clusters$cluster)
    } else {
      dd <- dist(ris$data)
      ris$hcl <- hclust(dd, method = input$method)
    }
    
  })
  
  observeEvent(input$ColSelect_rows_selected, {
    updateSelectInput(session, "col1", choices = names(dfram()[,input$ColSelect_rows_selected]),
                      selected = names(dfram()[,input$ColSelect_rows_selected[1]])
                      )
    updateSelectInput(session, "col2", choices = names(dfram()[,input$ColSelect_rows_selected]),
                      selected = names(dfram()[,input$ColSelect_rows_selected[1]])
    )
  })
  
  output$Plotidze <- renderPlot({
    
    if ( input$ctype == "K-Means") {
      req(!is.null(ris$cluster))
      
      ggplot(ris$data, aes_string(input$col1, input$col2)) + geom_point(aes(color = ris$cluster)) + guides(colour = guide_legend("Clusters"))
    } else { 
      req(!is.null(ris$hcl))
      ggdendrogram(ris$hcl, theme_dendro = FALSE)
    }
  })
  
}