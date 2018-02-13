server <- function(input, output, session) {
  source("functions.R")  
  values <- reactiveValues(dfram = NULL, pca.out = NULL)
  observeEvent(input$file1, {
               values$dfram <- read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quo)
               }) 
  
  output$textfile <- DT::renderDataTable({
    head(values$dfram, 10)
  })
  
  output$datatype.change <- renderRHandsontable({
    req(!is.null(values$dfram))
    dtch <- values$dfram
    all.types <- c("integer", "numeric", "factor", "character", "logical")
    types.dtch <- sapply(dtch, class)
    ntypes.dtch <- types.dtch
    dtchange <- data.frame(types.dtch, ntypes.dtch)
    colnames(dtchange) <- c("Existing data type", "New data type")
    rhandsontable(dtchange, stretchH = "all", rowHeaderWidth = 100) %>%
      hot_col("Existing data type", readOnly = TRUE) %>%
      hot_col("New data type", type = "dropdown", source = all.types)
  })
  
  observeEvent(input$change.apply, {
    req(!is.null(values$dfram))
    type.vector <- hot_to_r(input$datatype.change)
    type.vector <- as.vector(type.vector[, 2])
    #shlambo <- values$dfram
    #shlambo <- convert.types(shlambo, type.vector)
    #values$dfram <- shlambo
    values$dfram <- convert.types(values$dfram, type.vector)
    print(str(values$dfram))
  })
  
  output$ColSelect <- renderDataTable({
    req(!is.null(values$dfram))
    numers <- sapply(values$dfram, is.numeric)
    cleaned <- values$dfram[, numers]
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
    if (!is.null(input$ColSelect_rows_selected)) {
    if ( input$ctype == "K-Means") {
      req(!is.null(values$dfram))
      numers <- sapply(values$dfram, is.numeric)
      cleaned <- values$dfram[, numers]  
      ris$data <- cleaned
      features <- c(cleaned[, input$ColSelect_rows_selected])
      n_clusters <- input$centers
      iris_clusters <- kmeans(cleaned[, input$ColSelect_rows_selected], n_clusters, iter.max = input$iter, nstart = input$nstart, algorithm = input$algo)
      ris$cluster <- as.factor(iris_clusters$cluster)
    } else {
      dd <- dist(ris$data)
      ris$hcl <- hclust(dd, method = input$method)
    }
    } else {
      showNotification("Please select columns to cluster")
    }
  })
  
  observeEvent(input$ColSelect_rows_selected, {
    updateSelectInput(session, "col1", choices = names(values$dfram[,input$ColSelect_rows_selected]),
                      selected = names(values$dfram[,input$ColSelect_rows_selected[1]])
                      )
    updateSelectInput(session, "col2", choices = names(values$dfram[,input$ColSelect_rows_selected]),
                      selected = names(values$dfram[,input$ColSelect_rows_selected[1]])
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
  
  observeEvent(input$pcarun, {
    numers <- sapply(values$dfram, is.numeric)
    cleaned <- values$dfram[, numers]
    values$pca.out <- prcomp(cleaned, scale = input$pcscale, center = input$pccenter, rank. = input$pcnum)
  })
  
  output$pca <- renderPlot({
    req(!is.null(values$dfram), input$pcarun > 0)
    plot(values$pca.out$x[,1], values$pca.out$x[,2], xlab = "PC 1", ylab = "PC 2")
  })
  
  output$eigen <- renderTable({
    #doesn't work yet, not a square matrix
    
    #req(!is.null(values$dfram))
    #numers <- sapply(values$dfram, is.numeric)
    #cleaned <- values$dfram[, numers]
    #ev <- eigen(cleaned)
    #print(ev$values)
  })
  
}