server <- function(input, output, session) {
  source("functions.R")  
  
  values <- reactiveValues(dfram = NULL, pca.out = NULL, cleaned = NULL, sorted = NULL, 
                           train.poopi = NULL, test.poopi = NULL, accu.train = NULL, accu.test = NULL)
  ris = reactiveValues(data = NULL, cluster = NULL, hcl = NULL)
  
  observeEvent(input$file1, {
    values$dfram <- read.csv(input$file1$datapath,
                             header = input$header,
                             sep = input$sep,
                             quote = input$quo)
    numers <- sapply(values$dfram, is.numeric)
    values$cleaned <- values$dfram[, numers]
    colfact <- sapply(values$dfram, is.factor)
    updateSelectInput(session, "preval", choices = names(colfact[colfact]),
                      selected = names(colfact[colfact][1]))
    updateNumericInput(session, "mtry.sel", value = sqrt(ncol(values$dfram)))
  })
  
  
  observeEvent(values$dfram, {
    numers <- sapply(values$dfram, is.numeric)
    values$cleaned <- values$dfram[, numers]
    colfact <- sapply(values$dfram, is.factor)
    updateSelectInput(session, "preval", choices = names(colfact[colfact]),
                      selected = names(colfact[colfact][1]))
    updateNumericInput(session, "mtry.sel", value = sqrt(ncol(values$dfram)))
  })
  
  
  output$textfile <- DT::renderDataTable({
    head(values$dfram, 10)
  })
  
  
  output$datatypechange <- renderRHandsontable({
    req(!is.null(values$dfram))
    dtch <- values$dfram
    all.types <- c("integer", "numeric", "factor", "character", "logical")
    types.dtch <- sapply(dtch, class)
    ntypes.dtch <- types.dtch
    dtchange <- data.frame(types.dtch, ntypes.dtch)
    colnames(dtchange) <- c("Existing type", "New type")
    rhandsontable(dtchange, stretchH = "all", rowHeaderWidth = 100, width = 300) %>%
      hot_col("Existing type", readOnly = TRUE) %>%
      hot_col("New type", type = "dropdown", source = all.types)
  })
  
  
  observeEvent(input$change.apply, {
    req(!is.null(values$dfram))
    type.vector <- hot_to_r(input$datatypechange)
    type.vector <- as.vector(type.vector[, 2])
    values$dfram <- convert.types(values$dfram, type.vector)
  })
  
  
  output$ColSelect <- renderDataTable({
    req(!is.null(values$dfram))
    cnames <- colnames(values$cleaned)
    cmean <- colMeans(values$cleaned)
    cmean <- as.vector(cmean)
    cmedian <- apply(values$cleaned, 2, median)
    cmedian <- as.vector(cmedian)
    cmode <-  apply(values$cleaned, 2, mean)
    cmode <- as.vector(cmode)
    crange <- sapply(values$cleaned, range)
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
  
  
  observeEvent(input$clupdate, {
    if (!is.null(input$ColSelect_rows_selected)) {
      if ( input$ctype == "K-Means") {
        req(!is.null(values$dfram))
        ris$data <- values$cleaned
        features <- c(values$cleaned[, input$ColSelect_rows_selected])
        n_clusters <- input$centers
        iris_clusters <- kmeans(values$cleaned[, input$ColSelect_rows_selected], n_clusters, iter.max = input$iter, 
                                nstart = input$nstart, algorithm = input$algo)
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
    req(length(input$ColSelect_rows_selected) >= 2)
    updateSelectInput(session, "col2", choices = names(values$dfram[,input$ColSelect_rows_selected]),
                      selected = names(values$dfram[,input$ColSelect_rows_selected[2]])
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
    req(!is.null(values$dfram))
    if (input$pcnum <= length(values$cleaned)) {
      cleaned.cor <- cor(values$cleaned)
      values$pca.out <-  principal(values$cleaned, nfactors = input$pcnum, rotate = input$pcrotate)
      loadings.table <- data.frame(unclass(values$pca.out$loadings))
      sorting <- paste0(substring(colnames(loadings.table), 0, 2)[1], sort(as.integer(substring(colnames(loadings.table), 3))))
      sorted <- loadings.table[, sorting]
      for (i in 1:length(sorted)) {
        for (v in 1:length(sorted[, i])) {
          if ( sorted[v, i] > -input$pcafilter  && sorted[v, i] < input$pcafilter) { sorted[v, i] = NA }
        }
      }
      values$sorted <- sorted[order(-sorted[,1], -sorted[,2]), ]
    } else {showNotification("Invalid number of components")}
  })
  
  
  output$loadings <- DT::renderDataTable({
    req(!is.null(values$pca.out$loadings))
    values$sorted
  }, options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '200px', targets = "_all")),
    pageLength = 15
  ))
  
  
  output$eigen <- DT::renderDataTable({
    req(!is.null(values$cleaned))
    eigen.get.table(values$cleaned)
  }, rownames = FALSE, options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '100px', targets = "_all"))
  ))
  
  
  observeEvent(input$run.tree, {
    req(values$dfram)
    percent <- input$train.percent.sel * 0.01
    preval <- input$preval
    set.seed(555)
    train <- sample(1:nrow(values$dfram), percent * nrow(values$dfram))
    
    if (input$treechoose == "Decision Trees") {
      treeboy <- rpart(as.formula(paste0(preval, "~.")), data = values$dfram, subset = train, method = 'class')
      treeboy.train.pred <- predict(treeboy, values$dfram[train, ], type = "class")
      treeboy.test.pred <- predict(treeboy, values$dfram[-train, ], type = "class")
      values$accu.train <- confusionMatrix(treeboy.train.pred, values$dfram[train, ][[preval]])
      values$accu.test <- confusionMatrix(treeboy.test.pred, values$dfram[-train, ][[preval]])
      values$train.poopi <- with(values$dfram[train, ], table(treeboy.train.pred, values$dfram[train, ][[preval]]))
      values$test.poopi <- with(values$dfram[-train, ], table(treeboy.test.pred, values$dfram[-train, ][[preval]]))
    } else {
      rfboy <- randomForest(as.formula(paste0(preval, "~.")), data = values$dfram, subset = train, mtry = input$mtry.sel, ntree = input$ntree.sel)
      rfboy.train.pred <- predict(rfboy, values$dfram[train, ], type = "class")
      rfboy.test.pred <- predict(rfboy, values$dfram[-train, ], type = "class")
      values$accu.train <- confusionMatrix(rfboy.train.pred, values$dfram[train, ][[preval]])
      values$accu.test <- confusionMatrix(rfboy.test.pred, values$dfram[-train, ][[preval]])
      values$train.poopi <- with(values$dfram[train, ], table(rfboy.train.pred, values$dfram[train, ][[preval]]))
      values$test.poopi <- with(values$dfram[-train, ], table(rfboy.test.pred, values$dfram[-train, ][[preval]]))
    }
    
    bobik <- data.frame(matrix(nrow = nrow(values$train.poopi), ncol = 0))
    for (i in 1:nrow(values$train.poopi)) {
      bobik <- cbind(bobik, values$train.poopi[, i])
    }
    colnames(bobik) <- rownames(bobik)
    values$train.poopi <- bobik
    
    bobik2 <- data.frame(matrix(nrow = nrow(values$test.poopi), ncol = 0))
    for (i in 1:nrow(values$test.poopi)) {
      bobik2 <- cbind(bobik2, values$test.poopi[, i])
    }
    colnames(bobik2) <- rownames(bobik2)
    values$test.poopi <- bobik2
  })
  
  
  output$train.prediction <- renderRHandsontable({
    req(values$train.poopi)
    rhandsontable(values$train.poopi, rowHeaderWidth = 100)
  })
  
  
  output$test.prediction <- renderRHandsontable({
    req(values$train.poopi)
    rhandsontable(values$test.poopi, rowHeaderWidth = 100)
  })
  
  output$accutrain <- renderPrint({
    req(values$accu.train)
    values$accu.train$overall[1]
    })
  
  output$accutest <- renderPrint({
    req(values$accu.test)
    values$accu.test$overall[1]
    })
  
}