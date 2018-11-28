server <- function(input, output, session) {
  source("functions.R")  
  
  values <- reactiveValues(dfram = NULL, pca.out = NULL, cleaned = NULL, cleanedforpca = NULL, sorted = NULL, 
                           train.poopi = NULL, test.poopi = NULL, accu.train = NULL, accu.test = NULL, cluster = NULL, hcl = NULL)
  ris = reactiveValues(data = NULL)
  
  observeEvent(input$file1, {
    values$dfram <- read.csv(input$file1$datapath,
                             header = input$header,
                             sep = input$sep,
                             quote = input$quo)
    values$cluster <- NULL
    values$hcl <- NULL
    loginfo('data uploaded from %s', input$file1$name)
  })
  
  
  observeEvent(values$dfram, {
    numers <- sapply(values$dfram, is.numeric)
    values$cleaned <- values$dfram[, numers]
    colfact <- sapply(values$dfram, is.factor)
    updateSelectInput(session, "preval", choices = names(colfact[colfact]),
                      selected = names(colfact[colfact][1]))
    updateNumericInput(session, "mtry.sel", value = sqrt(ncol(values$dfram)))
    updateSelectInput(session, "coldisp", choices = names(values$cleaned),
                      selected = names(values$cleaned)[1])
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
    loginfo("data type changed")
  })
  
  
  output$textfile <- DT::renderDataTable({
    head(values$dfram, 10)
  })
  
  output$impNA <- renderRHandsontable({
    req(!is.null(values$cleaned))
    ds <- values$cleaned
    imp.types <- c("---", "mode", "median", "mean", "avg.q")
    varname <- colnames(values$cleaned)
    nanum <- apply(is.na(ds), 2, sum)
    nanum <- as.vector(nanum)
    default.impute <- vector(mode="numeric", length = length(nanum))
    for(i in 1:length(nanum)) {
      if(nanum[i] == 0) {
        default.impute[i] <- "---"
      } else {
        default.impute[i] <- "mode"
      }
    }
    out <- data.frame(varname, nanum, default.impute)
    colnames(out) <- c("Name", "NA's", "Impute type")
    rownames(out) <- 1:length(values$cleaned)
    rhandsontable(out, stretchH = "none", rowHeaderWidth = 30) %>% 
      hot_col("Name", readOnly = TRUE, width = 150) %>% 
      hot_col("NA's", readOnly = TRUE) %>% 
      hot_col("Impute type", type = "dropdown", source = imp.types)
  })
  
  observeEvent(input$impute.NA, {
    treatna <- hot_to_r(input$impNA)
    treatna <- as.vector(treatna[, 3])
    values$cleaned <- treat.na.pls(values$cleaned, treatna)
    loginfo("NA treated")
  })
  
  output$impMinmax <- renderRHandsontable({
    req(!is.null(values$cleaned))
    ds <- values$cleaned
    varname <- colnames(values$cleaned)
    pookie <- rep("---", ncol(values$cleaned))
    pookie2 <- rep("---", ncol(values$cleaned))
    plist <- c("---", "5%", "10%")
    plist2 <- c("---", "90%", "95%")
    out <- data.frame(varname, pookie, pookie2)
    colnames(out) <- c("Name", "From", "To")
    rownames(out) <- 1:length(values$cleaned)
    rhandsontable(out, stretchH = "none", rowHeaderWidth = 30) %>% 
      hot_col("Name", readOnly = TRUE, width = 150) %>% 
      hot_col("From", type = "dropdown", source = plist) %>% 
      hot_col("To", type = "dropdown", source = plist2)
  })
  
  observeEvent(input$impute.minmax, {
    impute <- hot_to_r(input$impMinmax)
    L.impute <- as.vector(impute[, 2])
    H.impute <- as.vector(impute[, 3])
    values$cleaned <- impute.minmax.pls(values$cleaned, L.impute, H.impute)
    loginfo("outliers treated")
  })
  
  output$vdisp <- renderPlot({
    req(values$cleaned)
    if (input$plotdisp == "Boxplot") {
      boxplot(values$cleaned[, input$coldisp], main = input$coldisp)
    } else {
      if (input$plotdisp == "Histogram") {
        hist(values$cleaned[, input$coldisp], main = input$coldisp, xlab = NULL)
      } else {
        stripchart(values$cleaned[, input$coldisp], main = input$coldisp)
      }
    }
    
    
  })
  
  output$ColSelect <- renderDataTable({
    req(!is.null(values$dfram))
    col.summary(values$cleaned)
  })
  
  observeEvent(input$ColSelect_rows_selected, {
    values$cleanedforpca <- values$cleaned[, input$ColSelect_rows_selected]
    updateSelectInput(session, "col1", choices = names(values$cleanedforpca),
                      selected = names(values$cleanedforpca)[1]
    )
    req(length(input$ColSelect_rows_selected) >= 2)
    updateSelectInput(session, "col2", choices = names(values$cleanedforpca),
                      selected = names(values$cleanedforpca)[2]
    )
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
    if (length(input$ColSelect_rows_selected) >=2) {
      if ( input$ctype == "K-Means") {
        req(!is.null(values$dfram))
        n_clusters <- input$centers
        iris_clusters <- kmeans(values$cleanedforpca, n_clusters, iter.max = input$iter, 
                                nstart = input$nstart, algorithm = input$algo)
        values$cluster <- as.factor(iris_clusters$cluster)
        loginfo("K-means clustering performed")
      } else {
        values$hcl <- hclust(dist(values$cleanedforpca), method = input$method)
        loginfo("H clustering performed")
      }
    } else {
      showNotification("Please select at least 2 columns to cluster")
    }
  })
  
  output$Plotidze <- renderPlot({
    if ( input$ctype == "K-Means") {
      req(!is.null(values$cluster), input$clupdate)
      
      ggplot(values$cleanedforpca, aes_string(input$col1, input$col2)) + geom_point(aes(color = values$cluster)) + guides(colour = guide_legend("Clusters"))
    } else { 
      req(!is.null(values$hcl))
      ggdendrogram(values$hcl, theme_dendro = FALSE)
    }
  })
  
  
  observeEvent(input$pcarun, {
    req(!is.null(values$cleanedforpca))
    conti <- sapply(values$cleanedforpca, function(x) var(x, na.rm = TRUE) == 0)
    if (any(conti)) { showNotification("Continuous rows man")
    } else {
      if (input$pcnum <= length(values$cleanedforpca)) {
        cleaned.cor <- cor(values$cleanedforpca, use = "pairwise.complete.obs")
        values$pca.out <-  principal(cleaned.cor, nfactors = input$pcnum, rotate = input$pcrotate)
        loadings.table <- data.frame(unclass(values$pca.out$loadings))
        sorting <- paste0(substring(colnames(loadings.table), 0, 2)[1], sort(as.integer(substring(colnames(loadings.table), 3))))
        sorted <- loadings.table[, sorting]
        for (i in 1:length(sorted)) {
          for (v in 1:length(sorted[, i])) {
            if ( sorted[v, i] > -input$pcafilter  && sorted[v, i] < input$pcafilter) { sorted[v, i] = NA }
          }
        }
        values$sorted <- sorted[order(-sorted[,1], -sorted[,2]), ]
        loginfo("PCA performed")
      } else {showNotification("Invalid number of components")}
    } 
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
    req(ncol(values$cleanedforpca) >= 2)
    conti <- sapply(values$cleanedforpca, function(x) var(x, na.rm = TRUE) == 0)
    validate (
      need(!any(conti), "Continuous rows man" )
    )
    eigen.get.table(values$cleanedforpca)
  }, rownames = FALSE, options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '100px', targets = "_all"))
  ))
  
  
  observeEvent(input$run.tree, {
    req(values$dfram, input$preval)
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
      loginfo("Decision trees classification applied")
    } else {
      rfboy <- randomForest(as.formula(paste0(preval, "~.")), data = values$dfram, 
                            subset = train, mtry = input$mtry.sel, ntree = input$ntree.sel)
      rfboy.train.pred <- predict(rfboy, values$dfram[train, ], type = "class")
      rfboy.test.pred <- predict(rfboy, values$dfram[-train, ], type = "class")
      values$accu.train <- confusionMatrix(rfboy.train.pred, values$dfram[train, ][[preval]])
      values$accu.test <- confusionMatrix(rfboy.test.pred, values$dfram[-train, ][[preval]])
      values$train.poopi <- with(values$dfram[train, ], table(rfboy.train.pred, values$dfram[train, ][[preval]]))
      values$test.poopi <- with(values$dfram[-train, ], table(rfboy.test.pred, values$dfram[-train, ][[preval]]))
      loginfo("Random Forests classification applied")
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