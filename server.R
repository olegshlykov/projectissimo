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
  
  output$controls <- renderUI({
    if (input$ctype == "K-Means") {
      tagList(numericInput("centers", "Please choose the number of clusters", value = 3, min = 2, max = 20),
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
  
  ris = reactiveValues(data = datasets::iris, cluster = NULL, hcl = NULL)
  
  observeEvent(input$clupdate, {
    if ( input$ctype == "K-Means") {
    features <- c("Petal.Length", "Petal.Width")
    n_clusters <- input$centers
    iris_clusters <- kmeans(ris$data[, features], n_clusters, iter.max = input$iter, nstart = input$nstart, algorithm = input$algo)
    ris$cluster <- as.factor(iris_clusters$cluster)
    } else {
      dd <- dist(ris$data[, -5])
      ris$hcl <- hclust(dd, method = input$method)
    }
    
  })
  
  output$Plotidze <- renderPlot({
    
    if ( input$ctype == "K-Means") {
      req(!is.null(ris$cluster))
    ggplot(ris$data, aes_string(input$col1, input$col2)) + aes(color = ris$cluster) + geom_point()
    } else { 
      req(!is.null(ris$hcl))
      ggdendrogram(ris$hcl, theme_dendro = FALSE)
      }
  })
    
}