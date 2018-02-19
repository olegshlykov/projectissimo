getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

convert.types <- function(obj, types){
  for(i in 1:length(obj)){
    func <- switch(types[i],
                   "integer" = as.integer,
                   "numeric" = as.numeric,
                   "factor" = as.factor,
                   "character" = as.character,
                   "logical" = as.logical)
    obj[,i] <- func(obj[,i])
  }
  obj
}

eigen.get.table <- function(x) {
  sqr.cleaned <- cor(x)
  ev <- eigen(sqr.cleaned)
  colnumchick <- 1:length(x)
  cumulcher <- cumsum(ev$values)/sum(ev$values)
  ev.data <- data.frame(colnumchick, ev$values, cumulcher)
  colnames(ev.data) <- c("Number of factors", "Eigenvalues", "Cumulative %")
  rownames(ev.data) <- NULL
  ev.data
}

col.summary <- function(x) {
  cnames <- colnames(x)
  cmean <- colMeans(x)
  cmean <- as.vector(cmean)
  cmedian <- apply(x, 2, median)
  cmedian <- as.vector(cmedian)
  cmode <-  apply(x, 2, mean)
  cmode <- as.vector(cmode)
  crange <- sapply(x, range)
  crange <- paste(crange[1, ], crange[2, ], sep=" - ")
  cnacheck <- apply(is.na(x), 2, sum)
  cnacheck <- as.vector(cnacheck)
  ColSel <- data.frame(cnames, cmean, cmedian, cmode, crange, cnacheck)
  colnames(ColSel) <- c("Column Name", "Mean", "Median", "Mode", "Range", "Number of NA's")
  ColSel
}