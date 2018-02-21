getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

Mode <- function(x) { 
  ux <- unique(x) # находим уникальные значения из выборки
  tab <- tabulate(match(x, ux)); # считаем кол-во повторений
  ux[tab == max(tab)] # выдаем моду(-ы), сравнивая кол-во повторений каждого числа с максимальным кол-вом повторений
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

treat.na.pls <- function(x, types){
  for (i in 1:length(x)) {
    if (types[i] == "mean") {
      x[, i][is.na(x[, i])] <- mean(x[, i], na.rm = TRUE)
    } else {
      if (types[i] == "mode") {
        pepan <- x[, i][!is.na(x[, i])]
        modik <- Mode(pepan)
        if (length(modik) == 1) {
          x[, i][is.na(x[, i])] <- modik
        } else {
          if (length(modik) == 2) {
            sortedVals <- sort(unique(pepan))
            var1 <- match(modik[1], sortedVals)
            var2 <- match(modik[2], sortedVals)
            if (var1-var2 == 1 | var2-var1 == 1) {
              x[, i][is.na(x[, i])] <- (var1 + var2) / 2
            }
          } else {
            showNotification(paste("unable to impute mode in column number", i))
          }
        }
      } else {
        if (types[i] == "median") {
          x[, i][is.na(x[, i])] <- median(x[, i], na.rm = TRUE)
        } else {
          if (types[i] == "avg.q"){
            a <- quantile(x[, i], na.rm = TRUE)
            names(a) <- NULL
            x[, i][is.na(x[, i])] <- (a[2] + a[4]) / 2
          }
        }
      }
    }
  }
  x
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

impute.minmax.pls <- function(x, low, high) {
  for (i in 1:length(x)){
    quant <- quantile(x[, i], c(.05, .10, .90, .95), na.rm = TRUE) 
    if (low[i] != "---" | high[i] != "---") {
      impl <- switch(low[i],
                     "---" = 1,
                     "5%" = quant[low[i]],
                     "10%" = quant[low[i]])
      imph <- switch(high[i],
                     "---" = 1,
                     "90%" = quant[high[i]],
                     "95%" = quant[high[i]])
      if (low[i] != "---") {
      x[x[, i] < quant[low[i]], i] <- impl
      }
      if (high[i] != "---"){
      x[x[, i] > quant[high[i]], i] <- imph
      }
    }
  } 
  x
}