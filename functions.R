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