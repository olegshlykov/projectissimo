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