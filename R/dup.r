
dup.fun <- function(data,alpha){
  if (dim(data)[1]==1){
    data$dup <- NA
  }else{
    pool.new <- c()
    pools <- list()
    for (i in 1:dim(data)[1]){
      pool.new[i] <- paste(data$pools[-i],collapse = ' ')
      pools[[i]] <- as.numeric(unique(sapply(unlist(strsplit(pool.new[i],' ')),
                                             function(x){strsplit(x,':')[[1]][1]})))
      if (any((pools[[i]] >= data$value[i]-alpha) & 
              (pools[[i]] <= data$value[i]+alpha))){
        data$dup[i] <- 'y'
      }else{
        data$dup[i] <- NA
      }
    }
  }
  return(data)
}
