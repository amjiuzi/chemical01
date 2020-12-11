
distFun <- function(data) {
  if (dim(data)[1]>1) {
    data <- matrix(data$Average.Mz,ncol = 1,
                   dimnames = list(data$tmpid,'value'))
    dis111 <- as.matrix(dist(data,method = 'maximum'))
    dis111[upper.tri(dis111)] <- 0
    dis112 <- reshape2::melt(dis111)
    dis112 <- dis112[dis112$value != 0,]
    colnames(dis112) <- c('final','initial','value')
    return(dis112)
  }
}
