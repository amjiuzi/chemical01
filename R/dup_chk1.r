
check.func <- function(data,alpha=0.05) {
  if (length(data$tmpid)==0) {
    data$tmpid <- seq(1,nrow(data))+100000
  }
  dt2 <- data[,c('tmpid','cluster','Average.Mz','MS.MS.spectrum')]
  colnames(dt2)[c(3,4)] <- c('value','pools')
  dt3 <- split(dt2,dt2$cluster)
  print(paste('Project Begins at',Sys.time()))
  res1 <- do.call(rbind,lapply(dt3,function(x) dup.fun(x,alpha)))
  print(paste('Project Ends at',Sys.time()))
  res2 <- merge(data,res1[,c('tmpid','dup')],by = 'tmpid')
  return(res2)
}
