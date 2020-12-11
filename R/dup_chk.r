
DupCheck <- function(rawFile=file.choose(),
                     name_diff = edit(name = 'Please input filename of JiaHe type and click SAVE'), 
                     alpha.type = edit(name = 'Please input alpha of JiaHe type and click SAVE'),
                     alpha.dup = edit(name = 'Please input alpha of Duplication and click SAVE')){
  setwd(dir = dirname(rawFile))
  files1 <- list.files()
  # raw data read in
  dt1 <- read.csv(rawFile,header = TRUE)
  dt1 <- dt1[order(dt1$cluster,dt1$Average.Mz),]
  dt1$tmpid <- seq(1,nrow(dt1))+100000
  dt2 <- dt1[,c('tmpid','cluster','Average.Mz')]
  # diff file read in
  name_diff <- as.character(name_diff)
  req <- read.csv(files1[grep(tolower(name_diff),tolower(files1))],header = TRUE)
  colnames(req) <- c('initial','final','diff')
  req <- req[order(req$diff),]
  breaks1 <- c(-Inf,req$diff-alpha.type-0.0000001,req$diff+alpha.type,Inf)
  breaks1 <- breaks1[order(breaks1)]
  labels1 <- c(as.vector(rbind(rep('_',nrow(req)),
                               paste(req$initial,req$final,sep = '_'))),'_')
  dt3 <- split(dt2,dt2$cluster)
  dis1 <- do.call(rbind,lapply(dt3,distFun))
  dis1$type1 <- as.character(cut(dis1$value,breaks = breaks1,labels = labels1))
  
  dis1$initial2 <- unlist(lapply(strsplit(dis1$type1,'_'),function(x)x[1]))
  dis1$final2 <- unlist(lapply(strsplit(dis1$type1,'_'),function(x)x[2]))
  # head(dis1)
  initi <- data.frame(tmpid = dis1$initial,adduct_type2=dis1$initial2)
  final <- data.frame(tmpid = dis1$final, adduct_type2 = dis1$final2)
  res1 <- unique(rbind(initi,final))
  res1 <- res1[nchar(res1$adduct_type2)>0,]
  res1 <- aggregate(adduct_type2~tmpid,data = res1,FUN = function(x){paste(x,collapse = ';')})
  res2 <- merge(dt1,res1,by = 'tmpid',all.x = TRUE)
  res2$adduct_type2[-grep('\\+',res2$adduct_type2)] <- NA
  
  res3 <- check.func(data = res2,alpha = alpha.dup) 
  res3$dup[!is.na(res3$adduct_type2)] <- NA
  res4 <- res3
  # res4 <- res3[is.na(res3$dup),]
  # res4[,-which(colnames(res4) %in% c('tmpid','dup'))]
  write.csv(res4,paste0('./res_dupChk',format(Sys.Date(),'%Y%m%d'),'.csv'),
            row.names = FALSE,na='')
}

