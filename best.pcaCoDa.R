best.pcaCoDa<-function(data, init.seed=0, samples=100){
  
  require(setRNG)
  require(robCompositions)
  
  seed.list <- list()
  pca.list <- list()
  pca.2sd <- vector()
  set.seed(init.seed)
  
  for (i in 1:samples){
    seed.list[[i]]<-getRNG()
    pca <- pcaCoDa(data)
    pca.list[[i]] <- pca
    pca.var <- cumsum((pca$princompOutputClr$sdev)^2) / sum(pca$princompOutputClr$sdev^2)
    pca.2sd[i] <- pca.var[2]
  }
  pca.best.index<-match(max(pca.2sd),pca.2sd)
  pca.best<-pca.list[[pca.best.index]]
  pca.best$seed <- seed.list[[pca.best.index]]
  pca.best$sub <- paste(as.character(100*round(pca.2sd[pca.best.index],digits=4)),"% of variance explained", sep="")
  print(pca.best$sub)
  
  return(pca.best)
  }