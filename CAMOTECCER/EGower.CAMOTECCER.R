EGower.CAMOTECCER<-function(data, var.set, out.file, d.type="d2",
                          excep.col=NULL, excep.val=NULL, excep.dist = 0){
  # var.set is a list of sets of variables of different kind
  # d.type states the kind of distance/similarity index (d2:neighbour interexchange, petrography,
  #                                                     d3:relative ranking, petrography,
  #                                                     d4:relative ranking, chemical composition and petrography)
  # excep.col is the vector of variables index to be searched for exceptions
  # excep.val is the value to be set with distance
  
  # order factors
  
  source(file="functions/CAMOTECCER/CAMOTECCERDATA.R")
  
  data <- CAMOTECCERDATA3(data)
  
  # set exception values as NAs
  for (v in excep.col){
    for (i in 1:nrow(data)){
      if (is.na(data[i,v])==FALSE){
        if (data[i,v]==excep.val){
          data[i,v] <- NA
        }
      }      
    }
  }
  
  ### Extended Gower with Exception
  
  require(ade4)
  source(file="functions/CAMOTECCER/dist.ktab.CAMOTECCER.R")
  data <- data.frame(data.matrix(data))
  
  # list separete variable sets
  list.var.set<-list()
  for (i in 1:length(var.set)){
    list.var.set[[i]]<-data[,var.set[[i]]]
  }
  ktab1 <- ktab.list.df(list.var.set)
  
  # calculate distance
  if(d.type=="d2"){
    
    dist <- dist.ktab.CAMOTECCER(ktab1, type=c("O"), option = "scaledBYrange", scann=F, dist.excep = excep.dist, d.type="d2")
    
    write.csv(data.frame(as.matrix(dist)), file=out.file, row.names = TRUE) 
    
  } else if(d.type=="d3"){
    
    dist <- dist.ktab.CAMOTECCER(ktab1, type=c("O"), option = "scaledBYrange", scann=F, dist.excep = excep.dist, d.type="d3")
    
    if (is.euclid(dist)==F){
      # Lingoes transformation (1971)
      dist <- lingoes(dist)
      print("Lingoes transformation done.")
    }
    
    write.csv(data.frame(as.matrix(dist)), file=out.file, row.names = TRUE)
    
  } else if(d.type=="d4"){
    
    dist <- dist.ktab.CAMOTECCER(ktab1, type=c("O","Q"), option = "scaledBYrange", scann=F, dist.excep = excep.dist, d.type="d4", weight=c(.5,.5))
    
    if (is.euclid(dist)==F){
      # Lingoes transformation (1971)
      dist.euc <- lingoes(dist)
      print("Lingoes transformation done.")
    }
    
    write.csv(data.frame(as.matrix(dist)), file=out.file, row.names = TRUE)
    
  }
  
  return(dist)
}