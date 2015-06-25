nmds.CAMOTECCER<-function(dist, data, k=2,trymax=100, init.seed=0, autotransform = FALSE){
  
  require(vegan)
  
  nmds<-metaMDS(dist, k=k, trymax=trymax, autotransform = autotransform)
  
  nmds$sub<-paste(as.character(100*round(nmds$stress,digits=4)),"% of stress",sep="")
  
  # calculate covariance axis vs. variables
  dt<-data.frame(data.matrix(data))
  transrank <- function(u){
    return(rank(u, na.last = "keep"))    
  }
  dt <- apply(dt, 2, transrank)
  
  covmat<-cov(cbind(dt,nmds$points),use="complete.obs") 
  nmds$loadings<-covmat[1:ncol(data),c(ncol(data)+1,ncol(data)+2)]
  dimnames(nmds$loadings)[[2]]<-c("MDS1","MDS2")
  
  # replace variables names with codes
  library(stringr)
  vcod <- vector()
  for (i in 1:nrow(nmds$loadings)){
    for (i in 1:nrow(pcoa$loadings)){
      if(names(data)[i]=="INCLUS_DISTRIB" | names(data)[i]=="INCLUS_ORIENT"){
        vcod[i]<-paste("I",i,sep="")
      } else if(names(data)[i]=="TEMP" | names(data)[i]=="ATM" | names(data)[i]=="POST_ATM"){
        new.i<-i-2
        vcod[i]<-paste("F",new.i,sep="")
      } else if(str_detect(names(data)[i],"VOID_")){
        new.i<-i-5
        vcod[i]<-paste("V",new.i,sep="")
      } else if(str_detect(names(data)[i],"COAR_")){
        new.i<-i-21
        vcod[i]<-paste("L",new.i,sep="")
      } else if(str_detect(names(data)[i],"FINE_")){
        new.i<-i-69
        vcod[i]<-paste("S",new.i,sep="")
      } else {
        new.i<-i-87
        vcod[i]<-names(data)[i]#paste("C",new.i,sep="")
      }
    }
  }
  dimnames(nmds$loadings)[[1]]<-vcod
  
  print(nmds$sub)
  return(nmds)
}