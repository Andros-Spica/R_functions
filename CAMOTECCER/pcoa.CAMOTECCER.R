pcoa.CAMOTECCER<-function(dist,data){
  
  pcoa<-cmdscale(dist, eig=T)
  
  # variance explained by the two first axes
  pcoa$sub<-paste(as.character(100*round(sum(pcoa$GOF),digits=4)),"% of variance explained",sep="")
  
  # calculate covariance axis vs. variables
  source(file="R_functions/CAMOTECCER/CAMOTECCERDATA.R")
  dt <- CAMOTECCERDATA.PETRO(data)
  dt <- data.frame(data.matrix(data))
  transrank <- function(u){
    
    return(rank(u, na.last = "keep"))
    
  }
  dt <- apply(dt, 2, transrank)
  
  covmat<-cov(cbind(dt,pcoa$points),use="pairwise.complete.obs") 
  pcoa$loadings<-covmat[1:ncol(data),c(ncol(data)+1,ncol(data)+2)]
  dimnames(pcoa$loadings)[[2]]<-c("A1","A2")
  
  # replace variables names with codes
  library(stringr)
  vcod <- vector()
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
  dimnames(pcoa$loadings)[[1]]<-vcod
    
  return(pcoa)
}