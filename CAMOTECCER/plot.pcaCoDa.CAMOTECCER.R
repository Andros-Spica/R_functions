plot.pcaCoDa.CAMOTECCER <- function(pca.list, factors.list, labels.list, arrow.lab.adj.list,test.list,
                                    otype=c("png"),dtype="Protocol_1", file.names=c("ALL","AMP","UZB","TW"),
                                    Swidth=3000,Sheight=3000,Lwidth=3000,Lheight=3000,gxlim=list(NULL,NULL,NULL,NULL),gylim=list(NULL,NULL,NULL,NULL),
                                    gcex=3,lcex=3,rcex=1.5,tcex=2,glwd=2,llwd=3,rlwd=3,cstar=1,cellipse=1,clabel=0.5,cpoint=0.8,
                                    lfig=c(0.02,0.35,0.06,0.25),
                                    rfig=c(0.7,0.99,0.01,0.3)){
  require(ade4)
  library(extrafont)
  
  plot.list<-list()
  for (pca.i in 1:length(pca.list)){
    pca <- pca.list[[pca.i]]
    for (out in otype){
      if(out=="png"){
        require(png)
        fn <- paste("Fabric_distances/",dtype,"/",dtype,"_",file.names[pca.i],".png",sep="")
        png(filename = fn, width = Swidth, height = Sheight)
      } 
      if(out=="eps"){
        fn <- paste("Fabric_distances/",dtype,"/",dtype,"_",file.names[pca.i],".eps",sep="")
        loadfonts(device="postscript")
        postscript(fn , pointsize=10, width=Swidth/100, height=Sheight/100, 
                   horizontal=FALSE, paper = "special", family="serif", colormodel="cmyk")
      }
      if(out=="tiff"){
        require(tiff)
        fn <- paste("Fabric_distances/",dtype,"/",dtype,"_",file.names[pca.i],".tiff",sep="")
        tiff(filename = fn, width = Swidth, height = Sheight)
      }
      if(out=="jpeg"){
        require(jpeg)
        fn <- paste("Fabric_distances/",dtype,"/",dtype,"_",file.names[pca.i],".jpeg",sep="")
        jpeg(filename = fn, width = Swidth, height = Sheight)
      }
      par(cex=gcex,mar=c(3,3,3,3), lwd=glwd)
      if (nchar(labels.list[[pca.i]][1])>1){
        this.labels<-rep("",length(labels.list[[pca.i]]))
      } else {
        this.labels<-labels.list[[pca.i]]
      }
      s.class(pca$scores[,c(1,2)], xax=1,yax=2,fac=factors.list[[pca.i]],
              cstar=cstar, cellipse=cellipse,axesell=F, sub=pca$sub, pch=this.labels,
              clabel=clabel, cpoint=cpoint)
      if (nchar(labels.list[[pca.i]][1])>1){
        for (i in 1:nrow(pca$scores)){
          text(x=pca$scores[i,1],y=pca$scores[i,2],labels=labels.list[[pca.i]][i])
        }
      }
      
      par(cex=lcex,fig=lfig, new = T , mar=c(.1,.1,1,.1), lwd=llwd)
      barplot(pca$eigenvalues, space=0, col="white", axes=FALSE)
      
      par(fig=rfig, new = T , mar=c(.1,.1,.1,.1), lwd=rlwd)
      plot(pca$loadings[,1],pca$loadings[,2], pch="", axes=FALSE,
           xaxt='n', yaxt='n', ann=FALSE, 
           xlim=c(min(pca$loadings[,1])-0.01,max(pca$loadings[,1])+0.01),
           ylim=c(min(pca$loadings[,2])-0.01,max(pca$loadings[,2])+0.01))
      arrows(0,0,pca$loadings[,1],pca$loadings[,2],length=0.5, col="darkgrey")
      for (i in 1:nrow(pca$loadings)){
        text(pca$loadings[i,1],pca$loadings[i,2],labels=row.names(pca$loadings)[i], adj=arrow.lab.adj.list[[pca.i]][i,], cex=rcex)
      }
      par(fig=c(0,0.3,0.8,1), new = T , mar=c(0,0,0,0))
      plot.new()
      text(0,0.7,labels=test.list[[pca.i]][1],cex=tcex,pos=4)
      text(0,0.6,labels=test.list[[pca.i]][2],cex=tcex,pos=4)
      text(0,0.2,labels=test.list[[pca.i]][3],cex=tcex,pos=4)
      par(fig=c(0,1,0,1))
      dev.off()
    } 
  }
  
  if("png" %in% otype){
    ALL<-readPNG(paste("Fabric_distances/",dtype,"/",dtype,"_ALL.png",sep=""))
    AMP<-readPNG(paste("Fabric_distances/",dtype,"/",dtype,"_AMP.png",sep=""))
    UZB<-readPNG(paste("Fabric_distances/",dtype,"/",dtype,"_UZB.png",sep=""))
    TW<-readPNG(paste("Fabric_distances/",dtype,"/",dtype,"_TW.png",sep=""))
    fn <- paste("Fabric_distances/",dtype,"/",dtype,".png",sep="")
    png(filename = fn, width = Lwidth, height = Lheight)
    par(mar=c(0,0,0,0),oma=c(0,0,0,0))
    plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes=F)
    rasterImage(ALL,-.03,.5,.5,1.03)
    rasterImage(AMP,.5,.5,1.03,1.03)
    rasterImage(UZB,-.03,-.03,.5,.5)
    rasterImage(TW,.5,-.03,1.03,.5)
    dev.off()
  }
  if("tiff" %in% otype){
    ALL<-readTIFF(paste("Fabric_distances/",dtype,"/",dtype,"_ALL.tiff",sep=""))
    AMP<-readTIFF(paste("Fabric_distances/",dtype,"/",dtype,"_AMP.tiff",sep=""))
    UZB<-readTIFF(paste("Fabric_distances/",dtype,"/",dtype,"_UZB.tiff",sep=""))
    TW<-readTIFF(paste("Fabric_distances/",dtype,"/",dtype,"_TW.tiff",sep=""))
    fn <- paste("Fabric_distances/",dtype,"/",dtype,".tiff",sep="")
    tiff(filename = fn, width = Lwidth, height = Lheight)
    par(mar=c(0,0,0,0))
    plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes=F)
    rasterImage(ALL,-.03,.5,.5,1.03)
    rasterImage(AMP,.5,.5,1.03,1.03)
    rasterImage(UZB,-.03,-.03,.5,.5)
    rasterImage(TW,.5,-.03,1.03,.5)
    dev.off()
  }
  if("jpeg" %in% otype){
    ALL<-readJPEG(paste("Fabric_distances/",dtype,"/",dtype,"_ALL.jpeg",sep=""))
    AMP<-readJPEG(paste("Fabric_distances/",dtype,"/",dtype,"_AMP.jpeg",sep=""))
    UZB<-readJPEG(paste("Fabric_distances/",dtype,"/",dtype,"_UZB.jpeg",sep=""))
    TW<-readJPEG(paste("Fabric_distances/",dtype,"/",dtype,"_TW.jpeg",sep=""))
    fn <- paste("Fabric_distances/",dtype,"/",dtype,".jpeg",sep="")
    jpeg(filename = fn, width = Lwidth, height = Lheight)
    par(mar=c(0,0,0,0))
    plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes=F)
    rasterImage(ALL,-.03,.5,.5,1.03)
    rasterImage(AMP,.5,.5,1.03,1.03)
    rasterImage(UZB,-.03,-.03,.5,.5)
    rasterImage(TW,.5,-.03,1.03,.5)
    dev.off()
  }
  fn <- paste("Fabric_distances/",dtype,"/",dtype,".eps",sep="")
  loadfonts(device="postscript")
  postscript(fn , pointsize=10, width=Lwidth/100, height=Lheight/100,
             horizontal=FALSE, paper = "special", family="serif", colormodel="cmyk")
  par(mar=c(0,0,0,0))
  plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes=F)
  rasterImage(ALL,-.03,.5,.5,1.03)
  rasterImage(AMP,.5,.5,1.03,1.03)
  rasterImage(UZB,-.03,-.03,.5,.5)
  rasterImage(TW,.5,-.03,1.03,.5)
  dev.off()
  graphics.off()
}