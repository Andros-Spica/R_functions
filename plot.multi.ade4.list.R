plot.multi.ade4.list <- function(pca.list, factors.list, ordType="PCA",
                                 labels.list, col.list=as.list(rep("black",length(pca.list))),
                                 arrow.lab.adj.list, col.arrow="darkorange", test.list,
                                 xtitle=rep("",length(pca.list)),ytitle=rep("",length(pca.list)),
                                 otype=c("png"),directory="Fabric_distances/",
                                 file.names=c("ALL","AMP","UZB","TW"),
                                 SmallWidth=3000,SmallHeight=3000,LargeWidth=3000,LargeHeight=3000,
                                 generalcex=3,leftcex=3,rightcex=1.5,testcex=2,titlecex=4,
                                 generallwd=2,leftlwd=3,rightlwd=3,
                                 cstar=1,cellipse=1,clabel=0.5,cpoint=0.8,
                                 leftfig=c(0.02,0.35,0.06,0.25),rightfig=c(0.7,0.99,0.01,0.3),
                                 testfig=c(0,0.3,0.8,1),xtitlefig=c(0.25,1,0.85,1),ytitlefig=c(0.91,1,0,1)) {
  require(ade4)
  require(extrafont)
  
  plot.list<-list()
  for (pca.i in 1:length(pca.list)){
    
    plot.multi.ade4(pca.list[[pca.i]], factor=factors.list[[pca.i]], ordType=ordType,
                    labels=labels.list[[pca.i]], col=col.list[[pca.i]],
                    arrow.lab.adj=arrow.lab.adj.list[[pca.i]], col.arrow=col.arrow, 
                    tests=test.list[[pca.i]], xtitle=xtitle[pca.i],ytitle=ytitle[pca.i],
                    otype=otype, directory=directory, file.name=file.names[pca.i],
                    width=SmallWidth,height=SmallHeight,
                    generalcex=generalcex,leftcex=leftcex,rightcex=rightcex,testcex=testcex,
                    titlecex=titlecex,generallwd=generallwd,leftlwd=leftlwd,rightlwd=rightlwd,
                    cstar=cstar,cellipse=cellipse,clabel=clabel,cpoint=cpoint,
                    leftfig=leftfig,rightfig=rightfig,
                    testfig=testfig,xtitlefig=xtitlefig,ytitlefig=ytitlefig)
  }
   
  if("png" %in% otype){
    require(png)
    p1<-readPNG(paste(directory, file.names[[1]],".png",sep=""))
    p2<-readPNG(paste(directory, file.names[[2]],".png",sep=""))
    p3<-readPNG(paste(directory, file.names[[3]],".png",sep=""))
    p4<-readPNG(paste(directory, file.names[[4]],".png",sep=""))
    fn <- paste(directory, ".png",sep="")
    png(filename = fn, width = LargeWidth, height = LargeHeight)
    par(mar=c(0,0,0,0),oma=c(0,0,0,0))
    plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes=F)
    rasterImage(p1,-.03,.5,.5,1.03)
    rasterImage(p2,.5,.5,1.03,1.03)
    rasterImage(p3,-.03,-.03,.5,.5)
    rasterImage(p4,.5,-.03,1.03,.5)
    dev.off()
  }
  if("tiff" %in% otype){
    require(tiff)
    p1<-readTIFF(paste(directory, file.names[[1]],".tiff",sep=""))
    p2<-readTIFF(paste(directory, file.names[[2]],".tiff",sep=""))
    p3<-readTIFF(paste(directory, file.names[[3]],".tiff",sep=""))
    p4<-readTIFF(paste(directory, file.names[[4]],".tiff",sep=""))
    fn <- paste(directory,".tiff",sep="")
    tiff(filename = fn, width = LargeWidth, height = LargeHeight)
    par(mar=c(0,0,0,0))
    plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes=F)
    rasterImage(p1,-.03,.5,.5,1.03)
    rasterImage(p2,.5,.5,1.03,1.03)
    rasterImage(p3,-.03,-.03,.5,.5)
    rasterImage(p4,.5,-.03,1.03,.5)
    dev.off()
  }
  if("jpeg" %in% otype){
    require(jpeg)
    p1<-readJPEG(paste(directory, file.names[[1]],".jpeg",sep=""))
    p2<-readJPEG(paste(directory, file.names[[2]],".jpeg",sep=""))
    p3<-readJPEG(paste(directory, file.names[[3]],".jpeg",sep=""))
    p4<-readJPEG(paste(directory, file.names[[4]],".jpeg",sep=""))
    fn <- paste(directory,".jpeg",sep="")
    jpeg(filename = fn, width = LargeWidth, height = LargeHeight)
    par(mar=c(0,0,0,0))
    plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes=F)
    rasterImage(p1,-.03,.5,.5,1.03)
    rasterImage(p2,.5,.5,1.03,1.03)
    rasterImage(p3,-.03,-.03,.5,.5)
    rasterImage(p4,.5,-.03,1.03,.5)
    dev.off()
  }
  fn <- paste(directory,".eps",sep="")
  loadfonts(device="postscript")
  postscript(fn , pointsize=10, width=LargeWidth/100, height=LargeHeight/100,
             horizontal=FALSE, paper = "special", family="serif", colormodel="cmyk")
  par(mar=c(0,0,0,0))
  plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes=F)
  rasterImage(p1,-.03,.5,.5,1.03)
  rasterImage(p2,.5,.5,1.03,1.03)
  rasterImage(p3,-.03,-.03,.5,.5)
  rasterImage(p4,.5,-.03,1.03,.5)
  dev.off()
  graphics.off()


}


plot.multi.ade4 <- function(pca, factor, ordType="PCA", labels, col="black",
                            lPlot=TRUE, rPlot=TRUE, showTests=TRUE,
                            arrow.lab.adj=NULL, col.arrow="darkgrey", tests=NULL, xtitle="", ytitle="",
                            otype=c("png"),directory="", file.name="Pca with ade4",
                            width=3000,height=3000,
                            generalcex=3,leftcex=3,rightcex=1.5,testcex=2,titlecex=4,
                            generallwd=2,leftlwd=3,rightlwd=3,
                            cstar=1,cellipse=1,clabel=0.5,cpoint=0.8,
                            leftfig=c(0.02,0.35,0.06,0.25),rightfig=c(0.7,0.99,0.01,0.3),
                            testfig=c(0,0.3,0.8,1),xtitlefig=c(0.25,1,0.85,1),ytitlefig=c(0.91,1,0,1)){
  require(ade4)
  library(extrafont)
  
  scores<-NULL
  loadings<-NULL
  sdev<-NULL
  eigenvalues<-NULL

  # PCAs
  if (ordType=="PCA") {
    if (is.null(pca$x)){
      scores <- pca$scores
      loadings <- pca$loadings
      sdev <- pca$sdev
      if (is.null(sdev)){
        sdev <- pca$princompOutputClr$sdev
      }
    } else {
      scores <- pca$x
      loadings <- pca$rotation
      sdev <- pca$sdev
    }
  }
  # LDA
  if (ordType=="LDA") {
    scores <- pca$values$x
    if (is.null(pca$loadings)){
      loadings <- pca$scaling
    } else {
      loadings <- pca$loadings
    }  
    sdev <- pca$svd
    eigenvalues <- (sdev)^2
    eigenvalues <- (eigenvalues / sum(eigenvalues)) * 100
  }
  cumvar <- cumsum((sdev)^2) / sum(sdev^2)
  sub <- paste(as.character(100*round(cumvar[2],digits=4)),"% of variance explained", sep="")
  eigenvalues <- (sdev)^2
  names(eigenvalues) <- rep("",length(eigenvalues))
  # NMDS
  if (ordType=="NMDS") {
    scores <- pca$points
    loadings <- pca$loadings
    sub <- pca$sub
  }  
  # PCOA
  if (ordType=="PCoA") {
    scores <- pca$points
    loadings <- pca$loadings
    sub <- pca$sub
    eigenvalues <- pca$eig
  } 
  
  if (lPlot==FALSE) {
    sub <- ""
  }
  
  for (out in otype){
    if(out=="png"){
      require(png)
      fn <- paste(directory,file.name,".png",sep="")
      png(filename = fn, width = width, height = height)
    } 
    if(out=="eps"){
      fn <- paste(directory,file.name,".eps",sep="")
      loadfonts(device="postscript")
      postscript(fn , pointsize=10, width=width/100, height=height/100, 
                 horizontal=FALSE, paper = "special", family="serif", colormodel="cmyk")
    }
    if(out=="tiff"){
      require(tiff)
      fn <- paste(directory,file.name,".tiff",sep="")
      tiff(filename = fn, width = width, height = height)
    }
    if(out=="jpeg"){
      require(jpeg)
      fn <- paste(directory,file.name,".jpeg",sep="")
      jpeg(filename = fn, width = width, height = height)
    }
    par(cex=generalcex,mar=c(3,3,3,3), lwd=generallwd)
    if (nchar(labels[1])>1){
      this.labels<-rep("",length(labels))
    } else {
      this.labels<-labels
    }
    
    s.class(scores[,c(1,2)], xax=1,yax=2,fac=factor, col=col,
            cstar=cstar, cellipse=cellipse,axesell=F, sub=sub, pch=this.labels,
            clabel=clabel, cpoint=cpoint)
    if (nchar(labels[1])>1){
      for (i in 1:nrow(pca$scores)){
        text(x=scores[i,1],y=scores[i,2],labels=labels[i], col=col[fac[i]],cex=cpoint)
      }
    }
    if (lPlot==TRUE) {
      if (ordType=="NMDS") {
        par(cex=leftcex,fig=leftfig, new = T , mar=c(.1,.1,1,.1), lwd=leftlwd)
        stressplot(pca, p.col = "darkgrey", l.col = "black", xaxt="n", yaxt="n")
      } else {
        par(cex=leftcex,fig=leftfig, new = T , mar=c(.1,.1,1,.1), lwd=leftlwd)
        barplot(eigenvalues, space=0, col="white", axes=FALSE, cex.axis=0)
      }
    }
    
    if (rPlot==TRUE) {
      # filter variables to represent (if there are more than 25)
      if (nrow(loadings)>25){
        loadings<-loadings[abs(loadings[,1])+abs(loadings[,2])>mean(abs(loadings[,1])+abs(loadings[,2])),1:2]   
      }
      
      par(fig=rightfig, new = T , mar=c(.1,.1,.1,.1), lwd=rightlwd)
      plot(loadings[,1],loadings[,2], pch="", axes=FALSE,
           xaxt='n', yaxt='n', ann=FALSE, 
           xlim=c(min(loadings[,1])-0.01,max(loadings[,1])+0.01),
           ylim=c(min(loadings[,2])-0.01,max(loadings[,2])+0.01))
      arrows(0,0,loadings[,1],loadings[,2],length=0.5, col=col.arrow)
      for (i in 1:nrow(loadings)){
        text(loadings[i,1],loadings[i,2],labels=row.names(loadings)[i], adj=arrow.lab.adj[i,], cex=rightcex)
      }
    }
    
    if (showTests==TRUE) {
      
      par(fig=testfig, new = T , mar=c(0,0,0,0))
      plot.new()
      text(0,0.7,labels=tests[1],cex=testcex,pos=4)
      text(0,0.6,labels=tests[2],cex=testcex,pos=4)
      text(0,0.2,labels=tests[3],cex=testcex,pos=4)
      par(fig=c(0,1,0,1))
    }
      
    par(fig=xtitlefig, new = T , mar=c(0,0,0,0))
    plot.new()
    text(0.3,0.7,labels=xtitle,cex=titlecex,pos=4)
    par(fig=c(0,1,0,1))
    
    par(fig=ytitlefig, new = T , mar=c(0,0,0,0))
    plot.new()
    text(0.3,0.7,labels=ytitle,cex=titlecex,pos=3,srt=90)
    par(fig=c(0,1,0,1))
    
    dev.off()
  }
  
}