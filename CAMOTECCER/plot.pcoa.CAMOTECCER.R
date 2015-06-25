plot.pcoa.CAMOTECCER <- function(pcoa.list, factors.list, labels.list, arrow.lab.adj.list,test.list,
                                    otype=c("png","tiff"),dtype="d3", file.names=c("ALL","AMP","UZB","TW"),
                                    Swidth=3000,Sheight=3000,Lwidth=3000,Lheight=3000,gxlim=list(NULL,NULL,NULL,NULL),gylim=list(NULL,NULL,NULL,NULL),
                                    gcex=3,lcex=3,rcex=1.5,tcex=2,glwd=2,llwd=3,rlwd=3,cstar=1,cellipse=1,clabel=0.5,cpoint=0.8,
                                    lfig=c(0.02,0.35,0.06,0.25),
                                    rfig=c(0.7,0.99,0.01,0.3)){
  require(ade4)
  library(extrafont)
  
  plot.list<-list()
  for (pcoa.i in 1:length(pcoa.list)){
    pcoa <- pcoa.list[[pcoa.i]]
    for (out in otype){
      if(out=="png"){
        require(png)
        fn <- paste("Fabric_distances/",dtype,"/",dtype,"_",file.names[pcoa.i],".png",sep="")
        png(filename = fn, width = Swidth, height = Sheight)
      } 
      if(out=="eps"){
        fn <- paste("Fabric_distances/",dtype,"/",dtype,"_",file.names[pcoa.i],".eps",sep="")
        loadfonts(device="postscript")
        postscript(fn , pointsize=10, width=Swidth/100, height=Sheight/100, 
                   horizontal=FALSE, paper = "special", family="serif", colormodel="cmyk")
      }
      if(out=="tiff"){
        require(tiff)
        fn <- paste("Fabric_distances/",dtype,"/",dtype,"_",file.names[pcoa.i],".tiff",sep="")
        tiff(filename = fn, width = Swidth, height = Sheight)
      }
      if(out=="jpeg"){
        require(jpeg)
        fn <- paste("Fabric_distances/",dtype,"/",dtype,"_",file.names[pcoa.i],".jpeg",sep="")
        jpeg(filename = fn, width = Swidth, height = Sheight)
      }
      par(cex=gcex,mar=c(3,3,3,3), lwd=glwd)
      
      if (nchar(labels.list[[pcoa.i]][1])>1){
        this.labels<-rep("",length(labels.list[[pcoa.i]]))
      } else {
        this.labels<-labels.list[[pcoa.i]]
      }
      s.class(pcoa$points, xax=1,yax=2,fac=factors.list[[pcoa.i]],
              xlim=gxlim[[pcoa.i]],ylim=gylim[[pcoa.i]],
              cstar=cstar, cellipse=cellipse,axesell=F, sub=pcoa$sub, pch=this.labels,
              clabel=clabel, cpoint=cpoint)
      if (nchar(labels.list[[pcoa.i]][1])>1){
        for (i in 1:nrow(pcoa$points)){
          text(x=pcoa$points[i,1],y=pcoa$points[i,2],labels=labels.list[[pcoa.i]][i])
        }
      }
      par(fig=lfig, new = T , mar=c(.1,.1,1,.1), lwd=llwd)
      barplot(pcoa$eig, space=0, col="white", axes=FALSE)
      
      par(fig=rfig, new = T , mar=c(.1,.1,.1,.1), lwd=rlwd)
      plot(pcoa$loadings[,1],pcoa$loadings[,2], pch="", axes=FALSE,
           xaxt='n', yaxt='n', ann=FALSE, 
           xlim=c(min(pcoa$loadings[,1])-0.01,max(pcoa$loadings[,1])+0.01),
           ylim=c(min(pcoa$loadings[,2])-0.01,max(pcoa$loadings[,2])+0.01))
      filtered.arrows<-pcoa$loadings[abs(pcoa$loadings[,1])+abs(pcoa$loadings[,2])>mean(abs(pcoa$loadings[,1])+abs(pcoa$loadings[,2])),1:2]
      arrows(0,0,filtered.arrows[,1],filtered.arrows[,2],length=0.5, col="darkgrey")
      for (i in 1:nrow(filtered.arrows)){
        text(filtered.arrows[i,1],filtered.arrows[i,2],labels=row.names(filtered.arrows)[i], adj=arrow.lab.adj.list[[pcoa.i]][i,], cex=rcex)
      }
      par(fig=c(0,0.3,0.8,1), new = T , mar=c(0,0,0,0))
      plot.new()
      text(0,0.7,labels=test.list[[pcoa.i]][1],cex=tcex,pos=4)
      text(0,0.6,labels=test.list[[pcoa.i]][2],cex=tcex,pos=4)
      text(0,0.2,labels=test.list[[pcoa.i]][3],cex=tcex,pos=4)
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