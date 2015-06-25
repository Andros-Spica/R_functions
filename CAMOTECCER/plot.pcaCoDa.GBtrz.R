plot.pcaCoDa.GBtrz <- function(pca.list, factors.list, factors2.list, labels.list, arrow.lab.adj.list,test.list,
                                    otype=c("png","tiff","eps"),dtype="ILRrob", file.names=c("trz","GB"),
                                    Swidth=3000,Sheight=3000,Lwidth=3000,Lheight=3000,
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
        fn <- paste("Fabric_distances/GB14 & trz14/",dtype,"_",file.names[pca.i],".png",sep="")
        png(filename = fn, width = Swidth, height = Sheight)
      } 
      if(out=="eps"){
        fn <- paste("Fabric_distances/GB14 & trz14/",dtype,"_",file.names[pca.i],".eps",sep="")
        loadfonts(device="postscript")
        postscript(fn , pointsize=10, width=Swidth/100, height=Sheight/100, 
                   horizontal=FALSE, paper = "special", family="serif", colormodel="cmyk")
      }
      if(out=="tiff"){
        require(tiff)
        fn <- paste("Fabric_distances/GB14 & trz14/",dtype,"_",file.names[pca.i],".tiff",sep="")
        tiff(filename = fn, width = Swidth, height = Sheight)
      }
      if(out=="jpeg"){
        require(jpeg)
        fn <- paste("Fabric_distances/GB14 & trz14/",dtype,"_",file.names[pca.i],".jpeg",sep="")
        jpeg(filename = fn, width = Swidth, height = Sheight)
      }
      par(cex=gcex,mar=c(3,3,3,3), lwd=glwd)
      if (nchar(labels.list[[pca.i]][1])>1){
        this.labels<-rep("",length(labels.list[[pca.i]]))
      } else {
        this.labels<-labels.list[[pca.i]]
      }
      if (pca.i==1){
        this.ylim<-c(-.52,.48)
        this.xlim<-c(-.8,.48)#with CLAY
      }
      if (pca.i==2){
        #this.ylim<-c(-.52,.48)
        this.ylim<-c(-.9,.48)#with CLAY
        this.xlim<-c(-.6,.6)#with CLAY
      }
      s.class(pca$scores[factors.list[[pca.i]]!="CLAY",c(1,2)], xax=1,yax=2,fac=factors.list[[pca.i]][factors.list[[pca.i]]!="CLAY"],
              cstar=cstar, cellipse=cellipse,axesell=F, sub=pca$sub, pch=this.labels[factors.list[[pca.i]]!="CLAY"],
              xlim=this.xlim,#with CLAY
              ylim=this.ylim,
              clabel=clabel, cpoint=cpoint)
      if (nchar(labels.list[[pca.i]][1])>1){
        for (i in 1:nrow(pca$scores)){
          #if(factors.list[[pca.i]][i]!="CLAY"){
          #  text(x=pca$scores[i,1],y=pca$scores[i,2],labels=labels.list[[pca.i]][i],cex=cpoint)
          #}
          if(pca.i==1){
            if(factors2.list[[pca.i]][i]=="Group 1"){
              text(x=pca$scores[i,1],y=pca$scores[i,2],labels=labels.list[[pca.i]][i],cex=cpoint,col="red")
            } else if(factors2.list[[pca.i]][i]=="Group 2"){
              text(x=pca$scores[i,1],y=pca$scores[i,2],labels=labels.list[[pca.i]][i],cex=cpoint,col="blue")
            } else if(factors2.list[[pca.i]][i]=="OUTLIER"){
              text(x=pca$scores[i,1],y=pca$scores[i,2],labels=labels.list[[pca.i]][i],cex=cpoint,font=2,col="purple")
            } else if(factors2.list[[pca.i]][i]=="CLAY"){
              text(x=pca$scores[i,1],y=pca$scores[i,2],labels=labels.list[[pca.i]][i],cex=cpoint,font=2)
            }
          }
          if(pca.i==2){
            if(factors2.list[[pca.i]][i]=="Group 1"){
              text(x=pca$scores[i,1],y=pca$scores[i,2],labels=labels.list[[pca.i]][i],cex=cpoint,col="red")
            } else if(factors2.list[[pca.i]][i]=="Group 2"){
              text(x=pca$scores[i,1],y=pca$scores[i,2],labels=labels.list[[pca.i]][i],cex=cpoint,col="blue")
            } else if(factors2.list[[pca.i]][i]=="OUTLIER"){
              text(x=pca$scores[i,1],y=pca$scores[i,2],labels=labels.list[[pca.i]][i],cex=cpoint,font=2,col="purple")
            } else if(factors2.list[[pca.i]][i]=="KPT"){
              text(x=pca$scores[i,1],y=pca$scores[i,2],labels=labels.list[[pca.i]][i],cex=cpoint,font=4,col="orange")
            } else if(factors2.list[[pca.i]][i]=="CLAY"){
              text(x=pca$scores[i,1],y=pca$scores[i,2],labels=labels.list[[pca.i]][i],cex=cpoint,font=2)
            }
          }
        }
      }
      if (pca.i==1){
        text(x=c(-.33,.25), y=c(.05,-.05), labels=c('Group 1', 'Group 2'),cex=cpoint*1.7, font=4)
      }
      if (pca.i==2){
        text(x=c(.2,-.1), y=c(-.28,.28), labels=c('Group 1', 'Group 2'),cex=cpoint*1.7, font=4)
      } 
      
      par(cex=lcex,fig=lfig, new = T , mar=c(.1,.1,1,.1), lwd=llwd)
      if(dtype=="ALR"){
        plot(pca,type="barplot", space=0, col="white", main="", axes=FALSE, axisnames=FALSE)
      }
      if(dtype=="ILR"){
        barplot(pca$eigenvalues, space=0, col="white", axes=FALSE)
      }   
      
      par(fig=rfig, new = T , mar=c(.1,.1,.1,.1), lwd=rlwd)
      plot(pca$loadings[,1],pca$loadings[,2], pch="", axes=FALSE,
           xaxt='n', yaxt='n', ann=FALSE, 
           xlim=c(min(pca$loadings[,1])-0.01,max(pca$loadings[,1])+0.01),
           ylim=c(min(pca$loadings[,2])-0.01,max(pca$loadings[,2])+0.01))
      arrows(0,0,pca$loadings[,1],pca$loadings[,2],length=0.5, col="darkgrey")
      for (i in 1:nrow(pca$loadings)){
        text(pca$loadings[i,1],pca$loadings[i,2],labels=row.names(pca$loadings)[i], adj=arrow.lab.adj.list[[pca.i]][i,], cex=rcex)
      }
      par(fig=c(0,0.4,0.7,1), new = T , mar=c(0,0,0,0))
      plot.new()
      text(0,0.9,labels='Two-group hypothesis',cex=tcex*1.2,pos=4, font=3)
      text(0,0.6,labels=test.list[[pca.i]][1],cex=tcex,pos=4)
      text(0,0.5,labels=test.list[[pca.i]][2],cex=tcex,pos=4)
      text(0,0.2,labels=test.list[[pca.i]][3],cex=tcex,pos=4)
      par(fig=c(0,1,0,1))
      dev.off()
    } 
  }
  
  if("png" %in% otype){
    trz<-readPNG(paste("Fabric_distances/GB14 & trz14/",dtype,"_trz.png",sep=""))
    GB<-readPNG(paste("Fabric_distances/GB14 & trz14/",dtype,"_GB.png",sep=""))
    fn <- paste("Fabric_distances/GB14 & trz14/",dtype,".png",sep="")
    png(filename = fn, width = Lwidth, height = Lheight)
    par(mar=c(0,0,0,0),oma=c(0,0,0,0))
    plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes=F)
    rasterImage(trz,-.03,-.03,.5,1.03)
    rasterImage(GB,.5,-.03,1.03,1.03)
    dev.off()
  }
  if("tiff" %in% otype){
    trz<-readTIFF(paste("Fabric_distances/GB14 & trz14/",dtype,"_trz.tiff",sep=""))
    GB<-readTIFF(paste("Fabric_distances/GB14 & trz14/",dtype,"_GB.tiff",sep=""))
    fn <- paste("Fabric_distances/GB14 & trz14/",dtype,".tiff",sep="")
    tiff(filename = fn, width = Lwidth, height = Lheight)
    par(mar=c(0,0,0,0))
    plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes=F)
    rasterImage(trz,-.03,-.03,.5,1.03)
    rasterImage(GB,.5,-.03,1.03,1.03)
    dev.off()
  }
  if("jpeg" %in% otype){
    trz<-readJPEG(paste("Fabric_distances/GB14 & trz14/",dtype,"_trz.jpeg",sep=""))
    GB<-readJPEG(paste("Fabric_distances/GB14 & trz14/",dtype,"_GB.jpeg",sep=""))
    fn <- paste("Fabric_distances/GB14 & trz14/",dtype,".jpeg",sep="")
    jpeg(filename = fn, width = Lwidth, height = Lheight)
    par(mar=c(0,0,0,0))
    plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes=F)
    rasterImage(trz,-.03,-.03,.5,1.03)
    rasterImage(GB,.5,-.03,1.03,1.03)
    dev.off()
  }
  fn <- paste("Fabric_distances/GB14 & trz14/",dtype,".eps",sep="")
  loadfonts(device="postscript")
  postscript(fn , pointsize=10, width=Lwidth/100, height=Lheight/100,
             horizontal=FALSE, paper = "special", family="serif", colormodel="cmyk")
  par(mar=c(0,0,0,0))
  plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes=F)
  rasterImage(trz,-.03,-.03,.5,1.03)
  rasterImage(GB,.5,-.03,1.03,1.03)
  dev.off()
  graphics.off()
}