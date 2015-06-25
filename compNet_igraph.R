compNet_igraph <- function(data, layout.matrix, layout.net="fruchterman.reingold", niter=10000, marea=30, metadata.color=NULL, node.color=c(),metadata.size=NULL, graphName="graphOfNetwork", outType=c("none"), title=c("Title"), nodeSize=4, labelSize=3, titleSize=10, legend.pos_x=0, legend.pos_y=0, legend.keywidth=0.05, legend.keyheight=0.05, legend.fontSize=8, width=2000, height=2000,family='serif'){
  # "data" is a set of data frames of type "edge list"
  # "layout.matrix" is the vector (row,column) instructing how to display the networks
  # "net.layout" is the string calling the function to be used for arranging the networks
  # "metadata.color" is the data frame with the case id and the categorical variable to be used as vertex attribute related to color
  # "metadata.size" is the data frame with the case id and the categorical variable to be used as vertex attribute related to size
  # " metadata.group" is a list with vectors expressing the groups of vertex (polygons)
  # "graphName" is the title of the graph, also used as file name
  # "nodeSize" is the size of vertices (cex scale)
  # "labelSize" is the size of the vertices labels (cex scale)
  # "titleSize" is the size of the graph main title (cex scale)
  # "legend.pos_x","legend.pos_y" are the x,y position  of the top left corner of the legend box (plot dimensions: xlim(-1,1) and ylim(-1,1))
  # "legend.keywidth", "legend.keyheight" are the dimensions of the legend key (plot scale)
  # "legend.fontSize" is the font size of legend labels
  # "width", "height" is the dimension of the entire image
  require(igraph)
  
  # redefine the palette, so there are more colors available (black is never used for coloring nodes)
  palette(node.color)
  
  # create graph objects 
  networks <- list()
  for (i in 1:length(data)){
    networks[[i]] <- graph.data.frame(data[[i]], directed=F)
  }
  
  # add vertex attribute related to color
  coldata <- metadata.color
  if (is.null(metadata.color)==FALSE){
    # relate each value of metadata as a color of the palette
    coldata$color <- as.numeric(coldata[,2])
    for (i in 1:length(networks)){
      V(networks[[i]])$att.color = as.character(coldata$color[match(V(networks[[i]])$name,coldata[,1])])
      V(networks[[i]])$color <- V(networks[[i]])$att.color
    }
  }
  # set node defalts
  for (i in 1:length(networks)){
    V(networks[[i]])$size <- nodeSize
    V(networks[[i]])$label <- NA
    E(networks[[i]])$color <- "black"
  }
  # network layouts
  list.layouts <- list(1:length(networks))
  for (i in 1:length(networks)){
    if (layout.net=="fruchterman.reingold"){
      list.layouts[[i]]<-layout.fruchterman.reingold(networks[[i]], niter=niter, area=marea*vcount(networks[[i]])^2)
    }
    if (layout.net=="reingold.tilford"){
      list.layouts[[i]]<-layout.reingold.tilford(networks[[i]], circular=F)
    }
    if (layout.net=="circular.reingold.tilford"){
      list.layouts[[i]]<-layout.reingold.tilford(networks[[i]], circular=T)
    }
    if (layout.net=="kamada.kawai"){
      list.layouts[[i]]<-layout.kamada.kawai(networks[[i]])
    }
    if (layout.net=="lgl"){
      list.layouts[[i]]<-layout.lgl(networks[[i]])
    }
    if (layout.net=="random"){
      list.layouts[[i]]<-layout.random(networks[[i]])
    }
  }
  
  # PLOT graphs
  for (out in outType){
    if (out=="png" | out=="PNG"){
      filename=paste(graphName,"png", sep=".")
      png(filename = filename, width = width, height = height, units='px', type="cairo")
    } else if (out=="tiff" | out=="TIFF"){
      filename=paste(graphName,"tiff", sep=".")
      tiff(filename = filename, width = width, height = height, units='px', type="cairo")
    } else if (out=="bmp" | out=="BMP"){
      filename=paste(graphName,"tiff", sep=".")
      bmp(filename = filename, width = width, height = height, units='px', type="cairo")
    } else if (out=="jpg" | out=="JPG"| out=="jpeg" | out=="JPEG"){
      filename=paste(graphName,"tiff", sep=".")
      jpeg(filename = filename, width = width, height = height, units='px', type="cairo")
    } else if (out=="eps" | out=="EPS"){
      filename=paste(graphName,"eps", sep=".")
      require(extrafont)
      require(extrafontdb)
      require(grDevices)
      loadfonts(device="postscript")
      cairo_ps(filename, width=(width/100), height=(height/100), 
               family=family) 
    } else {
    print("ERROR: outputs may be generated with the following formats: png, tiff, bmp, jpg, eps")
    }
    
    par(mar=c(0,0,0,0),oma=c(2,2,2,2), cex.main=titleSize)
    layout(layout.matrix)
    for (i in 1:length(networks)){
      plot(networks[[i]], layout=list.layouts[[i]])
      text(x=1,y=1,labels=title[i],cex=titleSize)
    }
    plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0, 10), ylim=c(0,10))
    if (is.null(coldata)==FALSE){
      pos_x <- legend.pos_x
      pos_y <- legend.pos_y   
      colorfactors <- levels(factor(coldata[,3]))
      colornames <- levels(factor(coldata[,2]))
      for (i in 1:length(colorfactors)){
        xleft <- pos_x
        ytop <- pos_y - legend.keyheight*(i - 1)*1.5
        ybottom <- ytop - legend.keyheight
        xright <- xleft + legend.keywidth
        rect(xleft=xleft, ybottom=ybottom, xright=xright, ytop=ytop, col=colorfactors[i])
        text_pos_x <- xright + (legend.keywidth / 2)
        text_pos_y <- ytop - (legend.keyheight / 2)
        text(x=text_pos_x, y=text_pos_y, paste("", as.character(colornames)[i], sep=" "), cex=legend.fontSize, adj=0)
      }   
    }
    dev.off()
  } 
  palette("default")
  graphics.off()
}