list_igraph <- function(data){
  # "data" is a set of data frames of type "edge list"
  
  # create graph objects 
  list.networks <- list()
  for (i in 1:length(data)){
    list.networks[[i]] <- graph.data.frame(data[[i]], directed=F)
  }
  return(list.networks)
}

layoutlist_igraph <- function(list.networks,layout.net="fruchterman.reingold", niter=10000, marea=30){
  # "net.layout" is the string calling the function to be used for arranging the networks
  # network layouts
  list.layouts <- list(1:length(list.networks))
  for (i in 1:length(list.networks)){
    if (layout.net=="fruchterman.reingold"){
      list.layouts[[i]]<-layout.fruchterman.reingold(list.networks[[i]], niter=niter, area=marea*vcount(list.networks[[i]])^2)
    }
    if (layout.net=="reingold.tilford"){
      list.layouts[[i]]<-layout.reingold.tilford(list.networks[[i]], circular=F)
    }
    if (layout.net=="circular.reingold.tilford"){
      list.layouts[[i]]<-layout.reingold.tilford(list.networks[[i]], circular=T)
    }
    if (layout.net=="kamada.kawai"){
      list.layouts[[i]]<-layout.kamada.kawai(list.networks[[i]])
    }
    if (layout.net=="lgl"){
      list.layouts[[i]]<-layout.lgl(list.networks[[i]])
    }
    if (layout.net=="random"){
      list.layouts[[i]]<-layout.random(list.networks[[i]])
    }
  }
  return(list.layouts)
}

plotlist_igraph <- function(list.networks, list.layouts,
			metadata.color=NULL, metadata.size=NULL,
			graphName="graphOfNetwork", outType=c("none"), title=c("Title"),
			mlabels.cols=rep("",ncol(layout.matrix)-1), mlabels.rows=rep("",nrow(layout.matrix)-1),
			layout.matrix=layout.matrix, lwidths=rep(1,ncol(layout.matrix)), lheights=rep(1,nrow(layout.matrix)),
			nodeSize=4, labelSize=3, titleSize=10, node.color=c(),
			legend.distr="vertical", legend.pos_x=0, legend.pos_y=0,
			legend.keywidth=0.05, legend.keyheight=0.05, legend.fontSize=8,
			width=2000, height=2000,family='serif'){

  # "layout.matrix" is the output of layout() instructing how to display the networks, col and row annotations and legend (in this order))
  # "metadata.color" is the data frame with the case id and the categorical variable to be used as vertex attribute related to color
  # "metadata.size" is the data frame with the case id and the categorical variable to be used as vertex attribute related to size
  # "metadata.group" is a list with vectors expressing the groups of vertex (polygons) *** not implemented
  # "graphName" is the title of the graph, also used as file name
  # "mlabels.cols" and "mlabels.rows" are vectors indicating col and row titles. The respective string should be placed following the order of the list of networks (not annotated="")
  # "nodeSize" is the size of vertices (cex scale)
  # "labelSize" is the size of the vertices labels (cex scale)
  # "titleSize" is the size of the graph main title (cex scale)
  # "node.color" is the vector indicating the colors to be used for nodes (it could also be "rainbow", etc)
  # "legend.distr" specify the distribution of the legend bullets ("vertical" or "horizontal")
  # "legend.pos_x","legend.pos_y" are the x,y position  of the top left corner of the legend box (plot dimensions: xlim(-1,1) and ylim(-1,1))
  # "legend.keywidth", "legend.keyheight" are the dimensions of the legend key (plot scale)
  # "legend.fontSize" is the font size of legend labels
  # "width", "height" is the dimension of the entire image
  require(igraph)
  
  # redefine the palette, so there are more colors available (black is never used for coloring nodes)
  palette(node.color)

  # add vertex attribute related to color
  coldata <- metadata.color
  if (is.null(metadata.color)==FALSE){
    # relate each value of metadata as a color of the palette
    coldata$color <- as.numeric(coldata[,2])
    for (i in 1:length(list.networks)){
      V(list.networks[[i]])$att.color = as.character(coldata$color[match(V(list.networks[[i]])$name,coldata[,1])])
      V(list.networks[[i]])$color <- V(list.networks[[i]])$att.color
    }
  }

  # set node defalts
  for (i in 1:length(list.networks)){
    V(list.networks[[i]])$size <- nodeSize
    V(list.networks[[i]])$label <- NA
    E(list.networks[[i]])$color <- "black"
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
    
    layout(layout.matrix,widths=lwidths,heights=lheights, TRUE)

    par(mar=c(0,0,0,0),oma=c(0,0,0,0),family=family,font=4)
    for (i in 1:length(list.networks)){
	plot(list.networks[[i]], layout=list.layouts[[i]])
	text(x=1,y=1,labels=title[i],cex=titleSize)
    }
    if (is.null(mlabels.cols)==FALSE){
	for (i in 1:length(mlabels.cols)){
		plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0, 10), ylim=c(0,10))
		#pos_x <- 1.5 + (10 / (length(mlabels.cols)+1)) * (i - 1)
		text(x=5, y=5, mlabels.cols[i], cex=titleSize, adj=c(.5,.5))
	}
    }
    if (is.null(mlabels.rows)==FALSE){
	for (i in 1:length(mlabels.rows)){
		plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0, 10), ylim=c(0,10))
		#pos_y <- 9 - (10 / length(mlabels.rows)) * (i -1)
		text(x=5, y=5, mlabels.rows[i], cex=titleSize, adj=c(.5,.5))
	}
    }
    plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0, 10), ylim=c(0,10))
    if (is.null(coldata)==FALSE){
      pos_x <- legend.pos_x
      pos_y <- legend.pos_y   
      colorfactors <- levels(factor(coldata[,3]))
      colornames <- levels(factor(coldata[,2]))
	if (legend.distr == "vertical") {
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
	} else {
        for (i in 1:length(colorfactors)){
          ytop <- pos_y
	    xleft <- pos_x + legend.keywidth*(i - 1)*5
          ybottom <- ytop - legend.keyheight
          xright <- xleft + legend.keywidth
          rect(xleft=xleft, ybottom=ybottom, xright=xright, ytop=ytop, col=colorfactors[i])
          text_pos_x <- xright + (legend.keywidth / 2)
          text_pos_y <- ytop - (legend.keyheight / 2)
          text(x=text_pos_x, y=text_pos_y, paste("", as.character(colornames)[i], sep=" "), cex=legend.fontSize, adj=0)
        } 
	} 
    }
    dev.off()
  } 
  palette("default")
  graphics.off()
}