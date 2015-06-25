plot_igraph <- function(net, metadata.color=NULL, metadata.label=NULL, metadata.polygon=NULL, graphName="graphOfDistances", layout, nodeSize=4, edgeSize=2, edgeColor="black", labelSize=3, titleSize=10, legcol.pos_x=0, legcol.pos_y=0, leglab.pos_x=0, leglab.pos_y=0, legend.width=60, legend.keywidth=0.05, legend.keyheight=0.05, legend.keymargin=1.5, legend.fontSize=8, width=2000, height=2000){
  # "net" is a graph object (edges must be already filtered)
  # "metadata.color" is the data frame with the case id and the categorical variable to be used as vertex attribute related to color
  # "metadata.polygon" is the data frame with the case id and the categorical variable to be used to draw translucidpolygons
  # "polygons.colors" is a vector with the colors of the translucid polygons
  # "graphName" is the title of the graph, also used as file name
  # "nodeSize" is the size of vertices (cex scale)
  # "labelSize" is the size of the vertices labels (cex scale)
  # "titleSize" is the size of the graph main title (cex scale)
  # "legend.pos_x","legend.pos_y" are the x,y position  of the top left corner of the legend box (plot dimensions: xlim(-1,1) and ylim(-1,1))
  # "legend.width" is the size of the right margin of the plot (in par("mar"))
  # "legend.keywidth", "legend.keyheight" are the dimensions of the legend key (plot scale)
  # "legend.fontSize" is the font size of legend labels
  # "width", "height" is the dimension of the entire image
  require(igraph)
  
  # redefine the palette, so there are more colors available (black is never used for coloring nodes)
  palette(c(rainbow(50),"black"))
  
  # add vertex attribute related to color and size
  coldata <- metadata.color
  if (is.null(metadata.color)==FALSE){
    if (exists("coldata[,3]")==TRUE){
      # get specification on color
      coldata$color <- coldata[,3]
    } else {
      # relate each value of metadata as a color of the palette (between 0 and 50)
      coldata$color <- as.numeric(coldata[,2])
      coldata$color <- ((coldata$color - min(coldata$color)) / (max(coldata$color)  - min(coldata$color))) * 50 
    }
    V(net)$att.color = as.character(coldata$color[match(V(net)$name,coldata[,1])])
    V(net)$color <- V(net)$att.color 
  }
  labdata <- metadata.label
  if (is.null(metadata.label)==FALSE){
    # relate each value of metadata as a color of the palette (between 0 and 50)
    labdata$label <- as.numeric(labdata[,2])
    V(net)$label <- as.character(labdata$label[match(V(net)$name,labdata[,1])])
  }
  # set node defalts
  V(net)$label.color <- 51
  V(net)$label.cex <- labelSize
  V(net)$size <- nodeSize
  E(net)$color <- edgeColor
  E(net)$size <- edgeSize
  
  if (is.null(metadata.polygon)==FALSE){
    listPolygons <- list()
    numPolygons <- length(levels(metadata.polygon[,2]))
    poldata <- metadata.polygon
    row.names(poldata) <- seq(length=nrow(poldata))
    for (i in 1:numPolygons){
      group <- subset(poldata, poldata[,2]==levels(poldata[,2])[i])
      polygon <- vector()
      for (v in 1:nrow(group)){
        thisVertex <- as.numeric(row.names(group)[v])
        polygon <- c(polygon, thisVertex)
      }
      listPolygons[[i]] <- polygon
    }
    
    polygons.colors <- 1:length(levels(metadata.polygon[,2]))
    polygons.colors <- ((polygons.colors - min(polygons.colors)) / (max(polygons.colors)  - min(polygons.colors))) * 360
    polygons.colors <- hcl(polygons.colors, alpha=0.5)
    
    # PLOT the graph
    filename=paste(graphName,"png", sep=".")
    png(filename, width=width, height=height, units='px', type="cairo")
    par(mar=c(1,1,10,legend.width), cex.main=titleSize)
    plot(net,main=graphName, layout=layout,
         mark.groups= listPolygons, # draws polygon around nodes
         #mark.shape=2,
         mark.col=polygons.colors,
         mark.border=rep("black",length(polygons.colors)),
         mark.expand=10
    )
  } else {
    # PLOT the graph
    filename=paste(graphName,"png", sep=".")
    png(filename, width=width, height=height, units='px', type="cairo")
    par(mar=c(1,1,10,legend.width), cex.main=titleSize)
    plot(net,main=graphName, layout=layout)
  }

  if (is.null(coldata)==FALSE){
    pos_x <- legcol.pos_x
    pos_y <- legcol.pos_y
    colorfactors <- levels(factor(coldata$color))
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
  if (is.null(metadata.label)==FALSE){
    pos_x <- leglab.pos_x
    pos_y <- leglab.pos_y   
    maxwidth <- legend.width - 2*legend.fontSize
    labelfactors <- levels(factor(labdata[,3]))
    labelnames <- levels(factor(labdata[,2]))
    for (i in 1:length(labelfactors)){
      if(nchar(labelnames[i])>maxwidth){
        labelnames[i] <- str_wrap(labelnames[i], width = maxwidth)
      }
    }
    lineindex <- 0
    for (i in 1:length(labelfactors)){
      xlab <- pos_x
      ylab <- pos_y - legend.keyheight*lineindex*legend.keymargin
      xtext <- xlab + legend.keywidth*1.5
      ytext <- ylab - 0.004
      text(x=xlab, y=ylab, as.character(as.numeric(labelfactors[i])), cex=legend.fontSize, adj=c(0.5,0.5))
      text(x=xtext, y=ytext, as.character(labelnames)[i], cex=legend.fontSize, adj=c(0,0.5))
      lineindex <- lineindex + 1
      if(i != 1){
        if(nchar(labelnames[i])>maxwidth){
          lineindex <- lineindex + 0.5
        }
        ii = i + 1
        if(nchar(labelnames[ii])>maxwidth){
          lineindex <- lineindex + 0.5
        }
      }
    }
  }
  dev.off()
  palette("default")
}