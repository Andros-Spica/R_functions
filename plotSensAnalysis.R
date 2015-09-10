plotSensAnalysis <- function(data, obsVar, legVar=NULL, layout=.~parameter, modelType='deterministic', smooth.method='lm', se=T, annData=NULL, annSize=10, annColor="black", parameterNames, y_axis_name, x_axis_name, y_axis_breaks=NULL, x_axis_breaks=NULL, show_legend=TRUE, legend_name="", legend_labels=c(), legend_values=c(),  legend_type="color", graphName="plotSensAnalysis", outType="NONE", point.size=5, point.alpha=0.5, line.size=2, strip.text.size=50, strip.text.vjust=0, strip.text.angle=0, panel.margin.width=1.5, legend.title.size=50, legend.title.align=0.5, legend.text.size=50, axis.title.size=50, axis.title.x.vjust=-2, axis.title.y.vjust=0.2, axis.text.size=27, width=2200, height=1000, family='serif'){
  # "data" is the data frame with:
  #         a column with the names of the parameters explored (must be the first column),
  #         a column with the values of a variable (obsVar),
  #         optionally, a column with the values of a variable to be shown in legend (legVar) and
  #         n columns with the values of each parameter explored.
  # "layout" is the formula to be passed in facet_grid(). Only accepts ".~parameter" or "parameter~.".
  # "modelType" is the string defining the nature of the model explored, depending on which 
  #         one or more points are generated per parameter value ('deterministic', 'stochastic')
  # "smooth.method" is the method used in geom_smooth(), when modelType='stochastic'.
  # "annData" refers to a data frame giving the positions and labels of the annotations (x,y,labs)
  # "parameterNames" is a list matching column names with the text to be shown as parameter names
  # "y_axis_name" and "x_axis_name" are the strings to be shown as axis names.
  # "y_axis_breaks" and "x_axis_breaks"  is the vector expressing the breaks of the axes (min, max, step)
  # "legend_name" is string to be shown as legend name (if zVar!=NULL)
  # "legend_labels" and "legend_values" arguments to be passed to scale_manual functions
  # "graphName" is the title of the graph, also used as file name
  # "point.size", "point.alpha" and "line.size" are the size and alpha of the geom represented (line, when modelType='deterministic')
  # "strip.text.size" and "strip.text.vjust" are the properties of strip text
  # "panel.margin.width" is the margin width (around the plot)
  # "legend.title.size", "legend.title.align" and "legend.text.size"
  #         are the properties of the legend title and labels
  # "axis.title.size", "axis.title.x.vjust", "axis.title.y.vjust" and "axis.text.size"
  #         are the properties of axis title and labels
  # "width", "height" are the dimensions of the entire image
  require(scales)
  require(grid)
  require(ggplot2)
  require(reshape2)
  
  # variables as numeric
  for (v in 2:ncol(data)){
    if (is.null(legVar)){
      data[,v] <- as.numeric(paste(data[,v]))      
    } else {
      if (v!=legVar){
        data[,v] <- as.numeric(paste(data[,v]))
      }
    }   
  }
  # bind annLabs, x, y
  if (is.null(annData)==FALSE){
    data <- cbind(data,annData)
    data <- data.frame(data)
  } else {
    x <- rep(0,nrow(data))
    y <- rep(0,nrow(data))
    annLabs <- rep('',nrow(data))
    data <- cbind(data,annLabs,x,y)
    data <- data.frame(data)
  }
  annLabVar <- which(names(data)=="annLabs")
  annXVar <- annLabVar + 1
  annYVar <- annLabVar + 2
  
  # labeller function
  parameter_labeller <- function(parameter,value){
    if (parameter=="parameter"){
      return(parameterNames[value])
    } else {
      return(legend_labels[value])
    }
    
  }
  
  # create plots with or without grouping
  if (is.null(legVar)){
    # format data frame: parameter, obsVar, variable, value
    data.long <- melt(data,id.vars=names(data)[c(1,obsVar,annLabVar,annXVar,annYVar)])
    data.long[,1] <- factor(data.long[,1], levels = levels(data.long$variable))
    names(data.long) <- c("parameter", "obsVar","annLabs", "x", "y", "variable", "value")
    # build plot
    p1 <- ggplot(data.long)
    for (v in levels(data.long$parameter)){
      if (modelType=='deterministic'){
        if (layout==as.formula(".~parameter")){
          p1 <- p1 + geom_path(data=subset(data.long,variable==v & parameter==v), aes(value, obsVar), alpha = 1, color = "black", size = line.size)
        } else {
          p1 <- p1 + geom_path(data=subset(data.long,variable==v & parameter==v), aes(obsVar, value), alpha = 1, color = "black", size = line.size)
        }
      }
      if (modelType=='stochastic'){
        if (layout==as.formula(".~parameter")){
          p1 <- p1 + geom_point(data=subset(data.long,variable==v & parameter==v), aes(value, obsVar), alpha = point.alpha, color = "black", size = point.size)  +
                geom_smooth(data=subset(data.long,variable==v & parameter==v), aes(value, obsVar), method=smooth.method)
        } else {
          p1 <- p1 + geom_point(data=subset(data.long,variable==v & parameter==v), aes(obsVar, value), alpha = point.alpha, color = "black", size = point.size)  +
            geom_smooth(data=subset(data.long,variable==v & parameter==v), aes(obsVar, value), method=smooth.method)
        }
      }
      # annotation
      p1 <- p1 + geom_text(aes(x,y,label=annLabs),
                           data=subset(data.long,variable==v & parameter==v),
                           size=annSize, color=annColor)
    }
  } else {
    # format data frame: parameter, obsVar, legVar, variable, value
    data.long <- melt(data,id.vars=names(data)[c(1,obsVar,legVar,annLabVar,annXVar,annYVar)])
    data.long[,1] <- factor(data.long[,1], levels = levels(data.long$variable))
    names(data.long) <- c("parameter", "obsVar", "legVar", "annLabs", "x", "y", "variable", "value")
    # build plot
    p1 <- ggplot(data.long)
    for (v in levels(data.long$parameter)){
      if (modelType=='deterministic'){
        if (layout==as.formula(".~parameter")){
          p1 <- p1 + geom_path(data=subset(data.long,variable==v & parameter==v), aes(value, obsVar, linetype=legVar), alpha = 1, size = line.size, show_guide=show_legend)
        } else {
          p1 <- p1 + geom_path(data=subset(data.long,variable==v & parameter==v), aes(obsVar, value, linetype=legVar), alpha = 1, size = line.size, show_guide=show_legend)
        }
        p1 <- p1 + scale_linetype_manual(values = legend_values,
                              labels=legend_labels,
                              name=legend_name)
      }
      if (modelType=='stochastic'){
        if (layout==as.formula(".~parameter") | layout==as.formula("legVar~parameter")){
          if(legend_type=="color"){
            p1 <- p1 + geom_point(data=subset(data.long,variable==v & parameter==v), aes(value, obsVar, color=legVar), alpha = point.alpha, size = point.size, show_guide=show_legend) +
              geom_smooth(data=subset(data.long,variable==v & parameter==v), aes(value, obsVar, color=legVar, fill=legVar), se=se, size=line.size, method=smooth.method, show_guide=FALSE)
          } else if(legend_type=="B&W"){
            p1 <- p1 + geom_point(data=subset(data.long,variable==v & parameter==v), aes(value, obsVar), alpha = point.alpha, size = point.size, show_guide=show_legend) +
              geom_smooth(data=subset(data.long,variable==v & parameter==v), aes(value, obsVar), color='black', se=se, size=line.size, method=smooth.method, show_guide=FALSE)
          }
        } else {
          if(legend_type=="color"){
            p1 <- p1 + geom_point(data=subset(data.long,variable==v & parameter==v), aes(obsVar, value, color=legVar), alpha = point.alpha, size = point.size, show_guide=show_legend) +
              geom_smooth(data=subset(data.long,variable==v & parameter==v), aes(obsVar, value, color=legVar, fill=legVar), se=se, size=line.size, method=smooth.method, show_guide=FALSE)
          } else if(legend_type=="B&W"){
            p1 <- p1 + geom_point(data=subset(data.long,variable==v & parameter==v), aes(obsVar, value), alpha = point.alpha, size = point.size, show_guide=show_legend) +
              geom_smooth(data=subset(data.long,variable==v & parameter==v), aes(obsVar, value), color='black', se=se, size=line.size, method=smooth.method, show_guide=FALSE)
          }
        }
        
        # legend
        if(length(legend_values)!=0){
          if (legend_type=="color") {
            p1 <- p1 + scale_color_manual(values = legend_values,
                                         labels=legend_labels,
                                         name=legend_name) +
                     scale_fill_manual(values = legend_values)
          }
        } else {
          if (length(legend_labels)!=0){
            if (legend_type=="color") {
              p1 <- p1 + scale_color_discrete(labels=legend_labels,
                                          name=legend_name)
            }
          } else {
            if (legend_type=="color") {
              p1 <- p1 + scale_color_discrete(name=legend_name)
            }
          }
        }
        p1 <- p1 + geom_text(aes(x,y,label=annLabs),
                             data=subset(data.long,variable==v & parameter==v),
                             size=annSize, color=annColor)
      }
    }   
  }
  p2 <- p1 + facet_grid(layout,scales="free",labeller=parameter_labeller) +
    labs(x = x_axis_name, y = y_axis_name) +
    theme_bw() +
    theme(strip.text.x = element_text(size=strip.text.size, face="italic", family="serif", vjust=strip.text.vjust, angle=strip.text.angle),
          strip.text.y = element_text(size=strip.text.size*1.5, face="italic", family="serif",angle=0),
          panel.margin = unit(panel.margin.width,"cm"),
          plot.margin = unit(c(1, 1, 2, 2), "lines"),
          legend.title = element_text(size=legend.title.size, face="italic", family="serif"),
          legend.title.align=legend.title.align,
          legend.text = element_text(size=legend.text.size, family="serif"),
          legend.key = element_blank(),
          legend.key.height = unit(2.4,"cm"),
          legend.key.width = unit(2.4,"cm"),
          axis.title.x = element_text(size = axis.title.size, family="serif", face="italic", vjust=axis.title.x.vjust),
          axis.title.y = element_text(size = axis.title.size, family="serif", face="italic", vjust=axis.title.y.vjust),
          axis.text.x = element_text(size = axis.text.size, family="serif"),
          axis.text.y = element_text(size = axis.text.size, family="serif"))
  if (is.null(x_axis_breaks)==FALSE){
    p2 <- p2 + 
      coord_cartesian(xlim = c((x_axis_breaks[1]-5),(x_axis_breaks[2]+5))) +
      scale_x_continuous(breaks=seq(x_axis_breaks[1], x_axis_breaks[2], x_axis_breaks[3]))
  }
  if (is.null(y_axis_breaks)==FALSE){
    p2 <- p2 + 
      coord_cartesian(ylim = c((y_axis_breaks[1]-5),(y_axis_breaks[2]+5))) +
      scale_y_continuous(breaks=seq(y_axis_breaks[1], y_axis_breaks[2], y_axis_breaks[3]))
  }
  # annotate
  if (is.null(annData)==FALSE){
    for (v in levels(annData$parameter)){
      if (is.null(legVar)){
        p2 <- p2 + geom_text(aes(x,y,label=labs,group=NULL),
                             data=subset(annData,parameter==v),
                             size=annSize, color=annColor)
      } else {
        for (vv in levels(annData$sce)){
          p2 <- p2 + geom_text(aes(x,y,label=labs,group=NULL),
                               data=subset(annData,parameter==v & sce==vv),
                               size=annSize, color=annColor)
        }
      }      
    }
  }
  if (outType=="none" | outType=="NONE"){
    # image is printed on R device
    return(p2)
    readkey()
  } else if (outType=="png" | outType=="PNG"){
    filename=paste(graphName,"png", sep=".")
    png(filename = filename, width = width, height = height)
  } else if (outType=="tiff" | outType=="TIFF"){
    filename=paste(graphName,"tiff", sep=".")
    tiff(filename = filename, width = width, height = height)
  } else if (outType=="bmp" | outType=="BMP"){
    filename=paste(graphName,"bmp", sep=".")
    bmp(filename = filename, width = width, height = height)
  } else if (outType=="jpeg" | outType=="JPEG" | outType=="jpg" | outType=="JPG"){
    filename=paste(graphName,"jpg", sep=".")
    jpeg(filename = filename, width = width, height = height)
  } else if (outType=="eps" | outType=="rps"){
    filename=paste(graphName,"eps", sep=".")
    require(extrafont)
    require(extrafontdb)
    require(grDevices)
    loadfonts(device="postscript")
    cairo_ps(filename , width=(width/100), height=(height/100) , family=family)
    #postscript(filename , pointsize=12, width=(width/100), height=(height/100) , 
    #           horizontal=FALSE, paper = "special", family=family)
  } else {
    print("ERROR: outputs may be generated with the following formats: png, tiff, bmp, jpg, eps, rps, or else")
  }
  print(p2)
  dev.off()
}