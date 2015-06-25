plotAttractors <- function(data, obsVar, legVar=NULL, layout=.~variable, plot.density=F, plot.model=F, modelType='deterministic', smooth.method='lm', variableNames, y_axis_name, x_axis_name, y_axis_breaks=NULL, x_axis_breaks=NULL, legend_name="", legend_labels=c(), legend_values=c(),  legend_type="color", graphName="plotAttractors", outType="none", point.size=5, point.alpha=0.5, strip.text.size=50, strip.text.vjust=0, strip.text.angle=0, panel.margin.width=1.5, legend.title.size=50, legend.title.align=0.5, legend.text.size=50, axis.title.size=50, axis.title.x.vjust=-2, axis.title.y.vjust=0.2, axis.text.size=27, width=2200, height=1000, family='serif'){
  # "data" is the data frame with:
  #         a column with the values of the variable to be represented in the y axis (obsVar),
  #         optionally, a column with the values of a variable to be shown in legend (legVar) and
  #         n columns with the values of other variables.
  # "layout" is the formula to be passed in facet_grid(). Only accepts ".~variable" or "variable~.".
  # "minValues" and "maxValues" are vectors with the min and max limits of each variable to be represented in the x axis
  # "modelType" is the string defining the nature of the model explored, depending on which 
  #         one or more points are generated per parameter value ('deterministic', 'stochastic')
  # "smooth.method" is the method used in geom_smooth(), when modelType='stochastic'.
  # "variableNames" is a list matching column names with the text to be shown as parameter names
  # "y_axis_name" and "x_axis_name" are the strings to be shown as axis names.
  # "legend_name" is string to be shown as legend name (if zVar!=NULL)
  # "legend_labels" and "legend_values" arguments to be passed to scale_manual functions
  # "graphName" is the title of the graph, also used as file name
  # "point.size" and "point.alpha" are the size and alpha of points
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
  for (v in 1:ncol(data)){
    if (is.null(legVar)){
      data[,v] <- as.numeric(paste(data[,v]))      
    } else {
      if (v!=legVar){
        data[,v] <- as.numeric(paste(data[,v]))
      }
    }   
  } 
  
  variable_labeller <- function(variable,value){
    return(variableNames[value])
  }
  
  # create plots with or without grouping
  if (is.null(legVar)){
    # format data frame: parameter, obsVar, variable, value
    data.long <- melt(data,id.vars=names(data)[obsVar])
    names(data.long) <- c("obsVar", "variable", "value")
    # build plot
    p1 <- ggplot(data.long)
    for (v in levels(data.long$variable)){
      if (modelType=='deterministic'){
        if (layout==as.formula(".~variable")){
          p1 <- p1 + geom_point(data=subset(data.long,variable==v), aes(value, obsVar), alpha = 1, color = "black", size = point.size)
        } else {
          p1 <- p1 + geom_point(data=subset(data.long,variable==v), aes(obsVar, value), alpha = 1, color = "black", size = point.size)
        }
      }
      if (modelType=='stochastic'){
        if (layout==as.formula(".~variable")){
          p1 <- p1 + geom_point(data=subset(data.long,variable==v), aes(value, obsVar), alpha = point.alpha, color = "black", size = point.size)
          if (plot.density==T){
            p1 <- p1 + geom_density2d(data=subset(data.long,variable==v), aes(value, obsVar), color = "grey")
          }
          if (plot.model==T){
            p1 <- p1 + geom_smooth(data=subset(data.long,variable==v), aes(value, obsVar), method=smooth.method)
          }
        } else {
          p1 <- p1 + geom_point(data=subset(data.long,variable==v), aes(obsVar, value), alpha = point.alpha, color = "black", size = point.size)
          if (plot.density==T){
            p1 <- p1 + geom_density2d(data=subset(data.long,variable==v), aes(obsVar, value), color = "grey")
          }
          if (plot.model==T){
            p1 <- p1 + geom_smooth(data=subset(data.long,variable==v), aes(obsVar, value), method=smooth.method)
          }
        }
      }
    }
  } else {
    # format data frame: parameter, obsVar, legVar, variable, value
    data.long <- melt(data,id.vars=names(data)[c(obsVar,legVar)])
    names(data.long) <- c("obsVar", "legVar", "variable", "value")
    # build plot
    p1 <- ggplot(data.long)
    for (v in levels(data.long$variable)){
      if (modelType=='deterministic'){
        if (layout==as.formula(".~variable")){
          if (legend_type=="color") {
            p1 <- p1 + geom_point(data=subset(data.long,variable==v), aes(value, obsVar, color=legVar), alpha = 1, size = point.size)
          }
          if (legend_type=="B&W") {
            p1 <- p1 + geom_point(data=subset(data.long,variable==v), aes(value, obsVar, shape=legVar), color='black', alpha = 1, size = point.size, show_guide=F)
          }
        } else {
          if (legend_type=="color") {
            p1 <- p1 + geom_point(data=subset(data.long,variable==v), aes(obsVar, value, color=legVar), alpha = 1, size = point.size)
          }
          if (legend_type=="B&W") {
            p1 <- p1 + geom_point(data=subset(data.long,variable==v), aes(obsVar, value, shape=legVar), color='black', alpha = 1, size = point.size, show_guide=F)
          }
        }
        if(length(legend_values)!=0){
          if (legend_type=="color") {
            p1 <- p1 + scale_color_manual(values = legend_values,
                                        labels=legend_labels,
                                        name=legend_name)
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
      }
      if (modelType=='stochastic'){
        if (layout==as.formula(".~variable")){
          if (legend_type=="color") {
            p1 <- p1 + geom_point(data=subset(data.long,variable==v), aes(value, obsVar, color=legVar), alpha = point.alpha, size = point.size)
            if (plot.density==T){
              p1 <- p1 + geom_density2d(data=subset(data.long,variable==v), aes(value, obsVar, color=legVar), show_guide=F)
            }
          }
          if (legend_type=="B&W") {
            p1 <- p1 + geom_point(data=subset(data.long,variable==v), aes(value, obsVar, shape=legVar), color='black', alpha = point.alpha, size = point.size, show_guide=F)
            if (plot.density==T){
              p1 <- p1 + geom_density2d(data=subset(data.long,variable==v), aes(value, obsVar, shape=legVar), color='black', show_guide=F)
            }
          }
          if (plot.model==T){
            p1 <- p1 + geom_smooth(data=subset(data.long,variable==v), aes(value, obsVar), color='black', method=smooth.method)
          }          
        } else {
          if (legend_type=="color") {
            p1 <- p1 + geom_point(data=subset(data.long,variable==v), aes(obsVar, value, color=legVar), alpha = point.alpha, size = point.size)
            if (plot.density==T){
              p1 <- p1 + geom_density2d(data=subset(data.long,variable==v), aes(obsVar,value, color=legVar), show_guide=F)
            }
            if (plot.model==T){
              p1 <- p1 + geom_smooth(data=subset(data.long,variable==v), aes(obsVar, value), color='black', method=smooth.method)

            }
          }
          if (legend_type=="B&W") {
            p1 <- p1 + geom_point(data=subset(data.long,variable==v), aes(obsVar, value, shape=legVar), color='black', alpha = point.alpha, size = point.size, show_guide=F)
            if (plot.density==T){
              p1 <- p1 + geom_density2d(data=subset(data.long,variable==v), aes(obsVar,value, shape=legVar), color='black', show_guide=F)
            }
            if (plot.model==T){
              p1 <- p1 + geom_smooth(data=subset(data.long,variable==v), aes(obsVar, value), color='black', method=smooth.method)
              
            }
          }
        }
        if(length(legend_values)!=0){
          if (legend_type=="color") {
            p1 <- p1 + scale_color_manual(values = legend_values,
                                          labels=legend_labels,
                                          name=legend_name)
          }
          if (legend_type=="B&W") {
            p1 <- p1 + scale_shape_manual(values = legend_values)
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
      }
    }   
  } 
  p2 <- p1 + facet_grid(layout,scales="free_x", labeller=variable_labeller) +
    labs(x = x_axis_name, y = y_axis_name) +
    theme_bw() +
    theme(strip.text = element_text(size=strip.text.size, face="italic", family="serif", vjust=strip.text.vjust, angle=strip.text.angle),
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
    cairo_ps(filename, width=(width/100), height=(height/100), 
             family=family) 
    #postscript(filename , pointsize=12, width=(width/100), height=(height/100) , 
    #           horizontal=FALSE, paper = "special", family=family)
  } else {
    print("ERROR: outputs may be generated with the following formats: png, tiff, bmp, jpg, eps, rps, or else")
  }
  print(p2)
  dev.off()
}