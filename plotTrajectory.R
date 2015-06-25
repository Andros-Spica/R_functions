plotTrajectory <- function(data, obsVar, timeVar, layout=.~parameter,
                           modelType='deterministic', runVar=NULL,
                           smooth.method='lm', parameterNames, y_axis_name, x_axis_name,
                           legend_name="", legend_labels=c(), legend_values=c(),
                           graphName="plotTrajectory", line.size=2, line.alpha=1,
                           strip.text.size=50, strip.text.vjust=0,
                           strip.text.angle=0, panel.margin.width=1.5,
                           legend.title.size=50, legend.title.align=0.5,
                           legend.text.size=50, axis.title.size=50,
                           axis.title.x.vjust=-2, axis.title.y.vjust=0.2,
                           axis.text.size=27, width=2200, height=1000){
  # "data" is the data frame with:
  #         a column with the names of the parameters explored (must be the first column),
  #         a column with the values of a variable (obsVar),
  #         a column with the time steps (timeVar) and
  #         n columns with the values of each parameter explored.
  # "layout" is the formula to be passed in facet_grid(). Only accepts ".~parameter" or "parameter~.".
  # "modelType" is the string defining the nature of the model explored, depending on which 
  #         one or more points are generated per parameter value ('deterministic', 'stochastic')
  # "smooth.method" is the method used in geom_smooth(), when modelType='stochastic'.
  # "parameterNames" is a list matching column names with the text to be shown as parameter names
  # "y_axis_name" and "x_axis_name" are the strings to be shown as axis names.
  # "legend_name" is string to be shown as legend name
  # "legend_labels" and "legend_values" arguments to be passed to scale_manual functions
  # "graphName" is the title of the graph, also used as file name
  # "line.size" and "line.alpha" are the size and alpha of the lines
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
    data[,v] <- as.numeric(paste(data[,v]))  
  }
  # standardize parameters
  for (v in 2:ncol(data)){
    if (v!=timeVar & v!=obsVar){
        data[,v] <- (data[,v] - min(data[,v])) / (max(data[,v])  - min(data[,v]))
    }
  }
  
  parameter_labeller <- function(parameter,value){
    return(parameterNames[value])
  }
  if (modelType=='deterministic'){
    # format data frame: parameter, timeVar, obsVar, variable, value
    data.long <- melt(data,id.vars=names(data)[c(1,timeVar,obsVar)])
    data.long[,1] <- factor(data.long[,1], levels = levels(data.long$variable))
    names(data.long) <- c("parameter", "timeVar", "obsVar", "variable", "value")
  }
  if (modelType=='stochastic'){
    # format data frame: parameter, runVar, timeVar, obsVar, variable, value
    data.long <- melt(data,id.vars=names(data)[c(1,runVar,timeVar,obsVar)])
    data.long[,1] <- factor(data.long[,1], levels = levels(data.long$variable))
    names(data.long) <- c("parameter", "runVar", "timeVar", "obsVar", "variable", "value")
  }  
  
  # build plot
  p1 <- ggplot(data.long)
  for (v in levels(data.long$parameter)){
    if (modelType=='deterministic'){
      if (layout==as.formula(".~parameter")){
        p1 <- p1 + geom_path(data=subset(data.long,variable==v & parameter==v), aes(obsVar, timeVar, linetype = factor(value)), alpha=line.alpha, size = line.size)
      } else {
        p1 <- p1 + geom_path(data=subset(data.long,variable==v & parameter==v), aes(timeVar, obsVar, linetype = factor(value)), alpha=line.alpha, size = line.size)
      }
    }
    if (modelType=='stochastic'){
      if (layout==as.formula(".~parameter")){
        for (i in levels(data.long$runVar)){
          p1 <- p1 + geom_path(data=subset(data.long,variable==v & parameter==v & runVar==i), aes(value, obsVar), alpha = point.alpha, color = "black", size = point.size)
        }
        #p1 <- p1 + geom_smooth(data=subset(data.long,variable==v & parameter==v), aes(value, obsVar), method=smooth.method)
      } else {
        for (i in levels(data.long$runVar)){
          p1 <- p1 + geom_path(data=subset(data.long,variable==v & parameter==v & runVar==i), aes(obsVar, value), alpha = point.alpha, color = "black", size = point.size)
        }
        #p1 <- p1 + geom_smooth(data=subset(data.long,variable==v & parameter==v), aes(obsVar, value), method=smooth.method)
      }
    }
  }
  p1 <- p1 + scale_linetype_manual(values = legend_values,
                                 labels=legend_labels,
                                 name=legend_name)
   
  p2 <- p1 + facet_grid(layout,scales="free_x", labeller=parameter_labeller) +
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
  
  filename=paste(graphName,"png", sep=".")
  png(filename = filename, width = width, height = height)
  print(p2)
  dev.off()
}