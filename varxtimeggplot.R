varxtimeggplot <- function(data, variables, seriesVar="none", timeVar, discSeries=FALSE, timeLabel="time steps", varLabel="value", outType="png", width=1200, height=800, family="serif", ...){

	# "data" is a data frame
	# "variables" is a numeric vector that includes the column numbers referencing those variables to be plotted as y axis
	# "seriesVar" is the number referencing the variable distinguishing series
	# "timeVar" is the number referencing the variable distinguishing time steps
  # "outType" is the string defining the output image type ("none" for printing on R device)
  # "..." stands for "par" arguments
  require(reshape2)
  require(ggplot2)
  
  if (seriesVar == "none"){
    
    data.red <- data[,c(timeVar, variables)]
    data.long <- melt(data.red,id.vars=names(data.red)[1])
    data.long$time <- data.long[,1]
    data.long <- data.long[,-1]
    p <- ggplot(data.long, aes(x = time))
    for (v in levels(data.long$variable)){
      p <- p + geom_line(data=subset(data.long,variable==v), aes(time,value))
    }
    p <- p + facet_grid(variable~.,scales="free_y") + xlab(timeLabel) + ylab(varLabel)
  }else{
    
    data.red <- data[,c(seriesVar, timeVar, variables)]
    data.long <- melt(data.red,id.vars=names(data.red)[c(1, 2)])
    data.long$series <- data.long[,1]
    data.long$time <- data.long[,2]
    data.long <- data.long[,c(-1,-2)]
    if (discSeries == FALSE){
		  p <- ggplot(data.long, aes(x = time, group = series))
		  for (v in levels(data.long$variable)){
			  dt <- subset(data.long,variable==v)
        p <- p + geom_line(data=dt, aes(time, value, colour = series))
		  }
		  p <- p + scale_colour_gradient(low="blue", high="red", name=names(data.red)[1]) + 
			  facet_grid(variable~.,scales="free_y") +
			  xlab(timeLabel)
	  }else{
		  p <- ggplot(data.long, aes(x = time, group = factor(series)))
		  for (v in levels(data.long$variable)){
			  p <- p + geom_point(data=subset(data.long,variable==v), aes(time,value, colour = factor(series)), size = 1)
		  }
		  cl <- rainbow(length(levels(factor(data.long$series))))
		  p <- p + scale_colour_manual(values = cl, name=names(data.red)[1]) + 
			  facet_grid(variable~.,scales="free_y") +
			  xlab(timeLabel)
	  }
  }
  seriesName <- names(data)[seriesVar]
  yNames <- names(data)[variables]
  allYNames <- ""
  for (name in yNames){
    allYNames <- paste(allYNames, name, sep=" AND ")
  }
  plotName <- paste("VarxTimeGgplot - ", seriesName, " x ", allYNames, sep="")
  fileName <- paste(plotName, outType, sep=".")
  if (outType=="none" | outType=="NONE"){
    # image is printed on R device
    return(p)
    readkey()
  } else if (outType=="png" | outType=="PNG"){
    png(filename = fileName, width = width, height = height)
  } else if (outType=="tiff" | outType=="TIFF"){
    tiff(filename = fileName, width = width, height = height)
  } else if (outType=="bmp" | outType=="BMP"){
    bmp(filename = fileName, width = width, height = height)
  } else if (outType=="jpeg" | outType=="JPEG" | outType=="jpg" | outType=="JPG"){
    jpeg(filename = fileName, width = width, height = height)
  } else if (outType=="eps" | outType=="rps"){
    require(extrafont)
    require(extrafontdb)
    require(fontcm)
    loadfonts(device="postscript")
    postscript(fileName , pointsize=12, width=(width/100), height=(height/100) , 
               horizontal=FALSE, paper = "special", family=family)
  } else {
    print("ERROR: threeWayScat only generates outputs with the following formats: png, tiff, bmp, jpg, eps, rps")
  }
  print(p)
  
  dev.off()
}