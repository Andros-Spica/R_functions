threeWayScat <- function(data, xVariable, yVariables, lVariable, lVarFactor = FALSE, lVarAes = c(TRUE,FALSE,FALSE), smooth = TRUE, smoothMethod = "loess", colorGradient = c("red", "blue"), pointColor = "black", pointShape = 20, pointSize = 5, outType = "png", width=700, height=600, family="serif", ...){
  
  # "data" is a data frame
  # "xVariable" is the column position of the variable to be represented in the x axis
  # "yVariables" is the vector of column positions of the variables to be represented in the y axis
  # "lVariable" is the column position of the variable to be represented by the aes of points (legend)
  # "lVarFactor" is a boolean defining if "lVariable" should be plotted as a factor (discretized)
  # "lVarAes" is a set of booleans defining how "lVariable" is represented ("color", "shape", "size")
  # "colorGradient" is a vector with two colors giving two extremes of color variation if lVarAes = (TRUE, ..., ...)  
  # "pointColor" is the color of points if lVarAes = (FALSE, ..., ...)
  # "pointShape" is the shape of points if lVarAes = (..., FALSE, ...)
  # "pointSize" is a number setting the size of points if lVarAes = (..., ..., FALSE)
  # "outType" is the string defining the output image type ("none" for printing on R device)
  # "..." stands for "par" arguments
  
  xVarName <- names(data)[xVariable]
  lVarName <- names(data)[lVariable]
  
  data.temp <- as.data.frame(data[,1])
  data.temp$xVar <- data[,xVariable]
  
  if (lVarFactor == TRUE){
    data.temp$lVar <- factor(data[,lVariable])
  } else {
    data.temp$lVar <- data[,lVariable]
  }
  
  for (v in yVariables){
    data.temp$yVar <- data[,v]
    p <- ggplot(data.temp, aes(x = xVar, y = yVar)) +
      xlab(xVarName) + ylab(names(data)[v])
    # How to represent lVariable?
    if (lVarAes[1] == TRUE & lVarAes[2] == FALSE & lVarAes[3] == FALSE){
      # only color
      p <- p + geom_point(aes(color = lVar), size = pointSize, shape = pointShape) +
        labs(color = lVarName)
      if (lVarFactor == FALSE){
        p <- p + scale_colour_gradient(low = colorGradient[1], high = colorGradient[2])
      }
    } else if (lVarAes[1] == FALSE & lVarAes[2] == TRUE & lVarAes[3] == FALSE){
      # only shape
      p <- p + geom_point(aes(shape = lVar), size = pointSize, color = pointColor) + labs(shape = lVarName)
    } else if (lVarAes[1] == FALSE & lVarAes[2] == FALSE & lVarAes[3] == TRUE){
      # only size
      p <- p + geom_point(aes(size = lVar), shape = pointShape, color = pointColor) + labs(size = lVarName)
    } else if (lVarAes[1] == TRUE & lVarAes[2] == TRUE & lVarAes[3] == FALSE){
      # only color and shape
      p <- p + geom_point(aes(color = lVar, shape = lVar), size = pointSize) + 
        labs(color = lVarName, shape = lVarName)
      if (lVarFactor == FALSE){
        p <- p + scale_colour_gradient(low = colorGradient[1], high = colorGradient[2])
      }
    } else if (lVarAes[1] == TRUE & lVarAes[2] == FALSE & lVarAes[3] == TRUE){
      # only color and size
      p <- p + geom_point(aes(color = lVar, size = lVar), size = pointShape) + 
        labs(color = lVarName, size = lVarName)
      if (lVarFactor == FALSE){
        p <- p + scale_colour_gradient(low = colorGradient[1], high = colorGradient[2])
      }
    } else if (lVarAes[1] == FALSE & lVarAes[2] == TRUE & lVarAes[3] == TRUE){
      # only shape and size
      p <- p + geom_point(aes(shape = lVar, size = lVar), color = pointColor) + 
        labs(shape = lVarName, size = lVarName)
    } else if (lVarAes[1] == TRUE & lVarAes[2] == TRUE & lVarAes[3] == TRUE){
      # color, shape and size
      p <- p + geom_point(aes(color = lVar, shape = lVar, size = lVar)) + 
        labs(color = lVarName, shape = lVarName, size = lVarName)
      if (lVarFactor == FALSE){
        p <- p + scale_colour_gradient(low = colorGradient[1], high = colorGradient[2])
      }
    }
    if (smooth == TRUE){
      p <- p + geom_smooth(method = smoothMethod)
    }
    
    plotName <- paste("threeWayScat - ", lVarName, " x ", xVarName, " x ", names(data)[v], sep="")
    fileName <- paste(plotName, outType, sep=".")
    if (outType=="none" | outType=="NONE"){
      # image is printed on R device
      return(p)
      ### introduce wait user
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
}
    