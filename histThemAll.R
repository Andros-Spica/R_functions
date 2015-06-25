histThemAll <- function(data, histXpage=6, histXrow=3, outType, width=1200, height=600, family="serif", ...){
  
  # "data" is a data frame
  # "..." stands for "par" arguments
  numOfPages <- ceiling(ncol(data) / histXpage)
  histXcol <- ceiling(histXpage / histXrow)
  for (p in 1:numOfPages){
    pageName <- paste("histThemAll_page", as.character(p), sep="")
    fileName <- paste(pageName, outType, sep=".")
    startHist <- 1 + ((p - 1) * histXpage)
    if (p == numOfPages){
      endHist <- ncol(data)
    } else { 
      endHist <- startHist + (histXpage - 1)
    }
    
    if (outType=="png" | outType=="PNG"){
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
      print("ERROR: histThemAll only generates outputs with the following formats: png, tiff, bmp, jpg, eps, rps")
    }
    
    #par(cex.main = .8 , cex.lab = .8 , cex.axis=.65, font.main = 4 , font.lab = 3,
    #    mar = c(1,1,0.5,1), oma=c(2,2,1,1), yaxs="i" , family=family)
    
    layout(matrix(1:histXpage,histXcol,histXrow))
    for (i in startHist:endHist){
      namevar <- names(data)[[i]]
      plot(data[,i], main=namevar)
    }
    dev.off()
  }
}
  