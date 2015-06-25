summaryVarPerPair <- function(data, posX, posY){

summaryPerPair <- c(1:30)

dt.posX <- as.numeric(levels(factor(data[,posX])))
dt.posY <- as.numeric(levels(factor(data[,posY])))

for (x in dt.posX ){
  for (y in dt.posY ){
#     cat(i,"x",j, "\n")
    dt <- data[data[,posX]==x & data[,posY]==y,]
    
    meanCountLandUseF <- mean(dt$countLandUseF)
    sdCountLandUseF <- sd(dt$countLandUseF)
    
    meanDilemmaEvents <- mean(dt$dilemmaEvents)
    sdDilemmaEvents <- sd(dt$dilemmaEvents)

    meanLandUseChangeEvents <- mean(dt$landUseChangeEvents)
    sdLandUseChangeEvents <- sd(dt$landUseChangeEvents)
    
    meanCountCentersF <- mean(dt$countCentersF)
    sdCountCentersF <- sd(dt$countCentersF)

    meanCountCentersH <- mean(dt$countCentersH)
    sdCountCentersH <- sd(dt$countCentersH)

    meanBigGroupFSize <- mean(dt$bigGroupSizeF,na.rm=T)
    sdBigGroupFSize <- sd(dt$bigGroupSizeF,na.rm=T)
    
    meanBigGroupHSize <- mean(dt$bigGroupSizeH,na.rm=T)
    sdBigGroupHSize <- sd(dt$bigGroupSizeH,na.rm=T)
    
    meanMeanGroupSizeF <- mean(dt$meanGroupSizeF,na.rm=T)
    sdMeanGroupSizeF <- sd(dt$meanGroupSizeF,na.rm=T)
    
    meanMeanGroupSizeH <- mean(dt$meanGroupSizeH,na.rm=T)
    sdMeanGroupSizeH <- sd(dt$meanGroupSizeH,na.rm=T)
    
    meanMeanIndepFF <- mean(dt$meanIndepFF,na.rm=T)
    sdMeanIndepFF <- sd(dt$meanIndepFF,na.rm=T)

    meanMeanIndepFH <- mean(dt$meanIndepFH,na.rm=T)
    sdMeanIndepFH <- sd(dt$meanIndepFH,na.rm=T)
    
    meanMeanIndepHH <- mean(dt$meanIndepHH,na.rm=T)
    sdMeanIndepHH <- sd(dt$meanIndepHH,na.rm=T)

    meanMeanIndepHF <- mean(dt$meanIndepHF,na.rm=T)
    sdMeanIndepHF <- sd(dt$meanIndepHF,na.rm=T)

    meanMoransI <- mean(dt$MoransI)
    sdMoransI <- sd(dt$MoransI)
    
    pt <- c(x,y,meanCountLandUseF, sdCountLandUseF,
            meanDilemmaEvents, sdDilemmaEvents,
		meanLandUseChangeEvents, sdLandUseChangeEvents,
		meanCountCentersF, sdCountCentersF,
		meanCountCentersH, sdCountCentersH,
            meanBigGroupFSize, sdBigGroupFSize,
            meanBigGroupHSize, sdBigGroupHSize,
            meanMeanGroupSizeF, sdMeanGroupSizeF,
            meanMeanGroupSizeH, sdMeanGroupSizeH,
            meanMeanIndepFF, sdMeanIndepFF,
		meanMeanIndepFH, sdMeanIndepFH,
            meanMeanIndepHH, sdMeanIndepHH,
		meanMeanIndepHF, sdMeanIndepHF,
		meanMoransI, sdMoransI)
    
    summaryPerPair <- rbind(summaryPerPair, pt)
  }
}
summaryPerPair <- as.data.frame(summaryPerPair[-1,])
names(summaryPerPair)<-c("x","y","meanCountLandUseF", "sdCountLandUseF",
                              "meanDilemmaEvents", "sdDilemmaEvents",
					"meanLandUseChangeEvents", "sdLandUseChangeEvents",
					"meanCountCentersF", "sdCountCentersF",
					"meanCountCentersH", "sdCountCentersH",
                              "meanBigGroupFSize", "sdBigGroupFSize",
                              "meanBigGroupHSize", "sdBigGroupHSize",
                              "meanMeanGroupSizeF", "sdMeanGroupSizeF",
                              "meanMeanGroupSizeH", "sdMeanGroupSizeH",
                              "meanMeanIndepFF", "sdMeanIndepFF",
                              "meanMeanIndepFH", "sdMeanIndepFH",
					"meanMeanIndepHH", "sdMeanIndepHH",
					"meanMeanIndepHF", "sdMeanIndepHF",
					"meanMoransI", "sdMoransI")

numPairs <- length(levels(data[,posX]))*length(levels(data[,posY]))
#row.names(summaryPerPair) <- 1:numPairs

return(summaryPerPair)
}
