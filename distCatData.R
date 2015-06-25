distCatData <- function(data, method="Manhatan", stdScale=TRUE){
  ### Calculates distance based on several categorical variables
  # "data" is the dataframe of categorical variables with one id variable (the first column)
  # "method" is the approach used to calculate distance ("Manhatan", "Category Matching", "Euclidean", "Squared Euclidean")
  source(file="functions/ManhatanDistanceSp.R")
  source(file="functions/catMatchingDistance.R")
  
  # create main table with start and end nodes
  caseNames <- data[,1]
  a <- c()
  b <- c()
  for (i in 1:length(caseNames)) {
    for (ii in 1:length(caseNames)){
      if (i < ii) {
        a = append(a, as.character(caseNames[i])) # name of case i
        b = append(b, as.character(caseNames[ii])) # name of case ii
      }
    }
  }
  df1 <- as.data.frame(cbind(a,b))
  # fill one column of pairwise distances per categorical varible in the original data frame
  for (v in 2:ncol(data)){
    if (method=="Category Matching"){
      temp <- catMatchingDistance(data[,v])
    } else {
      temp <- ManhatanDistanceSp(factor(data[,v]), singDif="none", selfDif="indeterminate")
    }
    
    if (method=="Euclidean" | method=="Squared Euclidean"){
      temp <- temp^2
    }
    
    if (stdScale==TRUE & method!="Euclidean"){
      if (min(temp)!=max(temp)){
        temp <- (temp - min(temp)) / (max(temp) - min(temp))
      }    
    }
    
    df1 <- cbind(df1, temp)
    numCol <- 2 + v - 1
    names(df1)[numCol] <- names(data)[v]   
  }
  # sum distances per pairs
  df2 <- df1[,c(1,2)] # copy pair names
  for (i in 1:nrow(df1)){
    distancesOfPair <- vector()
    for (v in 3:ncol(df1)){
      distancesOfPair <- c(distancesOfPair, df1[i,v])
    }
    df2[i,3] <- sum(distancesOfPair) # add sum in the third column
  }
  names(df2)[3] <- "collapsedDistance"
  
  if (method=="Euclidean"){
    df2[,3] <- sqrt(df2[,3])
  }
  
  if (stdScale==TRUE){
    df2[,3] <- (df2[,3] - min(df2[,3])) / (max(df2[,3]) - min(df2[,3]))
  }
  
  # return table with distances
  return(df2)
}