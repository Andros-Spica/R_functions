replaceNas <- function(variable, type="categorical", asNa=c("NULL"), method="NULL"){
  ### Replaces Na values
  # variable: a single vector
  # type: the type of data (categorical, numerical)
  # asNa: specifies which values may be considered Nas
  # method: the method used for choosing the replacement (NULL, random, mode, normal ...)
  # NOTE: method must be compatible with type
  
  for (i in 1:length(asNa)){
    variable[variable==asNa[i]] <- NA
  }
  if (method=="NULL"){
    return(variable)
  }
  if (type=="categorical"){
    variableLevels <- levels(variable[variable!=NA])
    if (method=="random"){
      variable[is.na(variable)] <- sample(variableLevels, 1)
    } else if (method=="mode"){
      Mode <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
      }
      md <- Mode(catVar)
      catVar[is.na(catVar)] <- md
    } else {
      print("ERROR: Method passed for naValues is incorrect")
    }
  } else if (type=="numerical"){
    if (method=="random"){
      minV = min(variable, na.rm=TRUE)
      maxV = max(variable, na.rm=TRUE)
      if (minV==maxV){
        variable[is.na(variable)] <- minV
      } else {
        variable[is.na(variable)] <- runif(1, minV, maxV)
      }
    } else if (method=="normal"){
      meanV = mean(variable, na.rm=TRUE)
      sdV = sd(variable, na.rm=TRUE)
      variable[is.na(variable)] <- rnorm(1, mean = meanV, sd = sdV)
    } else {
      print("ERROR: Method passed for naValues is incorrect")
    }
  }
  return(variable)
}