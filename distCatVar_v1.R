
distCatVar <- function(catVar, asNa="NULL", naValues="random"){
  ### Calculates a distance based on a categorical variable
  # "catVar" is the column/vector of the categorical variable
  # "asNa" is a string with a value to be considered a NA value
  # "naValues" is a string indicating how to calculte the distance to a NA value
  #           "random": Na values are replaced by a value ramdomly sampled (out of the values of the variable)
  #           "mode": Na values are replaced by the mode of the variable
  if (asNa!="NULL"){
    catVar[catVar==asNa] <- NA
  }
  catVarLevels <- levels(catVar[catVar!=NA])
  if (naValues=="random"){
    catVar[is.na(catVar)] <- sample(catVarLevels, 1)
  } else if (naValues=="mode"){
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    md <- Mode(catVar)
    catVar[is.na(catVar)] <- md
  } else {
    print("ERROR: Method passed for naValues is incorrect")
  }
  catVarRange <- max(as.numeric(catVar)) - min(as.numeric(catVar))
  
  dist <- c()
  for (i in 1:length(catVar)){
    for (j in 1:length(catVar)){
      if(i < j){
        d = abs(as.numeric(catVar[i]) - as.numeric(catVar[j])) # distance
        dist = append(dist, d) # attach distance
      }
    }
  }
  return(dist)
}