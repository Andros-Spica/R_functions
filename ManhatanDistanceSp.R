
ManhatanDistanceSp <- function(catVar, singDif="NULL", selfDif="NULL"){
  ### Calculates a Special Manhatan distance based on a ordinate categorical variable
  # "catVar" is the column/vector of the categorical variable
  # "singDif" is the value to be interpreted as singularly different (it has the maximum distance to all other values)
  # "selfDif" is the value to be interpreted as self different (it has the maximum distance to other values and itself)
  
  catVarRange <- max(as.numeric(catVar)) - min(as.numeric(catVar))
  dist <- c()
  d = 0
  for (i in 1:length(catVar)){
    for (j in 1:length(catVar)){
      if(i < j){
        if (catVar[i]==selfDif | catVar[j]==selfDif){
          d = catVarRange
        } else if (catVar[i]==singDif | catVar[j]==singDif){
          if (catVar[i]==singDif & catVar[j]==singDif){
            d = 0
          } else {
            d = catVarRange
          }
        } else {
          d = abs(as.numeric(catVar[i]) - as.numeric(catVar[j])) # Manhatan distance
        }     
        dist = append(dist, d) # attach distance
      }
    }
  }  
  return(dist)
}