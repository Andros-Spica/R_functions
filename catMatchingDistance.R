catMatchingDistance <- function(catVar){
  ### Calculates a distance based on a boolean assessment of a categorical variable (not dichotomic)
  # "catVar" is the column/vector of the categorical variable
  
  dist <- c()
  d = 0
  for (i in 1:length(catVar)){
    for (j in 1:length(catVar)){
      if(i < j){
        if (catVar[i]==catVar[j]){
          d = 0
        } else {
          d = 1
        }     
        dist = append(dist, d) # attach distance
      }
    }
  }
  return(dist)
}