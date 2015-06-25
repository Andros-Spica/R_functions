dist_igraph <- function(edgeslist, threshold=NULL, k=NULL){
  require(igraph)
  
  if (is.null(threshold)==FALSE){
    # Cut out pairs with distance above the threshold
    filtered.edgeslist = subset(edgeslist, edgeslist[,3] < threshold)    
  }
  if (is.null(k)==FALSE){
    numid = 1:nrow(edgeslist)
    edgeslist2 <- cbind(edgeslist, numid)
    filtered.edgeslist = subset(edgeslist2, edgeslist[,3] < 0)
    # Select only the k edges with smallest distences
    listOfVertex1 = levels(factor(as.character(edgeslist2[,1])))
    listOfVertex2 = levels(factor(as.character(edgeslist2[,2])))
    listOfVertex3 = c(listOfVertex1,listOfVertex2)
    listOfVertex = levels(factor(listOfVertex3))
    print(length(listOfVertex))
    for (i in 1:length(listOfVertex)){
      edgesWithThisVertex = subset(edgeslist2, edgeslist2[,1] == listOfVertex[i] | edgeslist2[,2] == listOfVertex[i])
      subseted = subset(edgesWithThisVertex, edgesWithThisVertex[,3] < 0)
      for (id in 1:nrow(edgesWithThisVertex)){
        if (edgesWithThisVertex$numid[id] %in% filtered.edgeslist$numid == FALSE){
          notchosen = edgesWithThisVertex[id,]
          subseted = rbind(subseted, notchosen)
        }       
      }
      for (sel in 1:k){
        chosen = subset(subseted, subseted[,3] == min(subseted[,3]))
        filtered.edgeslist = rbind(filtered.edgeslist, chosen)
        subseted = subset(subseted, subseted$numid != chosen$numid[1])
      }
    }
    print(str(filtered.edgeslist))
  }
  
  # create object graph
  distNet = graph.data.frame(filtered.edgeslist, directed=F)
  return(distNet)
}
