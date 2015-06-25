test.dist<-function(x,dist,factor){
  
  require(vegan)
  
  test.anosim <- anosim(dist,factor)
  x$test.anosim <- test.anosim
  
  test.betadisp <- permutest(betadisper(dist,factor),pairwise = TRUE)
  x$test.betadisp <- test.betadisp
  
  test.permanova <- adonis(dist~factor)
  
  x$test.permanova<-test.permanova
  
  return(x)
}